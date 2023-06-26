// Copyright 2022 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

//! Utility functions used by several parts of this crate.
//!
//! This module is for anything that doesn't fit into the other top-level modules. Try not to add
//! new code here unless it really doesn't belong anywhere else.

use std::io::Cursor;
use std::io::Seek;

use bytes::Buf;

use crate::codec::h264::nalu::Header;
use crate::codec::h264::nalu_reader;
use crate::codec::h264::parser::Nalu;
use crate::codec::h264::parser::NaluType;
use crate::codec::h265::parser::Nalu as H265Nalu;
use crate::codec::h265::parser::NaluType as H265NaluType;
use crate::decoder::stateless::DecodeError;
use crate::decoder::stateless::StatelessVideoDecoder;
use crate::decoder::BlockingMode;
use crate::decoder::DecodedHandle;
use crate::decoder::DecoderEvent;
use crate::decoder::StreamInfo;
use crate::DecodedFormat;

/// Iterator over IVF packets.
pub struct IvfIterator<'a> {
    cursor: Cursor<&'a [u8]>,
}

impl<'a> IvfIterator<'a> {
    pub fn new(data: &'a [u8]) -> Self {
        let mut cursor = Cursor::new(data);

        // Skip the IVH header entirely.
        cursor.seek(std::io::SeekFrom::Start(32)).unwrap();

        Self { cursor }
    }
}

impl<'a> Iterator for IvfIterator<'a> {
    type Item = &'a [u8];

    fn next(&mut self) -> Option<Self::Item> {
        // Make sure we have a header.
        if self.cursor.remaining() < 6 {
            return None;
        }

        let len = self.cursor.get_u32_le() as usize;
        // Skip PTS.
        let _ = self.cursor.get_u64_le();

        if self.cursor.remaining() < len {
            return None;
        }

        let start = self.cursor.position() as usize;
        let _ = self.cursor.seek(std::io::SeekFrom::Current(len as i64));
        let end = self.cursor.position() as usize;

        Some(&self.cursor.get_ref()[start..end])
    }
}

/// A H.264 Access Unit.
#[derive(Debug, Default)]
struct AccessUnit<T> {
    pub nalus: Vec<Nalu<T>>,
}

/// A parser that produces Access Units from a list of NALUs. It does not use
/// section 7.4.1.2.4 of the specification for the detection of the first VCL
/// NAL unit of a primary coded picture and instead uses an heuristic from
/// GStreamer that works well enough for most streams.
#[derive(Debug, Default)]
struct AccessUnitParser<T> {
    picture_started: bool,
    nalus: Vec<Nalu<T>>,
}

impl<T: AsRef<[u8]>> AccessUnitParser<T> {
    /// Use GStreamer's gsth264parse's heuristic to break into access units.
    /// Only yields back an access unit if:
    /// We had previously established that a picture had started and an AUD is seen.
    /// We had previously established that a picture had started, but SEI|SPS|PPS is seen.
    /// We had previously established that a picture had started, and the
    /// current slice refers to the next picture.
    pub fn accumulate(&mut self, nalu: Nalu<T>) -> Option<AccessUnit<T>> {
        if matches!(nalu.header().nalu_type(), NaluType::AuDelimiter) && self.picture_started {
            self.picture_started = false;
            return Some(AccessUnit {
                nalus: self.nalus.drain(..).collect::<Vec<_>>(),
            });
        }

        self.nalus.push(nalu);
        let nalu = self.nalus.last().unwrap();

        if !self.picture_started {
            self.picture_started = matches!(
                nalu.header().nalu_type(),
                NaluType::Slice
                    | NaluType::SliceDpa
                    | NaluType::SliceDpb
                    | NaluType::SliceDpc
                    | NaluType::SliceIdr
                    | NaluType::SliceExt
            );

            return None;
        }

        match nalu.header().nalu_type() {
            NaluType::Sei | NaluType::Sps | NaluType::Pps => {
                self.picture_started = false;
                Some(AccessUnit {
                    nalus: self.nalus.drain(..).collect::<Vec<_>>(),
                })
            }
            NaluType::Slice | NaluType::SliceDpa | NaluType::SliceIdr => {
                let data = nalu.as_ref();
                let mut r = nalu_reader::NaluReader::new(&data[nalu.header().len()..]);
                let first_mb_in_slice = r.read_ue::<u32>();

                if first_mb_in_slice.is_ok() && first_mb_in_slice.unwrap() == 0 {
                    Some(AccessUnit {
                        nalus: self.nalus.drain(..self.nalus.len() - 1).collect::<Vec<_>>(),
                    })
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

/// Iterator over groups of Nalus that can contain a whole frame.
pub struct H264FrameIterator<'a> {
    cursor: Cursor<&'a [u8]>,
    aud_parser: AccessUnitParser<&'a [u8]>,
}

impl<'a> H264FrameIterator<'a> {
    pub fn new(stream: &'a [u8]) -> Self {
        Self {
            cursor: Cursor::new(stream),
            aud_parser: Default::default(),
        }
    }
}

impl<'a> Iterator for H264FrameIterator<'a> {
    type Item = &'a [u8];

    fn next(&mut self) -> Option<Self::Item> {
        while let Ok(Some(nalu)) = Nalu::next(&mut self.cursor) {
            if let Some(access_unit) = self.aud_parser.accumulate(nalu) {
                let start_nalu = access_unit.nalus.first().unwrap();
                let end_nalu = access_unit.nalus.last().unwrap();

                let start_offset = start_nalu.sc_offset();
                let end_offset = end_nalu.offset() + end_nalu.size();

                let data = &self.cursor.get_ref()[start_offset..end_offset];

                return Some(data);
            }
        }

        // Process any left over NALUs, even if we could not fit them into an AU using the
        // heuristic.
        if !self.aud_parser.nalus.is_empty() {
            let nalus = self.aud_parser.nalus.drain(..).collect::<Vec<_>>();
            let start_nalu = nalus.first().unwrap();
            let end_nalu = nalus.last().unwrap();

            let start_offset = start_nalu.sc_offset();
            let end_offset = end_nalu.offset() + end_nalu.size();

            let data = &self.cursor.get_ref()[start_offset..end_offset];

            Some(data)
        } else {
            None
        }
    }
}

/// A H.265 Access Unit.
#[derive(Debug, Default)]
struct H265AccessUnit<T> {
    pub nalus: Vec<H265Nalu<T>>,
}

#[derive(Debug, Default)]
struct H265AccessUnitParser<T> {
    picture_started: bool,
    nalus: Vec<H265Nalu<T>>,
}

impl<T: AsRef<[u8]>> H265AccessUnitParser<T> {
    /// Whether this is a slice type.
    fn is_slice(nalu: &H265Nalu<T>) -> bool {
        matches!(
            nalu.header().type_(),
            H265NaluType::BlaWLp
                | H265NaluType::BlaWRadl
                | H265NaluType::BlaNLp
                | H265NaluType::IdrWRadl
                | H265NaluType::IdrNLp
                | H265NaluType::TrailN
                | H265NaluType::TrailR
                | H265NaluType::TsaN
                | H265NaluType::TsaR
                | H265NaluType::StsaN
                | H265NaluType::StsaR
                | H265NaluType::RadlN
                | H265NaluType::RadlR
                | H265NaluType::RaslN
                | H265NaluType::RaslR
                | H265NaluType::CraNut
        )
    }

    /// Whether this is a delimiter, which would automatically start a new AU.
    fn is_delimiter(nalu: &H265Nalu<T>) -> bool {
        matches!(
            nalu.header().type_(),
            H265NaluType::AudNut | H265NaluType::EobNut | H265NaluType::EosNut
        )
    }

    /// Whether this specifies a new AU, regardless of being one of the
    /// delimiters as per `is_delimiter`. See this paragraph in F.7.4.2.4.4:
    ///
    /// The first of any of the following NAL units preceding the first VCL NAL
    /// unit firstVclNalUnitInAu and succeeding the last VCL NAL unit preceding
    /// firstVclNalUnitInAu, if any, specifies the start of a new access unit:
    fn specifies_new_au(nalu: &H265Nalu<T>) -> bool {
        matches!(
            nalu.header().type_(),
            H265NaluType::PrefixSeiNut
                | H265NaluType::VpsNut
                | H265NaluType::SpsNut
                | H265NaluType::PpsNut
                | H265NaluType::RsvNvcl41
                | H265NaluType::RsvNvcl42
                | H265NaluType::RsvNvcl43
                | H265NaluType::RsvNvcl44
        )
    }

    /// Whether this is a slice of the next access unit.
    fn is_slice_of_next_au(nalu: &H265Nalu<T>) -> bool {
        if Self::is_slice(nalu) {
            let data = nalu.as_ref();
            let mut r = nalu_reader::NaluReader::new(&data[nalu.header().len()..]);
            let first_slice_segment_in_pic = r.read_bit().expect("Malformed slice");

            first_slice_segment_in_pic
        } else {
            false
        }
    }

    fn collect(&mut self) -> Option<H265AccessUnit<T>> {
        self.picture_started = false;
        return Some(H265AccessUnit {
            nalus: self.nalus.drain(..).collect::<Vec<_>>(),
        });
    }

    /// Accumulates NALUs into Access Units based on gsth265parser's heuristics
    /// and on "F.7.4.2.4.4 Order of NAL units and coded pictures and
    /// association to access units" to accumulate NAL units.
    pub fn accumulate(&mut self, nalu: H265Nalu<T>) -> Option<H265AccessUnit<T>> {
        if Self::is_delimiter(&nalu) && self.picture_started {
            return self.collect();
        }

        // Accumulate until we see a slice.
        self.nalus.push(nalu);
        let nalu = self.nalus.last().unwrap();

        if !self.picture_started {
            self.picture_started = Self::is_slice(&nalu);
            return None;
        }

        if Self::specifies_new_au(&nalu) || Self::is_slice_of_next_au(&nalu) {
            return self.collect();
        }

        None
    }
}

/// Iterator over groups of Nalus that can contain a whole frame.
pub struct H265FrameIterator<'a> {
    cursor: Cursor<&'a [u8]>,
    aud_parser: H265AccessUnitParser<&'a [u8]>,
}

impl<'a> H265FrameIterator<'a> {
    pub fn new(stream: &'a [u8]) -> Self {
        Self {
            cursor: Cursor::new(stream),
            aud_parser: Default::default(),
        }
    }
}

impl<'a> Iterator for H265FrameIterator<'a> {
    type Item = &'a [u8];

    fn next(&mut self) -> Option<Self::Item> {
        while let Ok(Some(nalu)) = H265Nalu::next(&mut self.cursor) {
            if let Some(access_unit) = self.aud_parser.accumulate(nalu) {
                let start_nalu = access_unit.nalus.first().unwrap();
                let end_nalu = access_unit.nalus.last().unwrap();

                let start_offset = start_nalu.sc_offset();
                let end_offset = end_nalu.offset() + end_nalu.size();

                let data = &self.cursor.get_ref()[start_offset..end_offset];

                return Some(data);
            }
        }

        // Process any left over NALUs, even if we could not fit them into an AU using the
        // heuristic.
        if !self.aud_parser.nalus.is_empty() {
            let nalus = self.aud_parser.nalus.drain(..).collect::<Vec<_>>();
            let start_nalu = nalus.first().unwrap();
            let end_nalu = nalus.last().unwrap();

            let start_offset = start_nalu.sc_offset();
            let end_offset = end_nalu.offset() + end_nalu.size();

            let data = &self.cursor.get_ref()[start_offset..end_offset];

            Some(data)
        } else {
            None
        }
    }
}

/// Simple decoding loop that plays the stream once from start to finish.
pub fn simple_playback_loop<D, R, I, M>(
    decoder: &mut D,
    stream_iter: I,
    on_new_frame: &mut dyn FnMut(Box<dyn DecodedHandle>),
    allocate_new_surfaces: &mut dyn FnMut(&StreamInfo, usize) -> Vec<M>,
    output_format: DecodedFormat,
    blocking_mode: BlockingMode,
) where
    D: StatelessVideoDecoder<M> + ?Sized,
    R: AsRef<[u8]>,
    I: Iterator<Item = R>,
{
    // Closure that drains all pending decoder events and calls `on_new_frame` on each
    // completed frame.
    let mut check_events = |decoder: &mut D| {
        while let Some(event) = decoder.next_event() {
            match event {
                DecoderEvent::FrameReady(frame) => {
                    on_new_frame(frame);
                }
                DecoderEvent::FormatChanged(mut format_setter) => {
                    format_setter.try_format(output_format).unwrap();
                    // Allocate the missing number of buffers in our pool for decoding to succeed.
                    let min_num_surfaces = format_setter.stream_info().min_num_surfaces;
                    let pool_num_surfaces = format_setter.surface_pool().num_managed_surfaces();
                    if pool_num_surfaces < min_num_surfaces {
                        let surfaces = allocate_new_surfaces(
                            format_setter.stream_info(),
                            min_num_surfaces - pool_num_surfaces,
                        );
                        let pool = format_setter.surface_pool();
                        pool.add_surfaces(surfaces).unwrap();
                    }
                }
            }
        }
    };

    for (frame_num, packet) in stream_iter.enumerate() {
        loop {
            match decoder.decode(frame_num as u64, packet.as_ref()) {
                Ok(()) => {
                    if blocking_mode == BlockingMode::Blocking {
                        check_events(decoder);
                    }
                    // Break the loop so we can process the next NAL if we sent the current one
                    // successfully.
                    break;
                }
                Err(DecodeError::CheckEvents) => check_events(decoder),
                Err(e) => panic!("{:#}", e),
            }
        }
    }

    decoder.flush();
    check_events(decoder);
}

/// Surface allocation callback that results in self-allocated memory.
pub fn simple_playback_loop_owned_surfaces(_: &StreamInfo, nb_surfaces: usize) -> Vec<()> {
    vec![(); nb_surfaces]
}
