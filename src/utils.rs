// Copyright 2022 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

//! Utility functions used by several parts of this crate.
//!
//! This module is for anything that doesn't fit into the other top-level modules. Try not to add
//! new code here unless it really doesn't belong anywhere else.

use std::io::Cursor;
use std::io::Seek;
use std::os::fd::OwnedFd;

use bytes::Buf;

use crate::codec::h264::nalu::Header;
use crate::codec::h264::nalu_reader;
use crate::codec::h264::parser::Nalu as H264Nalu;
use crate::codec::h265::parser::Nalu as H265Nalu;
use crate::codec::h265::parser::NaluType as H265NaluType;
use crate::decoder::stateless::DecodeError;
use crate::decoder::stateless::StatelessVideoDecoder;
use crate::decoder::BlockingMode;
use crate::decoder::DecodedHandle;
use crate::decoder::DecoderEvent;
use crate::decoder::StreamInfo;
use crate::DecodedFormat;
use crate::Fourcc;
use crate::PlaneLayout;
use crate::Resolution;
use crate::SurfaceLayout;

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

/// Iterator over groups of Nalus that can contain a whole frame.
pub struct H264FrameIterator<'a>(Cursor<&'a [u8]>);

impl<'a> H264FrameIterator<'a> {
    pub fn new(stream: &'a [u8]) -> Self {
        Self(Cursor::new(stream))
    }
}

impl<'a> Iterator for H264FrameIterator<'a> {
    type Item = &'a [u8];

    fn next(&mut self) -> Option<Self::Item> {
        H264Nalu::next(&mut self.0)
            .map(|n| {
                let start = n.sc_offset();
                let end = n.offset() + n.size();
                &n.data()[start..end]
            })
            .ok()
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
            nalu.header().nalu_type(),
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
            nalu.header().nalu_type(),
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
            nalu.header().nalu_type(),
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

            // first_slice_segment_in_pic
            r.read_bit().expect("Malformed slice")
        } else {
            false
        }
    }

    fn collect(&mut self, include_current_nalu: bool) -> Option<H265AccessUnit<T>> {
        let len = if include_current_nalu {
            self.nalus.len()
        } else {
            self.nalus.len() - 1
        };

        let au = H265AccessUnit {
            nalus: self.nalus.drain(..len).collect::<Vec<_>>(),
        };

        log::debug!("Collecting access unit: (Nalu, Size): {:?}", {
            au.nalus
                .iter()
                .map(|nalu| (nalu.header().nalu_type(), nalu.size()))
                .collect::<Vec<_>>()
        });

        let no_slices_left = !self.nalus.iter().any(|nalu| Self::is_slice(nalu));

        if no_slices_left {
            // Wait for a new slice.
            self.picture_started = false;
        }

        Some(au)
    }

    /// Accumulates NALUs into Access Units based on gsth265parser's heuristics
    /// and on "F.7.4.2.4.4 Order of NAL units and coded pictures and
    /// association to access units" to accumulate NAL units.
    pub fn accumulate(&mut self, nalu: H265Nalu<T>) -> Option<H265AccessUnit<T>> {
        // Accumulate until we see a slice.
        self.nalus.push(nalu);
        let nalu = self.nalus.last().unwrap();

        if Self::is_delimiter(nalu) && self.picture_started {
            return self.collect(true);
        }

        if !self.picture_started {
            self.picture_started = Self::is_slice(nalu);
            return None;
        }

        if Self::specifies_new_au(nalu) {
            return self.collect(false);
        }
        if Self::is_slice_of_next_au(nalu) {
            return self.collect(false);
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
        while let Ok(nalu) = H265Nalu::next(&mut self.cursor) {
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
    on_new_frame: &mut dyn FnMut(Box<dyn DecodedHandle<Descriptor = M>>),
    allocate_new_surfaces: &mut dyn FnMut(&StreamInfo, usize) -> anyhow::Result<Vec<M>>,
    output_format: DecodedFormat,
    blocking_mode: BlockingMode,
) -> anyhow::Result<()>
where
    D: StatelessVideoDecoder<M> + ?Sized,
    R: AsRef<[u8]>,
    I: Iterator<Item = R>,
{
    // Closure that drains all pending decoder events and calls `on_new_frame` on each
    // completed frame.
    let mut check_events = |decoder: &mut D| -> anyhow::Result<()> {
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
                        )?;
                        let pool = format_setter.surface_pool();
                        pool.add_surfaces(surfaces).unwrap();
                    }
                }
            }
        }

        Ok(())
    };

    for (frame_num, packet) in stream_iter.enumerate() {
        let mut bitstream = packet.as_ref();
        loop {
            match decoder.decode(frame_num as u64, bitstream) {
                Ok(bytes_decoded) => {
                    bitstream = &bitstream[bytes_decoded..];

                    if blocking_mode == BlockingMode::Blocking {
                        check_events(decoder)?;
                    }

                    if bitstream.is_empty() {
                        // Break the loop so we can process the next NAL if we sent the current one
                        // successfully.
                        break;
                    }
                }
                Err(DecodeError::CheckEvents) | Err(DecodeError::NotEnoughOutputBuffers(_)) => {
                    check_events(decoder)?
                }
                Err(e) => anyhow::bail!(e),
            }
        }
    }

    decoder.flush()?;
    check_events(decoder)
}

/// Surface allocation callback that results in self-allocated memory.
pub fn simple_playback_loop_owned_surfaces(
    _: &StreamInfo,
    nb_surfaces: usize,
) -> anyhow::Result<Vec<()>> {
    Ok(vec![(); nb_surfaces])
}

/// Surface allocation callback that returns user-allocated memory for the surfaces.
pub fn simple_playback_loop_userptr_surfaces(
    stream_info: &StreamInfo,
    nb_surfaces: usize,
) -> anyhow::Result<Vec<UserPtrSurface>> {
    let alloc_function = match stream_info.format {
        DecodedFormat::I420 | DecodedFormat::NV12 => &UserPtrSurface::new_nv12,
        _ => anyhow::bail!(
            "{:?} format is unsupported with user memory",
            stream_info.format
        ),
    };

    Ok((0..nb_surfaces)
        .map(|_| alloc_function(stream_info.coded_resolution))
        .collect::<Vec<_>>())
}

/// A structure that holds user-allocated memory for a surface as well as its layout.
#[derive(Debug)]
pub struct UserPtrSurface {
    pub buffers: Vec<*mut u8>,
    pub mem_layout: std::alloc::Layout,
    pub layout: SurfaceLayout,
}

impl UserPtrSurface {
    /// Allocate enough memory to back a NV12 surface of `size` dimension.
    pub fn new_nv12(size: Resolution) -> Self {
        /// Add what is needed to a value in order to make it a multiple of some alignment.
        macro_rules! align {
            ($n:expr, $r:expr) => {
                ($n + ($r - 1)) & !($r - 1)
            };
        }

        // Align according to VAAPI constraints.
        let width = align!(size.width, 16) as usize;
        let height = align!(size.height, 4) as usize;
        let stride = align!(width, 64);
        let uv_start = height * stride;
        let uv_size = (height / 2) * stride;
        let buffer_size = align!(uv_start + uv_size, 4096);

        // Safe because the invariants of `Layout` are respected.
        let layout = unsafe { std::alloc::Layout::from_size_align_unchecked(buffer_size, 4096) };
        let mem = unsafe { std::alloc::alloc(layout) };

        Self {
            buffers: vec![mem],
            mem_layout: layout,
            layout: SurfaceLayout {
                format: (Fourcc::from(b"NV12"), 0),
                size: Resolution::from((width as u32, height as u32)),
                planes: vec![
                    PlaneLayout {
                        buffer_index: 0,
                        offset: 0,
                        stride,
                    },
                    PlaneLayout {
                        buffer_index: 0,
                        offset: uv_start,
                        stride,
                    },
                ],
            },
        }
    }
}

pub struct DmabufSurface {
    pub fds: Vec<OwnedFd>,
    pub layout: SurfaceLayout,
}

impl Drop for UserPtrSurface {
    fn drop(&mut self) {
        for buffer in std::mem::take(&mut self.buffers).into_iter() {
            // Safe because we allocated the memory using `std::alloc::alloc`.
            unsafe { std::alloc::dealloc(buffer, self.mem_layout) }
        }
    }
}
