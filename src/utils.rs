// Copyright 2022 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

//! Utility functions used by several parts of this crate.
//!
//! This module is for anything that doesn't fit into the other top-level modules. Try not to add
//! new code here unless it really doesn't belong anywhere else.

use std::borrow::Cow;
use std::io::Cursor;
use std::io::Seek;
use std::io::Write;
use std::marker::PhantomData;
use std::os::fd::OwnedFd;

use bytes::Buf;
use thiserror::Error;

use crate::codec::h264::parser::Nalu as H264Nalu;
use crate::codec::h265::parser::Nalu as H265Nalu;
use crate::decoder::stateless::DecodeError;
use crate::decoder::stateless::PoolLayer;
use crate::decoder::stateless::StatelessVideoDecoder;
use crate::decoder::BlockingMode;
use crate::decoder::DecodedHandle;
use crate::decoder::DecoderEvent;
use crate::decoder::FramePool;
use crate::decoder::StreamInfo;
use crate::DecodedFormat;
use crate::Fourcc;
use crate::FrameLayout;
use crate::PlaneLayout;
use crate::Resolution;

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

/// Helper struct for synthesizing IVF file header
pub struct IvfFileHeader {
    pub magic: [u8; 4],
    pub version: u16,
    pub header_size: u16,
    pub codec: [u8; 4],
    pub width: u16,
    pub height: u16,
    pub framerate: u32,
    pub timescale: u32,
    pub frame_count: u32,
    pub unused: u32,
}

impl Default for IvfFileHeader {
    fn default() -> Self {
        Self {
            magic: Self::MAGIC,
            version: 0,
            header_size: 32,
            codec: Self::CODEC_VP9,
            width: 320,
            height: 240,
            framerate: 1,
            timescale: 1000,
            frame_count: 1,
            unused: Default::default(),
        }
    }
}

impl IvfFileHeader {
    pub const MAGIC: [u8; 4] = *b"DKIF";
    pub const CODEC_VP8: [u8; 4] = *b"VP80";
    pub const CODEC_VP9: [u8; 4] = *b"VP90";
    pub const CODEC_AV1: [u8; 4] = *b"AV01";

    pub fn new(codec: [u8; 4], width: u16, height: u16, framerate: u32, frame_count: u32) -> Self {
        let default = Self::default();

        Self {
            codec,
            width,
            height,
            framerate: framerate * default.timescale,
            frame_count,
            ..default
        }
    }
}

impl IvfFileHeader {
    /// Writes header into writer
    pub fn writo_into(&self, writer: &mut impl std::io::Write) -> std::io::Result<()> {
        writer.write_all(&self.magic)?;
        writer.write_all(&self.version.to_le_bytes())?;
        writer.write_all(&self.header_size.to_le_bytes())?;
        writer.write_all(&self.codec)?;
        writer.write_all(&self.width.to_le_bytes())?;
        writer.write_all(&self.height.to_le_bytes())?;
        writer.write_all(&self.framerate.to_le_bytes())?;
        writer.write_all(&self.timescale.to_le_bytes())?;
        writer.write_all(&self.frame_count.to_le_bytes())?;
        writer.write_all(&self.unused.to_le_bytes())?;

        Ok(())
    }
}

/// Helper struct for synthesizing IVF frame header
pub struct IvfFrameHeader {
    pub frame_size: u32,
    pub timestamp: u64,
}

impl IvfFrameHeader {
    /// Writes header into writer
    pub fn writo_into(&self, writer: &mut impl std::io::Write) -> std::io::Result<()> {
        writer.write_all(&self.frame_size.to_le_bytes())?;
        writer.write_all(&self.timestamp.to_le_bytes())?;
        Ok(())
    }
}

/// Iterator NALUs in a bitstream.
pub struct NalIterator<'a, Nalu>(Cursor<&'a [u8]>, PhantomData<Nalu>);

impl<'a, Nalu> NalIterator<'a, Nalu> {
    pub fn new(stream: &'a [u8]) -> Self {
        Self(Cursor::new(stream), PhantomData)
    }
}

impl<'a> Iterator for NalIterator<'a, H264Nalu<'a>> {
    type Item = Cow<'a, [u8]>;

    fn next(&mut self) -> Option<Self::Item> {
        H264Nalu::next(&mut self.0).map(|n| n.data).ok()
    }
}

impl<'a> Iterator for NalIterator<'a, H265Nalu<'a>> {
    type Item = Cow<'a, [u8]>;

    fn next(&mut self) -> Option<Self::Item> {
        H265Nalu::next(&mut self.0).map(|n| n.data).ok()
    }
}

#[derive(Error, Debug)]
pub enum BitWriterError {
    #[error("invalid bit count")]
    InvalidBitCount,
    #[error(transparent)]
    Io(#[from] std::io::Error),
}

pub type BitWriterResult<T> = std::result::Result<T, BitWriterError>;

pub struct BitWriter<W: Write> {
    out: W,
    nth_bit: u8,
    curr_byte: u8,
}

impl<W: Write> BitWriter<W> {
    pub fn new(writer: W) -> Self {
        Self {
            out: writer,
            curr_byte: 0,
            nth_bit: 0,
        }
    }

    /// Writes fixed bit size integer (up to 32 bit)
    pub fn write_f<T: Into<u32>>(&mut self, bits: usize, value: T) -> BitWriterResult<usize> {
        let value = value.into();

        if bits > 32 {
            return Err(BitWriterError::InvalidBitCount);
        }

        let mut written = 0;
        for bit in (0..bits).rev() {
            let bit = (1 << bit) as u32;

            self.write_bit((value & bit) == bit)?;
            written += 1;
        }

        Ok(written)
    }

    /// Takes a single bit that will be outputed to [`std::io::Write`]
    pub fn write_bit(&mut self, bit: bool) -> BitWriterResult<()> {
        self.curr_byte |= (bit as u8) << (7u8 - self.nth_bit);
        self.nth_bit += 1;

        if self.nth_bit == 8 {
            self.out.write_all(&[self.curr_byte])?;
            self.nth_bit = 0;
            self.curr_byte = 0;
        }

        Ok(())
    }

    /// Immediately outputs any cached bits to [`std::io::Write`]
    pub fn flush(&mut self) -> BitWriterResult<()> {
        if self.nth_bit != 0 {
            self.out.write_all(&[self.curr_byte])?;
            self.nth_bit = 0;
            self.curr_byte = 0;
        }

        self.out.flush()?;
        Ok(())
    }

    /// Returns `true` if ['Self`] hold data that wasn't written to [`std::io::Write`]
    pub fn has_data_pending(&self) -> bool {
        self.nth_bit != 0
    }

    pub(crate) fn inner(&self) -> &W {
        &self.out
    }

    pub(crate) fn inner_mut(&mut self) -> &mut W {
        &mut self.out
    }
}

impl<W: Write> Drop for BitWriter<W> {
    fn drop(&mut self) {
        if let Err(e) = self.flush() {
            log::error!("Unable to flush bits {e:?}");
        }
    }
}

/// Simple decoding loop that plays the stream once from start to finish.
#[allow(clippy::type_complexity)]
pub fn simple_playback_loop<D, R, I, H, FP>(
    decoder: &mut D,
    stream_iter: I,
    on_new_frame: &mut dyn FnMut(H),
    allocate_new_frames: &mut dyn FnMut(&StreamInfo, usize) -> anyhow::Result<Vec<H::Descriptor>>,
    output_format: DecodedFormat,
    blocking_mode: BlockingMode,
) -> anyhow::Result<()>
where
    H: DecodedHandle,
    FP: FramePool<Descriptor = H::Descriptor>,
    D: StatelessVideoDecoder<Handle = H, FramePool = FP> + ?Sized,
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
                    let stream_info = format_setter.stream_info().clone();
                    let min_num_frames = stream_info.min_num_frames;
                    /* we need to account for multiple layers if applicable for
                     * the stream */
                    let pools = format_setter.frame_pool(PoolLayer::All);
                    let nb_pools = pools.len();
                    for pool in pools {
                        // Allocate the missing number of buffers in our pool for decoding to succeed.
                        let pool_num_frames = pool.num_managed_frames();
                        if pool_num_frames < (min_num_frames / nb_pools) {
                            let frames = allocate_new_frames(
                                &stream_info,
                                min_num_frames - pool_num_frames,
                            )?;
                            pool.add_frames(frames).unwrap();
                        }
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

/// Frame allocation callback that results in self-allocated memory.
pub fn simple_playback_loop_owned_frames(
    _: &StreamInfo,
    nb_frames: usize,
) -> anyhow::Result<Vec<()>> {
    Ok(vec![(); nb_frames])
}

/// Frame allocation callback that returns user-allocated memory for the frames.
pub fn simple_playback_loop_userptr_frames(
    stream_info: &StreamInfo,
    nb_frames: usize,
) -> anyhow::Result<Vec<UserPtrFrame>> {
    let alloc_function = match stream_info.format {
        DecodedFormat::I420 | DecodedFormat::NV12 => &UserPtrFrame::new_nv12,
        _ => anyhow::bail!(
            "{:?} format is unsupported with user memory",
            stream_info.format
        ),
    };

    Ok((0..nb_frames)
        .map(|_| alloc_function(stream_info.coded_resolution))
        .collect::<Vec<_>>())
}

/// A structure that holds user-allocated memory for a frame as well as its layout.
#[derive(Debug)]
pub struct UserPtrFrame {
    pub buffers: Vec<*mut u8>,
    pub mem_layout: std::alloc::Layout,
    pub layout: FrameLayout,
}

impl UserPtrFrame {
    /// Allocate enough memory to back a NV12 frame of `size` dimension.
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

        Self::alloc(
            FrameLayout {
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
            uv_start.max(uv_size),
        )
    }

    pub fn alloc(layout: FrameLayout, buffer_size: usize) -> Self {
        let buffer_count = layout
            .planes
            .iter()
            .map(|plane| plane.buffer_index)
            .collect::<std::collections::HashSet<usize>>()
            .len();

        // SAFETY: the invariants of `Layout` are respected.
        let mem_layout =
            unsafe { std::alloc::Layout::from_size_align_unchecked(buffer_size, 4096) };

        let buffers = (0..buffer_count)
            .map(|_| {
                // SAFETY: the invariants of `Layout` are respected.
                unsafe { std::alloc::alloc(mem_layout) }
            })
            .collect();

        Self {
            buffers,
            mem_layout,
            layout,
        }
    }
}

#[derive(Debug)]
pub struct DmabufFrame {
    pub fds: Vec<OwnedFd>,
    pub layout: FrameLayout,
}

impl Drop for UserPtrFrame {
    fn drop(&mut self) {
        for buffer in std::mem::take(&mut self.buffers).into_iter() {
            // Safe because we allocated the memory using `std::alloc::alloc`.
            unsafe { std::alloc::dealloc(buffer, self.mem_layout) }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ivf_file_header() {
        let mut hdr = IvfFileHeader {
            version: 0,
            codec: IvfFileHeader::CODEC_VP9,
            width: 256,
            height: 256,
            framerate: 30_000,
            timescale: 1_000,
            frame_count: 1,

            ..Default::default()
        };

        let mut buf = Vec::new();
        hdr.writo_into(&mut buf).unwrap();

        const EXPECTED: [u8; 32] = [
            0x44, 0x4b, 0x49, 0x46, 0x00, 0x00, 0x20, 0x00, 0x56, 0x50, 0x39, 0x30, 0x00, 0x01,
            0x00, 0x01, 0x30, 0x75, 0x00, 0x00, 0xe8, 0x03, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
        ];

        assert_eq!(&buf, &EXPECTED);

        hdr.width = 1920;
        hdr.height = 800;
        hdr.framerate = 24;
        hdr.timescale = 1;
        hdr.frame_count = 100;

        buf.clear();
        hdr.writo_into(&mut buf).unwrap();

        const EXPECTED2: [u8; 32] = [
            0x44, 0x4b, 0x49, 0x46, 0x00, 0x00, 0x20, 0x00, 0x56, 0x50, 0x39, 0x30, 0x80, 0x07,
            0x20, 0x03, 0x18, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x64, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
        ];

        assert_eq!(&buf, &EXPECTED2);
    }

    #[test]
    fn test_ivf_frame_header() {
        let mut hdr = IvfFrameHeader {
            frame_size: 199249,
            timestamp: 0,
        };

        let mut buf = Vec::new();
        hdr.writo_into(&mut buf).unwrap();

        const EXPECTED: [u8; 12] = [
            0x51, 0x0a, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        ];

        assert_eq!(&buf, &EXPECTED);

        hdr.timestamp = 1;
        hdr.frame_size = 52;

        buf.clear();
        hdr.writo_into(&mut buf).unwrap();

        const EXPECTED2: [u8; 12] = [
            0x34, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        ];

        assert_eq!(&buf, &EXPECTED2);
    }

    #[test]
    fn test_bitwriter_f1() {
        let mut buf = Vec::<u8>::new();
        {
            let mut writer = BitWriter::new(&mut buf);
            writer.write_f(1, true).unwrap();
            writer.write_f(1, false).unwrap();
            writer.write_f(1, false).unwrap();
            writer.write_f(1, false).unwrap();
            writer.write_f(1, true).unwrap();
            writer.write_f(1, true).unwrap();
            writer.write_f(1, true).unwrap();
            writer.write_f(1, true).unwrap();
        }
        assert_eq!(buf, vec![0b10001111u8]);
    }

    #[test]
    fn test_bitwriter_f3() {
        let mut buf = Vec::<u8>::new();
        {
            let mut writer = BitWriter::new(&mut buf);
            writer.write_f(3, 0b100u8).unwrap();
            writer.write_f(3, 0b101u8).unwrap();
            writer.write_f(3, 0b011u8).unwrap();
        }
        assert_eq!(buf, vec![0b10010101u8, 0b10000000u8]);
    }

    #[test]
    fn test_bitwriter_f4() {
        let mut buf = Vec::<u8>::new();
        {
            let mut writer = BitWriter::new(&mut buf);
            writer.write_f(4, 0b1000u8).unwrap();
            writer.write_f(4, 0b1011u8).unwrap();
        }
        assert_eq!(buf, vec![0b10001011u8]);
    }
}
