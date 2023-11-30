// Copyright 2022 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

//! Utility functions used by several parts of this crate.
//!
//! This module is for anything that doesn't fit into the other top-level modules. Try not to add
//! new code here unless it really doesn't belong anywhere else.

use std::io::Cursor;
use std::io::Seek;
use std::marker::PhantomData;
use std::os::fd::OwnedFd;

use bytes::Buf;

use crate::codec::h264::parser::Nalu as H264Nalu;
use crate::codec::h265::parser::Nalu as H265Nalu;
use crate::decoder::stateless::DecodeError;
use crate::decoder::stateless::PoolLayer;
use crate::decoder::stateless::StatelessVideoDecoder;
use crate::decoder::BlockingMode;
use crate::decoder::DecodedHandle;
use crate::decoder::DecoderEvent;
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

/// Iterator NALUs in a bitstream.
pub struct NalIterator<'a, Nalu>(Cursor<&'a [u8]>, PhantomData<Nalu>);

impl<'a, Nalu> NalIterator<'a, Nalu> {
    pub fn new(stream: &'a [u8]) -> Self {
        Self(Cursor::new(stream), PhantomData)
    }
}

impl<'a> Iterator for NalIterator<'a, H264Nalu<'a>> {
    type Item = &'a [u8];

    fn next(&mut self) -> Option<Self::Item> {
        H264Nalu::next(&mut self.0)
            .map(|n| {
                let start = n.sc_offset;
                let end = n.offset + n.size;
                &n.data[start..end]
            })
            .ok()
    }
}

impl<'a> Iterator for NalIterator<'a, H265Nalu<'a>> {
    type Item = &'a [u8];

    fn next(&mut self) -> Option<Self::Item> {
        H265Nalu::next(&mut self.0)
            .map(|n| {
                let start = n.sc_offset;
                let end = n.offset + n.size;
                &n.data[start..end]
            })
            .ok()
    }
}

/// Simple decoding loop that plays the stream once from start to finish.
pub fn simple_playback_loop<D, R, I, M>(
    decoder: &mut D,
    stream_iter: I,
    on_new_frame: &mut dyn FnMut(Box<dyn DecodedHandle<Descriptor = M>>),
    allocate_new_frames: &mut dyn FnMut(&StreamInfo, usize) -> anyhow::Result<Vec<M>>,
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
                    let stream_info = format_setter.stream_info().clone();
                    let min_num_frames = stream_info.min_num_frames;
                    /* we need to account for multiple layers if applicable for
                     * the stream */
                    let pools = format_setter.frame_pool(PoolLayer::All);
                    for pool in pools {
                        // Allocate the missing number of buffers in our pool for decoding to succeed.
                        let pool_num_frames = pool.num_managed_frames();
                        if pool_num_frames < min_num_frames {
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
        let buffer_size = align!(uv_start + uv_size, 4096);

        // Safe because the invariants of `Layout` are respected.
        let layout = unsafe { std::alloc::Layout::from_size_align_unchecked(buffer_size, 4096) };
        let mem = unsafe { std::alloc::alloc(layout) };

        Self {
            buffers: vec![mem],
            mem_layout: layout,
            layout: FrameLayout {
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
