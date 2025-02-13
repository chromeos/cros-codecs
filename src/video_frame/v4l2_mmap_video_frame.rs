// Copyright 2025 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::cell::RefCell;
use std::fmt;
use std::fmt::Debug;
use std::iter::zip;
use std::sync::Arc;

use crate::utils::align_up;
use crate::video_frame::ReadMapping;
use crate::video_frame::VideoFrame;
use crate::video_frame::WriteMapping;
use crate::Fourcc;
use crate::Resolution;

use crate::v4l2r::device::Device;
use v4l2r::ioctl::{mmap, PlaneMapping, V4l2Buffer};
use v4l2r::memory::MmapHandle;
use v4l2r::Format;

pub struct V4l2MmapMapping {
    planes: Vec<PlaneMapping>,
}

impl<'a> ReadMapping<'a> for V4l2MmapMapping {
    fn get(&self) -> Vec<&[u8]> {
        self.planes.iter().map(|x| &*x.data).collect()
    }
}

impl<'a> WriteMapping<'a> for V4l2MmapMapping {
    fn get(&self) -> Vec<RefCell<&'a mut [u8]>> {
        todo!("Writable mappings not yet supported!")
    }
}

pub struct V4l2MmapVideoFrame {
    fourcc: Fourcc,
    resolution: Resolution,
    handle: MmapHandle,
    device: Option<Arc<Device>>,
    queue_format: Option<Format>,
    buffer: Option<V4l2Buffer>,
}

impl Debug for V4l2MmapVideoFrame {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("MmapVideoFrame")
            .field("fourcc", &self.fourcc)
            .field("resolution", &self.resolution)
            .field("queue_format", &self.queue_format)
            .field("buffer", &self.buffer)
            .finish()
    }
}

impl V4l2MmapVideoFrame {
    pub fn new(fourcc: Fourcc, resolution: Resolution) -> Self {
        let ret = V4l2MmapVideoFrame {
            fourcc: fourcc,
            resolution: resolution,
            handle: MmapHandle {},
            device: None,
            queue_format: None,
            buffer: None,
        };

        if ret.is_contiguous() {
            todo!("Contiguous formats are not currently supported for MMAP!");
        }

        ret
    }

    fn map_helper(&self) -> Result<V4l2MmapMapping, String> {
        let device = self.device.as_ref().ok_or("No V4L2 device!".to_string())?;
        // UNSAFE: The unsafe block is just because we're accessing a union, which is guaranteed
        // to be initialized in this circumstance because the handle is of type MmapHandle.
        // TODO: This will not work for single planar video formats such as NV12.
        Ok(V4l2MmapMapping {
            planes: self
                .buffer
                .as_ref()
                .ok_or("No V4L2 buffer!")?
                .as_v4l2_planes()
                .iter()
                .map(|x| unsafe { mmap(device, x.m.mem_offset, x.length) })
                .collect::<Result<Vec<_>, _>>()
                .map_err(|err| format!("Error mmap'ing buffer {err}"))?,
        })
    }
}

impl VideoFrame for V4l2MmapVideoFrame {
    type NativeHandle = MmapHandle;

    fn fourcc(&self) -> Fourcc {
        self.fourcc.clone()
    }

    fn resolution(&self) -> Resolution {
        self.resolution.clone()
    }

    fn get_plane_size(&self) -> Vec<usize> {
        match self.buffer.as_ref() {
            Some(buffer) => buffer.as_v4l2_planes().iter().map(|x| x.length as usize).collect(),
            None => {
                let mut plane_size: Vec<usize> = vec![];
                let vertical_subsampling = self.get_vertical_subsampling();
                let horizontal_subsampling = self.get_horizontal_subsampling();
                let bpp = self.get_bytes_per_element();
                for i in 0..self.num_planes() {
                    plane_size.push(
                        align_up(self.resolution.width as usize, horizontal_subsampling[i])
                            / horizontal_subsampling[i]
                            * align_up(self.resolution.height as usize, vertical_subsampling[i])
                            / vertical_subsampling[i]
                            * bpp[i],
                    );
                }
                plane_size
            }
        }
    }

    fn get_plane_pitch(&self) -> Vec<usize> {
        match self.queue_format.as_ref() {
            Some(format) => format.plane_fmt.iter().map(|x| x.bytesperline as usize).collect(),
            None => zip(self.get_bytes_per_element(), self.get_horizontal_subsampling())
                .map(|x| align_up(self.resolution.width as usize, x.1) / x.1 * x.0)
                .collect(),
        }
    }

    fn map<'a>(&'a self) -> Result<Box<dyn ReadMapping<'a> + 'a>, String> {
        Ok(Box::new(self.map_helper()?))
    }

    fn map_mut<'a>(&'a mut self) -> Result<Box<dyn WriteMapping<'a> + 'a>, String> {
        Ok(Box::new(self.map_helper()?))
    }

    #[cfg(feature = "v4l2")]
    fn to_native_handle(&self, _plane: usize) -> Result<&Self::NativeHandle, String> {
        Ok(&self.handle)
    }

    #[cfg(feature = "v4l2")]
    fn process_dqbuf(&mut self, device: Arc<Device>, format: &Format, buf: &V4l2Buffer) {
        self.device = Some(device);
        self.queue_format = Some(format.clone());
        self.buffer = Some(buf.clone());
    }
}
