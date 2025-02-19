// Copyright 2025 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::collections::VecDeque;
#[cfg(feature = "vaapi")]
use std::rc::Rc;
use std::sync::Arc;
use std::sync::Mutex;
use std::sync::Weak;

use crate::decoder::StreamInfo;
use crate::video_frame::ReadMapping;
use crate::video_frame::VideoFrame;
use crate::video_frame::WriteMapping;
use crate::Fourcc;
use crate::Resolution;

#[cfg(feature = "v4l2")]
use crate::v4l2r::device::Device;
#[cfg(feature = "vaapi")]
use libva::Display;
#[cfg(feature = "v4l2")]
use v4l2r::bindings::v4l2_plane;
#[cfg(feature = "v4l2")]
use v4l2r::ioctl::V4l2Buffer;
#[cfg(feature = "v4l2")]
use v4l2r::Format;

#[derive(Debug)]
pub struct PooledVideoFrame<V: VideoFrame> {
    inner: Option<V>,
    pool: Weak<Mutex<VecDeque<V>>>,
}

impl<V: VideoFrame> VideoFrame for PooledVideoFrame<V> {
    #[cfg(feature = "vaapi")]
    type MemDescriptor = V::MemDescriptor;

    type NativeHandle = V::NativeHandle;

    fn fourcc(&self) -> Fourcc {
        self.inner.as_ref().unwrap().fourcc()
    }

    fn resolution(&self) -> Resolution {
        self.inner.as_ref().unwrap().resolution()
    }

    fn get_plane_size(&self) -> Vec<usize> {
        self.inner.as_ref().unwrap().get_plane_size()
    }

    fn get_plane_pitch(&self) -> Vec<usize> {
        self.inner.as_ref().unwrap().get_plane_pitch()
    }

    fn map<'a>(&'a self) -> Result<Box<dyn ReadMapping<'a> + 'a>, String> {
        self.inner.as_ref().unwrap().map()
    }

    fn map_mut<'a>(&'a mut self) -> Result<Box<dyn WriteMapping<'a> + 'a>, String> {
        self.inner.as_mut().unwrap().map_mut()
    }

    #[cfg(feature = "v4l2")]
    fn fill_v4l2_plane(&self, index: usize, plane: &mut v4l2_plane) {
        self.inner.as_ref().unwrap().fill_v4l2_plane(index, plane)
    }

    #[cfg(feature = "v4l2")]
    fn process_dqbuf(&mut self, device: Arc<Device>, format: &Format, buf: &V4l2Buffer) {
        self.inner.as_mut().unwrap().process_dqbuf(device, format, buf)
    }

    #[cfg(feature = "vaapi")]
    fn to_native_handle(&self, display: &Rc<Display>) -> Result<Self::NativeHandle, String> {
        self.inner.as_ref().unwrap().to_native_handle(display)
    }
}

impl<V: VideoFrame> Drop for PooledVideoFrame<V> {
    fn drop(&mut self) {
        if let Some(pool) = self.pool.upgrade() {
            (*pool.lock().unwrap()).push_back(self.inner.take().unwrap());
        }
    }
}

pub struct FramePool<V: VideoFrame> {
    alloc_cb: Box<dyn FnMut(&StreamInfo) -> V + Send + 'static>,
    pool: Option<Arc<Mutex<VecDeque<V>>>>,
}

impl<V: VideoFrame> FramePool<V> {
    pub fn new(alloc_cb: impl FnMut(&StreamInfo) -> V + Send + 'static) -> FramePool<V> {
        Self { alloc_cb: Box::new(alloc_cb), pool: None }
    }

    pub fn resize(&mut self, stream_info: &StreamInfo) {
        let mut pool: VecDeque<V> = VecDeque::new();
        for _i in 0..stream_info.min_num_frames {
            pool.push_back((self.alloc_cb)(stream_info));
        }
        self.pool = Some(Arc::new(Mutex::new(pool)))
    }

    pub fn alloc(&mut self) -> Option<PooledVideoFrame<V>> {
        let frame = (*self.pool.as_mut().unwrap().lock().unwrap()).pop_front()?;
        Some(PooledVideoFrame {
            inner: Some(frame),
            pool: Arc::downgrade(self.pool.as_ref().unwrap()),
        })
    }
}
