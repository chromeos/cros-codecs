// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use crate::device::v4l2::stateless::queue::V4l2CaptureBuffer;
use crate::device::v4l2::stateless::queue::V4l2CaptureQueue;
use crate::device::v4l2::stateless::queue::V4l2OutputBuffer;
use crate::device::v4l2::stateless::queue::V4l2OutputQueue;
use crate::device::v4l2::stateless::request::V4l2Request;
use crate::Resolution;

use std::cell::RefCell;
use std::collections::HashMap;
use std::os::fd::AsRawFd;
use std::os::fd::RawFd;
use std::path::Path;
use std::rc::Rc;
use std::sync::Arc;

use v4l2r::device::Device as VideoDevice;
use v4l2r::device::DeviceConfig;
use v4l2r::ioctl;
use v4l2r::nix::fcntl::open;
use v4l2r::nix::fcntl::OFlag;
use v4l2r::nix::sys::stat::Mode;

//TODO: handle memory backends other than mmap
//TODO: handle video formats other than h264
//TODO: handle queue start/stop at runtime
//TODO: handle DRC at runtime
struct DeviceHandle {
    video_device: Arc<VideoDevice>,
    media_device: RawFd,
    output_queue: V4l2OutputQueue,
    capture_queue: V4l2CaptureQueue,
    capture_buffers: HashMap<u64, V4l2CaptureBuffer>,
}

impl DeviceHandle {
    fn new() -> Self {
        // TODO: pass video device path and config via function arguments
        let video_device_path = Path::new("/dev/video-dec0");
        let video_device_config = DeviceConfig::new().non_blocking_dqbuf();
        let video_device = Arc::new(
            VideoDevice::open(video_device_path, video_device_config)
                .expect("Failed to open video device"),
        );
        // TODO: probe capabilties to find releted media device path
        let media_device_path = Path::new("/dev/media-dec0");
        let media_device = open(
            media_device_path,
            OFlag::O_RDWR | OFlag::O_CLOEXEC,
            Mode::empty(),
        )
        .unwrap_or_else(|_| panic!("Cannot open {}", media_device_path.display()));
        // TODO: handle custom configuration
        const NUM_OUTPUT_BUFFERS: u32 = 8;
        const NUM_CAPTURE_BUFFERS: u32 = 8;
        let output_queue = V4l2OutputQueue::new(video_device.clone(), NUM_OUTPUT_BUFFERS);
        let capture_queue = V4l2CaptureQueue::new(video_device.clone(), NUM_CAPTURE_BUFFERS);
        Self {
            video_device,
            media_device,
            output_queue,
            capture_queue,
            capture_buffers: HashMap::<u64, V4l2CaptureBuffer>::new(),
        }
    }
    fn alloc_request(&self) -> ioctl::Request {
        ioctl::Request::alloc(&self.media_device).expect("Failed to alloc request handle")
    }
    fn alloc_buffer(&self) -> V4l2OutputBuffer {
        self.output_queue.alloc_buffer()
    }
    fn sync(&mut self, timestamp: u64) -> V4l2CaptureBuffer {
        // TODO: handle synced buffers internally by capture queue
        loop {
            match self.capture_buffers.remove(&timestamp) {
                Some(buffer) => return buffer,
                _ => self.recycle_buffers(), // TODO: poll/select
            };
        }
    }
    fn recycle_buffers(&mut self) {
        self.output_queue.drain();
        // TODO: handle synced buffers internally by capture queue
        loop {
            match self.capture_queue.dequeue_buffer() {
                Some(buffer) => {
                    self.capture_buffers.insert(buffer.timestamp(), buffer);
                }
                _ => break,
            }
        }
        self.capture_queue.refill();
    }
}

#[derive(Clone)]
pub struct V4l2Device {
    handle: Rc<RefCell<DeviceHandle>>,
}

impl V4l2Device {
    pub fn new() -> Self {
        Self {
            handle: Rc::new(RefCell::new(DeviceHandle::new())),
        }
    }
    pub fn num_free_buffers(&self) -> usize {
        self.handle.borrow().output_queue.num_free_buffers()
    }
    pub fn num_buffers(&self) -> usize {
        self.handle.borrow().output_queue.num_buffers()
    }
    pub fn set_resolution(&mut self, resolution: Resolution) -> &mut Self {
        self.handle
            .borrow_mut()
            .output_queue
            .set_resolution(resolution);
        self.handle
            .borrow_mut()
            .capture_queue
            .set_resolution(resolution);
        self
    }
    pub fn alloc_request(&self, timestamp: u64) -> V4l2Request {
        V4l2Request::new(
            self.clone(),
            timestamp,
            self.handle.borrow().alloc_request(),
            self.handle.borrow().alloc_buffer(),
        )
    }
    pub fn sync(&self, timestamp: u64) -> V4l2CaptureBuffer {
        self.handle.borrow_mut().sync(timestamp)
    }
    pub fn recycle_buffers(&self) {
        self.handle.borrow_mut().recycle_buffers()
    }
}

impl AsRawFd for V4l2Device {
    fn as_raw_fd(&self) -> i32 {
        self.handle.borrow().video_device.as_raw_fd()
    }
}
