// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use crate::decoder::stateless::DecodeError;
use crate::device::v4l2::stateless::queue::QueueError;
use crate::device::v4l2::stateless::queue::V4l2CaptureBuffer;
use crate::device::v4l2::stateless::queue::V4l2CaptureQueue;
use crate::device::v4l2::stateless::queue::V4l2OutputQueue;
use crate::device::v4l2::stateless::request::V4l2Request;
use crate::Fourcc;
use crate::Rect;
use crate::Resolution;

use std::cell::RefCell;
use std::collections::HashMap;
use std::os::fd::AsRawFd;
use std::os::fd::RawFd;
use std::path::Path;
use std::rc::Rc;
use std::sync::Arc;
use std::thread::sleep;
use std::time::Duration;

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
        let output_queue = V4l2OutputQueue::new(video_device.clone());
        let capture_queue = V4l2CaptureQueue::new(video_device.clone());
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
    fn dequeue_output_buffer(&self) {
        let mut back_off_duration = Duration::from_millis(1);
        loop {
            match self.output_queue.dequeue_buffer() {
                Ok(_) => {
                    break;
                }
                Err(QueueError::Dequeue) => {
                    sleep(back_off_duration);
                    back_off_duration = back_off_duration + back_off_duration;
                    continue;
                }
                Err(_) => panic!("handle this better"),
            }
        }
    }
    fn dequeue_capture_buffer(&mut self) {
        let mut back_off_duration = Duration::from_millis(1);
        loop {
            match self.capture_queue.dequeue_buffer() {
                Ok(Some(buffer)) => {
                    self.capture_buffers.insert(buffer.timestamp(), buffer);
                    break;
                }
                _ => {
                    sleep(back_off_duration);
                    back_off_duration = back_off_duration + back_off_duration;
                    continue;
                }
            }
        }
    }
    fn sync(&mut self, timestamp: u64) -> V4l2CaptureBuffer {
        // TODO: handle synced buffers internally by capture queue
        loop {
            match self.capture_buffers.remove(&timestamp) {
                Some(buffer) => return buffer,
                _ => self.dequeue_capture_buffer(),
            };
        }
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
        self.handle.borrow().capture_queue.num_buffers()
    }
    pub fn initialize_queues(
        &mut self,
        format: Fourcc,
        coded_size: Resolution,
        visible_rect: Rect,
        num_buffers: u32,
    ) -> Result<(), anyhow::Error> {
        self.handle
            .borrow_mut()
            .output_queue
            .initialize(format, coded_size)?;
        self.handle
            .borrow_mut()
            .capture_queue
            .initialize(visible_rect, num_buffers)?;
        Ok(())
    }
    pub fn alloc_request(&self, timestamp: u64) -> Result<V4l2Request, DecodeError> {
        if self.handle.borrow().capture_queue.num_free_buffers() == 0 {
            return Err(DecodeError::NotEnoughOutputBuffers(0));
        }

        let output_buffer = self.handle.borrow().output_queue.alloc_buffer();

        let output_buffer = match output_buffer {
            Ok(buffer) => buffer,
            Err(DecodeError::NotEnoughOutputBuffers(_)) => {
                self.handle.borrow_mut().dequeue_output_buffer();
                match self.handle.borrow().output_queue.alloc_buffer() {
                    Ok(buffer) => buffer,
                    Err(e) => return Err(e),
                }
            }
            Err(error) => return Err(error),
        };

        // dequeue capture/output
        self.handle.borrow().capture_queue.queue_buffer()?;

        Ok(V4l2Request::new(
            self.clone(),
            timestamp,
            self.handle.borrow().alloc_request(),
            output_buffer,
        ))
    }
    pub fn sync(&self, timestamp: u64) -> V4l2CaptureBuffer {
        self.handle.borrow_mut().sync(timestamp)
    }
    pub fn recycle_buffers(&self) {
        //self.handle.borrow_mut().recycle_buffers()
    }
}

impl AsRawFd for V4l2Device {
    fn as_raw_fd(&self) -> i32 {
        self.handle.borrow().video_device.as_raw_fd()
    }
}
