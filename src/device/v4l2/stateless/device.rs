// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::cell::RefCell;
use std::collections::HashMap;
use std::os::fd::{AsRawFd, RawFd};
use std::path::Path;
use std::rc::{Rc, Weak};
use std::sync::Arc;
use std::thread::sleep;
use std::time::Duration;

use v4l2r::device::Device as VideoDevice;
use v4l2r::device::DeviceConfig;
use v4l2r::ioctl;
use v4l2r::nix::fcntl::open;
use v4l2r::nix::fcntl::OFlag;
use v4l2r::nix::sys::stat::Mode;

use crate::decoder::stateless::DecodeError;
use crate::device::v4l2::stateless::queue::QueueError;
use crate::device::v4l2::stateless::queue::V4l2CaptureBuffer;
use crate::device::v4l2::stateless::queue::V4l2CaptureQueue;
use crate::device::v4l2::stateless::queue::V4l2OutputQueue;
use crate::device::v4l2::stateless::request::V4l2Request;
use crate::video_frame::VideoFrame;
use crate::Fourcc;
use crate::Resolution;

//TODO: handle other memory backends for OUTPUT queue
//TODO: handle video formats other than h264
//TODO: handle queue start/stop at runtime
//TODO: handle DRC at runtime
struct DeviceHandle<V: VideoFrame> {
    video_device: Arc<VideoDevice>,
    media_device: RawFd,
    output_queue: V4l2OutputQueue,
    capture_queue: V4l2CaptureQueue<V>,
    requests: HashMap<u64, Weak<RefCell<V4l2Request<V>>>>,
}

impl<V: VideoFrame> DeviceHandle<V> {
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
        let media_device = open(media_device_path, OFlag::O_RDWR | OFlag::O_CLOEXEC, Mode::empty())
            .unwrap_or_else(|_| panic!("Cannot open {}", media_device_path.display()));
        let output_queue = V4l2OutputQueue::new(video_device.clone());
        let capture_queue = V4l2CaptureQueue::new(video_device.clone());
        Self {
            video_device,
            media_device,
            output_queue,
            capture_queue,
            requests: HashMap::<u64, Weak<RefCell<V4l2Request<V>>>>::new(),
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
                Err(QueueError::BufferDequeue) => {
                    sleep(back_off_duration);
                    back_off_duration = back_off_duration + back_off_duration;
                    continue;
                }
                Err(_) => panic!("handle this better"),
            }
        }
    }
    fn insert_request_into_hash(&mut self, request: Weak<RefCell<V4l2Request<V>>>) {
        let timestamp = request.upgrade().unwrap().as_ref().borrow().timestamp();
        self.requests.insert(timestamp, request);
    }
    fn try_dequeue_capture_buffers(&mut self) {
        loop {
            match self.capture_queue.dequeue_buffer() {
                Ok(Some(buffer)) => {
                    let timestamp = buffer.timestamp();
                    let request = self.requests.remove(&timestamp).unwrap();
                    match request.upgrade().unwrap().as_ref().try_borrow_mut() {
                        Ok(mut request) => {
                            request.associate_dequeued_buffer(buffer);
                        }
                        _ => (),
                    }
                    continue;
                }
                _ => break,
            }
        }
    }
    fn sync(&mut self, timestamp: u64) -> V4l2CaptureBuffer<V> {
        let mut back_off_duration = Duration::from_millis(1);
        let time_out = Duration::from_millis(120);
        loop {
            match self.capture_queue.dequeue_buffer() {
                Ok(Some(buffer)) => {
                    let dequeued_timestamp = buffer.timestamp();
                    let request = self.requests.remove(&dequeued_timestamp).unwrap();
                    if dequeued_timestamp == timestamp {
                        return buffer;
                    } else {
                        match request.upgrade().unwrap().as_ref().try_borrow_mut() {
                            Ok(mut request) => {
                                request.associate_dequeued_buffer(buffer);
                            }
                            _ => (),
                        }
                    }
                }
                _ => (),
            }
            back_off_duration = back_off_duration + back_off_duration;
            if back_off_duration > time_out {
                panic!("there should not be a scenario where a queued frame is not returned.");
            }
            sleep(back_off_duration);
        }
    }
}

pub struct V4l2Device<V: VideoFrame> {
    handle: Rc<RefCell<DeviceHandle<V>>>,
}

impl<V: VideoFrame> V4l2Device<V> {
    pub fn new() -> Self {
        Self { handle: Rc::new(RefCell::new(DeviceHandle::new())) }
    }
    pub fn initialize_queues(
        &mut self,
        format: Fourcc,
        coded_size: Resolution,
        num_buffers: u32,
    ) -> Result<(), anyhow::Error> {
        self.handle.borrow_mut().output_queue.initialize(format, coded_size)?;
        self.handle.borrow_mut().capture_queue.initialize(num_buffers)?;
        Ok(())
    }
    pub fn alloc_request(
        &self,
        timestamp: u64,
        frame: V,
    ) -> Result<Rc<RefCell<V4l2Request<V>>>, DecodeError> {
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
        self.handle.borrow().capture_queue.queue_buffer(frame)?;
        self.handle.borrow_mut().try_dequeue_capture_buffers();

        let request = Rc::new(RefCell::new(V4l2Request::new(
            self.clone(),
            timestamp,
            self.handle.borrow().alloc_request(),
            output_buffer,
        )));
        self.handle.borrow_mut().insert_request_into_hash(Rc::downgrade(&request.clone()));
        Ok(request)
    }
    pub fn sync(&self, timestamp: u64) -> V4l2CaptureBuffer<V> {
        self.handle.borrow_mut().sync(timestamp)
    }
}

impl<V: VideoFrame> Clone for V4l2Device<V> {
    fn clone(&self) -> Self {
        Self { handle: self.handle.clone() }
    }
}

impl<V: VideoFrame> AsRawFd for V4l2Device<V> {
    fn as_raw_fd(&self) -> i32 {
        self.handle.borrow().video_device.as_raw_fd()
    }
}
