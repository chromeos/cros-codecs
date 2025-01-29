// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use crate::decoder::stateless::DecodeError;
use crate::device::v4l2::stateless::queue::QueueError;
use crate::device::v4l2::stateless::queue::V4l2CaptureBuffer;
use crate::device::v4l2::stateless::queue::V4l2CaptureQueue;
use crate::device::v4l2::stateless::queue::V4l2OutputQueue;
use crate::device::v4l2::stateless::request::V4l2Request;
use crate::video_frame::gbm_video_frame::GbmDevice;
use crate::video_frame::VideoFrame;
use crate::Fourcc;
use crate::Rect;
use crate::Resolution;

use std::cell::RefCell;
use std::collections::HashMap;
use std::fs::File;
use std::os::fd::AsRawFd;
use std::os::fd::RawFd;
use std::path::Path;
use std::path::PathBuf;
use std::rc::Rc;
use std::sync::Arc;
use std::thread::sleep;
use std::time::Duration;

use v4l2r::device::Device as VideoDevice;
use v4l2r::device::DeviceConfig;
use v4l2r::ioctl;
use v4l2r::memory::DmaBufHandle;
use v4l2r::memory::PlaneHandle;
use v4l2r::nix::fcntl::open;
use v4l2r::nix::fcntl::OFlag;
use v4l2r::nix::sys::stat::Mode;

//TODO: handle other memory backends for OUTPUT queue
//TODO: handle video formats other than h264
//TODO: handle queue start/stop at runtime
//TODO: handle DRC at runtime
struct DeviceHandle<C: PlaneHandle> {
    video_device: Arc<VideoDevice>,
    media_device: RawFd,
    output_queue: V4l2OutputQueue,
    capture_queue: V4l2CaptureQueue<C>,
    capture_buffers: HashMap<u64, V4l2CaptureBuffer<C>>,
}

impl<C: PlaneHandle> DeviceHandle<C> {
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
            capture_buffers: HashMap::<u64, V4l2CaptureBuffer<C>>::new(),
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
    fn try_dequeue_capture_buffers(
        &mut self,
        frame_import_cb: &impl Fn(
            Fourcc,
            Resolution,
            Vec<usize>,
            Vec<C>,
        ) -> Box<dyn VideoFrame<NativeHandle = Vec<C>>>,
    ) {
        loop {
            match self.capture_queue.dequeue_buffer(frame_import_cb) {
                Ok(Some(buffer)) => {
                    self.capture_buffers.insert(buffer.timestamp(), buffer);
                    continue;
                }
                _ => break,
            }
        }
    }
    fn sync(
        &mut self,
        timestamp: u64,
        frame_import_cb: &impl Fn(
            Fourcc,
            Resolution,
            Vec<usize>,
            Vec<C>,
        ) -> Box<dyn VideoFrame<NativeHandle = Vec<C>>>,
    ) -> V4l2CaptureBuffer<C> {
        // TODO: handle synced buffers internally by capture queue
        let mut back_off_duration = Duration::from_millis(1);
        let time_out = Duration::from_millis(120);
        loop {
            match self.capture_buffers.remove(&timestamp) {
                Some(buffer) => return buffer,
                _ => {
                    sleep(back_off_duration);
                    back_off_duration = back_off_duration + back_off_duration;
                    self.try_dequeue_capture_buffers(frame_import_cb);
                    if back_off_duration > time_out {
                        panic!("unable to dequeue frame");
                    }
                }
            }
        }
    }
}

#[derive(Clone)]
pub struct V4l2Device {
    handle: Rc<RefCell<DeviceHandle<DmaBufHandle<File>>>>,
    gbm_device: Arc<GbmDevice>,
}

impl V4l2Device {
    pub fn new() -> Self {
        Self {
            handle: Rc::new(RefCell::new(DeviceHandle::new())),
            // Hacky. This will go away once we properly plumb the Gralloc VideoFrames.
            gbm_device: GbmDevice::open(PathBuf::from("/dev/dri/renderD128"))
                .expect("Could not open GBM device!"),
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
    pub fn alloc_request(&self, timestamp: u64) -> Result<Rc<RefCell<V4l2Request>>, DecodeError> {
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

        let frame = Box::new(
            self.gbm_device
                .clone()
                .new_frame(
                    Fourcc::from(
                        self.handle
                            .borrow()
                            .capture_queue
                            .format
                            .pixelformat
                            .to_u32(),
                    ),
                    Resolution {
                        width: self.handle.borrow().capture_queue.format.width,
                        height: self.handle.borrow().capture_queue.format.height,
                    },
                )
                .expect("Failed to allocate capture buffer!"),
        );
        self.handle.borrow().capture_queue.queue_buffer(frame)?;
        self.handle
            .borrow_mut()
            .try_dequeue_capture_buffers(
                &move |fourcc: Fourcc,
                       resolution: Resolution,
                       strides: Vec<usize>,
                       native_handle: Vec<DmaBufHandle<File>>| {
                    Box::new(
                        <Arc<GbmDevice> as Clone>::clone(&self.gbm_device)
                            .import_from_v4l2(fourcc, resolution, strides, native_handle)
                            .expect("Failed to import V4L2 handles!"),
                    )
                },
            );

        let request = Rc::new(RefCell::new(V4l2Request::new(
            self.clone(),
            timestamp,
            self.handle.borrow().alloc_request(),
            output_buffer,
        )));
        Ok(request)
    }
    pub fn sync(&self, timestamp: u64) -> V4l2CaptureBuffer<DmaBufHandle<File>> {
        self.handle.borrow_mut().sync(
            timestamp,
            &move |fourcc: Fourcc,
                   resolution: Resolution,
                   strides: Vec<usize>,
                   native_handle: Vec<DmaBufHandle<File>>| {
                Box::new(
                    <Arc<GbmDevice> as Clone>::clone(&self.gbm_device)
                        .import_from_v4l2(fourcc, resolution, strides, native_handle)
                        .expect("Failed to import V4L2 handles!"),
                )
            },
        )
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
