// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use anyhow::anyhow;
use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;

use v4l2r::bindings::v4l2_format;
use v4l2r::device::queue::direction::Capture;
use v4l2r::device::queue::direction::Output;
use v4l2r::device::queue::dqbuf::DqBuffer;
use v4l2r::device::queue::qbuf::QBuffer;
use v4l2r::device::queue::BuffersAllocated;
use v4l2r::device::queue::GetFreeCaptureBuffer;
use v4l2r::device::queue::GetFreeOutputBuffer;
use v4l2r::device::queue::Queue;
use v4l2r::device::queue::QueueInit;
use v4l2r::device::AllocatedQueue;
use v4l2r::device::Device;
use v4l2r::device::Stream;
use v4l2r::device::TryDequeue;
use v4l2r::memory::MemoryType;
use v4l2r::memory::MmapHandle;
use v4l2r::nix::sys::time::TimeVal;
use v4l2r::Format;
use v4l2r::PixelFormat;
use v4l2r::PlaneLayout;

use crate::decoder::stateless::DecodeError;
use crate::image_processing::nv12_copy;
use crate::DecodedFormat;
use crate::Rect;
use crate::Resolution;

//TODO: handle memory backends other than mmap
pub struct V4l2OutputBuffer {
    queue: V4l2OutputQueue,
    handle: QBuffer<
        Output,
        Vec<MmapHandle>,
        Vec<MmapHandle>,
        Rc<Queue<Output, BuffersAllocated<Vec<MmapHandle>>>>,
    >,
    length: usize,
}

impl V4l2OutputBuffer {
    fn new(
        queue: V4l2OutputQueue,
        handle: QBuffer<
            Output,
            Vec<MmapHandle>,
            Vec<MmapHandle>,
            Rc<Queue<Output, BuffersAllocated<Vec<MmapHandle>>>>,
        >,
    ) -> Self {
        Self {
            queue,
            handle,
            length: 0,
        }
    }
    pub fn index(&self) -> usize {
        self.handle.index()
    }
    pub fn length(&self) -> usize {
        self.length
    }
    pub fn write(&mut self, data: &[u8]) -> &mut Self {
        let mut mapping = self
            .handle
            .get_plane_mapping(0)
            .expect("Failed to mmap output buffer");

        mapping.as_mut()[self.length..self.length + 3].copy_from_slice(&[0, 0, 1]);
        self.length += 3;

        mapping.as_mut()[self.length..self.length + data.len()].copy_from_slice(data);
        self.length += data.len();

        drop(mapping);
        self
    }
    pub fn submit(self, timestamp: u64, request_fd: i32) {
        let handle = &*self.queue.handle.borrow();
        let queue = match handle {
            V4l2OutputQueueHandle::Streaming(queue) => queue,
            _ => panic!("ERROR"),
        };
        self.handle
            .set_timestamp(TimeVal::new(/* FIXME: sec */ 0, timestamp as i64))
            .set_request(request_fd)
            .queue(&[self.length])
            .expect("Failed to queue output buffer");
    }
}

//TODO: handle memory backends other than mmap
//TODO: handle video formats other than h264
//TODO: handle queue start/stop at runtime
//TODO: handle DRC at runtime
#[derive(Default)]
enum V4l2OutputQueueHandle {
    Init(Queue<Output, QueueInit>),
    Streaming(Rc<Queue<Output, BuffersAllocated<Vec<MmapHandle>>>>),
    #[default]
    Unknown,
}

#[derive(Clone)]
pub struct V4l2OutputQueue {
    handle: Rc<RefCell<V4l2OutputQueueHandle>>,
    num_buffers: u32,
}

impl V4l2OutputQueue {
    pub fn new(device: Arc<Device>, num_buffers: u32) -> Self {
        let handle = Queue::get_output_mplane_queue(device).expect("Failed to get output queue");
        log::debug!("Output queue:\n\tstate: None -> Init\n");
        let handle = Rc::new(RefCell::new(V4l2OutputQueueHandle::Init(handle)));
        Self {
            handle,
            num_buffers,
        }
    }
    pub fn set_coded_size(&mut self, res: Resolution) -> &mut Self {
        self.handle.replace(match self.handle.take() {
            V4l2OutputQueueHandle::Init(mut handle) => {
                let (width, height) = res.into();

                handle
                    .change_format()
                    .expect("Failed to change output format")
                    .set_size(width as usize, height as usize)
                    .set_pixelformat(PixelFormat::from_fourcc(b"S264"))
                    // 1 MB per decoding unit should be enough for most streams.
                    .set_planes_layout(vec![PlaneLayout {
                        sizeimage: 1024 * 1024,
                        ..Default::default()
                    }])
                    .apply::<v4l2_format>()
                    .expect("Failed to apply output format");

                let format: Format = handle.get_format().expect("Failed to get output format");
                log::debug!("Output format:\n\t{:?}\n", format);

                let handle = handle
                    .request_buffers_generic::<Vec<MmapHandle>>(MemoryType::Mmap, self.num_buffers)
                    .expect("Failed to request output buffers");
                log::debug!(
                    "Output queue:\n\t
                    num_buffers: {}\n\t
                    num_queued_buffers: {}\n\t
                    num_free_buffers: {}\n",
                    handle.num_buffers(),
                    handle.num_queued_buffers(),
                    handle.num_free_buffers()
                );

                // TODO: handle start/stop at runtime
                handle.stream_on().expect("Failed to start output queue");

                log::debug!("Output queue:\n\tstate: Init -> Streaming\n");
                V4l2OutputQueueHandle::Streaming(handle.into())
            }
            _ => {
                /* TODO: handle DRC */
                todo!()
            }
        });
        self
    }
    pub fn num_buffers(&self) -> usize {
        let handle = &*self.handle.borrow();
        match handle {
            V4l2OutputQueueHandle::Streaming(handle) => handle.num_buffers(),
            _ => 0,
        }
    }
    pub fn num_free_buffers(&self) -> usize {
        let handle = &*self.handle.borrow();
        match handle {
            V4l2OutputQueueHandle::Streaming(handle) => handle.num_free_buffers(),
            _ => 0,
        }
    }
    pub fn alloc_buffer(&self) -> Result<V4l2OutputBuffer, DecodeError> {
        let handle = &*self.handle.borrow();
        match handle {
            V4l2OutputQueueHandle::Streaming(handle) => match handle.try_get_free_buffer() {
                Ok(buffer) => Ok(V4l2OutputBuffer::new(self.clone(), buffer)),
                Err(_) => Err(DecodeError::NotEnoughOutputBuffers(1)),
            },
            _ => Err(DecodeError::DecoderError(anyhow!(
                "Invalid hardware handle"
            ))),
        }
    }
    pub fn drain(&self) {
        let handle = &*self.handle.borrow();
        match handle {
            V4l2OutputQueueHandle::Streaming(handle) => loop {
                match handle.try_dequeue() {
                    Ok(buffer) => continue,
                    _ => break,
                }
            },
            _ => panic!("ERROR"),
        }
    }
}

// TODO: handle other memory backends
pub struct V4l2CaptureBuffer {
    handle: DqBuffer<Capture, Vec<MmapHandle>>,
    visible_rect: Rect,
    format: Format,
}

impl V4l2CaptureBuffer {
    fn new(handle: DqBuffer<Capture, Vec<MmapHandle>>, visible_rect: Rect, format: Format) -> Self {
        Self {
            handle,
            visible_rect,
            format,
        }
    }
    pub fn index(&self) -> usize {
        self.handle.data.index() as usize
    }
    pub fn timestamp(&self) -> u64 {
        self.handle.data.timestamp().tv_usec as u64
    }
    //TODO make this work for formats other then 420
    pub fn length(&self) -> usize {
        (Resolution::from(self.visible_rect).get_area() * 3) / 2
    }
    pub fn read(&self, data: &mut [u8]) {
        let decoded_format: DecodedFormat = self
            .format
            .pixelformat
            .to_string()
            .parse()
            .expect("Unable to output");

        match decoded_format {
            DecodedFormat::NV12 => {
                let plane = self
                    .handle
                    .get_plane_mapping(0)
                    .expect("Failed to mmap capture buffer");
                let buffer_size = self.length();
                let stride: usize = self.format.plane_fmt[0].bytesperline.try_into().unwrap();
                let width: usize = Resolution::from(self.visible_rect)
                    .width
                    .try_into()
                    .unwrap();
                let height: usize = Resolution::from(self.visible_rect)
                    .height
                    .try_into()
                    .unwrap();
                let (src_y, src_uv) = plane.split_at(stride * height);
                let (data_y, data_uv) =
                    data.split_at_mut(Resolution::from(self.visible_rect).get_area());

                nv12_copy(
                    &src_y, stride, data_y, width, &src_uv, stride, data_uv, width, width, height,
                );
            }
            _ => panic!("handle me"),
        }
    }
}

//TODO: handle memory backends other than mmap
//TODO: handle video formats other than h264
//TODO: handle queue start/stop at runtime
//TODO: handle DRC at runtime
//TODO: handle synced buffers in Streaming state
#[derive(Default)]
enum V4l2CaptureQueueHandle {
    Init(Queue<Capture, QueueInit>),
    Streaming(Queue<Capture, BuffersAllocated<Vec<MmapHandle>>>),
    #[default]
    Unknown,
}

pub struct V4l2CaptureQueue {
    handle: RefCell<V4l2CaptureQueueHandle>,
    num_buffers: u32,
    visible_rect: Rect,
    format: Format,
}

impl V4l2CaptureQueue {
    pub fn new(device: Arc<Device>, num_buffers: u32) -> Self {
        let handle = Queue::get_capture_mplane_queue(device).expect("Failed to get capture queue");
        log::debug!("Capture queue:\n\tstate: None -> Init\n");
        let handle = RefCell::new(V4l2CaptureQueueHandle::Init(handle));
        Self {
            handle,
            num_buffers,
            visible_rect: Default::default(),
            format: Default::default(),
        }
    }
    pub fn set_visible_rect(&mut self, visible_rect: Rect) -> &mut Self {
        self.visible_rect = visible_rect;
        self.handle.replace(match self.handle.take() {
            V4l2CaptureQueueHandle::Init(handle) => {
                self.format = handle.get_format().expect("Failed to get capture format");
                log::debug!("Capture format:\n\t{:?}\n", self.format);
                let handle = handle
                    .request_buffers_generic::<Vec<MmapHandle>>(MemoryType::Mmap, self.num_buffers)
                    .expect("Failed to request capture buffers");
                log::debug!(
                    "Capture queue:\n\t
                    num_buffers: {}\n\t
                    num_queued_buffers: {}\n\t
                    num_free_buffers: {}\n",
                    handle.num_buffers(),
                    handle.num_queued_buffers(),
                    handle.num_free_buffers()
                );

                // TODO: handle start/stop at runtime
                handle.stream_on().expect("Failed to start capture queue");

                log::debug!("Capture queue:\n\tstate: Init -> Streaming\n");
                V4l2CaptureQueueHandle::Streaming(handle)
            }
            _ => {
                /* TODO: handle DRC */
                todo!()
            }
        });
        self
    }
    pub fn dequeue_buffer(&self) -> Option<V4l2CaptureBuffer> {
        let handle = &*self.handle.borrow();
        match handle {
            V4l2CaptureQueueHandle::Streaming(handle) => match handle.try_dequeue() {
                Ok(buffer) => Some(V4l2CaptureBuffer::new(
                    buffer,
                    self.visible_rect,
                    self.format.clone(),
                )),
                _ => None,
            },
            _ => panic!("ERROR"),
        }
    }
    pub fn refill(&self) {
        let handle = &*self.handle.borrow();
        match handle {
            V4l2CaptureQueueHandle::Streaming(handle) => {
                while handle.num_free_buffers() != 0 {
                    let buffer = handle
                        .try_get_free_buffer()
                        .expect("Failed to alloc capture buffer");
                    log::debug!("capture >> index: {}\n", buffer.index());
                    buffer.queue().expect("Failed to queue capture buffer");
                }
            }
            _ => panic!("ERROR"),
        }
    }
}
