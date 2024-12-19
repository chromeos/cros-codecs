// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use anyhow::anyhow;
use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;

use v4l2r::bindings::v4l2_format;
use v4l2r::device::queue::direction;
use v4l2r::device::queue::direction::Capture;
use v4l2r::device::queue::direction::Output;
use v4l2r::device::queue::dqbuf::DqBuffer;
use v4l2r::device::queue::qbuf::QBuffer;
use v4l2r::device::queue::BuffersAllocated;
use v4l2r::device::queue::CreateQueueError;
use v4l2r::device::queue::GetFreeCaptureBuffer;
use v4l2r::device::queue::GetFreeOutputBuffer;
use v4l2r::device::queue::Queue;
use v4l2r::device::queue::QueueInit;
use v4l2r::device::queue::RequestBuffersError;
use v4l2r::device::AllocatedQueue;
use v4l2r::device::Device;
use v4l2r::device::Stream;
use v4l2r::device::TryDequeue;
use v4l2r::ioctl::GFmtError;
use v4l2r::ioctl::SFmtError;
use v4l2r::ioctl::StreamOnError;
use v4l2r::memory::MemoryType;
use v4l2r::memory::MmapHandle;
use v4l2r::nix::sys::time::TimeVal;
use v4l2r::Format;
use v4l2r::PixelFormat;
use v4l2r::PlaneLayout;

use crate::decoder::stateless::DecodeError;
use crate::image_processing::mm21_to_nv12;
use crate::image_processing::nv12_copy;
use crate::image_processing::MM21_TILE_HEIGHT;
use crate::DecodedFormat;
use crate::Fourcc;
use crate::Rect;
use crate::Resolution;

//TODO: handle memory backends other than mmap
//TODO: handle video formats other than h264
//TODO: handle queue start/stop at runtime
//TODO: handle DRC at runtime
//TODO: handle synced buffers in Streaming state
#[derive(Default)]
enum V4l2QueueHandle<T: v4l2r::device::queue::direction::Direction> {
    Init(Queue<T, QueueInit>),
    Streaming(Rc<Queue<T, BuffersAllocated<Vec<MmapHandle>>>>),
    #[default]
    Unknown,
}

#[derive(Debug)]
pub enum QueueError {
    Creation,
    FormatGet,
    FormatSet,
    RequestBuffers,
    StreamOn,
    UnsupportedPixelFormat(Fourcc),
}

impl From<CreateQueueError> for QueueError {
    fn from(_err: CreateQueueError) -> Self {
        QueueError::Creation
    }
}

impl From<GFmtError> for QueueError {
    fn from(_err: GFmtError) -> Self {
        QueueError::FormatGet
    }
}

impl From<SFmtError> for QueueError {
    fn from(_err: SFmtError) -> Self {
        QueueError::FormatSet
    }
}

impl From<RequestBuffersError> for QueueError {
    fn from(_err: RequestBuffersError) -> Self {
        QueueError::RequestBuffers
    }
}

impl From<StreamOnError> for QueueError {
    fn from(_err: StreamOnError) -> Self {
        QueueError::StreamOn
    }
}

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
            V4l2QueueHandle::Streaming(queue) => queue,
            _ => panic!("ERROR"),
        };
        self.handle
            .set_timestamp(TimeVal::new(/* FIXME: sec */ 0, timestamp as i64))
            .set_request(request_fd)
            .queue(&[self.length])
            .expect("Failed to queue output buffer");
    }
}

#[derive(Clone)]
pub struct V4l2OutputQueue {
    handle: Rc<RefCell<V4l2QueueHandle<direction::Output>>>,
}

fn buffer_size_for_area(width: u32, height: u32) -> u32 {
    let area = width * height;
    let mut buffer_size: u32 = 1024 * 1024;

    if area > 720 * 480 {
        buffer_size *= 2;
    }
    if area > 1920 * 1080 {
        buffer_size *= 2;
    }
    buffer_size
}

// TODO because of the queueing model the OUTPUT buffer count must be equal to
// or greater than the CAPTURE buffer count. The OUTPUT buffer count needs only
// to be 2 buffers for ping pong submissions. There is no need to queue a deep
// pipeline of buffers in the V4L2 driver because the hardware only works on one
// frame at a time.
const NUM_OUTPUT_BUFFERS: u32 = 20;
impl V4l2OutputQueue {
    pub fn new(device: Arc<Device>) -> Self {
        let handle = Queue::get_output_mplane_queue(device).expect("Failed to get output queue");
        log::debug!("Output queue:\n\tstate: None -> Init\n");
        let handle = Rc::new(RefCell::new(V4l2QueueHandle::Init(handle)));
        Self { handle }
    }

    pub fn initialize(
        &mut self,
        fourcc: Fourcc,
        resolution: Resolution,
    ) -> Result<&mut Self, QueueError> {
        self.handle.replace(match self.handle.take() {
            V4l2QueueHandle::Init(mut handle) => {
                let (width, height) = resolution.into();
                handle
                    .change_format()?
                    .set_size(width as usize, height as usize)
                    .set_pixelformat(fourcc)
                    .set_planes_layout(vec![PlaneLayout {
                        sizeimage: buffer_size_for_area(width, height),
                        ..Default::default()
                    }])
                    .apply::<v4l2_format>()?;

                let format: Format = handle.get_format()?;
                if format.pixelformat != fourcc.into() {
                    return Err(QueueError::UnsupportedPixelFormat(fourcc));
                }

                let handle = handle.request_buffers_generic::<Vec<MmapHandle>>(
                    MemoryType::Mmap,
                    NUM_OUTPUT_BUFFERS,
                )?;

                handle.stream_on()?;

                V4l2QueueHandle::Streaming(handle.into())
            }
            _ => {
                todo!("DRC is not supported")
            }
        });
        Ok(self)
    }

    pub fn num_free_buffers(&self) -> usize {
        let handle = &*self.handle.borrow();
        match handle {
            V4l2QueueHandle::Streaming(handle) => handle.num_free_buffers(),
            _ => 0,
        }
    }
    pub fn alloc_buffer(&self) -> Result<V4l2OutputBuffer, DecodeError> {
        let handle = &*self.handle.borrow();
        match handle {
            V4l2QueueHandle::Streaming(handle) => match handle.try_get_free_buffer() {
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
            V4l2QueueHandle::Streaming(handle) => loop {
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
                let stride: usize = self.format.plane_fmt[0].bytesperline as usize;
                let width: usize = Resolution::from(self.visible_rect).width as usize;
                let height: usize = Resolution::from(self.visible_rect).height as usize;
                let (src_y, src_uv) = plane.split_at(stride * height);
                let (data_y, data_uv) =
                    data.split_at_mut(Resolution::from(self.visible_rect).get_area());

                nv12_copy(
                    &src_y, stride, data_y, width, &src_uv, stride, data_uv, width, width, height,
                );
            }
            DecodedFormat::MM21 => {
                // check planes count
                self.handle.data.num_planes();
                let src_y = self
                    .handle
                    .get_plane_mapping(0)
                    .expect("Failed to mmap capture buffer");
                let src_uv = self
                    .handle
                    .get_plane_mapping(1)
                    .expect("Failed to mmap capture buffer");
                let width: usize = Resolution::from(self.visible_rect).width as usize;
                let height: usize = Resolution::from(self.visible_rect).height as usize;
                let height_tiled = height + (MM21_TILE_HEIGHT - 1) & !(32 - 1);
                let src_stride = self.format.plane_fmt[0].bytesperline as usize;
                let mut pivot_y = vec![0; src_stride * height_tiled];
                let mut pivot_uv = vec![0; src_stride * height_tiled / 2];
                mm21_to_nv12(
                    &src_y,
                    &mut pivot_y,
                    &src_uv,
                    &mut pivot_uv,
                    src_stride,
                    height_tiled,
                )
                .expect("Unable to convert mm21 to nv12");

                let (data_y, data_uv) =
                    data.split_at_mut(Resolution::from(self.visible_rect).get_area());
                nv12_copy(
                    &pivot_y, src_stride, data_y, width, &pivot_uv, src_stride, data_uv, width,
                    width, height,
                );
            }
            _ => panic!("handle me"),
        }
    }
}

pub struct V4l2CaptureQueue {
    handle: RefCell<V4l2QueueHandle<direction::Capture>>,
    num_buffers: u32,
    visible_rect: Rect,
    format: Format,
}

impl V4l2CaptureQueue {
    pub fn new(device: Arc<Device>) -> Self {
        let handle = Queue::get_capture_mplane_queue(device).expect("Failed to get capture queue");
        log::debug!("Capture queue:\n\tstate: None -> Init\n");
        let handle = RefCell::new(V4l2QueueHandle::Init(handle));
        Self {
            handle,
            num_buffers: 0,
            visible_rect: Default::default(),
            format: Default::default(),
        }
    }
    pub fn initialize_queue(&mut self, visible_rect: Rect, num_buffers: u32) -> &mut Self {
        self.visible_rect = visible_rect;
        self.num_buffers = num_buffers;
        self.handle.replace(match self.handle.take() {
            V4l2QueueHandle::Init(handle) => {
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
                V4l2QueueHandle::Streaming(handle.into())
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
            V4l2QueueHandle::Streaming(handle) => match handle.try_dequeue() {
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
            V4l2QueueHandle::Streaming(handle) => {
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
    pub fn num_buffers(&self) -> usize {
        let handle = &*self.handle.borrow();
        match handle {
            V4l2QueueHandle::Streaming(handle) => handle.num_buffers(),
            _ => 0,
        }
    }
}
