// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use anyhow::anyhow;
use std::cell::RefCell;
use std::fs::File;
use std::rc::Rc;
use std::sync::Arc;
use thiserror::Error;

use v4l2r::bindings::v4l2_format;
use v4l2r::device::queue::direction;
use v4l2r::device::queue::direction::Capture;
use v4l2r::device::queue::direction::Output;
use v4l2r::device::queue::dqbuf::DqBuffer;
use v4l2r::device::queue::qbuf::QBuffer;
use v4l2r::device::queue::BuffersAllocated;
use v4l2r::device::queue::CaptureQueueable;
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
use v4l2r::memory::Memory;
use v4l2r::memory::MemoryType;
use v4l2r::memory::MmapHandle;
use v4l2r::memory::PlaneHandle;
use v4l2r::nix::sys::time::TimeVal;
use v4l2r::Format;
use v4l2r::PlaneLayout;

use crate::decoder::stateless::DecodeError;
use crate::decoder::stateless::StatelessBackendError;
use crate::image_processing::mm21_to_nv12;
use crate::image_processing::nv12_to_i420;
use crate::image_processing::MM21_TILE_HEIGHT;
use crate::image_processing::MM21_TILE_WIDTH;
use crate::utils::align_up;
use crate::utils::buffer_size_for_area;
use crate::video_frame::VideoFrame;
use crate::video_frame::UV_PLANE;
use crate::video_frame::Y_PLANE;
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
enum V4l2OutputQueueHandle<T: v4l2r::device::queue::direction::Direction> {
    Init(Queue<T, QueueInit>),
    Streaming(Rc<Queue<T, BuffersAllocated<Vec<MmapHandle>>>>),
    #[default]
    Unknown,
}

#[derive(Default)]
enum V4l2CaptureQueueHandle<T: v4l2r::device::queue::direction::Direction, H: PlaneHandle> {
    Init(Queue<T, QueueInit>),
    Streaming(Rc<Queue<T, BuffersAllocated<Vec<H>>>>),
    #[default]
    Unknown,
}

#[derive(Debug, Error)]
pub enum QueueError {
    #[error("unable to create queue.")]
    Creation,
    #[error("failed to get format for queue.")]
    FormatGet,
    #[error("failed to set format for queue.")]
    FormatSet,
    #[error("failed requesting buffers.")]
    RequestBuffers,
    #[error("unable to stream on.")]
    StreamOn,
    #[error("driver does not support {0}.")]
    UnsupportedPixelFormat(Fourcc),
    #[error("operation can not be performed in this state.")]
    State,
    #[error("no buffer to dequeue.")]
    Dequeue,
}

impl From<QueueError> for DecodeError {
    fn from(err: QueueError) -> Self {
        DecodeError::BackendError(StatelessBackendError::Other(anyhow::anyhow!(err)))
    }
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

        mapping.as_mut()[self.length..self.length + data.len()].copy_from_slice(data);
        self.length += data.len();

        drop(mapping);
        self
    }
    pub fn submit(self, timestamp: u64, request_fd: i32) -> Result<(), QueueError> {
        let handle = &*self.queue.handle.borrow();
        match handle {
            V4l2OutputQueueHandle::Streaming(_) => {
                self.handle
                    .set_timestamp(TimeVal::new(/* FIXME: sec */ 0, timestamp as i64))
                    .set_request(request_fd)
                    .queue(&[self.length])
                    .expect("Failed to queue output buffer");
                Ok(())
            }
            _ => Err(QueueError::State),
        }
    }
}

#[derive(Clone)]
pub struct V4l2OutputQueue {
    handle: Rc<RefCell<V4l2OutputQueueHandle<direction::Output>>>,
}

const NUM_OUTPUT_BUFFERS: u32 = 2;
impl V4l2OutputQueue {
    pub fn new(device: Arc<Device>) -> Self {
        let handle = Queue::get_output_mplane_queue(device).expect("Failed to get output queue");
        log::debug!("Output queue:\n\tstate: None -> Init\n");
        let handle = Rc::new(RefCell::new(V4l2OutputQueueHandle::Init(handle)));
        Self { handle }
    }

    pub fn initialize(
        &mut self,
        fourcc: Fourcc,
        resolution: Resolution,
    ) -> Result<&mut Self, QueueError> {
        self.handle.replace(match self.handle.take() {
            V4l2OutputQueueHandle::Init(mut handle) => {
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

                V4l2OutputQueueHandle::Streaming(handle.into())
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
    pub fn drain(&self) -> Result<(), QueueError> {
        let handle = &*self.handle.borrow();
        match handle {
            V4l2OutputQueueHandle::Streaming(handle) => loop {
                if let Err(_) = handle.try_dequeue() {
                    break Ok(());
                }
            },
            _ => return Err(QueueError::State),
        }
    }
    pub fn dequeue_buffer(&self) -> Result<(), QueueError> {
        let handle = &*self.handle.borrow();
        match handle {
            V4l2OutputQueueHandle::Streaming(handle) => {
                handle.try_dequeue().map_err(|_| QueueError::Dequeue)?;
                Ok(())
            }
            _ => Err(QueueError::State),
        }
    }
}

pub struct V4l2CaptureBuffer<H: PlaneHandle> {
    frame: Box<dyn VideoFrame<NativeHandle = Vec<H>>>,
    // We need to hang on to this DqBuffer object even though we took out the underlying data
    // because its Drop() function is tied to reference frame management.
    handle: DqBuffer<Capture, Vec<H>>,
    visible_rect: Rect,
    format: Format,
}

impl<H: PlaneHandle> V4l2CaptureBuffer<H> {
    fn new(
        frame: Box<dyn VideoFrame<NativeHandle = Vec<H>>>,
        handle: DqBuffer<Capture, Vec<H>>,
        visible_rect: Rect,
        format: Format,
    ) -> Self {
        Self {
            frame,
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
    // TODO enable once upstream v4l2r has rolled
    //    pub fn has_error(&self) -> bool {
    //        self.handle.data.has_error() as u64
    //    }

    //TODO make this work for formats other then 420
    pub fn length(&self) -> usize {
        (Resolution::from(self.visible_rect).get_area() * 3) / 2
    }

    // TODO: Directly expose VideoFrame to the framework instead of doing an extra memcpy. We need
    // the VA-API changes to merge before we can do this.
    pub fn read(&self, data: &mut [u8]) {
        let decoded_format: DecodedFormat = self
            .format
            .pixelformat
            .to_string()
            .parse()
            .expect("Unable to output");

        // TODO: Replace this with convert_video_frame() in the image_processing module when the
        // necessary conversions are merged.
        match decoded_format {
            DecodedFormat::NV12 => {
                let width = Resolution::from(self.visible_rect).width as usize;
                let height = Resolution::from(self.visible_rect).height as usize;
                let (data_y, data_uv) =
                    data.split_at_mut(Resolution::from(self.visible_rect).get_area());
                let (data_u, data_v) =
                    data_uv.split_at_mut((((width + 1) / 2) * ((height + 1) / 2)) as usize);

                let src_pitches = self.frame.get_plane_pitch();
                let src_mapping = self.frame.map().expect("Mapping failed!");
                let src_planes = src_mapping.get();
                nv12_to_i420(
                    src_planes[Y_PLANE],
                    src_pitches[Y_PLANE],
                    data_y,
                    width,
                    src_planes[UV_PLANE],
                    src_pitches[UV_PLANE],
                    data_u,
                    (width + 1) / 2,
                    data_v,
                    (width + 1) / 2,
                    width,
                    height,
                );
            }
            DecodedFormat::MM21 => {
                let width = Resolution::from(self.visible_rect).width as usize;
                let height = Resolution::from(self.visible_rect).height as usize;
                let aligned_width = align_up(width, MM21_TILE_WIDTH);
                let aligned_height = align_up(height, MM21_TILE_HEIGHT);
                let src_strides = self.frame.get_plane_pitch();
                let src_y_stride = src_strides[Y_PLANE];
                let src_uv_stride = src_strides[UV_PLANE];
                let mut pivot_y = vec![0; aligned_width * aligned_height];
                let mut pivot_uv = vec![0; aligned_width * aligned_height / 2];
                let src_mapping = self.frame.map().expect("Mapping failed!");
                let src_planes = src_mapping.get();
                mm21_to_nv12(
                    src_planes[Y_PLANE],
                    src_y_stride,
                    &mut pivot_y,
                    aligned_width,
                    src_planes[UV_PLANE],
                    src_uv_stride,
                    &mut pivot_uv,
                    aligned_width,
                    aligned_width,
                    aligned_height as isize,
                )
                .expect("Unable to convert mm21 to nv12");

                let (data_y, data_uv) =
                    data.split_at_mut(Resolution::from(self.visible_rect).get_area());
                // TODO: Replace with align_up function.
                let (data_u, data_v) = data_uv.split_at_mut(
                    (((self.visible_rect.width + 1) / 2) * ((self.visible_rect.height + 1) / 2))
                        as usize,
                );
                nv12_to_i420(
                    &pivot_y,
                    aligned_width,
                    data_y,
                    width,
                    &pivot_uv,
                    aligned_width,
                    data_u,
                    align_up(width, 2) / 2,
                    data_v,
                    align_up(width, 2) / 2,
                    width,
                    height,
                );
            }
            _ => panic!("handle me"),
        }
    }
}

pub struct V4l2CaptureQueue<H: PlaneHandle> {
    pub visible_rect: Rect,
    pub format: Format,

    handle: RefCell<V4l2CaptureQueueHandle<direction::Capture, H>>,
    num_buffers: u32,
}

impl<H: PlaneHandle> V4l2CaptureQueue<H> {
    pub fn new(device: Arc<Device>) -> Self {
        let handle = Queue::get_capture_mplane_queue(device).expect("Failed to get capture queue");
        log::debug!("Capture queue:\n\tstate: None -> Init\n");
        let handle = RefCell::new(V4l2CaptureQueueHandle::Init(handle));
        Self {
            handle,
            num_buffers: 0,
            visible_rect: Default::default(),
            format: Default::default(),
        }
    }

    pub fn initialize(
        &mut self,
        visible_rect: Rect,
        num_buffers: u32,
    ) -> Result<&mut Self, QueueError> {
        self.visible_rect = visible_rect;
        // TODO: 20 is chosen as a magic number necessary to keep the buffers
        // flowing. Ideally it would be as close to the dpb as possible.
        self.num_buffers = num_buffers + 20;
        self.handle.replace(match self.handle.take() {
            V4l2CaptureQueueHandle::Init(handle) => {
                // TODO: check if decoded format is supported.
                self.format = handle.get_format()?;
                // TODO: handle 10 bit format negotiation.
                // TODO: We probably want to switch this back to MmapHandle some day. V4L2 MMAP
                // buffers are CPU cache-able on some drivers (such as MTK's vcodec), while DMA
                // buffers in general are not. This is important for detiling efficiency. This will
                // require us to write a V4L2MMAP backend for VideoFrame though.
                let handle = handle.request_buffers_generic::<Vec<H>>(
                    <H as PlaneHandle>::Memory::MEMORY_TYPE,
                    self.num_buffers,
                )?;

                handle.stream_on()?;

                V4l2CaptureQueueHandle::Streaming(handle.into())
            }
            _ => todo!("DRC is not supported"),
        });

        Ok(self)
    }

    pub fn dequeue_buffer(
        &self,
        frame_import_cb: &impl Fn(
            Fourcc,
            Resolution,
            Vec<usize>,
            Vec<H>,
        ) -> Box<dyn VideoFrame<NativeHandle = Vec<H>>>,
    ) -> Result<Option<V4l2CaptureBuffer<H>>, QueueError> {
        let handle = &*self.handle.borrow();
        match handle {
            V4l2CaptureQueueHandle::Streaming(handle) => match handle.try_dequeue() {
                Ok(mut buffer) => {
                    // TODO handle buffer dequeuing successfully, but having an error
                    // buffer.data.has_error();
                    let fourcc = Fourcc::from(self.format.pixelformat.to_u32());
                    let resolution = Resolution {
                        width: self.format.width,
                        height: self.format.height,
                    };

                    let strides: Vec<usize> = self
                        .format
                        .plane_fmt
                        .iter()
                        .map(|x| x.bytesperline as usize)
                        .collect();
                    let native_handle = buffer.take_handles().unwrap();
                    Ok(Some(V4l2CaptureBuffer::new(
                        frame_import_cb(fourcc, resolution, strides, native_handle),
                        buffer,
                        self.visible_rect,
                        self.format.clone(),
                    )))
                }
                _ => Ok(None),
            },
            _ => Err(QueueError::State),
        }
    }

    // TODO: Plumb in VideoFrames from external Gralloc frame pool instead of reallocating every
    // frame every time.
    pub fn queue_buffer(
        &self,
        frame: Box<dyn VideoFrame<NativeHandle = Vec<H>>>,
    ) -> Result<(), QueueError> {
        let handle = &*self.handle.borrow();
        match handle {
            V4l2CaptureQueueHandle::Streaming(handle) => {
                let buffer = handle
                    .try_get_free_buffer()
                    .expect("Failed to alloc capture buffer");
                log::debug!("capture >> index: {}\n", buffer.index());
                let native_handle = frame
                    .to_native_handle()
                    .expect("Failed to export VideoFrame to V4L2 handle");
                buffer
                    .queue_with_handles(native_handle)
                    .expect("Failed to queue capture buffer");
            }
            _ => return Err(QueueError::State),
        }
        Ok(())
    }
    pub fn num_buffers(&self) -> usize {
        let handle = &*self.handle.borrow();
        match handle {
            V4l2CaptureQueueHandle::Streaming(handle) => handle.num_buffers(),
            _ => 0,
        }
    }
    pub fn num_free_buffers(&self) -> usize {
        let handle = &*self.handle.borrow();
        match handle {
            V4l2CaptureQueueHandle::Streaming(handle) => handle.num_free_buffers(),
            _ => 0,
        }
    }
}
