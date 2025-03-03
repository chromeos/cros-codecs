// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use anyhow::anyhow;
use std::cell::RefCell;
use std::convert::Infallible;
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
use v4l2r::device::queue::GetFreeBufferError;
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
use v4l2r::ioctl::IoctlConvertError;
use v4l2r::ioctl::QBufIoctlError;
use v4l2r::ioctl::SFmtError;
use v4l2r::ioctl::StreamOnError;
use v4l2r::memory::BufferHandles;
use v4l2r::memory::Memory;
use v4l2r::memory::MemoryType;
use v4l2r::memory::MmapHandle;
use v4l2r::memory::PlaneHandle;
use v4l2r::nix::sys::time::TimeVal;
use v4l2r::Format;
use v4l2r::PlaneLayout;

use crate::decoder::stateless::DecodeError;
use crate::decoder::stateless::NewStatelessDecoderError;
use crate::decoder::stateless::StatelessBackendError;
use crate::utils::buffer_size_for_area;
use crate::video_frame::V4l2VideoFrame;
use crate::video_frame::VideoFrame;
use crate::Fourcc;
use crate::Resolution;

//TODO: handle memory backends other than mmap
//TODO: handle video formats other than h264
//TODO: handle synced buffers in Streaming state
#[derive(Default)]
enum V4l2OutputQueueHandle<T: v4l2r::device::queue::direction::Direction> {
    Init(Queue<T, QueueInit>),
    Streaming(Rc<Queue<T, BuffersAllocated<Vec<MmapHandle>>>>),
    #[default]
    Unknown,
}

#[derive(Default)]
enum V4l2CaptureQueueHandle<T: v4l2r::device::queue::direction::Direction, V: VideoFrame> {
    Init(Queue<T, QueueInit>),
    Streaming(Rc<Queue<T, BuffersAllocated<V4l2VideoFrame<V>>>>),
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
    BufferDequeue,
    #[error("failed to queue buffer.")]
    BufferQueue,
    #[error("requested {0} buffers, only {1} allocated")]
    NotEnoughRequestedBuffers(usize, usize),
    #[error("failed to stream off.")]
    StreamOff,
    #[error("failed to get queue handle.")]
    QueueHandle,
    #[error("failed to free buffers.")]
    FreeBuffer,
    #[error("failed to reset OUTPUT queue.")]
    ResetOutputQueue,
    #[error("failed to reset CAPTURE queue.")]
    ResetCaptureQueue,
}

impl From<QueueError> for DecodeError {
    fn from(err: QueueError) -> Self {
        DecodeError::BackendError(StatelessBackendError::Other(anyhow::anyhow!(err)))
    }
}

impl<B: BufferHandles> From<v4l2r::device::queue::qbuf::QueueError<B>> for QueueError {
    fn from(_err: v4l2r::device::queue::qbuf::QueueError<B>) -> Self {
        QueueError::BufferQueue
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

impl From<GetFreeBufferError> for QueueError {
    fn from(_err: GetFreeBufferError) -> Self {
        QueueError::BufferDequeue
    }
}

impl From<IoctlConvertError<QBufIoctlError, Infallible>> for QueueError {
    fn from(_err: IoctlConvertError<QBufIoctlError, Infallible>) -> Self {
        QueueError::BufferQueue
    }
}

fn check_requested_buffer_count(requested: u32, received: usize) -> Result<(), QueueError> {
    if received < requested as usize {
        return Err(QueueError::NotEnoughRequestedBuffers(requested as usize, received));
    }
    Ok(())
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
        Self { queue, handle, length: 0 }
    }
    pub fn index(&self) -> usize {
        self.handle.index()
    }
    pub fn length(&self) -> usize {
        self.length
    }
    pub fn write(&mut self, data: &[u8]) -> &mut Self {
        let mut mapping = self.handle.get_plane_mapping(0).expect("Failed to mmap output buffer");

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
                    .queue(&[self.length])?;
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
    pub fn new(device: Arc<Device>) -> Result<Self, NewStatelessDecoderError> {
        let handle = Queue::get_output_mplane_queue(device)
            .map_err(|_| NewStatelessDecoderError::DriverInitialization)?;
        log::debug!("output queue created");
        let handle = Rc::new(RefCell::new(V4l2OutputQueueHandle::Init(handle)));
        Ok(Self { handle })
    }

    pub fn reset(&mut self, device: Arc<Device>) -> Result<(), QueueError> {
        let handle = self.handle.take();
        let (ret, handle) = match handle {
            V4l2OutputQueueHandle::Streaming(handle) => {
                handle.stream_off().map_err(|_| QueueError::StreamOff)?;
                (Rc::into_inner(handle).ok_or(QueueError::QueueHandle)?)
                    .free_buffers()
                    .map_err(|_| QueueError::FreeBuffer)?;

                let queue_handle = Queue::get_output_mplane_queue(device.clone())
                    .map_err(|_| QueueError::QueueHandle);

                (Ok(()), V4l2OutputQueueHandle::Init(queue_handle?))
            }
            _ => (Err(QueueError::State), handle),
        };
        self.handle.replace(handle);
        ret
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

                check_requested_buffer_count(NUM_OUTPUT_BUFFERS, handle.num_free_buffers())?;

                handle.stream_on()?;

                V4l2OutputQueueHandle::Streaming(handle.into())
            }
            _ => return Err(QueueError::State),
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
            _ => Err(DecodeError::DecoderError(anyhow!("Invalid hardware handle"))),
        }
    }
    pub fn dequeue_buffer(&self) -> Result<(), QueueError> {
        let handle = &*self.handle.borrow();
        match handle {
            V4l2OutputQueueHandle::Streaming(handle) => {
                handle.try_dequeue().map_err(|_| QueueError::BufferDequeue)?;
                Ok(())
            }
            _ => Err(QueueError::State),
        }
    }
}

pub struct V4l2CaptureBuffer<V: VideoFrame> {
    pub frame: Arc<V>,
    handle: DqBuffer<Capture, V4l2VideoFrame<V>>,
}

impl<V: VideoFrame> V4l2CaptureBuffer<V> {
    fn new(frame: Arc<V>, handle: DqBuffer<Capture, V4l2VideoFrame<V>>) -> Self {
        Self { frame, handle }
    }
    pub fn timestamp(&self) -> u64 {
        self.handle.data.timestamp().tv_usec as u64
    }
    // TODO enable once upstream v4l2r has rolled
    //    pub fn has_error(&self) -> bool {
    //        self.handle.data.has_error() as u64
    //    }
}

pub struct V4l2CaptureQueue<V: VideoFrame> {
    format: Format,
    handle: RefCell<V4l2CaptureQueueHandle<direction::Capture, V>>,
    num_buffers: u32,
    device: Arc<Device>,
}

impl<V: VideoFrame> V4l2CaptureQueue<V> {
    pub fn new(device: Arc<Device>) -> Result<Self, NewStatelessDecoderError> {
        let handle = Queue::get_capture_mplane_queue(device.clone())
            .map_err(|_| NewStatelessDecoderError::DriverInitialization)?;
        log::debug!("capture queue created");
        let handle = RefCell::new(V4l2CaptureQueueHandle::Init(handle));
        Ok(Self { handle, num_buffers: 0, format: Default::default(), device: device })
    }

    pub fn reset(&mut self, device: Arc<Device>) -> Result<(), QueueError> {
        let handle = self.handle.take();
        let (ret, handle) = match handle {
            V4l2CaptureQueueHandle::Streaming(handle) => {
                handle.stream_off().map_err(|_| QueueError::StreamOff)?;
                (Rc::into_inner(handle).ok_or(QueueError::QueueHandle)?)
                    .free_buffers()
                    .map_err(|_| QueueError::FreeBuffer)?;

                let queue_handle = Queue::get_capture_mplane_queue(device.clone())
                    .map_err(|_| QueueError::QueueHandle);

                (Ok(()), V4l2CaptureQueueHandle::Init(queue_handle?))
            }
            _ => (Err(QueueError::State), handle),
        };
        self.handle.replace(handle);
        ret
    }

    pub fn initialize(&mut self, requested_num_buffers: u32) -> Result<&mut Self, QueueError> {
        // +2 due to HCMP1_HHI_A.h264 needing more
        let requested_num_buffers = requested_num_buffers + 2;

        self.handle.replace(match self.handle.take() {
            V4l2CaptureQueueHandle::Init(handle) => {
                // TODO: check if decoded format is supported.
                self.format = handle.get_format()?;
                // TODO: handle 10 bit format negotiation.
                let handle = handle.request_buffers_generic::<V4l2VideoFrame<V>>(
                    <V::NativeHandle as PlaneHandle>::Memory::MEMORY_TYPE,
                    requested_num_buffers,
                )?;

                check_requested_buffer_count(requested_num_buffers, handle.num_free_buffers())?;

                self.num_buffers = requested_num_buffers;

                handle.stream_on()?;

                V4l2CaptureQueueHandle::Streaming(handle.into())
            }
            _ => return Err(QueueError::State),
        });

        Ok(self)
    }

    pub fn dequeue_buffer(&self) -> Result<Option<V4l2CaptureBuffer<V>>, QueueError> {
        let handle = &*self.handle.borrow();
        match handle {
            V4l2CaptureQueueHandle::Streaming(handle) => match handle.try_dequeue() {
                Ok(mut buffer) => {
                    log::debug!("capture buffer {} dequeued", buffer.data.index());
                    // TODO handle buffer dequeuing successfully, but having an error
                    // buffer.data.has_error();
                    let mut frame = buffer.take_handles().expect("Missing handle on dequeue!").0;
                    frame.process_dqbuf(self.device.clone(), &self.format, &buffer.data);
                    Ok(Some(V4l2CaptureBuffer::new(Arc::new(frame), buffer)))
                }
                _ => Ok(None),
            },
            _ => Err(QueueError::State),
        }
    }

    // TODO: Plumb in VideoFrames from external Gralloc frame pool instead of reallocating every
    // frame every time.
    pub fn queue_buffer(&self, frame: V) -> Result<(), QueueError> {
        let handle = &*self.handle.borrow();
        match handle {
            V4l2CaptureQueueHandle::Streaming(handle) => {
                let buffer = handle.try_get_free_buffer()?;
                log::debug!("capture buffer {} queued", buffer.index());
                buffer.queue_with_handles(frame.into())?;
            }
            _ => return Err(QueueError::State),
        }
        Ok(())
    }

    pub fn num_free_buffers(&self) -> usize {
        let handle = &*self.handle.borrow();
        match handle {
            V4l2CaptureQueueHandle::Streaming(handle) => handle.num_free_buffers(),
            _ => 0,
        }
    }
}
