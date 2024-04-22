// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be

use std::collections::BTreeMap;
use std::fmt::Debug;
use std::marker::PhantomData;
use std::os::fd::AsRawFd;
use std::sync::Arc;

use nix::sys::stat::fstat;
use nix::sys::time::TimeVal;
use thiserror::Error;
use v4l2r::bindings::v4l2_streamparm;
use v4l2r::controls::codec::VideoBitrate;
use v4l2r::controls::codec::VideoBitrateMode;
use v4l2r::controls::codec::VideoConstantQuality;
use v4l2r::controls::codec::VideoForceKeyFrame;
use v4l2r::controls::codec::VideoHeaderMode;
use v4l2r::controls::ExtControlTrait;
use v4l2r::controls::SafeExtControl;
use v4l2r::device::poller::DeviceEvent;
use v4l2r::device::poller::PollError;
use v4l2r::device::poller::Poller;
use v4l2r::device::queue::direction::Capture;
use v4l2r::device::queue::direction::Output;
use v4l2r::device::queue::dqbuf::DqBuffer;
use v4l2r::device::queue::qbuf::get_free::GetFreeBufferError;
use v4l2r::device::queue::qbuf::get_free::GetFreeCaptureBuffer;
use v4l2r::device::queue::qbuf::get_free::GetFreeOutputBuffer;
use v4l2r::device::queue::qbuf::OutputQueueable;
use v4l2r::device::queue::qbuf::OutputQueueableProvider;
use v4l2r::device::queue::qbuf::QBuffer;
use v4l2r::device::queue::BuffersAllocated;
use v4l2r::device::queue::CreateQueueError;
use v4l2r::device::queue::Queue;
use v4l2r::device::queue::RequestBuffersError;
use v4l2r::device::AllocatedQueue;
use v4l2r::device::Device;
use v4l2r::device::Stream;
use v4l2r::device::TryDequeue;
use v4l2r::ioctl;
use v4l2r::ioctl::BufferFlags;
use v4l2r::ioctl::EncoderCommand;
use v4l2r::ioctl::StreamOnError;
use v4l2r::ioctl::V4l2BufferFromError;
use v4l2r::memory::BufferHandles;
use v4l2r::memory::DmaBufHandle;
use v4l2r::memory::MmapHandle;
use v4l2r::memory::PlaneHandle;
use v4l2r::memory::PrimitiveBufferHandles;
use v4l2r::memory::UserPtrHandle;
use v4l2r::Format;
use v4l2r::PixelFormat;
use v4l2r::QueueDirection;
use v4l2r::QueueType;

use crate::encoder::stateful::BackendOutput;
use crate::encoder::stateful::BackendRequest;
use crate::encoder::stateful::BackendRequestId;
use crate::encoder::stateful::StatefulBackendError;
use crate::encoder::stateful::StatefulBackendResult;
use crate::encoder::stateful::StatefulVideoEncoderBackend;
use crate::encoder::CodedBitstreamBuffer;
use crate::encoder::EncodeError;
use crate::encoder::FrameMetadata;
use crate::encoder::RateControl;
use crate::encoder::Tunings;
use crate::utils::DmabufFrame;
use crate::utils::UserPtrFrame;
use crate::Fourcc;
use crate::Resolution;

#[derive(Debug, Error)]
pub enum UnsupportedError {
    #[error("frame upscaling")]
    FrameUpscaling,

    #[error("buffer lacking TIMESTAMP_COPY flag")]
    NoTimestampCopyFlag,

    #[error("unsupported profile")]
    Profile,
}

#[derive(Debug, Error)]
pub enum InitializationError {
    #[error(transparent)]
    Unsupported(UnsupportedError),

    #[error("failed to create a CAPTURE queue: {0:?}")]
    CaptureQueueCreate(CreateQueueError),

    #[error("failed to create a OUTPUT queue: {0:?}")]
    OutputQueueCreate(CreateQueueError),

    #[error("failed to set format for CAPTURE: {0:?}")]
    SetFormatCapture(ioctl::SFmtError),

    #[error("failed to set format for OUTPUT: {0:?}")]
    SetFormatOutput(ioctl::SFmtError),

    #[error("failed to request CAPTURE buffers: {0:?}")]
    RequestBufferCatpure(RequestBuffersError),

    #[error("failed to request OUTPUT buffers: {0:?}")]
    RequestBufferOutput(RequestBuffersError),

    #[error("failed to stream on CAPTURE: {0:?}")]
    StreamOnCapture(StreamOnError),

    #[error("failed to stream on OUTPUT: {0:?}")]
    StreamOnOutput(StreamOnError),

    #[error(transparent)]
    EncoderStart(#[from] ioctl::EncoderCmdError),

    #[error(transparent)]
    CreatePoller(nix::Error),

    #[error(transparent)]
    SetSelection(ioctl::SSelectionError),

    #[error(transparent)]
    Contro(#[from] ControlError),
}

#[derive(Debug, Error)]
pub struct ControlError {
    which: &'static str,
    error: nix::errno::Errno,
}

impl std::fmt::Display for ControlError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "failed to set '{}': {:?}",
            self.which, self.error
        ))
    }
}

#[derive(Debug, Error)]
pub enum BackendError {
    #[error(transparent)]
    Unsupported(UnsupportedError),

    #[error(transparent)]
    GetFreeBufferError(#[from] GetFreeBufferError),

    #[error(transparent)]
    QueueBitstreamBuffer(anyhow::Error),

    #[error(transparent)]
    MapBitstreamBuffer(anyhow::Error),

    #[error(transparent)]
    QueueFrameHandleError(anyhow::Error),

    #[error(transparent)]
    DequeueBuffer(#[from] ioctl::DqBufError<V4l2BufferFromError>),

    #[error("failed to map capture buffer: {0:?}")]
    FailedToMapCapture(Timestamp),

    #[error(transparent)]
    DrainCommand(#[from] ioctl::EncoderCmdError),

    #[error(transparent)]
    Poll(#[from] PollError),

    #[error(transparent)]
    GetFormat(#[from] ioctl::GFmtError),

    #[error(transparent)]
    Control(#[from] ControlError),
}

pub type BackendResult<T> = std::result::Result<T, BackendError>;

impl From<BackendError> for StatefulBackendError {
    fn from(value: BackendError) -> Self {
        StatefulBackendError::Other(anyhow::anyhow!(value))
    }
}

impl From<BackendError> for EncodeError {
    fn from(value: BackendError) -> Self {
        EncodeError::StatefulBackendError(value.into())
    }
}

/// Frame timestamp helper struct
#[repr(transparent)]
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Timestamp(pub u64);

impl From<v4l2r::bindings::timeval> for Timestamp {
    fn from(value: v4l2r::bindings::timeval) -> Self {
        let timestamp = value.tv_sec.wrapping_mul(1_000_000);
        let timestamp = timestamp.wrapping_add(value.tv_usec);
        Timestamp(timestamp.max(0) as u64)
    }
}

impl From<&Timestamp> for TimeVal {
    fn from(value: &Timestamp) -> Self {
        let tv_sec = (value.0 / 1_000_000).min(i64::MAX as u64);
        let tv_usec = (value.0 % 1_000_000).min(i64::MAX as u64);
        Self::new(tv_sec as i64, tv_usec as i64)
    }
}

pub type OutputBuffer<'a, P> =
    <Queue<Output, BuffersAllocated<P>> as OutputQueueableProvider<'a, P>>::Queueable;

/// Encoder input frame handle, that can be queued to OUTPUT queue.
pub trait OutputBufferHandle {
    type PrimitiveBufferHandles: PrimitiveBufferHandles;

    fn queue(self, buffer: OutputBuffer<'_, Self::PrimitiveBufferHandles>) -> anyhow::Result<()>;
}

pub trait AlwaysEntireBufferUsed {}

impl AlwaysEntireBufferUsed for UserPtrFrame {}

impl AlwaysEntireBufferUsed for DmabufFrame {}

impl<T> OutputBufferHandle for T
where
    T: PrimitiveBufferHandles + AlwaysEntireBufferUsed,
{
    type PrimitiveBufferHandles = Self;

    fn queue(self, buffer: OutputBuffer<'_, Self>) -> anyhow::Result<()> {
        let mut bytes_used = Vec::new();
        for i in 0..self.len() {
            let mut plane = v4l2r::bindings::v4l2_plane::default();
            self.fill_v4l2_plane(i, &mut plane);
            bytes_used.push(plane.length as usize);
        }

        log::trace!("Queueing buffer bytes_used={bytes_used:?}");
        buffer.queue_with_handles(self, &bytes_used).unwrap();
        Ok(())
    }
}

impl BufferHandles for UserPtrFrame {
    type SupportedMemoryType = v4l2r::memory::MemoryType;

    fn fill_v4l2_plane(&self, index: usize, plane: &mut v4l2r::bindings::v4l2_plane) {
        let plane_layout = &self.layout.planes[index];

        plane.m.userptr = self.buffers[plane_layout.buffer_index] as _;
        plane.data_offset = plane_layout.offset as _;
        plane.length = self.mem_layout.size() as _;
    }

    fn len(&self) -> usize {
        self.layout.planes.len()
    }
}

impl PrimitiveBufferHandles for UserPtrFrame {
    type HandleType = UserPtrHandle<[u8; 0]>;
    const MEMORY_TYPE: Self::SupportedMemoryType = v4l2r::memory::MemoryType::UserPtr;
}

// SAFETY: Access to the frame is read only
unsafe impl Send for UserPtrFrame {}

// SAFETY: Access to the frame is read only
unsafe impl Sync for UserPtrFrame {}

impl BufferHandles for DmabufFrame {
    type SupportedMemoryType = v4l2r::memory::MemoryType;

    fn fill_v4l2_plane(&self, index: usize, plane: &mut v4l2r::bindings::v4l2_plane) {
        let plane_layout = &self.layout.planes[index];
        let fd = &self.fds[plane_layout.buffer_index];

        plane.m.fd = fd.as_raw_fd();
        plane.data_offset = plane_layout.offset as u32;
        plane.length = fstat(fd.as_raw_fd())
            .map(|stat| stat.st_size as u32)
            .unwrap_or(0);

        if plane.length == 0 {
            log::warn!("Failed to fstat proper plane size index={index}");
        }
    }

    fn len(&self) -> usize {
        self.layout.planes.len()
    }
}

impl PrimitiveBufferHandles for DmabufFrame {
    type HandleType = DmaBufHandle<std::fs::File>;
    const MEMORY_TYPE: Self::SupportedMemoryType = v4l2r::memory::MemoryType::DmaBuf;
}

/// Encoder's coded specific trait enabling setting codec specific tunings
pub trait EncoderCodec {
    /// Set's [`Tunings`] for the [`v4l2r::device::Device`]
    fn apply_tunings(device: &Device, tunings: &Tunings) -> Result<(), ControlError>;
}

/// Trait responsible for CAPTURE buffers of the encoder's [`V4L2Backend`]. Enable custom logic of
/// CAPTURE specific for device/client use case. Useful especially when MMAP buffer type is not
/// supported for CAPTURE queue. In such scenario the client may choose to implement this function
/// and use own logic for allocating DMABUF or USERPTR.
pub trait CaptureBuffers {
    /// [`PlaneHandle`] that is going to be used for CAPTURE buffers.
    type PlaneHandle: PlaneHandle;

    /// Queues the buffer with [`CaptureBuffers::PlaneHandle`]s and returns true,
    /// otherwise if the buffer may not be queue returns false.
    fn queue(
        &mut self,
        buffer: QBuffer<'_, Capture, Vec<Self::PlaneHandle>, Vec<Self::PlaneHandle>>,
    ) -> anyhow::Result<bool>;

    /// Maps the the buffer and returns its contents in form of [`Vec<u8>`]
    fn export(&self, buffer: DqBuffer<Capture, Vec<Self::PlaneHandle>>) -> anyhow::Result<Vec<u8>>;
}

/// [`CaptureBuffers`] implementation for MMAP memory type
pub struct MmapingCapture;

impl CaptureBuffers for MmapingCapture {
    type PlaneHandle = MmapHandle;

    fn queue(
        &mut self,
        buffer: QBuffer<'_, Capture, Vec<Self::PlaneHandle>, Vec<Self::PlaneHandle>>,
    ) -> anyhow::Result<bool> {
        buffer.queue()?;
        Ok(true)
    }

    fn export(&self, buffer: DqBuffer<Capture, Vec<Self::PlaneHandle>>) -> anyhow::Result<Vec<u8>> {
        let timestamp = Timestamp::from(buffer.data.timestamp());
        let Some(mapping) = buffer.get_plane_mapping(0) else {
            log::error!("CAPTURE: Failed to map buffer timestamp={timestamp:?}");
            return Err(BackendError::FailedToMapCapture(timestamp).into());
        };

        let bytesused = *buffer.data.get_first_plane().bytesused as usize;

        Ok(Vec::from(&mapping.data[..bytesused]))
    }
}

/// V4L2 stateful encoder implementation
pub struct V4L2Backend<Handle, CaptureBufferz, Codec>
where
    Handle: OutputBufferHandle,
    CaptureBufferz: CaptureBuffers,
    Self: EncoderCodec,
{
    /// V4L2 encoder device
    device: Arc<Device>,

    /// OUTPUT_MPLANE V4L2 queue
    output_queue: Queue<Output, BuffersAllocated<Handle::PrimitiveBufferHandles>>,

    /// CAPTURE_MPLANE V4L2 queue
    capture_queue: Queue<Capture, BuffersAllocated<Vec<CaptureBufferz::PlaneHandle>>>,

    /// [`CaptureBuffers`] implementation
    capture_buffers: CaptureBufferz,

    /// Buffers that are currently processed by the encoder device
    currently_processed: BTreeMap<Timestamp, (BackendRequestId, FrameMetadata)>,

    /// Currently set [`Tunings`] used to detected tunings change
    current_tunings: Tunings,

    /// Device poller for implementing [`StatefulVideoEncoderBackend::sync`]
    poller: Poller,

    _phantom: PhantomData<(Handle, Codec)>,
}

impl<Handle, CaptureBufferz, Codec> V4L2Backend<Handle, CaptureBufferz, Codec>
where
    Handle: OutputBufferHandle,
    CaptureBufferz: CaptureBuffers,
    Self: EncoderCodec,
{
    /// Checks if the device has the given control and sets it to desired value if it's diffrent
    pub(crate) fn apply_ctrl<C>(
        device: &Device,
        name: &'static str,
        value: C,
    ) -> Result<(), ControlError>
    where
        C: ExtControlTrait<PAYLOAD = i32> + Into<i32>,
    {
        let mut current = SafeExtControl::<C>::from_value(0);

        log::trace!("Trying to set control {name}");
        match ioctl::g_ext_ctrls(device, ioctl::CtrlWhich::Current, &mut current) {
            Ok(()) => (),
            Err(ioctl::ExtControlError {
                error_idx: _,
                error: ioctl::ExtControlErrorType::IoctlError(nix::errno::Errno::EINVAL),
            }) => {
                log::debug!("Setting/getting {name} control is not supported for this device");
                return Ok(());
            }
            Err(ioctl::ExtControlError {
                error_idx: _,
                error: ioctl::ExtControlErrorType::IoctlError(error),
            }) => {
                log::error!("Getting {name} control returned {:?}", error.desc());
                return Err(ControlError { which: name, error });
            }
        };

        let desired: i32 = value.into();
        if current.value() == desired {
            log::debug!("Control {name} already has desired value");
        }

        let mut value = SafeExtControl::<C>::from_value(desired);

        match ioctl::s_ext_ctrls(device, ioctl::CtrlWhich::Current, &mut value) {
            Ok(()) => (),
            Err(ioctl::ExtControlError {
                error_idx: _,
                error: ioctl::ExtControlErrorType::IoctlError(nix::errno::Errno::EINVAL),
            }) => {
                log::debug!("Setting/getting {name} control is not supported for this device");
                return Ok(());
            }
            Err(ioctl::ExtControlError {
                error_idx: _,
                error: ioctl::ExtControlErrorType::IoctlError(error),
            }) => return Err(ControlError { which: name, error }),
        };

        let value = value.value();

        if value != desired {
            // TODO: raise error?
            log::warn!("Failed to set desired {name} (to: {desired}, is: {value})",);
        } else {
            log::trace!("Control {name} set correctly to {value}");
        }

        Ok(())
    }

    /// Sets the frame rate using S_PARM ioctl for the queue type on the device.
    pub(crate) fn apply_parm(device: &Device, queue_type: QueueType, framerate: u32) {
        let mut parm = v4l2_streamparm {
            type_: queue_type as u32,
            ..Default::default()
        };

        let (num, denum) = if framerate != 0 {
            (1, framerate)
        } else {
            (0, 1)
        };

        if matches!(queue_type, v4l2r::QueueType::VideoOutputMplane) {
            parm.parm.output.capability = 0;
            parm.parm.output.outputmode = 0;
            parm.parm.output.timeperframe.numerator = num;
            parm.parm.output.timeperframe.denominator = denum;
        } else {
            parm.parm.capture.capability = 0;
            parm.parm.capture.timeperframe.numerator = num;
            parm.parm.capture.timeperframe.denominator = denum;
        }

        match v4l2r::ioctl::s_parm::<_, v4l2_streamparm>(device, parm) {
            Ok(parm) => match QueueType::n(parm.type_).as_ref().map(QueueType::direction) {
                // SAFETY: The type is set to output
                Some(QueueDirection::Output) => unsafe {
                    log::debug!(
                        "OUTPUT: Time per frame set to {}/{}",
                        parm.parm.output.timeperframe.numerator,
                        parm.parm.output.timeperframe.denominator,
                    );
                },
                // SAFETY: The type is set to capture
                Some(QueueDirection::Capture) => unsafe {
                    log::debug!(
                        "CAPTURE: Time per frame set to {}/{}",
                        parm.parm.capture.timeperframe.numerator,
                        parm.parm.capture.timeperframe.denominator,
                    );
                },
                _ => {}
            },
            Err(errno) => log::warn!(
                "{:?}: Failed to set parm: {errno:?}",
                queue_type.direction()
            ),
        }
    }

    /// Sets the rate mode and bitrate params on the device.
    fn apply_rate_control(
        device: &Device,
        framerate: u32,
        rate_control: &RateControl,
    ) -> Result<(), ControlError> {
        Self::apply_parm(device, QueueType::VideoOutputMplane, framerate);
        Self::apply_parm(device, QueueType::VideoCaptureMplane, 1000);

        Self::apply_ctrl(
            device,
            "bitrate mode",
            match rate_control {
                RateControl::ConstantBitrate(_) => VideoBitrateMode::ConstantBitrate,
                RateControl::ConstantQuality(_) => VideoBitrateMode::ConstantQuality,
            },
        )?;

        if let Some(bitrate) = rate_control.bitrate_target() {
            Self::apply_ctrl(device, "bitrate", VideoBitrate(bitrate as i32))?;
        }

        if let RateControl::ConstantQuality(qp) = rate_control {
            Self::apply_ctrl(device, "constant quality", VideoConstantQuality(*qp as i32))?;
        }

        Ok(())
    }

    /// Sets the crop.
    pub fn apply_selection(
        device: &Device,
        visible_size: Resolution,
    ) -> Result<(), ioctl::SSelectionError> {
        let rect = v4l2r::Rect {
            left: 0,
            top: 0,
            width: visible_size.width,
            height: visible_size.height,
        };

        log::trace!(
            "Trying to apply to selection to (left: {}, top: {}, width: {}, height: {})",
            rect.left,
            rect.top,
            rect.width,
            rect.height
        );

        let rect = ioctl::s_selection::<_, v4l2r::Rect>(
            device,
            ioctl::SelectionType::Output,
            ioctl::SelectionTarget::Crop,
            rect,
            ioctl::SelectionFlags::empty(),
        )?;

        if rect.left == 0
            && rect.top == 0
            && rect.width == visible_size.width
            && rect.height == visible_size.height
        {
            log::trace!("Selection set successfully");
        } else {
            log::warn!(
                "Driver set selection to (left: {}, top: {}, width: {}, height: {})",
                rect.left,
                rect.top,
                rect.width,
                rect.height
            );
        }

        Ok(())
    }

    /// Creates and sets up the backend instance using the given configuration
    pub fn create(
        device: Arc<Device>,
        capture_buffers: CaptureBufferz,
        fourcc: Fourcc,
        coded_size: Resolution,
        visible_size: Resolution,
        capture_pixfmt: v4l2r::PixelFormat,
        tunings: Tunings,
    ) -> Result<Self, InitializationError> {
        let mut capture_queue = Queue::get_capture_mplane_queue(device.clone())
            .map_err(InitializationError::CaptureQueueCreate)?;

        let mut output_queue = Queue::get_output_mplane_queue(device.clone())
            .map_err(InitializationError::OutputQueueCreate)?;

        // Coded buffer size multiplier. It's inteded to give head room for the encoder.
        const CODED_SIZE_MUL: u32 = 2;

        // Default coded buffer size if bitrate control is not used.
        const DEFAULT_CODED_SIZE: u32 = 1_500_000;

        let coded_buffer_size = tunings
            .rate_control
            .bitrate_target()
            .map(|e| e as u32 * CODED_SIZE_MUL)
            .unwrap_or(DEFAULT_CODED_SIZE);

        let capture_format = Format {
            width: coded_size.width,
            height: coded_size.height,
            pixelformat: capture_pixfmt,
            plane_fmt: vec![v4l2r::PlaneLayout {
                sizeimage: coded_buffer_size,
                bytesperline: 0,
            }],
        };

        let capture_format = capture_queue
            .set_format(capture_format)
            .map_err(InitializationError::SetFormatCapture)?;

        // TODO: Map single planar formats to mutli planar format if single planar is not
        // supported.
        let output_pixfmt: PixelFormat = fourcc.0.into();

        let output_format = Format {
            width: visible_size.width,
            height: visible_size.height,
            pixelformat: output_pixfmt,
            // Let the driver pick
            plane_fmt: vec![],
        };

        let output_format = output_queue
            .set_format(output_format)
            .map_err(InitializationError::SetFormatOutput)?;

        log::debug!("CAPTURE queue format = {capture_format:#?}");
        log::debug!("OUTPUT queue format = {output_format:#?}");

        Self::apply_rate_control(&device, tunings.framerate, &tunings.rate_control)?;
        Self::apply_tunings(&device, &tunings)?;

        Self::apply_ctrl(&device, "header mode", VideoHeaderMode::JoinedWith1stFrame)?;

        if visible_size.width > coded_size.width || visible_size.height > coded_size.height {
            return Err(InitializationError::Unsupported(
                UnsupportedError::FrameUpscaling,
            ));
        } else if visible_size != coded_size {
            log::info!("The frame visible size is not aligned to coded size, applying selection");
            if let Err(err) = Self::apply_selection(&device, visible_size) {
                log::error!("Failed to set selection: {err:?}");
            }
        }

        log::debug!("CAPTURE: Requesting buffers");
        let capture_queue = capture_queue
            .request_buffers::<_>(16)
            .map_err(InitializationError::RequestBufferOutput)?;

        log::debug!("OUTPUT: Requesting buffers");
        let output_queue = output_queue
            .request_buffers::<Handle::PrimitiveBufferHandles>(16)
            .map_err(InitializationError::RequestBufferOutput)?;

        log::debug!("CAPTURE: Invoking stream on");
        capture_queue
            .stream_on()
            .map_err(InitializationError::StreamOnCapture)?;

        log::debug!("OUTPUT: Invoking stream on");
        output_queue
            .stream_on()
            .map_err(InitializationError::StreamOnOutput)?;

        log::debug!("Sending start command to encoder");
        ioctl::encoder_cmd::<_, ()>(&device, &EncoderCommand::Start)
            .map_err(InitializationError::EncoderStart)?;

        let mut poller = Poller::new(device.clone()).map_err(InitializationError::CreatePoller)?;

        poller
            .enable_event(DeviceEvent::CaptureReady)
            .map_err(InitializationError::CreatePoller)?;

        Ok(Self {
            device,
            output_queue,
            capture_queue,
            capture_buffers,
            currently_processed: Default::default(),
            current_tunings: tunings,
            poller,
            _phantom: Default::default(),
        })
    }

    pub fn output_format<T: TryFrom<v4l2r::bindings::v4l2_format>>(&self) -> BackendResult<T> {
        Ok(self.output_queue.get_format()?)
    }

    fn poll_device(&mut self) -> BackendResult<()> {
        self.poller.poll(None)?;

        Ok(())
    }

    /// Attempts to queue all free CAPTURE buffer for filling with encoded bitstream
    fn queue_capture(&mut self) -> BackendResult<()> {
        while self.capture_queue.num_free_buffers() != 0 {
            let buffer = self.capture_queue.try_get_free_buffer()?;
            let buffer_index = buffer.index();

            let queued = self
                .capture_buffers
                .queue(buffer)
                .map_err(BackendError::QueueBitstreamBuffer)?;

            if !queued {
                log::warn!("CAPTURE: Capture buffer was queued. Will retry later");
                break;
            }

            log::trace!("CAPTURE: Queued new buffer index={}", buffer_index);
        }

        Ok(())
    }

    /// Tries to dequeue a CAPTURE buffer and transforms the buffer contents into [`BackendOutput`]
    fn dequeue_capture(&mut self) -> BackendResult<Option<BackendOutput>> {
        if self.capture_queue.num_queued_buffers() == 0 {
            // Don't dequeue if there is nothing to dequeue
            log::warn!("Polled while no buffer was queued on CAPTURE queue");
            return Ok(None);
        }

        let buffer = match self.capture_queue.try_dequeue() {
            Ok(buffer) => buffer,
            Err(ioctl::DqBufError::IoctlError(
                err @ ioctl::DqBufIoctlError::NotReady | err @ ioctl::DqBufIoctlError::Eos,
            )) => {
                log::trace!("Dequeue result: {err:?}");
                return Ok(None);
            }
            Err(err) => return Err(err.into()),
        };

        let timestamp = Timestamp::from(buffer.data.timestamp());
        log::debug!(
            "CAPTRUE: Dequeued buffer index={} timestamp={:?} is_last={} bytesused={}, flags={:?}",
            buffer.data.index(),
            timestamp,
            buffer.data.is_last(),
            *buffer.data.get_first_plane().bytesused,
            buffer.data.flags(),
        );

        if *buffer.data.get_first_plane().bytesused == 0 {
            // Don't warn about empty lasty buffer
            if !buffer.data.is_last() {
                log::warn!("CAPTURE: Dequeued empty buffer. Skipping it.");
            }
            return Ok(None);
        }

        if !buffer.data.flags().intersects(BufferFlags::TIMESTAMP_COPY) {
            log::error!("CAPTURE: Buffer does not have TIMESTAMP_COPY flag");
            return Err(BackendError::Unsupported(
                UnsupportedError::NoTimestampCopyFlag,
            ));
        }

        let Some((request_id, meta)) = self.currently_processed.remove(&timestamp) else {
            log::error!("CAPTURE: Failed to find buffer timestamp={timestamp:?}");
            return Err(BackendError::FailedToMapCapture(timestamp));
        };

        let bitstream = self
            .capture_buffers
            .export(buffer)
            .map_err(BackendError::MapBitstreamBuffer)?;

        let output = BackendOutput {
            request_id,
            buffer: CodedBitstreamBuffer::new(meta, bitstream),
        };

        Ok(Some(output))
    }

    /// Dequeues all processed OUTPUT buffers and drops them
    fn drain_output_queue(&mut self) -> BackendResult<()> {
        // Don't dequeue if there is nothing to dequeue
        while self.output_queue.num_queued_buffers() != 0 {
            match self.output_queue.try_dequeue() {
                Ok(buffer) => {
                    log::debug!(
                        "OUTPUT: Dequeued buffer index={} timestamp={:?}",
                        buffer.data.index(),
                        Timestamp::from(buffer.data.timestamp())
                    );
                    // Drop the finished buffer
                    drop(buffer);
                }
                Err(ioctl::DqBufError::IoctlError(ioctl::DqBufIoctlError::NotReady)) => break,
                Err(ioctl::DqBufError::IoctlError(ioctl::DqBufIoctlError::Eos)) => {}
                Err(err) => return Err(err.into()),
            }
        }

        Ok(())
    }

    /// Takes the [`BackendRequest`] and queues it to OUTPUT queue
    fn handle_request(&mut self, request: BackendRequest<Handle>) -> BackendResult<()> {
        if self.current_tunings != request.tunings {
            log::debug!("Changing tunings to {:#?}", request.tunings);
            Self::apply_rate_control(
                &self.device,
                request.tunings.framerate,
                &request.tunings.rate_control,
            )?;
            Self::apply_tunings(&self.device, &request.tunings)?;
            self.current_tunings = request.tunings;
        }

        let buffer = self.output_queue.try_get_free_buffer()?;

        let timestamp = Timestamp(request.meta.timestamp);
        let buffer = buffer.set_timestamp(TimeVal::from(&timestamp));

        let index = buffer.index();

        if request.meta.force_keyframe {
            let mut force = SafeExtControl::<VideoForceKeyFrame>::from_value(1);
            ioctl::s_ext_ctrls(&self.device, ioctl::CtrlWhich::Current, &mut force).map_err(
                |error| ControlError {
                    which: "force keyframe",
                    error: error.error.into(),
                },
            )?;
        }

        request
            .handle
            .queue(buffer)
            .map_err(BackendError::QueueFrameHandleError)?;

        log::debug!(
            "OUTPUT: Queued buffer index={} timestamp={:?}",
            index,
            timestamp
        );

        // TODO: Use RequestId for this?
        self.currently_processed
            .insert(timestamp, (request.request_id, request.meta));

        Ok(())
    }

    /// Performs the essential processing ie. queues and dequeues the buffers from CAPTURE and
    /// OUTPUT queue.
    fn handle_buffers(&mut self) -> BackendResult<()> {
        self.queue_capture()?;
        self.drain_output_queue()?;

        log::debug!(
            "Queue status: OUTPUT(free: {}, queued: {}) CAPTURE(free: {}, queued: {})",
            self.output_queue.num_free_buffers(),
            self.output_queue.num_queued_buffers(),
            self.capture_queue.num_free_buffers(),
            self.capture_queue.num_queued_buffers(),
        );

        Ok(())
    }
}

impl<Handle, CaptureBufferz, Codec> StatefulVideoEncoderBackend<Handle>
    for V4L2Backend<Handle, CaptureBufferz, Codec>
where
    Handle: OutputBufferHandle,
    CaptureBufferz: CaptureBuffers,
    Self: EncoderCodec,
{
    fn consume_request(
        &mut self,
        request: &mut Option<BackendRequest<Handle>>,
    ) -> StatefulBackendResult<()> {
        self.handle_buffers()?;

        if self.output_queue.num_free_buffers() == 0 {
            return Ok(());
        }

        let Some(request) = request.take() else {
            log::error!("StatefulEncoder passed an empty request");
            return Err(StatefulBackendError::InvalidInternalState);
        };

        self.handle_request(request)?;

        Ok(())
    }

    fn sync(&mut self) -> StatefulBackendResult<()> {
        self.poll_device()?;
        Ok(())
    }

    fn poll(&mut self) -> StatefulBackendResult<Option<BackendOutput>> {
        Ok(self.dequeue_capture()?)
    }

    fn drain(&mut self) -> StatefulBackendResult<Vec<BackendOutput>> {
        if self.currently_processed.is_empty() {
            log::info!("Skipping drain sequence, nothing to drain.");
            return Ok(Vec::new());
        }

        log::debug!(
            "Sending stop command to encoder. Currently processing count: {}",
            self.currently_processed.len()
        );

        ioctl::encoder_cmd::<_, ()>(&self.device, &EncoderCommand::Stop(false))
            .map_err(BackendError::DrainCommand)?;

        let mut drained_output = Vec::new();
        while !self.currently_processed.is_empty() {
            self.poll_device()?;
            self.handle_buffers()?;

            if let Some(output) = self.dequeue_capture()? {
                drained_output.push(output);
            }
        }

        // Dequeue is_last=true buffer
        if let Some(output) = self.dequeue_capture()? {
            drained_output.push(output);
        }

        log::debug!("Sending start command to encoder");
        ioctl::encoder_cmd::<_, ()>(&self.device, &EncoderCommand::Start)
            .map_err(BackendError::DrainCommand)?;

        log::debug!("Drain finished");
        Ok(drained_output)
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use std::os::fd::AsFd;
    use std::os::fd::BorrowedFd;
    use std::os::fd::OwnedFd;
    use std::path::Path;
    use std::path::PathBuf;

    use anyhow::Context;
    use v4l2r::device::queue::qbuf::CaptureQueueable;
    use v4l2r::device::DeviceConfig;
    use v4l2r::memory::DmaBufSource;

    use super::*;

    use crate::backend::v4l2::encoder::CaptureBuffers;
    use crate::encoder::simple_encode_loop;
    use crate::encoder::stateful::StatefulEncoder;
    use crate::encoder::tests::fill_test_frame_nm12;
    use crate::encoder::tests::fill_test_frame_nv12;
    use crate::encoder::tests::get_test_frame_t;
    use crate::FrameLayout;

    pub fn find_device_with_capture(pixfmt: v4l2r::PixelFormat) -> Option<PathBuf> {
        const MAX_DEVICE_NO: usize = 128;
        for dev_no in 0..MAX_DEVICE_NO {
            let device_path = PathBuf::from(format!("/dev/video{dev_no}"));
            let Ok(device) = Device::open(&device_path, DeviceConfig::new()) else {
                continue;
            };

            let device = Arc::new(device);

            let Ok(queue) = Queue::get_capture_mplane_queue(device) else {
                continue;
            };

            for fmt in queue.format_iter() {
                if fmt.pixelformat == pixfmt {
                    return Some(device_path);
                }
            }
        }

        None
    }

    /// A simple wrapper for a GBM device node.
    pub struct GbmDevice(std::fs::File);

    impl AsFd for GbmDevice {
        fn as_fd(&self) -> BorrowedFd<'_> {
            self.0.as_fd()
        }
    }

    impl drm::Device for GbmDevice {}

    /// Simple helper methods for opening a `Card`.
    impl GbmDevice {
        pub fn open<P: AsRef<Path>>(path: P) -> std::io::Result<Self> {
            std::fs::OpenOptions::new()
                .read(true)
                .write(true)
                .open(path)
                .map(GbmDevice)
        }
    }

    pub struct BoCaptureBuffer {
        bo: gbm::BufferObject<()>,
        fd: OwnedFd,
        len: u64,
    }

    impl AsRawFd for BoCaptureBuffer {
        fn as_raw_fd(&self) -> std::os::unix::prelude::RawFd {
            self.fd.as_raw_fd()
        }
    }

    impl AsFd for BoCaptureBuffer {
        fn as_fd(&self) -> BorrowedFd<'_> {
            self.fd.as_fd()
        }
    }

    impl DmaBufSource for BoCaptureBuffer {
        fn len(&self) -> u64 {
            self.len
        }
    }

    impl std::fmt::Debug for BoCaptureBuffer {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.debug_struct("BoCaptureBuffer").finish()
        }
    }

    unsafe impl Sync for BoCaptureBuffer {}

    unsafe impl Send for BoCaptureBuffer {}

    // SAFETY: copied from gbm.h
    pub const GBM_BO_USE_SW_READ_OFTEN: gbm::BufferObjectFlags =
        unsafe { gbm::BufferObjectFlags::from_bits_unchecked(1 << 9) };

    // SAFETY: copied from gbm.h
    pub const GBM_BO_USE_HW_VIDEO_ENCODER: gbm::BufferObjectFlags =
        unsafe { gbm::BufferObjectFlags::from_bits_unchecked(1 << 14) };

    pub struct BoPoolAllocator {
        gbm: Arc<gbm::Device<GbmDevice>>,
    }

    impl BoPoolAllocator {
        pub fn new(gbm: Arc<gbm::Device<GbmDevice>>) -> Self {
            Self { gbm }
        }
    }

    impl CaptureBuffers for BoPoolAllocator {
        type PlaneHandle = DmaBufHandle<BoCaptureBuffer>;

        fn queue(
            &mut self,
            buffer: QBuffer<'_, Capture, Vec<Self::PlaneHandle>, Vec<Self::PlaneHandle>>,
        ) -> anyhow::Result<bool> {
            let len = 2 * 1024 * 1024;

            log::trace!("Allocating new bo");
            let bo = self
                .gbm
                .create_buffer_object::<()>(
                    len as u32,
                    1,
                    gbm::Format::R8,
                    GBM_BO_USE_HW_VIDEO_ENCODER | GBM_BO_USE_SW_READ_OFTEN,
                )
                .context("gbm_bo_create")?;

            let fd = bo.fd_for_plane(0).unwrap();
            let handle = BoCaptureBuffer { bo, fd, len };

            buffer
                .queue_with_handles(vec![DmaBufHandle(handle)])
                .context("queue bo as dmabuf handle")?;

            Ok(true)
        }

        fn export(
            &self,
            mut buffer: DqBuffer<Capture, Vec<Self::PlaneHandle>>,
        ) -> anyhow::Result<Vec<u8>> {
            let timestamp = Timestamp::from(buffer.data.timestamp());

            let Some(mut handle) = buffer.take_handles() else {
                log::error!("CAPTURE: Failed to map buffer timestamp={timestamp:?}");
                return Err(BackendError::FailedToMapCapture(timestamp).into());
            };

            let Some(handle) = handle.pop() else {
                log::error!("CAPTURE: Failed to map buffer timestamp={timestamp:?}");
                return Err(BackendError::FailedToMapCapture(timestamp).into());
            };

            let bytesused = *buffer.data.get_first_plane().bytesused;

            let mut content = Vec::with_capacity(bytesused as usize);

            handle.0.bo.map(&self.gbm, 0, 0, bytesused, 1, |mapped| {
                content.extend(mapped.buffer());
            })??;

            Ok(content)
        }
    }

    pub fn v4l2_format_to_frame_layout(format: &v4l2r::Format) -> FrameLayout {
        let mut layout = FrameLayout {
            format: (Fourcc::from(format.pixelformat.to_u32()), 0),
            size: Resolution {
                width: format.width,
                height: format.height,
            },
            planes: format
                .plane_fmt
                .iter()
                .map(|plane| crate::PlaneLayout {
                    buffer_index: 0,
                    offset: 0,
                    stride: plane.bytesperline as usize,
                })
                .collect(),
        };

        // Patch FrameLayout
        match &format.pixelformat.to_fourcc() {
            b"NM12" if layout.planes.len() == 2 => {
                layout.planes[1].buffer_index = 1;
            }
            b"NV12" if layout.planes.len() == 1 => {}
            _ => panic!("Unknown format"),
        };

        layout
    }

    pub struct TestMmapFrame {
        meta: FrameMetadata,
        frame_count: u64,
    }

    impl OutputBufferHandle for TestMmapFrame {
        type PrimitiveBufferHandles = Vec<MmapHandle>;

        fn queue(
            self,
            buffer: OutputBuffer<'_, Self::PrimitiveBufferHandles>,
        ) -> anyhow::Result<()> {
            if self.meta.layout.format == (Fourcc::from(b"NM12"), 0) {
                let mut y_plane = buffer.get_plane_mapping(0).unwrap();
                let mut uv_plane = buffer.get_plane_mapping(1).unwrap();

                fill_test_frame_nm12(
                    self.meta.layout.size.width as usize,
                    self.meta.layout.size.height as usize,
                    [
                        self.meta.layout.planes[0].stride,
                        self.meta.layout.planes[1].stride,
                    ],
                    get_test_frame_t(self.meta.timestamp, self.frame_count),
                    y_plane.as_mut(),
                    uv_plane.as_mut(),
                );

                buffer.queue(&[y_plane.len(), uv_plane.len()])?;
            } else if self.meta.layout.format == (Fourcc::from(b"NV12"), 0) {
                let mut plane = buffer.get_plane_mapping(0).unwrap();

                let strides = [
                    self.meta.layout.planes[0].stride,
                    self.meta.layout.planes[0].stride,
                ];
                let offsets = [
                    self.meta.layout.planes[0].offset,
                    self.meta.layout.planes[0].stride * self.meta.layout.size.height as usize,
                ];

                fill_test_frame_nv12(
                    self.meta.layout.size.width as usize,
                    self.meta.layout.size.height as usize,
                    strides,
                    offsets,
                    get_test_frame_t(self.meta.timestamp, self.frame_count),
                    plane.as_mut(),
                );

                buffer.queue(&[plane.len()])?;
            } else {
                return Err(anyhow::anyhow!("unsupported format"));
            }

            Ok(())
        }
    }

    /// Helper struct. Procedurally generate NV12 or NM12 frames for test purposes.
    pub struct TestMmapFrameGenerator {
        counter: u64,
        max_count: u64,
        frame_layout: FrameLayout,
    }

    impl TestMmapFrameGenerator {
        pub fn new(max_count: u64, frame_layout: FrameLayout) -> Self {
            Self {
                counter: 0,
                max_count,
                frame_layout,
            }
        }
    }

    impl Iterator for TestMmapFrameGenerator {
        type Item = (FrameMetadata, TestMmapFrame);

        fn next(&mut self) -> Option<Self::Item> {
            if self.counter > self.max_count {
                return None;
            }

            self.counter += 1;

            let meta = FrameMetadata {
                timestamp: self.counter,
                layout: self.frame_layout.clone(),
                force_keyframe: false,
            };

            let handle = TestMmapFrame {
                meta: meta.clone(),
                frame_count: self.max_count,
            };

            Some((meta, handle))
        }
    }

    pub fn perform_v4l2_encoder_mmap_test<Codec>(
        frame_count: u64,
        mut encoder: StatefulEncoder<
            TestMmapFrame,
            V4L2Backend<TestMmapFrame, MmapingCapture, Codec>,
        >,
        coded_consumer: impl FnMut(CodedBitstreamBuffer),
    ) where
        V4L2Backend<TestMmapFrame, MmapingCapture, Codec>: EncoderCodec,
    {
        let format: v4l2r::Format = encoder.backend().output_format().unwrap();
        let layout = v4l2_format_to_frame_layout(&format);
        let mut frame_producer = TestMmapFrameGenerator::new(frame_count, layout);

        simple_encode_loop(&mut encoder, &mut frame_producer, coded_consumer).expect("encode loop");
    }

    /// Helper struct. Procedurally generate NV12 or NM12 frames for test purposes.
    pub struct TestDmabufFrameGenerator {
        counter: u64,
        max_count: u64,
        coded_size: Resolution,
        visible_size: Resolution,
        gbm: Arc<gbm::Device<GbmDevice>>,
    }

    impl TestDmabufFrameGenerator {
        pub fn new(
            max_count: u64,
            coded_size: Resolution,
            visible_size: Resolution,
            gbm: Arc<gbm::Device<GbmDevice>>,
        ) -> Self {
            Self {
                counter: 0,
                max_count,
                coded_size,
                visible_size,
                gbm,
            }
        }
    }

    impl Iterator for TestDmabufFrameGenerator {
        type Item = (FrameMetadata, DmabufFrame);

        fn next(&mut self) -> Option<Self::Item> {
            if self.counter > self.max_count {
                return None;
            }

            self.counter += 1;

            let bo = self
                .gbm
                .create_buffer_object::<()>(
                    self.coded_size.width,
                    self.coded_size.height,
                    gbm::Format::Nv12,
                    GBM_BO_USE_HW_VIDEO_ENCODER,
                )
                .expect("create bo");

            let plane_count = bo.plane_count().unwrap() as i32;
            let fourcc = bo.format().unwrap();

            if plane_count > 2 {
                // NOTE: NV12 should be at most 2 plane.
                panic!("Unsupported plane count for bo");
            }

            let mut fds: Vec<OwnedFd> = Vec::new();
            let mut inodes: Vec<u64> = Vec::new();
            let mut planes = Vec::new();

            for plane in 0..(bo.plane_count().unwrap() as i32) {
                let fd = bo.fd_for_plane(plane).unwrap();
                let stat = fstat(fd.as_raw_fd()).unwrap();
                let offset = bo.offset(plane as _).unwrap() as usize;
                let stride = bo.stride_for_plane(plane as _).unwrap() as usize;
                let buffer_index;

                // Deduplicate fds
                if let Some((index, _)) =
                    inodes.iter().enumerate().find(|(_, s)| **s == stat.st_ino)
                {
                    buffer_index = index;
                } else {
                    buffer_index = fds.len();
                    fds.push(fd);
                    inodes.push(stat.st_ino);
                }

                planes.push(crate::PlaneLayout {
                    buffer_index,
                    offset,
                    stride,
                })
            }

            let layout = FrameLayout {
                format: (Fourcc::from(fourcc as u32), 0),
                size: self.visible_size,
                planes,
            };
            dbg!(&layout);

            let meta = FrameMetadata {
                timestamp: self.counter,
                layout: layout.clone(),
                force_keyframe: false,
            };

            let frame = DmabufFrame { fds, layout };

            Some((meta, frame))
        }
    }

    pub fn perform_v4l2_encoder_dmabuf_test<Codec>(
        coded_size: Resolution,
        visible_size: Resolution,
        frame_count: u64,
        gbm: Arc<gbm::Device<GbmDevice>>,
        mut encoder: StatefulEncoder<DmabufFrame, V4L2Backend<DmabufFrame, BoPoolAllocator, Codec>>,
        coded_consumer: impl FnMut(CodedBitstreamBuffer),
    ) where
        V4L2Backend<DmabufFrame, BoPoolAllocator, Codec>: EncoderCodec,
    {
        let format: v4l2r::Format = encoder.backend().output_format().unwrap();

        let mut frame_producer =
            TestDmabufFrameGenerator::new(frame_count, coded_size, visible_size, gbm).map(
                |(meta, mut frame)| {
                    if frame.layout.format.0 == Fourcc::from(b"NV12")
                        && frame.layout.planes.len() == 2
                        && format.pixelformat == PixelFormat::from_fourcc(b"NV12")
                        && format.plane_fmt.len() == 1
                    {
                        // Remove last NV12 plane when GBM advertises 2 plaens and V4L2 expects a
                        // single frame.
                        frame.layout.planes.pop();
                    }

                    (meta, frame)
                },
            );

        simple_encode_loop(&mut encoder, &mut frame_producer, coded_consumer).expect("encode loop");
    }
}
