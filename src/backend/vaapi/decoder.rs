// Copyright 2023 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::cell::RefCell;
use std::path::PathBuf;
use std::rc::Rc;
use std::sync::Arc;

use anyhow::anyhow;
use anyhow::Context as AnyhowContext;
use libva::{
    Context, Display, Picture, PictureEnd, PictureNew, PictureSync, Surface,
    SurfaceMemoryDescriptor, VaError,
};

use crate::decoder::stateless::StatelessBackendResult;
use crate::decoder::stateless::StatelessCodec;
use crate::decoder::stateless::StatelessDecoderBackend;
use crate::decoder::stateless::StatelessDecoderBackendPicture;
use crate::decoder::stateless::TryFormat;
use crate::decoder::DecodedHandle as DecodedHandleTrait;
use crate::decoder::DynHandle;
use crate::decoder::MappableHandle;
use crate::decoder::StreamInfo;
use crate::image_processing::nv12_to_i420;
use crate::utils::align_up;
use crate::video_frame::gbm_video_frame::GbmDevice;
use crate::video_frame::gbm_video_frame::GbmVideoFrame;
use crate::video_frame::{VideoFrame, UV_PLANE, Y_PLANE};
use crate::DecodedFormat;
use crate::Fourcc;
use crate::Rect;
use crate::Resolution;

/// A decoded frame handle.
pub(crate) type DecodedHandle<M> = Rc<RefCell<VaapiDecodedHandle<M>>>;

/// Gets the VASurfaceID for the given `picture`.
pub(crate) fn va_surface_id<M: SurfaceMemoryDescriptor>(
    handle: &Option<DecodedHandle<M>>,
) -> libva::VASurfaceID {
    match handle {
        None => libva::VA_INVALID_SURFACE,
        Some(handle) => handle.borrow().surface().id(),
    }
}

impl<M: SurfaceMemoryDescriptor> DecodedHandleTrait for DecodedHandle<M> {
    type Descriptor = M;

    fn coded_resolution(&self) -> Resolution {
        self.borrow().surface().size().into()
    }

    fn display_resolution(&self) -> Resolution {
        self.borrow().display_resolution
    }

    fn timestamp(&self) -> u64 {
        self.borrow().timestamp()
    }

    fn dyn_picture<'a>(&'a mut self) -> Box<dyn DynHandle + 'a> {
        Box::new(self.borrow_mut())
    }

    fn is_ready(&self) -> bool {
        self.borrow().state.is_ready().unwrap_or(true)
    }

    fn sync(&self) -> anyhow::Result<()> {
        self.borrow_mut().sync().context("while syncing picture")?;

        Ok(())
    }

    fn resource(&self) -> std::cell::Ref<M> {
        std::cell::Ref::map(self.borrow(), |r| match &r.state {
            PictureState::Ready(p) => p.surface().as_ref(),
            PictureState::Pending(p) => p.surface().as_ref(),
            PictureState::Invalid => unreachable!(),
        })
    }
}

/// A trait for providing the basic information needed to setup libva for decoding.
pub(crate) trait VaStreamInfo {
    /// Returns the VA profile of the stream.
    fn va_profile(&self) -> anyhow::Result<i32>;
    /// Returns the RT format of the stream.
    fn rt_format(&self) -> anyhow::Result<u32>;
    /// Returns the minimum number of surfaces required to decode the stream.
    fn min_num_surfaces(&self) -> usize;
    /// Returns the coded size of the surfaces required to decode the stream.
    fn coded_size(&self) -> Resolution;
    /// Returns the visible rectangle within the coded size for the stream.
    fn visible_rect(&self) -> Rect;
}

/// Rendering state of a VA picture.
enum PictureState<M: SurfaceMemoryDescriptor> {
    Ready(Picture<PictureSync, Surface<M>>),
    Pending(Picture<PictureEnd, Surface<M>>),
    // Only set in sync when we take ownership of the VA picture.
    Invalid,
}

impl<M: SurfaceMemoryDescriptor> PictureState<M> {
    /// Make sure that all pending operations on the picture have completed.
    fn sync(&mut self) -> Result<(), VaError> {
        let res;

        (*self, res) = match std::mem::replace(self, PictureState::Invalid) {
            state @ PictureState::Ready(_) => (state, Ok(())),
            PictureState::Pending(picture) => match picture.sync() {
                Ok(picture) => (PictureState::Ready(picture), Ok(())),
                Err((e, picture)) => (PictureState::Pending(picture), Err(e)),
            },
            PictureState::Invalid => unreachable!(),
        };

        res
    }

    fn surface(&self) -> &Surface<M> {
        match self {
            PictureState::Ready(picture) => picture.surface(),
            PictureState::Pending(picture) => picture.surface(),
            PictureState::Invalid => unreachable!(),
        }
    }

    fn timestamp(&self) -> u64 {
        match self {
            PictureState::Ready(picture) => picture.timestamp(),
            PictureState::Pending(picture) => picture.timestamp(),
            PictureState::Invalid => unreachable!(),
        }
    }

    fn is_ready(&self) -> Result<bool, VaError> {
        match self {
            PictureState::Ready(_) => Ok(true),
            PictureState::Pending(picture) => picture
                .surface()
                .query_status()
                .map(|s| s == libva::VASurfaceStatus::VASurfaceReady),
            PictureState::Invalid => unreachable!(),
        }
    }

    fn new_from_same_surface(&self, timestamp: u64) -> Picture<PictureNew, Surface<M>> {
        match &self {
            PictureState::Ready(picture) => Picture::new_from_same_surface(timestamp, picture),
            PictureState::Pending(picture) => Picture::new_from_same_surface(timestamp, picture),
            PictureState::Invalid => unreachable!(),
        }
    }
}

/// VA-API backend handle.
///
/// This includes the VA picture which can be pending rendering or complete, as well as useful
/// meta-information.
pub struct VaapiDecodedHandle<M>
where
    M: SurfaceMemoryDescriptor,
{
    state: PictureState<M>,
    /// Actual resolution of the visible rectangle in the decoded buffer.
    display_resolution: Resolution,
    frame_import_cb: Box<dyn Fn(&Surface<M>) -> Box<dyn MappableHandle>>,
}

impl<M> VaapiDecodedHandle<M>
where
    M: SurfaceMemoryDescriptor,
{
    /// Creates a new pending handle on `surface_id`.
    fn new(
        picture: Picture<PictureNew, Surface<M>>,
        display_resolution: Resolution,
        frame_import_cb: Box<dyn Fn(&Surface<M>) -> Box<dyn MappableHandle>>,
    ) -> anyhow::Result<Self> {
        let picture = picture.begin()?.render()?.end()?;
        Ok(Self {
            state: PictureState::Pending(picture),
            display_resolution: display_resolution,
            frame_import_cb: frame_import_cb,
        })
    }

    fn sync(&mut self) -> Result<(), VaError> {
        self.state.sync()
    }

    /// Creates a new picture from the surface backing the current one. Useful for interlaced
    /// decoding.
    pub(crate) fn new_picture_from_same_surface(
        &self,
        timestamp: u64,
    ) -> Picture<PictureNew, Surface<M>> {
        self.state.new_from_same_surface(timestamp)
    }

    pub(crate) fn surface(&self) -> &Surface<M> {
        self.state.surface()
    }

    /// Returns the timestamp of this handle.
    fn timestamp(&self) -> u64 {
        self.state.timestamp()
    }
}

impl<'a, M: SurfaceMemoryDescriptor> DynHandle for std::cell::RefMut<'a, VaapiDecodedHandle<M>> {
    fn dyn_mappable_handle<'b>(&'b mut self) -> anyhow::Result<Box<dyn MappableHandle + 'b>> {
        Ok((self.frame_import_cb)(self.surface()))
    }
}

// TODO: Directly expose VideoFrame through StatelessDecoder interface and delete MappableHandle
impl<V, M> MappableHandle for V
where
    M: SurfaceMemoryDescriptor,
    V: VideoFrame<NativeHandle = Surface<M>>,
{
    fn read(&mut self, buffer: &mut [u8]) -> anyhow::Result<()> {
        let (dst_y, dst_uv) = buffer.split_at_mut(self.resolution().get_area());
        let (dst_u, dst_v) = dst_uv.split_at_mut(
            Resolution {
                width: align_up(self.resolution().width, 2),
                height: align_up(self.resolution().height, 2),
            }
            .get_area()
                / 4,
        );

        let src_strides = self.get_plane_pitch();
        let src_mapping = self.map().expect("Mapping failed!");
        let src_planes = src_mapping.get();

        nv12_to_i420(
            src_planes[Y_PLANE],
            src_strides[Y_PLANE],
            dst_y,
            self.resolution().width as usize,
            src_planes[UV_PLANE],
            src_strides[UV_PLANE],
            dst_u,
            align_up(self.resolution().width as usize, 2) / 2,
            dst_v,
            align_up(self.resolution().width as usize, 2) / 2,
            self.resolution().width as usize,
            self.resolution().height as usize,
        );

        Ok(())
    }

    fn image_size(&mut self) -> usize {
        let y_size = self.resolution().get_area();
        let uv_size = Resolution {
            width: align_up(self.resolution().width, 2),
            height: align_up(self.resolution().height, 2),
        }
        .get_area()
            / 2;

        y_size + uv_size
    }
}

pub struct VaapiBackend {
    pub context: Rc<Context>,
    pub stream_info: StreamInfo,

    display: Rc<Display>,
    supports_context_reuse: bool,
    gbm_device: Arc<GbmDevice>,
}

impl VaapiBackend {
    pub(crate) fn new(display: Rc<libva::Display>, supports_context_reuse: bool) -> Self {
        let init_stream_info = StreamInfo {
            format: DecodedFormat::NV12,
            coded_resolution: Resolution::from((16, 16)),
            display_resolution: Resolution::from((16, 16)),
            min_num_frames: 1,
        };
        let config = display
            .create_config(
                vec![libva::VAConfigAttrib {
                    type_: libva::VAConfigAttribType::VAConfigAttribRTFormat,
                    value: libva::VA_RT_FORMAT_YUV420,
                }],
                libva::VAProfile::VAProfileH264Main,
                libva::VAEntrypoint::VAEntrypointVLD,
            )
            .expect("Could not create initial VAConfig!");
        let context = display
            .create_context::<GbmVideoFrame>(
                &config,
                init_stream_info.coded_resolution.width,
                init_stream_info.coded_resolution.height,
                None,
                true,
            )
            .expect("Could not create initial VAContext!");
        Self {
            display: display,
            context: context,
            supports_context_reuse: supports_context_reuse,
            // This will go away once we properly plumb Gralloc frames
            gbm_device: GbmDevice::open(PathBuf::from("/dev/dri/renderD128"))
                .expect("Could not open GBM Device"),
            stream_info: init_stream_info,
        }
    }

    pub(crate) fn new_sequence<StreamData>(
        &mut self,
        stream_params: &StreamData,
    ) -> StatelessBackendResult<()>
    where
        for<'a> &'a StreamData: VaStreamInfo,
    {
        // TODO: Plumb frame pool reallocation event to gralloc

        self.stream_info.display_resolution = Resolution::from(stream_params.visible_rect());
        self.stream_info.coded_resolution = stream_params.coded_size().clone();
        self.stream_info.min_num_frames = stream_params.min_num_surfaces();

        // TODO: Handle context re-use
        // TODO: We should obtain RT_FORMAT from stream_info
        let config = self
            .display
            .create_config(
                vec![libva::VAConfigAttrib {
                    type_: libva::VAConfigAttribType::VAConfigAttribRTFormat,
                    value: libva::VA_RT_FORMAT_YUV420,
                }],
                stream_params.va_profile().map_err(|_| anyhow!("Could not get VAProfile!"))?,
                libva::VAEntrypoint::VAEntrypointVLD,
            )
            .map_err(|_| anyhow!("Could not create VAConfig!"))?;
        let context = self
            .display
            .create_context::<GbmVideoFrame>(
                &config,
                self.stream_info.coded_resolution.width,
                self.stream_info.coded_resolution.height,
                None,
                true,
            )
            .map_err(|_| anyhow!("Could not create VAContext!"))?;
        self.context = context;

        Ok(())
    }

    pub(crate) fn process_picture<Codec: StatelessCodec>(
        &mut self,
        picture: Picture<PictureNew, Surface<GbmVideoFrame>>,
    ) -> StatelessBackendResult<<Self as StatelessDecoderBackend>::Handle>
    where
        Self: StatelessDecoderBackendPicture<Codec>,
        for<'a> &'a Codec::FormatInfo: VaStreamInfo,
    {
        let gbm_device = Arc::clone(&self.gbm_device);
        Ok(Rc::new(RefCell::new(VaapiDecodedHandle::new(
            picture,
            self.stream_info.display_resolution.clone(),
            Box::new(move |surface| {
                Box::new(
                    <Arc<GbmDevice> as Clone>::clone(&gbm_device)
                        .import_from_vaapi(surface)
                        .expect("Failed to import VA-API handle!"),
                )
            }),
        )?)))
    }

    // TODO: Plumb from Gralloc instead.
    pub(crate) fn new_surface(&mut self) -> Surface<GbmVideoFrame> {
        // TODO: Implement actual format negotiation.
        let frame = Box::new(
            self.gbm_device
                .clone()
                .new_frame(
                    Fourcc::from(b"NV12"),
                    self.stream_info.display_resolution.clone(),
                    self.stream_info.coded_resolution.clone(),
                )
                .expect("Failed to allocate VaSurface!"),
        );
        frame.to_native_handle(&self.display).expect("Failed to export frame to VA-API surface!")
    }
}

/// Shortcut for pictures used for the VAAPI backend.
pub type VaapiPicture<M> = Picture<PictureNew, Surface<M>>;

impl StatelessDecoderBackend for VaapiBackend {
    type Handle = DecodedHandle<GbmVideoFrame>;

    fn stream_info(&self) -> Option<&StreamInfo> {
        Some(&self.stream_info)
    }
}

impl<Codec: StatelessCodec> TryFormat<Codec> for VaapiBackend
where
    for<'a> &'a Codec::FormatInfo: VaStreamInfo,
{
    // TODO: Replace with format negotiation with Gralloc pool
    fn try_format(
        &mut self,
        format_info: &Codec::FormatInfo,
        format: DecodedFormat,
    ) -> anyhow::Result<()> {
        if format == DecodedFormat::I420 {
            Ok(())
        } else {
            Err(anyhow!("Format {format:?} not yet support"))
        }
    }
}
