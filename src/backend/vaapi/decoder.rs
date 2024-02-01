// Copyright 2023 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;

use anyhow::anyhow;
use anyhow::Context as AnyhowContext;
use libva::Config;
use libva::Context;
use libva::Display;
use libva::Image;
use libva::Picture;
use libva::PictureEnd;
use libva::PictureNew;
use libva::PictureSync;
use libva::SurfaceMemoryDescriptor;
use libva::VaError;

use crate::backend::vaapi::p01x_to_i01x;
use crate::backend::vaapi::surface_pool::PooledSurface;
use crate::backend::vaapi::surface_pool::SurfacePool;
use crate::backend::vaapi::va_rt_format_to_string;
use crate::backend::vaapi::y21x_to_i21x;
use crate::backend::vaapi::FormatMap;
use crate::backend::vaapi::FORMAT_MAP;
use crate::decoder::stateless::PoolLayer;
use crate::decoder::stateless::StatelessBackendError;
use crate::decoder::stateless::StatelessBackendResult;
use crate::decoder::stateless::StatelessCodec;
use crate::decoder::stateless::StatelessDecoderBackend;
use crate::decoder::stateless::StatelessDecoderBackendPicture;
use crate::decoder::DecodedHandle as DecodedHandleTrait;
use crate::decoder::DynHandle;
use crate::decoder::FramePool;
use crate::decoder::MappableHandle;
use crate::decoder::StreamInfo;
use crate::i4xx_copy;
use crate::nv12_copy;
use crate::y410_to_i410;
use crate::DecodedFormat;
use crate::Fourcc;
use crate::Resolution;

use super::supported_formats_for_rt_format;
use super::y412_to_i412;

/// A decoded frame handle.
pub(crate) type DecodedHandle<M> = Rc<RefCell<VaapiDecodedHandle<M>>>;

impl<M: SurfaceMemoryDescriptor> DecodedHandleTrait for DecodedHandle<M> {
    type Descriptor = M;

    fn coded_resolution(&self) -> Resolution {
        self.borrow().coded_resolution
    }

    fn display_resolution(&self) -> Resolution {
        self.borrow().display_resolution
    }

    fn timestamp(&self) -> u64 {
        self.borrow().timestamp()
    }

    fn dyn_picture<'a>(&'a self) -> Box<dyn DynHandle + 'a> {
        Box::new(self.borrow())
    }

    fn is_ready(&self) -> bool {
        self.borrow().is_va_ready().unwrap_or(true)
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
    fn coded_size(&self) -> (u32, u32);
    /// Returns the visible rectangle within the coded size for the stream.
    fn visible_rect(&self) -> ((u32, u32), (u32, u32));
}

pub(crate) struct ParsedStreamMetadata {
    /// A VAContext from which we can decode from.
    pub(crate) context: Rc<Context>,
    /// The VAConfig that created the context. It must kept here so that
    /// it does not get dropped while it is in use.
    #[allow(dead_code)]
    config: Config,
    /// Information about the current stream, directly extracted from it.
    stream_info: StreamInfo,
    /// The image format we will use to map the surfaces. This is usually the
    /// same as the surface's internal format, but occasionally we can try
    /// mapping in a different format if requested and if the VA-API driver can
    /// do it.
    map_format: Rc<libva::VAImageFormat>,
    /// The rt_format parsed from the stream.
    rt_format: u32,
    /// The profile parsed from the stream.
    profile: i32,
}

/// Controls how the decoder should create its surface pool.
#[derive(Clone, Debug)]
pub(crate) enum PoolCreationMode {
    /// Create a single pool and assume a single spatial layer. Used for non-SVC
    /// content.
    Highest,
    /// Create a pool for each spatial layer. Used for SVC content.
    Layers(Vec<Resolution>),
}

/// State of the input stream, which can be either unparsed (we don't know the stream properties
/// yet) or parsed (we know the stream properties and are ready to decode).
pub(crate) enum StreamMetadataState {
    /// The metadata for the current stream has not yet been parsed.
    Unparsed,
    /// The metadata for the current stream has been parsed and a suitable
    /// VAContext has been created to accomodate it.
    Parsed(ParsedStreamMetadata),
}

type StreamStateWithPool<M> = (StreamMetadataState, Vec<Rc<RefCell<SurfacePool<M>>>>);

impl StreamMetadataState {
    /// Returns a reference to the parsed metadata state or an error if we haven't reached that
    /// state yet.
    pub(crate) fn get_parsed(&self) -> anyhow::Result<&ParsedStreamMetadata> {
        match self {
            StreamMetadataState::Unparsed { .. } => Err(anyhow!("Stream metadata not parsed yet")),
            StreamMetadataState::Parsed(parsed_metadata) => Ok(parsed_metadata),
        }
    }

    /// Initializes or reinitializes the codec state.
    fn open<S: VaStreamInfo, M: SurfaceMemoryDescriptor>(
        display: &Rc<Display>,
        hdr: S,
        format_map: Option<&FormatMap>,
        old_metadata_state: StreamMetadataState,
        old_surface_pools: Vec<Rc<RefCell<SurfacePool<M>>>>,
        supports_context_reuse: bool,
        pool_creation_mode: PoolCreationMode,
    ) -> anyhow::Result<StreamStateWithPool<M>> {
        let va_profile = hdr.va_profile()?;
        let rt_format = hdr.rt_format()?;

        let coded_resolution =
            Resolution::from(hdr.coded_size()).round(crate::ResolutionRoundMode::Even);

        let format_map = if let Some(format_map) = format_map {
            format_map
        } else {
            // Pick the first one that fits
            FORMAT_MAP
                .iter()
                .find(|&map| map.rt_format == rt_format)
                .ok_or(anyhow!(
                    "format {} is not supported by your hardware or by the implementation for the current codec",
                    va_rt_format_to_string(rt_format)
                ))?
        };

        let map_format = display
            .query_image_formats()?
            .iter()
            .find(|f| f.fourcc == format_map.va_fourcc)
            .cloned()
            .ok_or_else(|| {
                anyhow!(
                    "fourcc {} is not supported by your hardware or by the implementation for the current codec",
                    Fourcc::from(format_map.va_fourcc)
                )
            })?;

        let min_num_surfaces = hdr.min_num_surfaces();

        let visible_rect = hdr.visible_rect();

        let display_resolution = Resolution {
            width: visible_rect.1 .0 - visible_rect.0 .0,
            height: visible_rect.1 .1 - visible_rect.0 .1,
        };

        let layers = match pool_creation_mode {
            PoolCreationMode::Highest => vec![coded_resolution],
            PoolCreationMode::Layers(layers) => layers,
        };

        let (config, context, surface_pools) = match old_metadata_state {
            // Nothing has changed for VAAPI, reuse current context.
            //
            // This can happen as the decoder cannot possibly know whether a
            // given backend will really need to renegotiate on a given change
            // of stream parameters.
            StreamMetadataState::Parsed(old_state)
                if old_state.stream_info.coded_resolution == coded_resolution
                    && old_state.rt_format == rt_format
                    && old_state.profile == va_profile =>
            {
                (old_state.config, old_state.context, old_surface_pools)
            }
            // The resolution has changed, but we support context reuse. Reuse
            // current context.
            StreamMetadataState::Parsed(old_state)
                if supports_context_reuse
                    && old_state.rt_format == rt_format
                    && old_state.profile == va_profile =>
            {
                (old_state.config, old_state.context, old_surface_pools)
            }
            // Create new context.
            _ => {
                let config = display.create_config(
                    vec![libva::VAConfigAttrib {
                        type_: libva::VAConfigAttribType::VAConfigAttribRTFormat,
                        value: rt_format,
                    }],
                    va_profile,
                    libva::VAEntrypoint::VAEntrypointVLD,
                )?;

                let context = display.create_context::<M>(
                    &config,
                    coded_resolution.width,
                    coded_resolution.height,
                    None,
                    true,
                )?;

                let surface_pools = layers
                    .iter()
                    .map(|layer| {
                        SurfacePool::new(
                            Rc::clone(display),
                            rt_format,
                            Some(libva::UsageHint::USAGE_HINT_DECODER),
                            *layer,
                        )
                    })
                    .collect();

                (config, context, surface_pools)
            }
        };

        /* sanity check */
        assert!(surface_pools.len() == layers.len());
        for (pool, layer) in surface_pools.iter().zip(layers.iter()) {
            let mut pool = pool.borrow_mut();
            if !pool.coded_resolution().can_contain(*layer) {
                /* this will purge the old surfaces by not reclaiming them */
                pool.set_coded_resolution(*layer);
            }
        }

        Ok((
            StreamMetadataState::Parsed(ParsedStreamMetadata {
                context,
                config,
                stream_info: StreamInfo {
                    format: match rt_format {
                        libva::constants::VA_RT_FORMAT_YUV420 => DecodedFormat::I420,
                        libva::constants::VA_RT_FORMAT_YUV422 => DecodedFormat::I422,
                        libva::constants::VA_RT_FORMAT_YUV444 => DecodedFormat::I444,
                        libva::constants::VA_RT_FORMAT_YUV420_10 => DecodedFormat::I010,
                        libva::constants::VA_RT_FORMAT_YUV420_12 => DecodedFormat::I012,
                        libva::constants::VA_RT_FORMAT_YUV422_10 => DecodedFormat::I210,
                        libva::constants::VA_RT_FORMAT_YUV422_12 => DecodedFormat::I212,
                        libva::constants::VA_RT_FORMAT_YUV444_10 => DecodedFormat::I410,
                        libva::constants::VA_RT_FORMAT_YUV444_12 => DecodedFormat::I412,
                        _ => panic!("unrecognized RT format {}", rt_format),
                    },
                    coded_resolution,
                    display_resolution,
                    min_num_frames: min_num_surfaces,
                },
                map_format: Rc::new(map_format),
                rt_format,
                profile: va_profile,
            }),
            surface_pools,
        ))
    }
}

/// VA-API backend handle.
///
/// This includes the VA picture which can be pending rendering or complete, as well as useful
/// meta-information.
pub struct VaapiDecodedHandle<M: SurfaceMemoryDescriptor> {
    state: PictureState<M>,
    /// The decoder resolution when this frame was processed. Not all codecs
    /// send resolution data in every frame header.
    coded_resolution: Resolution,
    /// Actual resolution of the visible rectangle in the decoded buffer.
    display_resolution: Resolution,
    /// Image format for this surface, taken from the pool it originates from.
    map_format: Rc<libva::VAImageFormat>,
}

impl<M: SurfaceMemoryDescriptor> VaapiDecodedHandle<M> {
    /// Creates a new pending handle on `surface_id`.
    fn new(
        picture: Picture<PictureNew, PooledSurface<M>>,
        metadata: &ParsedStreamMetadata,
    ) -> anyhow::Result<Self> {
        let picture = picture.begin()?.render()?.end()?;
        Ok(Self {
            state: PictureState::Pending(picture),
            coded_resolution: metadata.stream_info.coded_resolution,
            display_resolution: metadata.stream_info.display_resolution,
            map_format: Rc::clone(&metadata.map_format),
        })
    }

    fn sync(&mut self) -> Result<(), VaError> {
        let res;

        (self.state, res) = match std::mem::replace(&mut self.state, PictureState::Invalid) {
            state @ PictureState::Ready(_) => (state, Ok(())),
            PictureState::Pending(picture) => match picture.sync() {
                Ok(picture) => (PictureState::Ready(picture), Ok(())),
                Err((e, picture)) => (PictureState::Pending(picture), Err(e)),
            },
            PictureState::Invalid => unreachable!(),
        };

        res
    }

    /// Returns a mapped VAImage. this maps the VASurface onto our address space.
    /// This can be used in place of "DynMappableHandle::map()" if the client
    /// wants to access the backend mapping directly for any reason.
    ///
    /// Note that DynMappableHandle is downcastable.
    fn image(&self) -> anyhow::Result<Image> {
        match &self.state {
            PictureState::Ready(picture) => {
                // Map the VASurface onto our address space.
                let image = picture.create_image(
                    *self.map_format,
                    self.coded_resolution.into(),
                    self.display_resolution.into(),
                )?;

                Ok(image)
            }
            // Either we are in `Ready` state or we didn't call `sync()`.
            PictureState::Pending(_) | PictureState::Invalid => {
                Err(anyhow::anyhow!("picture is not in Ready state"))
            }
        }
    }

    /// Returns the picture of this handle.
    pub(crate) fn picture(&self) -> Option<&Picture<PictureSync, PooledSurface<M>>> {
        match &self.state {
            PictureState::Ready(picture) => Some(picture),
            PictureState::Pending(_) => None,
            PictureState::Invalid => unreachable!(),
        }
    }

    /// Returns the timestamp of this handle.
    fn timestamp(&self) -> u64 {
        match &self.state {
            PictureState::Ready(picture) => picture.timestamp(),
            PictureState::Pending(picture) => picture.timestamp(),
            PictureState::Invalid => unreachable!(),
        }
    }

    /// Returns the id of the VA surface backing this handle.
    pub(crate) fn surface_id(&self) -> libva::VASurfaceID {
        match &self.state {
            PictureState::Ready(picture) => picture.surface().id(),
            PictureState::Pending(picture) => picture.surface().id(),
            PictureState::Invalid => unreachable!(),
        }
    }

    fn is_va_ready(&self) -> Result<bool, VaError> {
        match &self.state {
            PictureState::Ready(_) => Ok(true),
            PictureState::Pending(picture) => picture
                .surface()
                .query_status()
                .map(|s| s == libva::VASurfaceStatus::VASurfaceReady),
            PictureState::Invalid => unreachable!(),
        }
    }
}

impl<'a, M: SurfaceMemoryDescriptor> DynHandle for std::cell::Ref<'a, VaapiDecodedHandle<M>> {
    fn dyn_mappable_handle<'b>(&'b self) -> anyhow::Result<Box<dyn MappableHandle + 'b>> {
        self.image().map(|i| Box::new(i) as Box<dyn MappableHandle>)
    }
}

/// Rendering state of a VA picture.
enum PictureState<M: SurfaceMemoryDescriptor> {
    Ready(Picture<PictureSync, PooledSurface<M>>),
    Pending(Picture<PictureEnd, PooledSurface<M>>),
    // Only set in the destructor when we take ownership of the VA picture.
    Invalid,
}

impl<'a> MappableHandle for Image<'a> {
    fn read(&mut self, buffer: &mut [u8]) -> anyhow::Result<()> {
        let image_size = self.image_size();
        let image_inner = self.image();

        let display_resolution = self.display_resolution();
        let width = display_resolution.0 as usize;
        let height = display_resolution.1 as usize;

        if buffer.len() != image_size {
            return Err(anyhow!(
                "buffer size is {} while image size is {}",
                buffer.len(),
                image_size
            ));
        }

        let pitches = image_inner.pitches.map(|x| x as usize);
        let offsets = image_inner.offsets.map(|x| x as usize);

        match image_inner.format.fourcc {
            libva::constants::VA_FOURCC_NV12 => {
                nv12_copy(self.as_ref(), buffer, width, height, pitches, offsets);
            }
            libva::constants::VA_FOURCC_I420 => {
                i4xx_copy(
                    self.as_ref(),
                    buffer,
                    width,
                    height,
                    pitches,
                    offsets,
                    (true, true),
                );
            }
            libva::constants::VA_FOURCC_422H => {
                i4xx_copy(
                    self.as_ref(),
                    buffer,
                    width,
                    height,
                    pitches,
                    offsets,
                    (true, false),
                );
            }
            libva::constants::VA_FOURCC_444P => {
                i4xx_copy(
                    self.as_ref(),
                    buffer,
                    width,
                    height,
                    pitches,
                    offsets,
                    (false, false),
                );
            }
            libva::constants::VA_FOURCC_P010 => {
                p01x_to_i01x(self.as_ref(), buffer, 10, width, height, pitches, offsets);
            }
            libva::constants::VA_FOURCC_P012 => {
                p01x_to_i01x(self.as_ref(), buffer, 12, width, height, pitches, offsets);
            }
            libva::constants::VA_FOURCC_Y210 => {
                y21x_to_i21x(self.as_ref(), buffer, 10, width, height, pitches, offsets);
            }
            libva::constants::VA_FOURCC_Y212 => {
                y21x_to_i21x(self.as_ref(), buffer, 12, width, height, pitches, offsets);
            }
            libva::constants::VA_FOURCC_Y410 => {
                y410_to_i410(self.as_ref(), buffer, width, height, pitches, offsets);
            }
            libva::constants::VA_FOURCC_Y412 => {
                y412_to_i412(self.as_ref(), buffer, width, height, pitches, offsets);
            }
            _ => return Err(StatelessBackendError::UnsupportedFormat.into()),
        }

        Ok(())
    }

    fn image_size(&mut self) -> usize {
        let image = self.image();
        let display_resolution = self.display_resolution();
        crate::decoded_frame_size(
            (&image.format).try_into().unwrap(),
            display_resolution.0 as usize,
            display_resolution.1 as usize,
        )
    }
}

pub struct VaapiBackend<M>
where
    M: SurfaceMemoryDescriptor,
{
    /// VA display in use for this stream.
    display: Rc<Display>,
    /// Pools of surfaces. We reuse surfaces as they are expensive to allocate.
    /// We allow for multiple pools so as to support one spatial layer per pool
    /// when needed.
    pub(crate) surface_pools: Vec<Rc<RefCell<SurfacePool<M>>>>,
    /// The metadata state. Updated whenever the decoder reads new data from the stream.
    pub(crate) metadata_state: StreamMetadataState,
    /// Whether the codec supports context reuse on DRC. This is only supported
    /// by VP9 and AV1.
    supports_context_reuse: bool,
    /// Controls the creation of surface pools.
    pool_creation_mode: PoolCreationMode,
}

impl<M> VaapiBackend<M>
where
    M: SurfaceMemoryDescriptor + 'static,
{
    pub(crate) fn new(display: Rc<libva::Display>, supports_context_reuse: bool) -> Self {
        // Create a pool with reasonable defaults, as we don't know the format of the stream yet.
        let surface_pools = vec![SurfacePool::new(
            Rc::clone(&display),
            libva::constants::VA_RT_FORMAT_YUV420,
            Some(libva::UsageHint::USAGE_HINT_DECODER),
            Resolution::from((16, 16)),
        )];

        Self {
            display,
            surface_pools,
            metadata_state: StreamMetadataState::Unparsed,
            supports_context_reuse,
            pool_creation_mode: PoolCreationMode::Highest,
        }
    }

    pub(crate) fn new_sequence<StreamData>(
        &mut self,
        stream_params: &StreamData,
        pool_creation_mode: PoolCreationMode,
    ) -> StatelessBackendResult<()>
    where
        for<'a> &'a StreamData: VaStreamInfo,
    {
        let old_metadata_state =
            std::mem::replace(&mut self.metadata_state, StreamMetadataState::Unparsed);

        let old_surface_pools = self.surface_pools.drain(..).collect();
        (self.metadata_state, self.surface_pools) = StreamMetadataState::open(
            &self.display,
            stream_params,
            None,
            old_metadata_state,
            old_surface_pools,
            self.supports_context_reuse,
            pool_creation_mode.clone(),
        )?;

        self.pool_creation_mode = pool_creation_mode;

        Ok(())
    }

    pub(crate) fn process_picture<Codec: StatelessCodec>(
        &mut self,
        picture: Picture<PictureNew, PooledSurface<M>>,
    ) -> StatelessBackendResult<<Self as StatelessDecoderBackend<Codec>>::Handle>
    where
        Self: StatelessDecoderBackendPicture<Codec>,
        for<'a> &'a Codec::FormatInfo: VaStreamInfo,
    {
        let metadata = self.metadata_state.get_parsed()?;

        Ok(Rc::new(RefCell::new(VaapiDecodedHandle::new(
            picture, metadata,
        )?)))
    }

    /// Gets a set of supported formats for the particular stream being
    /// processed. This requires that some buffers be processed before this call
    /// is made. Only formats that are compatible with the current color space,
    /// bit depth, and chroma format are returned such that no conversion is
    /// needed.
    fn supported_formats_for_stream(&self) -> anyhow::Result<HashSet<DecodedFormat>> {
        let metadata = self.metadata_state.get_parsed()?;
        let image_formats = self.display.query_image_formats()?;

        let formats = supported_formats_for_rt_format(
            &self.display,
            metadata.rt_format,
            metadata.profile,
            libva::VAEntrypoint::VAEntrypointVLD,
            &image_formats,
        )?;

        Ok(formats.into_iter().map(|f| f.decoded_format).collect())
    }

    pub(crate) fn highest_pool(&mut self) -> &Rc<RefCell<SurfacePool<M>>> {
        /* we guarantee that there is at least one pool, at minimum */
        self.surface_pools
            .iter()
            .max_by_key(|p| p.borrow().coded_resolution().height)
            .unwrap()
    }

    pub(crate) fn pool(&mut self, layer: Resolution) -> Option<&Rc<RefCell<SurfacePool<M>>>> {
        self.surface_pools
            .iter()
            .find(|p| p.borrow().coded_resolution() == layer)
    }
}

/// Shortcut for pictures used for the VAAPI backend.
pub type VaapiPicture<M> = Picture<PictureNew, PooledSurface<M>>;

impl<Codec: StatelessCodec, M> StatelessDecoderBackend<Codec> for VaapiBackend<M>
where
    VaapiBackend<M>: StatelessDecoderBackendPicture<Codec>,
    for<'a> &'a Codec::FormatInfo: VaStreamInfo,
    M: SurfaceMemoryDescriptor + 'static,
{
    type Handle = DecodedHandle<M>;

    fn try_format(
        &mut self,
        format_info: &Codec::FormatInfo,
        format: crate::DecodedFormat,
    ) -> anyhow::Result<()> {
        let supported_formats_for_stream = self.supported_formats_for_stream()?;

        if supported_formats_for_stream.contains(&format) {
            let map_format = FORMAT_MAP
                .iter()
                .find(|&map| map.decoded_format == format)
                .ok_or_else(|| {
                    anyhow!(
                        "cannot find corresponding VA format for decoded format {:?}",
                        format
                    )
                })?;

            let old_metadata_state =
                std::mem::replace(&mut self.metadata_state, StreamMetadataState::Unparsed);

            // TODO: since we have established that it's best to let the VA
            // driver choose the surface's internal (tiled) format, and map to
            // the fourcc we want on-the-fly, this call to open() becomes
            // redundant.
            //
            // Let's fix it at a later commit, because it involves other,
            // non-related, cleanups.
            //
            // This does not apply to other (future) backends, like V4L2, which
            // need to reallocate on format change.
            let old_surface_pools = self.surface_pools.drain(..).collect();
            (self.metadata_state, self.surface_pools) = StreamMetadataState::open(
                &self.display,
                format_info,
                Some(map_format),
                old_metadata_state,
                old_surface_pools,
                self.supports_context_reuse,
                self.pool_creation_mode.clone(),
            )?;

            Ok(())
        } else {
            Err(anyhow!("Format {:?} is unsupported.", format))
        }
    }

    fn frame_pool(&mut self, layer: PoolLayer) -> Vec<&mut dyn FramePool<M>> {
        if let PoolLayer::Highest = layer {
            return vec![self
                .surface_pools
                .iter_mut()
                .max_by_key(|other| other.coded_resolution().height)
                .unwrap()];
        }

        self.surface_pools
            .iter_mut()
            .filter(|pool| {
                match layer {
                    PoolLayer::Highest => unreachable!(),
                    PoolLayer::Layer(resolution) => pool.coded_resolution() == resolution,
                    PoolLayer::All => {
                        /* let all through */
                        true
                    }
                }
            })
            .map(|x| x as &mut dyn FramePool<M>)
            .collect()
    }

    fn stream_info(&self) -> Option<&StreamInfo> {
        self.metadata_state
            .get_parsed()
            .ok()
            .map(|m| &m.stream_info)
    }
}
