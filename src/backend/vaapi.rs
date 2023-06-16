// Copyright 2022 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::cell::RefCell;
use std::cell::RefMut;
use std::collections::HashSet;
use std::fmt::Debug;
use std::marker::PhantomData;
use std::rc::Rc;

use anyhow::anyhow;
use anyhow::Context as AnyhowContext;
use byteorder::ByteOrder;
use byteorder::LittleEndian;
use libva::Config;
use libva::Context;
use libva::Display;
use libva::Image;
use libva::Picture;
use libva::PictureEnd;
use libva::PictureNew;
use libva::PictureSync;
use libva::SurfaceMemoryDescriptor;
use libva::VAConfigAttrib;
use libva::VAConfigAttribType;
use libva::VaError;

use crate::backend::vaapi::surface_pool::SurfacePool;
use crate::decoder::stateless::StatelessBackendError;
use crate::decoder::stateless::StatelessBackendResult;
use crate::decoder::stateless::StatelessDecoderBackend;
use crate::decoder::DecodedHandle as DecodedHandleTrait;
use crate::decoder::DynHandle;
use crate::decoder::MappableHandle;
use crate::i4xx_copy;
use crate::nv12_copy;
use crate::y410_to_i410;
use crate::DecodedFormat;
use crate::Fourcc;
use crate::Resolution;

pub(crate) use surface_pool::PooledSurface;

fn va_rt_format_to_string(va_rt_format: u32) -> String {
    String::from(match va_rt_format {
        libva::constants::VA_RT_FORMAT_YUV420 => "YUV420",
        libva::constants::VA_RT_FORMAT_YUV422 => "YUV422",
        libva::constants::VA_RT_FORMAT_YUV444 => "YUV444",
        libva::constants::VA_RT_FORMAT_YUV420_10 => "YUV420_10",
        libva::constants::VA_RT_FORMAT_YUV420_12 => "YUV420_12",
        libva::constants::VA_RT_FORMAT_YUV422_10 => "YUV422_10",
        libva::constants::VA_RT_FORMAT_YUV422_12 => "YUV422_12",
        libva::constants::VA_RT_FORMAT_YUV444_10 => "YUV444_10",
        libva::constants::VA_RT_FORMAT_YUV444_12 => "YUV444_12",
        other => return format!("unknown VA rt_format {}", other),
    })
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
struct FormatMap {
    pub rt_format: u32,
    pub va_fourcc: u32,
    pub decoded_format: DecodedFormat,
}

/// Maps a given VA_RT_FORMAT to a compatible decoded format in an arbitrary
/// preferred order.
const FORMAT_MAP: [FormatMap; 10] = [
    FormatMap {
        rt_format: libva::constants::VA_RT_FORMAT_YUV420,
        va_fourcc: libva::constants::VA_FOURCC_NV12,
        decoded_format: DecodedFormat::NV12,
    },
    FormatMap {
        rt_format: libva::constants::VA_RT_FORMAT_YUV420,
        va_fourcc: libva::constants::VA_FOURCC_I420,
        decoded_format: DecodedFormat::I420,
    },
    FormatMap {
        rt_format: libva::constants::VA_RT_FORMAT_YUV422,
        va_fourcc: libva::constants::VA_FOURCC_422H,
        decoded_format: DecodedFormat::I422,
    },
    FormatMap {
        rt_format: libva::constants::VA_RT_FORMAT_YUV444,
        va_fourcc: libva::constants::VA_FOURCC_444P,
        decoded_format: DecodedFormat::I444,
    },
    FormatMap {
        rt_format: libva::constants::VA_RT_FORMAT_YUV420_10,
        va_fourcc: libva::constants::VA_FOURCC_P010,
        decoded_format: DecodedFormat::I010,
    },
    FormatMap {
        rt_format: libva::constants::VA_RT_FORMAT_YUV420_12,
        va_fourcc: libva::constants::VA_FOURCC_P012,
        decoded_format: DecodedFormat::I012,
    },
    FormatMap {
        rt_format: libva::constants::VA_RT_FORMAT_YUV422_10,
        va_fourcc: libva::constants::VA_FOURCC_Y210,
        decoded_format: DecodedFormat::I210,
    },
    FormatMap {
        rt_format: libva::constants::VA_RT_FORMAT_YUV422_12,
        va_fourcc: libva::constants::VA_FOURCC_Y212,
        decoded_format: DecodedFormat::I212,
    },
    FormatMap {
        rt_format: libva::constants::VA_RT_FORMAT_YUV444_10,
        va_fourcc: libva::constants::VA_FOURCC_Y410,
        decoded_format: DecodedFormat::I410,
    },
    FormatMap {
        rt_format: libva::constants::VA_RT_FORMAT_YUV444_12,
        va_fourcc: libva::constants::VA_FOURCC_Y412,
        decoded_format: DecodedFormat::I412,
    },
];

/// Returns a set of supported decoded formats given `rt_format`
fn supported_formats_for_rt_format(
    display: &Display,
    rt_format: u32,
    profile: i32,
    entrypoint: u32,
    image_formats: &[libva::VAImageFormat],
) -> anyhow::Result<HashSet<FormatMap>> {
    let mut attrs = vec![VAConfigAttrib {
        type_: VAConfigAttribType::VAConfigAttribRTFormat,
        value: 0,
    }];

    display.get_config_attributes(profile, entrypoint, &mut attrs)?;

    // See whether this RT_FORMAT is supported by the given VAProfile and
    // VAEntrypoint pair.
    if attrs[0].value == libva::constants::VA_ATTRIB_NOT_SUPPORTED
        || attrs[0].value & rt_format == 0
    {
        return Err(anyhow!(
            "rt_format {:?} not supported for profile {:?} and entrypoint {:?}",
            rt_format,
            profile,
            entrypoint
        ));
    }

    let mut supported_formats = HashSet::new();

    for format in FORMAT_MAP {
        if format.rt_format == rt_format {
            supported_formats.insert(format);
        }
    }

    // Only retain those that the hardware can actually map into.
    supported_formats.retain(|&entry| {
        image_formats
            .iter()
            .any(|fmt| fmt.fourcc == entry.va_fourcc)
    });

    Ok(supported_formats)
}

/// A decoded frame handle.
pub(crate) type DecodedHandle = Rc<RefCell<GenericBackendHandle<()>>>;

impl DecodedHandleTrait for DecodedHandle {
    fn coded_resolution(&self) -> Resolution {
        self.borrow().coded_resolution
    }

    fn display_resolution(&self) -> Resolution {
        self.borrow().display_resolution
    }

    fn timestamp(&self) -> u64 {
        self.borrow().timestamp()
    }

    fn dyn_picture_mut(&self) -> RefMut<dyn DynHandle> {
        self.borrow_mut()
    }

    fn is_ready(&self) -> bool {
        self.borrow().is_va_ready().unwrap_or(true)
    }

    fn sync(&self) -> anyhow::Result<()> {
        self.borrow_mut().sync().context("while syncing picture")?;

        Ok(())
    }
}

mod surface_pool {
    use std::borrow::Borrow;
    use std::cell::RefCell;
    use std::collections::VecDeque;
    use std::rc::Rc;
    use std::rc::Weak;

    use libva::Display;
    use libva::Surface;
    use libva::SurfaceMemoryDescriptor;
    use libva::VaError;

    use crate::Resolution;

    /// A VA Surface obtained from a `[SurfacePool]`.
    ///
    /// The surface will automatically be returned to its pool upon dropping, provided the pool still
    /// exists and the surface is still compatible with it.
    pub struct PooledSurface<D: SurfaceMemoryDescriptor> {
        surface: Option<Surface<D>>,
        pool: Weak<RefCell<SurfacePool<D>>>,
    }

    impl<D: SurfaceMemoryDescriptor> PooledSurface<D> {
        fn new(surface: Surface<D>, pool: &Rc<RefCell<SurfacePool<D>>>) -> Self {
            Self {
                surface: Some(surface),
                pool: Rc::downgrade(pool),
            }
        }

        /// Detach this surface from the pool. It will not be returned, and we can dispose of it
        /// freely.
        pub fn detach_from_pool(mut self) -> Surface<D> {
            // `unwrap` will never fail as `surface` is `Some` up to this point.
            self.surface.take().unwrap()
        }
    }

    impl<D: SurfaceMemoryDescriptor> Borrow<Surface<D>> for PooledSurface<D> {
        fn borrow(&self) -> &Surface<D> {
            // `unwrap` will never fail as `surface` is `Some` until the object is dropped.
            self.surface.as_ref().unwrap()
        }
    }

    impl<D: SurfaceMemoryDescriptor> Drop for PooledSurface<D> {
        fn drop(&mut self) {
            if let Some(surface) = self.surface.take() {
                if let Some(pool) = self.pool.upgrade() {
                    // It is OK if the pool rejects the surface. It means that the
                    // surface is stale and will be gracefully dropped.
                    if let Err(surface) = pool.borrow_mut().add_surface(surface) {
                        log::debug!(
                            "Dropping stale surface: {}, ({:?})",
                            surface.id(),
                            surface.size()
                        )
                    }
                }
            }
        }
    }

    /// A surface pool to reduce the number of costly Surface allocations.
    ///
    /// The pool only houses Surfaces that fits the pool's coded resolution.
    /// Stale surfaces are dropped when either the pool resolution changes, or when
    /// stale surfaces are retrieved.
    ///
    /// This means that this pool is suitable for inter-frame DRC, as the stale
    /// surfaces will gracefully be dropped, which is arguably better than the
    /// alternative of having more than one pool active at a time.
    pub(crate) struct SurfacePool<D: SurfaceMemoryDescriptor> {
        display: Rc<Display>,
        rt_format: u32,
        usage_hint: Option<libva::UsageHint>,
        coded_resolution: Resolution,
        surfaces: VecDeque<Surface<D>>,
    }

    impl<D: SurfaceMemoryDescriptor> SurfacePool<D> {
        /// Create a new pool.
        ///
        /// # Arguments
        ///
        /// * `display` - the VA display to create the surfaces from.
        /// * `rt_format` - the VA RT format to use for the surfaces.
        /// * `usage_hint` - hint about how the surfaces from this pool will be used.
        /// * `coded_resolution` - resolution of the surfaces.
        pub(crate) fn new(
            display: Rc<Display>,
            rt_format: u32,
            usage_hint: Option<libva::UsageHint>,
            coded_resolution: Resolution,
        ) -> Self {
            Self {
                display,
                rt_format,
                usage_hint,
                coded_resolution,
                surfaces: VecDeque::new(),
            }
        }

        /// Create new surfaces and add them to the pool.
        ///
        /// `num_surfaces` is the number of surfaces to be created and added.
        pub(crate) fn create_surfaces(
            &mut self,
            descriptors: Vec<D>,
        ) -> Result<(), (VaError, Vec<D>)> {
            let surfaces = self.display.create_surfaces(
                self.rt_format,
                // Let the hardware decide the best internal format - we will get the desired fourcc
                // when creating the image.
                None,
                self.coded_resolution.width,
                self.coded_resolution.height,
                self.usage_hint,
                descriptors,
            )?;

            for surface in surfaces {
                self.surfaces.push_back(surface);
            }

            Ok(())
        }

        /// Retrieve the current coded resolution of the pool
        pub(crate) fn coded_resolution(&self) -> Resolution {
            self.coded_resolution
        }

        /// Sets the coded resolution of the pool. Releases any stale surfaces.
        pub(crate) fn set_coded_resolution(&mut self, resolution: Resolution) {
            self.coded_resolution = resolution;
            self.surfaces
                .retain(|s| Resolution::from(s.size()).can_contain(self.coded_resolution));
        }

        /// Add a surface to the pool.
        ///
        /// This can be an entirely new surface, or one that has been previously obtained using
        /// `get_surface` and is returned.
        ///
        /// Returns an error (and the passed `surface` back) if the surface is not at least as
        /// large as the current coded resolution of the pool.
        pub(crate) fn add_surface(&mut self, surface: Surface<D>) -> Result<(), Surface<D>> {
            if Resolution::from(surface.size()).can_contain(self.coded_resolution) {
                self.surfaces.push_back(surface);
                Ok(())
            } else {
                Err(surface)
            }
        }

        /// Gets a free surface from the pool.
        ///
        /// `return_pool` is a reference to the smart pointer containing the pool. It is a bit
        /// inelegant, but we unfortunately cannot declare `self` to be `&Rc<RefCell<Self>>` so we
        /// have to use this workaround.
        pub(crate) fn get_surface(
            &mut self,
            return_pool: &Rc<RefCell<Self>>,
        ) -> Option<PooledSurface<D>> {
            let surface = self.surfaces.pop_front();

            // Make sure the invariant holds when debugging. Can save costly
            // debugging time during future refactors, if any.
            debug_assert!({
                match surface.as_ref() {
                    Some(s) => Resolution::from(s.size()).can_contain(self.coded_resolution),
                    None => true,
                }
            });

            surface.map(|s| PooledSurface::new(s, return_pool))
        }

        /// Returns new number of surfaces left.
        pub(crate) fn num_surfaces_left(&self) -> usize {
            self.surfaces.len()
        }
    }
}

/// A trait for providing the basic information needed to setup libva for decoding.
pub(crate) trait StreamInfo {
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
    /// A pool of surfaces. We reuse surfaces as they are expensive to allocate.
    pub(crate) surface_pool: Rc<RefCell<SurfacePool<()>>>,
    /// The number of surfaces required to parse the stream.
    min_num_surfaces: usize,
    /// The decoder current coded resolution.
    coded_resolution: Resolution,
    /// The decoder current display resolution.
    display_resolution: Resolution,
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

/// State of the input stream, which can be either unparsed (we don't know the stream properties
/// yet) or parsed (we know the stream properties and are ready to decode).
pub(crate) enum StreamMetadataState {
    /// The metadata for the current stream has not yet been parsed.
    Unparsed,
    /// The metadata for the current stream has been parsed and a suitable
    /// VAContext has been created to accomodate it.
    Parsed(ParsedStreamMetadata),
}

impl StreamMetadataState {
    /// Returns a reference to the parsed metadata state or an error if we haven't reached that
    /// state yet.
    pub(crate) fn get_parsed(&self) -> anyhow::Result<&ParsedStreamMetadata> {
        match self {
            StreamMetadataState::Unparsed { .. } => Err(anyhow!("Stream metadata not parsed yet")),
            StreamMetadataState::Parsed(parsed_metadata) => Ok(parsed_metadata),
        }
    }

    /// Returns a mutable reference to the parsed metadata state or an error if we haven't reached
    /// that state yet.
    pub(crate) fn get_parsed_mut(&mut self) -> anyhow::Result<&mut ParsedStreamMetadata> {
        match self {
            StreamMetadataState::Unparsed { .. } => Err(anyhow!("Stream metadata not parsed yet")),
            StreamMetadataState::Parsed(parsed_metadata) => Ok(parsed_metadata),
        }
    }

    /// Initializes or reinitializes the codec state.
    fn open<S: StreamInfo>(
        display: &Rc<Display>,
        hdr: S,
        format_map: Option<&FormatMap>,
        old_metadata_state: StreamMetadataState,
    ) -> anyhow::Result<StreamMetadataState> {
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

        let (create_new_surfaces, surface_pool) = match old_metadata_state {
            StreamMetadataState::Unparsed => (
                true,
                Rc::new(RefCell::new(SurfacePool::new(
                    Rc::clone(display),
                    rt_format,
                    Some(libva::UsageHint::USAGE_HINT_DECODER),
                    coded_resolution,
                ))),
            ),
            StreamMetadataState::Parsed(ParsedStreamMetadata {
                min_num_surfaces: old_min_num_surfaces,
                ref surface_pool,
                ..
            }) => {
                let create_new_surfaces = min_num_surfaces > old_min_num_surfaces
                    || !surface_pool
                        .borrow()
                        .coded_resolution()
                        .can_contain(coded_resolution);

                (create_new_surfaces, Rc::clone(surface_pool))
            }
        };

        if !surface_pool
            .borrow()
            .coded_resolution()
            .can_contain(coded_resolution)
        {
            // Purge the old surfaces to receive the new ones below. This
            // ensures that the pool is always set to the largest resolution in
            // the stream, so that no new allocations are needed when we come
            // across a smaller resolution. In particular, for
            // video-conferencing applications, which are subject to bandwidth
            // fluctuations, this can be very advantageous as it avoid
            // reallocating all the time.
            surface_pool
                .borrow_mut()
                .set_coded_resolution(coded_resolution);
        }

        let (config, context) = match old_metadata_state {
            // Reuse current context.
            StreamMetadataState::Parsed(old_state)
                if old_state.rt_format == rt_format && old_state.profile == va_profile =>
            {
                (old_state.config, old_state.context)
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

                let context = display.create_context::<()>(
                    &config,
                    coded_resolution.width,
                    coded_resolution.height,
                    None,
                    true,
                )?;

                (config, context)
            }
        };

        if create_new_surfaces {
            surface_pool
                .borrow_mut()
                .create_surfaces(vec![(); min_num_surfaces])
                .map_err(|e| e.0)?;
        }

        Ok(StreamMetadataState::Parsed(ParsedStreamMetadata {
            context,
            config,
            surface_pool,
            min_num_surfaces,
            coded_resolution,
            display_resolution,
            map_format: Rc::new(map_format),
            rt_format,
            profile: va_profile,
        }))
    }
}

/// VA-API backend handle.
///
/// This includes the VA picture which can be pending rendering or complete, as well as useful
/// meta-information.
pub struct GenericBackendHandle<D: SurfaceMemoryDescriptor> {
    state: PictureState<D>,
    /// The decoder resolution when this frame was processed. Not all codecs
    /// send resolution data in every frame header.
    coded_resolution: Resolution,
    /// Actual resolution of the visible rectangle in the decoded buffer.
    display_resolution: Resolution,
    /// Image format for this surface, taken from the pool it originates from.
    map_format: Rc<libva::VAImageFormat>,
}

impl GenericBackendHandle<()> {
    /// Creates a new pending handle on `surface_id`.
    fn new(
        picture: Picture<PictureNew, PooledSurface<()>>,
        metadata: &ParsedStreamMetadata,
    ) -> anyhow::Result<Self> {
        let picture = picture.begin()?.render()?.end()?;
        Ok(Self {
            state: PictureState::Pending(picture),
            coded_resolution: metadata.coded_resolution,
            display_resolution: metadata.display_resolution,
            map_format: Rc::clone(&metadata.map_format),
        })
    }
}

impl<D: SurfaceMemoryDescriptor> GenericBackendHandle<D> {
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
    fn image(&mut self) -> anyhow::Result<Image> {
        // Image can only be retrieved in the `Ready` state.
        self.sync()?;

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
            // Either we are in `Ready` state or `sync` failed and we returned.
            PictureState::Pending(_) | PictureState::Invalid => unreachable!(),
        }
    }

    /// Returns the picture of this handle.
    pub(crate) fn picture(&self) -> Option<&Picture<PictureSync, PooledSurface<D>>> {
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

impl<D: SurfaceMemoryDescriptor> DynHandle for GenericBackendHandle<D> {
    fn dyn_mappable_handle_mut<'a>(&'a mut self) -> Box<dyn MappableHandle + 'a> {
        Box::new(self.image().unwrap())
    }
}

/// Rendering state of a VA picture.
enum PictureState<D: SurfaceMemoryDescriptor> {
    Ready(Picture<PictureSync, PooledSurface<D>>),
    Pending(Picture<PictureEnd, PooledSurface<D>>),
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

impl TryFrom<&libva::VAImageFormat> for DecodedFormat {
    type Error = anyhow::Error;

    fn try_from(value: &libva::VAImageFormat) -> Result<Self, Self::Error> {
        match value.fourcc {
            libva::constants::VA_FOURCC_I420 => Ok(DecodedFormat::I420),
            libva::constants::VA_FOURCC_NV12 => Ok(DecodedFormat::NV12),
            libva::constants::VA_FOURCC_P010 => Ok(DecodedFormat::I010),
            libva::constants::VA_FOURCC_P012 => Ok(DecodedFormat::I012),
            libva::constants::VA_FOURCC_Y210 => Ok(DecodedFormat::I210),
            libva::constants::VA_FOURCC_Y212 => Ok(DecodedFormat::I212),
            libva::constants::VA_FOURCC_Y410 => Ok(DecodedFormat::I410),
            libva::constants::VA_FOURCC_Y412 => Ok(DecodedFormat::I412),
            _ => Err(anyhow!("Unsupported format")),
        }
    }
}

pub(crate) struct VaapiBackend<StreamData>
where
    for<'a> &'a StreamData: StreamInfo,
{
    /// VA display in use for this stream.
    display: Rc<Display>,
    /// The metadata state. Updated whenever the decoder reads new data from the stream.
    pub(crate) metadata_state: StreamMetadataState,
    /// Make sure the backend is typed by stream information provider.
    _stream_data: PhantomData<StreamData>,
}

impl<StreamData> VaapiBackend<StreamData>
where
    StreamData: Clone,
    for<'a> &'a StreamData: StreamInfo,
{
    pub(crate) fn new(display: Rc<libva::Display>) -> Self {
        Self {
            display,
            metadata_state: StreamMetadataState::Unparsed,
            _stream_data: PhantomData,
        }
    }

    pub(crate) fn new_sequence(
        &mut self,
        stream_params: &StreamData,
    ) -> StatelessBackendResult<()> {
        let old_metadata_state =
            std::mem::replace(&mut self.metadata_state, StreamMetadataState::Unparsed);

        self.metadata_state =
            StreamMetadataState::open(&self.display, stream_params, None, old_metadata_state)?;

        Ok(())
    }

    pub(crate) fn process_picture(
        &mut self,
        picture: libva::Picture<PictureNew, PooledSurface<()>>,
    ) -> StatelessBackendResult<<Self as StatelessDecoderBackend<StreamData>>::Handle> {
        let metadata = self.metadata_state.get_parsed()?;

        Ok(Rc::new(RefCell::new(GenericBackendHandle::new(
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
}

impl<StreamData> StatelessDecoderBackend<StreamData> for VaapiBackend<StreamData>
where
    StreamData: Clone,
    for<'a> &'a StreamData: StreamInfo,
{
    type Handle = DecodedHandle;

    fn coded_resolution(&self) -> Option<Resolution> {
        self.metadata_state
            .get_parsed()
            .map(|m| m.coded_resolution)
            .ok()
    }

    fn display_resolution(&self) -> Option<Resolution> {
        self.metadata_state
            .get_parsed()
            .map(|m| m.display_resolution)
            .ok()
    }

    fn num_resources_total(&self) -> usize {
        self.metadata_state
            .get_parsed()
            .map(|m| m.min_num_surfaces)
            .unwrap_or(0)
    }

    fn num_resources_left(&self) -> usize {
        self.metadata_state
            .get_parsed()
            .map(|m| m.surface_pool.borrow().num_surfaces_left())
            .unwrap_or(0)
    }

    fn try_format(
        &mut self,
        format_info: &StreamData,
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
            self.metadata_state = StreamMetadataState::open(
                &self.display,
                format_info,
                Some(map_format),
                old_metadata_state,
            )?;

            Ok(())
        } else {
            Err(anyhow!("Format {:?} is unsupported.", format))
        }
    }
}

/// Copies `src` into `dst` removing all padding and converting from biplanar to triplanar format.
///
/// `useful_pixels` is the number of useful pixels in each sample, e.g. `10` for `P010`, `12` for
/// `P012`, etc.
///
/// This function is VAAPI-specific because of the unusual the source pixels are laid out: VAAPI
/// writes the `useful_pixels` MSBs, but software generally expects the LSBs to contain the data.
fn p01x_to_i01x(
    src: &[u8],
    dst: &mut [u8],
    useful_pixels: usize,
    width: usize,
    height: usize,
    strides: [usize; 3],
    offsets: [usize; 3],
) {
    let sample_shift = 16 - useful_pixels;

    // Copy Y.
    //
    // VAAPI's Y samples are two byte little endian with the bottom six bits ignored. We need to
    // convert that to two byte little endian with top 6 bits ignored.

    let src_y_lines = src[offsets[0]..]
        .chunks(strides[0])
        .map(|line| &line[..width * 2]);
    let dst_y_lines = dst.chunks_mut(width * 2);

    for (src_line, dst_line) in src_y_lines.zip(dst_y_lines).take(height) {
        for (src_y, dst_y) in src_line.chunks(2).zip(dst_line.chunks_mut(2)) {
            LittleEndian::write_u16(dst_y, LittleEndian::read_u16(src_y) >> sample_shift);
        }
    }

    let dst_u_offset = width * 2 * height;

    // Align width and height to 2 for UV plane.
    let width = if width % 2 == 1 { width + 1 } else { width };
    let height = if height % 2 == 1 { height + 1 } else { height };
    // 1 sample per 4 pixels, but we have two components per line so width remains as-is.
    let height = height / 2;

    let dst_u_size = width * height;

    // Copy U and V and deinterleave into different planes.
    //
    // We need to perform the same bit shift as luma, but also to de-interleave the data.
    let src_uv_lines = src[offsets[1]..]
        .chunks(strides[1])
        .map(|line| &line[..width * 2]);
    let (dst_u_plane, dst_v_plane) = dst[dst_u_offset..].split_at_mut(dst_u_size);
    let dst_u_lines = dst_u_plane.chunks_mut(width);
    let dst_v_lines = dst_v_plane.chunks_mut(width);
    for (src_line, (dst_u_line, dst_v_line)) in
        src_uv_lines.zip(dst_u_lines.zip(dst_v_lines)).take(height)
    {
        for ((src_u, src_v), (dst_u, dst_v)) in src_line
            .chunks(4)
            .map(|chunk| (&chunk[0..2], &chunk[2..4]))
            .zip(dst_u_line.chunks_mut(2).zip(dst_v_line.chunks_mut(2)))
        {
            LittleEndian::write_u16(dst_u, LittleEndian::read_u16(src_u) >> sample_shift);
            LittleEndian::write_u16(dst_v, LittleEndian::read_u16(src_v) >> sample_shift);
        }
    }
}

/// Copies `src` into `dst` as I21x, removing all padding and changing the layout from packed to
/// triplanar.
///
/// `useful_pixels` is the number of useful pixels in each sample, e.g. `10` for `Y210` or `16` for
/// `Y216`.
///
/// This function is VAAPI-specific because of the unusual the source pixels are laid out: VAAPI
/// writes the `useful_pixels` MSBs, but software generally expects the LSBs to contain the data.
///
/// WARNING: this function could not be tested for lack of supporting hardware.
fn y21x_to_i21x(
    src: &[u8],
    dst: &mut [u8],
    useful_pixels: usize,
    width: usize,
    height: usize,
    strides: [usize; 3],
    offsets: [usize; 3],
) {
    let sample_shift = 16 - useful_pixels;
    // Align width to 2 for U and V planes and divide by 2.
    // This should not be necessary as the sampling method requires that width is a multiple of 2
    // to begin with.
    let uv_width = if width % 2 == 1 { width + 1 } else { width } / 2;

    // YUYV representation, i.e. 4 16-bit words per two Y samples meaning we have 4 * width bytes
    // of data per line.
    let src_lines = src[offsets[0]..]
        .chunks(strides[0])
        .map(|line| &line[..width * 4]);

    let dst_y_size = width * 2 * height;
    let dst_u_size = uv_width * 2 * height;

    let (dst_y_plane, dst_uv_planes) = dst.split_at_mut(dst_y_size);
    let (dst_u_plane, dst_v_plane) = dst_uv_planes.split_at_mut(dst_u_size);
    let dst_y_lines = dst_y_plane.chunks_mut(width * 2);
    let dst_u_lines = dst_u_plane.chunks_mut(uv_width * 2);
    let dst_v_lines = dst_v_plane.chunks_mut(uv_width * 2);

    for (src_line, (dst_y_line, (dst_u_line, dst_v_line))) in src_lines
        .zip(dst_y_lines.zip(dst_u_lines.zip(dst_v_lines)))
        .take(height)
    {
        for (src, (dst_y, (dst_u, dst_v))) in src_line.chunks(8).zip(
            dst_y_line
                .chunks_mut(4)
                .zip(dst_u_line.chunks_mut(2).zip(dst_v_line.chunks_mut(2))),
        ) {
            let y0 = LittleEndian::read_u16(&src[0..2]) >> sample_shift;
            let u = LittleEndian::read_u16(&src[2..4]) >> sample_shift;
            let y1 = LittleEndian::read_u16(&src[4..6]) >> sample_shift;
            let v = LittleEndian::read_u16(&src[6..8]) >> sample_shift;

            LittleEndian::write_u16(&mut dst_y[0..2], y0);
            LittleEndian::write_u16(&mut dst_y[2..4], y1);
            LittleEndian::write_u16(dst_u, u);
            LittleEndian::write_u16(dst_v, v);
        }
    }
}

/// Copies `src` into `dst` as I412, removing all padding and changing the layout from packed to
/// triplanar. Also drops the alpha channel.
///
/// This function is VAAPI-specific because the samples need to be rolled somehow...
fn y412_to_i412(
    src: &[u8],
    dst: &mut [u8],
    width: usize,
    height: usize,
    strides: [usize; 3],
    offsets: [usize; 3],
) {
    let src_lines = src[offsets[0]..]
        .chunks(strides[0])
        .map(|line| &line[..width * 8]);

    let dst_y_size = width * 2 * height;
    let dst_u_size = width * 2 * height;

    let (dst_y_plane, dst_uv_planes) = dst.split_at_mut(dst_y_size);
    let (dst_u_plane, dst_v_plane) = dst_uv_planes.split_at_mut(dst_u_size);
    let dst_y_lines = dst_y_plane.chunks_mut(width * 2);
    let dst_u_lines = dst_u_plane.chunks_mut(width * 2);
    let dst_v_lines = dst_v_plane.chunks_mut(width * 2);

    for (src_line, (dst_y_line, (dst_u_line, dst_v_line))) in src_lines
        .zip(dst_y_lines.zip(dst_u_lines.zip(dst_v_lines)))
        .take(height)
    {
        for (src, (dst_y, (dst_u, dst_v))) in src_line.chunks(8).zip(
            dst_y_line
                .chunks_mut(2)
                .zip(dst_u_line.chunks_mut(2).zip(dst_v_line.chunks_mut(2))),
        ) {
            let y = LittleEndian::read_u16(&src[2..4]);
            let u = LittleEndian::read_u16(&src[0..2]);
            let v = LittleEndian::read_u16(&src[4..6]);
            // Why is that rotate_right neeed??
            LittleEndian::write_u16(dst_y, y.rotate_right(4));
            LittleEndian::write_u16(dst_u, u.rotate_right(4));
            LittleEndian::write_u16(dst_v, v.rotate_right(4));
        }
    }
}
