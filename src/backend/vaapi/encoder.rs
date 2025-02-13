// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::any::Any;
use std::marker::PhantomData;
use std::rc::Rc;

use anyhow::anyhow;
use libva::Config;
use libva::Context;
use libva::Display;
use libva::EncCodedBuffer;
use libva::MappedCodedBuffer;
use libva::Picture;
use libva::PictureEnd;
use libva::Surface;
use libva::SurfaceMemoryDescriptor;
use libva::UsageHint;
use libva::VAEntrypoint::VAEntrypointEncSlice;
use libva::VAEntrypoint::VAEntrypointEncSliceLP;
use libva::VAProfile;
use libva::VASurfaceStatus;

use crate::backend::vaapi::surface_pool::PooledVaSurface;
use crate::backend::vaapi::surface_pool::VaSurfacePool;
use crate::backend::vaapi::FORMAT_MAP;
use crate::decoder::FramePool;
use crate::encoder::stateless::BackendPromise;
use crate::encoder::stateless::StatelessBackendError;
use crate::encoder::stateless::StatelessBackendResult;
use crate::encoder::stateless::StatelessEncoderBackendImport;
use crate::encoder::FrameMetadata;
use crate::encoder::RateControl;
use crate::encoder::Tunings;
use crate::video_frame::VideoFrame;
use crate::Fourcc;
use crate::Resolution;

/// The number of frames that encoder backend should initialize scratch pool with.
const INITIAL_SCRATCH_POOL_SIZE: usize = 16;
/// The maximum size of scratch pool size, after which the backend will refure to allocate more
/// scratch frames.
const MAX_SCRATCH_POOL_SIZE: usize = INITIAL_SCRATCH_POOL_SIZE * 4;

impl From<libva::VaError> for StatelessBackendError {
    fn from(value: libva::VaError) -> Self {
        Self::Other(value.into())
    }
}

pub(crate) fn tunings_to_libva_rc<const CLAMP_MIN_QP: u32, const CLAMP_MAX_QP: u32>(
    tunings: &Tunings,
) -> StatelessBackendResult<libva::EncMiscParameterRateControl> {
    let bits_per_second = tunings.rate_control.bitrate_target().unwrap_or(0);
    let bits_per_second = u32::try_from(bits_per_second).map_err(|e| anyhow::anyhow!(e))?;

    // At the moment we don't support variable bitrate therefore target 100%
    const TARGET_PERCENTAGE: u32 = 100;

    // Window size in ms that the RC should apply to
    const WINDOW_SIZE: u32 = 1_500;

    // Clamp minium QP
    let min_qp = tunings.min_quality.clamp(CLAMP_MIN_QP, CLAMP_MAX_QP);

    let basic_unit_size = 0;

    // Don't reset the rate controller
    const RESET: u32 = 0;

    // Don't skip frames
    const DISABLE_FRAME_SKIP: u32 = 1;

    // Allow bit stuffing
    const DISABLE_BIT_STUFFING: u32 = 0;

    // Use default
    const MB_RATE_CONTROL: u32 = 0;

    // SVC encoding is not supported for now
    const TEMPORAL_ID: u32 = 0;

    // Don't ensure intraframe size
    const CFS_I_FRAMES: u32 = 0;

    // We don't use hierarchical B frames currently
    const ENABLE_PARALLEL_BRC: u32 = 0;

    // Disable dynamic scaling
    const ENABLE_DYNAMIC_SCALING: u32 = 0;

    // Use default tolerance mode
    const FRAME_TOLERANCE_MODE: u32 = 0;

    // ICQ mode is not used
    const ICQ_QUALITY_FACTOR: u32 = 0;

    // Clamp maximum QP
    let max_qp = tunings.max_quality.clamp(CLAMP_MIN_QP, CLAMP_MAX_QP);

    // Unsed
    const QUALITY_FACTOR: u32 = 0;

    // No limits
    const TARGET_FRAME_SIZE: u32 = 0;

    // If ConstantQuality is used then set to it's value, otherwise use middle
    let initial_qp = match tunings.rate_control {
        RateControl::ConstantQuality(qp) => qp.clamp(min_qp, max_qp),
        _ => (min_qp + max_qp) / 2,
    };

    Ok(libva::EncMiscParameterRateControl::new(
        bits_per_second,
        TARGET_PERCENTAGE,
        WINDOW_SIZE,
        initial_qp,
        min_qp,
        basic_unit_size,
        libva::RcFlags::new(
            RESET,
            DISABLE_FRAME_SKIP,
            DISABLE_BIT_STUFFING,
            MB_RATE_CONTROL,
            TEMPORAL_ID,
            CFS_I_FRAMES,
            ENABLE_PARALLEL_BRC,
            ENABLE_DYNAMIC_SCALING,
            FRAME_TOLERANCE_MODE,
        ),
        ICQ_QUALITY_FACTOR,
        max_qp,
        QUALITY_FACTOR,
        TARGET_FRAME_SIZE,
    ))
}

pub struct Reconstructed(PooledVaSurface<()>);

impl Reconstructed {
    pub(crate) fn surface(&self) -> &Surface<()> {
        use std::borrow::Borrow;
        Borrow::<Surface<()>>::borrow(&self.0)
    }

    pub(crate) fn surface_id(&self) -> u32 {
        self.surface().id()
    }
}

pub struct VaapiBackend<M, H>
where
    M: SurfaceMemoryDescriptor,
    H: std::borrow::Borrow<Surface<M>> + 'static,
{
    /// VA config.
    #[allow(dead_code)]
    va_config: Config,

    /// VA context used for encoding.
    context: Rc<Context>,

    _va_profile: VAProfile::Type,
    scratch_pool: VaSurfacePool<()>,
    _phantom: PhantomData<(M, H)>,
}

impl<M, H> VaapiBackend<M, H>
where
    M: SurfaceMemoryDescriptor,
    H: std::borrow::Borrow<Surface<M>>,
{
    pub fn new(
        display: Rc<Display>,
        va_profile: VAProfile::Type,
        fourcc: Fourcc,
        coded_size: Resolution,
        bitrate_control: u32,
        low_power: bool,
    ) -> StatelessBackendResult<Self> {
        let format_map = FORMAT_MAP
            .iter()
            .find(|&map| map.va_fourcc == fourcc.0)
            .ok_or_else(|| StatelessBackendError::UnsupportedFormat)?;

        let rt_format = format_map.rt_format;

        let va_config = display.create_config(
            vec![
                libva::VAConfigAttrib {
                    type_: libva::VAConfigAttribType::VAConfigAttribRTFormat,
                    value: rt_format,
                },
                libva::VAConfigAttrib {
                    type_: libva::VAConfigAttribType::VAConfigAttribRateControl,
                    value: bitrate_control,
                },
            ],
            va_profile,
            if low_power { VAEntrypointEncSliceLP } else { VAEntrypointEncSlice },
        )?;

        let context = display.create_context::<M>(
            &va_config,
            coded_size.width,
            coded_size.height,
            None,
            true,
        )?;

        let mut scratch_pool = VaSurfacePool::new(
            Rc::clone(&display),
            rt_format,
            Some(UsageHint::USAGE_HINT_ENCODER),
            coded_size,
        );

        // TODO: Allow initial size to be changed
        scratch_pool.add_frames(vec![(); INITIAL_SCRATCH_POOL_SIZE])?;

        Ok(Self {
            va_config,
            context,
            scratch_pool,
            _va_profile: va_profile,
            _phantom: Default::default(),
        })
    }

    pub(crate) fn context(&self) -> &Rc<Context> {
        &self.context
    }

    pub(crate) fn new_coded_buffer(
        &self,
        rate_control: &RateControl,
    ) -> StatelessBackendResult<EncCodedBuffer> {
        // Coded buffer size multiplier. It's inteded to give head room for the encoder.
        const CODED_SIZE_MUL: usize = 2;

        // Default coded buffer size if bitrate control is not used.
        const DEFAULT_CODED_SIZE: usize = 1_500_000;

        let coded_size = rate_control
            .bitrate_target()
            .map(|e| e as usize * CODED_SIZE_MUL)
            .unwrap_or(DEFAULT_CODED_SIZE);

        Ok(self.context().create_enc_coded(coded_size)?)
    }

    // Creates an empty surface that will be filled with reconstructed picture during encoding
    // which will be later used as frame reference
    pub(crate) fn new_scratch_picture(&mut self) -> StatelessBackendResult<Reconstructed> {
        if self.scratch_pool.num_free_frames() == 0 {
            if self.scratch_pool.num_managed_frames() >= MAX_SCRATCH_POOL_SIZE {
                log::error!("Scratch pool is exhausted and hit the size limit");
                return Err(StatelessBackendError::OutOfResources);
            }

            log::debug!(
                "Scratch pool empty, allocating one more surface. (previous pool size: {})",
                self.scratch_pool.num_managed_frames()
            );
            self.scratch_pool.add_frames(vec![()])?;
        }

        let surface =
            self.scratch_pool.get_surface().ok_or(StatelessBackendError::OutOfResources)?;

        Ok(Reconstructed(surface))
    }
}

impl<M, Handle> StatelessEncoderBackendImport<Handle, Handle> for VaapiBackend<M, Handle>
where
    M: SurfaceMemoryDescriptor,
    Handle: std::borrow::Borrow<Surface<M>>,
{
    fn import_picture(
        &mut self,
        _metadata: &FrameMetadata,
        handle: Handle,
    ) -> StatelessBackendResult<Handle> {
        Ok(handle)
    }
}

impl<V: VideoFrame> StatelessEncoderBackendImport<V, Surface<V::MemDescriptor>>
    for VaapiBackend<V::MemDescriptor, Surface<V::MemDescriptor>>
{
    fn import_picture(
        &mut self,
        _metadata: &FrameMetadata,
        handle: V,
    ) -> StatelessBackendResult<Surface<V::MemDescriptor>> {
        Ok(handle.to_native_handle(self.context.display()).map_err(|err| anyhow!(err))?.into())
    }
}

/// Vaapi's implementation of [`crate::encoder::stateless::BackendPromise`]
pub struct CodedOutputPromise<M, P>
where
    M: SurfaceMemoryDescriptor,
    P: std::borrow::Borrow<Surface<M>>,
{
    /// Currently processed picture/surface.
    handle: Picture<PictureEnd, P>,

    /// Hold reference frames/object from being dropped while `handle` is processed.
    references: Vec<Rc<dyn Any>>,

    // VaBuffer where the coded output will be present after processing
    // is finished.
    coded_buf: EncCodedBuffer,

    /// Container for the request output. Moved from
    /// [`crate::encoder::stateless::StatelessVideoEncoderBackend`] request. The output will be
    /// appended to it.
    coded_output: Vec<u8>,

    _phantom: PhantomData<M>,
}

impl<M, P> CodedOutputPromise<M, P>
where
    M: SurfaceMemoryDescriptor,
    P: std::borrow::Borrow<Surface<M>>,
{
    pub fn new(
        handle: Picture<PictureEnd, P>,
        references: Vec<Rc<dyn Any>>,
        coded_buf: EncCodedBuffer,
        coded_output: Vec<u8>,
    ) -> Self {
        Self { handle, references, coded_buf, coded_output, _phantom: Default::default() }
    }
}

impl<M, H> BackendPromise for CodedOutputPromise<M, H>
where
    M: SurfaceMemoryDescriptor,
    H: std::borrow::Borrow<Surface<M>>,
{
    type Output = Vec<u8>;

    fn sync(mut self) -> StatelessBackendResult<Self::Output> {
        if let Err((err, _)) = self.handle.sync() {
            // TODO consider going back to PictureEnd
            return Err(err.into());
        }

        // Drop all references as processing is finished
        self.references.clear();

        // Map coded buffer and collect bitstream
        let coded = MappedCodedBuffer::new(&self.coded_buf)?;
        let mut bitstream = self.coded_output;
        for segment in coded.segments() {
            // TODO: Handle flags?
            // NOTE: on flags: 0-7 bits are average QP value
            if segment.bit_offset > 0 {
                log::warn!("unsupported bit_offset != 0 (yet)");
            }
            bitstream.extend(segment.buf)
        }

        Ok(bitstream)
    }

    fn is_ready(&self) -> bool {
        match self.handle.surface().query_status() {
            Ok(status) => status == VASurfaceStatus::VASurfaceReady,
            Err(_) => {
                // An error occurred while processing or checking the status of the underlying
                // processing, in both cases consider it is done. In either cases it will be
                // returned with [`sync`].
                true
            }
        }
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use std::borrow::Borrow;

    use libva::VA_FOURCC_NV12;
    use libva::VA_FOURCC_P010;

    use super::*;
    use crate::encoder::tests::fill_test_frame_nv12;
    use crate::encoder::tests::fill_test_frame_p010;
    use crate::encoder::tests::get_test_frame_t;
    use crate::encoder::FrameMetadata;
    use crate::FrameLayout;

    fn map_surface<'a, M: SurfaceMemoryDescriptor>(
        display: &Rc<Display>,
        surface: &'a Surface<M>,
        fourcc: u32,
    ) -> libva::Image<'a> {
        let image_fmts = display.query_image_formats().unwrap();
        let image_fmt = image_fmts.into_iter().find(|f| f.fourcc == fourcc).unwrap();

        libva::Image::create_from(surface, image_fmt, surface.size(), surface.size()).unwrap()
    }

    fn map_surface_nv12<'a, M: SurfaceMemoryDescriptor>(
        display: &Rc<Display>,
        surface: &'a Surface<M>,
    ) -> libva::Image<'a> {
        map_surface(display, surface, VA_FOURCC_NV12)
    }

    fn map_surface_p010<'a, M: SurfaceMemoryDescriptor>(
        display: &Rc<Display>,
        surface: &'a Surface<M>,
    ) -> libva::Image<'a> {
        map_surface(display, surface, VA_FOURCC_P010)
    }

    /// Uploads raw NV12 to Surface
    pub fn upload_nv12_img<M: SurfaceMemoryDescriptor>(
        display: &Rc<Display>,
        surface: &Surface<M>,
        width: u32,
        height: u32,
        data: &[u8],
    ) {
        let mut image = map_surface_nv12(display, surface);

        let va_image = *image.image();
        let dest = image.as_mut();
        let width = width as usize;
        let height = height as usize;

        let mut src: &[u8] = data;
        let mut dst = &mut dest[va_image.offsets[0] as usize..];

        // Copy luma
        for _ in 0..height {
            dst[..width].copy_from_slice(&src[..width]);
            dst = &mut dst[va_image.pitches[0] as usize..];
            src = &src[width..];
        }

        // Advance to the offset of the chroma plane
        let mut src = &data[width * height..];
        let mut dst = &mut dest[va_image.offsets[1] as usize..];

        let height = height / 2;

        // Copy chroma
        for _ in 0..height {
            dst[..width].copy_from_slice(&src[..width]);
            dst = &mut dst[va_image.pitches[1] as usize..];
            src = &src[width..];
        }

        surface.sync().unwrap();
        drop(image);
    }

    /// Helper struct. [`Iterator`] to fetch frames from [`SurfacePool`].
    pub struct PooledFrameIterator {
        counter: u64,
        display: Rc<Display>,
        pool: VaSurfacePool<()>,
        frame_layout: FrameLayout,
    }

    impl PooledFrameIterator {
        pub fn new(
            display: Rc<Display>,
            pool: VaSurfacePool<()>,
            frame_layout: FrameLayout,
        ) -> Self {
            Self { counter: 0, display, pool, frame_layout }
        }
    }

    impl Iterator for PooledFrameIterator {
        type Item = (FrameMetadata, PooledVaSurface<()>);

        fn next(&mut self) -> Option<Self::Item> {
            let handle = self.pool.get_surface().unwrap();

            let meta = FrameMetadata {
                layout: self.frame_layout.clone(),
                force_keyframe: false,
                timestamp: self.counter,
            };

            self.counter += 1;

            Some((meta, handle))
        }
    }

    /// Helper struct. Uses [`Iterator`] with raw chunks and uploads to pooled surface from
    /// [`SurfacePool`] to produce frames.
    pub struct NV12FrameProducer<'l, I>
    where
        I: Iterator<Item = &'l [u8]>,
    {
        raw_iterator: I,
        pool_iter: PooledFrameIterator,
    }

    impl<'l, I> NV12FrameProducer<'l, I>
    where
        I: Iterator<Item = &'l [u8]>,
    {
        #[allow(dead_code)]
        pub fn new(
            raw_iterator: I,
            display: Rc<Display>,
            pool: VaSurfacePool<()>,
            frame_layout: FrameLayout,
        ) -> Self {
            Self { raw_iterator, pool_iter: PooledFrameIterator::new(display, pool, frame_layout) }
        }
    }

    impl<'l, I> Iterator for NV12FrameProducer<'l, I>
    where
        I: Iterator<Item = &'l [u8]>,
    {
        type Item = (FrameMetadata, PooledVaSurface<()>);

        fn next(&mut self) -> Option<Self::Item> {
            let raw = match self.raw_iterator.next() {
                Some(raw) => raw,
                None => return None,
            };

            let (meta, handle) = self.pool_iter.next().unwrap();

            let width = meta.layout.size.width;
            let height = meta.layout.size.height;
            debug_assert_eq!((width * height + width * height / 2) as usize, raw.len());

            upload_nv12_img(&self.pool_iter.display, handle.borrow(), width, height, raw);

            Some((meta, handle))
        }
    }

    pub fn upload_test_frame_nv12<M: SurfaceMemoryDescriptor>(
        display: &Rc<Display>,
        surface: &Surface<M>,
        t: f32,
    ) {
        let mut image = map_surface_nv12(display, surface);

        let (width, height) = image.display_resolution();

        let offsets = image.image().offsets;
        let pitches = image.image().pitches;

        fill_test_frame_nv12(
            width as usize,
            height as usize,
            [pitches[0] as usize, pitches[1] as usize],
            [offsets[0] as usize, offsets[1] as usize],
            t,
            image.as_mut(),
        );

        drop(image);
        surface.sync().unwrap();
    }

    pub fn upload_test_frame_p010<M: SurfaceMemoryDescriptor>(
        display: &Rc<Display>,
        surface: &Surface<M>,
        t: f32,
    ) {
        let mut image = map_surface_p010(display, surface);

        let (width, height) = image.display_resolution();

        let offsets = image.image().offsets;
        let pitches = image.image().pitches;

        fill_test_frame_p010(
            width as usize,
            height as usize,
            [pitches[0] as usize, pitches[1] as usize],
            [offsets[0] as usize, offsets[1] as usize],
            t,
            image.as_mut(),
        );

        drop(image);
        surface.sync().unwrap();
    }

    /// Helper struct. Procedurally generate NV12 frames for test purposes.
    pub struct TestFrameGenerator {
        counter: u64,
        max_count: u64,
        pool_iter: PooledFrameIterator,
        display: Rc<Display>,
        fourcc: Fourcc,
    }

    impl TestFrameGenerator {
        pub fn new(
            max_count: u64,
            display: Rc<Display>,
            pool: VaSurfacePool<()>,
            frame_layout: FrameLayout,
        ) -> Self {
            Self {
                counter: 0,
                max_count,
                fourcc: frame_layout.format.0,
                pool_iter: PooledFrameIterator::new(display.clone(), pool, frame_layout),
                display,
            }
        }
    }

    impl Iterator for TestFrameGenerator {
        type Item = (FrameMetadata, PooledVaSurface<()>);

        fn next(&mut self) -> Option<Self::Item> {
            if self.counter > self.max_count {
                return None;
            }

            self.counter += 1;

            let (meta, handle) = self.pool_iter.next().unwrap();

            let surface: &Surface<()> = handle.borrow();

            let t = get_test_frame_t(meta.timestamp, self.max_count);
            match self.fourcc.0 {
                VA_FOURCC_NV12 => upload_test_frame_nv12(&self.display, surface, t),
                VA_FOURCC_P010 => upload_test_frame_p010(&self.display, surface, t),
                _ => unreachable!(),
            }

            Some((meta, handle))
        }
    }
}
