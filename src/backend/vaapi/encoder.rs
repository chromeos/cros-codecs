// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::any::Any;
use std::marker::PhantomData;
use std::rc::Rc;

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
use crate::encoder::stateless::BackendPromise;
use crate::encoder::stateless::StatelessBackendError;
use crate::encoder::stateless::StatelessBackendResult;
use crate::encoder::stateless::StatelessVideoEncoderBackend;
use crate::encoder::FrameMetadata;
use crate::Fourcc;
use crate::Resolution;

/// The number of frames that encoder backend should initialize scratch pool with.
const INITIAL_SCRATCH_POOL_SIZE: usize = 16;
/// The maximum size of scratch pool size, after which the backend will refure to allocate more
/// scratch frames.
const MAX_SCRATCH_POOL_SIZE: usize = INITIAL_SCRATCH_POOL_SIZE * 4;

pub struct Reference(PooledVaSurface<()>);

impl Reference {
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
    H: std::borrow::Borrow<Surface<M>>,
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
            if low_power {
                VAEntrypointEncSliceLP
            } else {
                VAEntrypointEncSlice
            },
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
        scratch_pool.add_surfaces(vec![(); INITIAL_SCRATCH_POOL_SIZE])?;

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

    // Creates an empty surface that will be filled with reconstructed picture during encoding
    // which will be later used as frame reference
    pub(crate) fn new_scratch_picture(&mut self) -> StatelessBackendResult<Reference> {
        if self.scratch_pool.num_surfaces_left() == 0 {
            if self.scratch_pool.num_managed_surfaces() >= MAX_SCRATCH_POOL_SIZE {
                log::error!("Scratch pool is exhausted and hit the size limit");
                return Err(StatelessBackendError::OutOfResources);
            }

            log::debug!(
                "Scratch pool empty, allocating one more surface. (previous pool size: {})",
                self.scratch_pool.num_managed_surfaces()
            );
            self.scratch_pool.add_surfaces(vec![()])?;
        }

        let surface = self
            .scratch_pool
            .get_surface()
            .ok_or(StatelessBackendError::OutOfResources)?;

        Ok(Reference(surface))
    }
}

impl<M, H> StatelessVideoEncoderBackend<H> for VaapiBackend<M, H>
where
    M: SurfaceMemoryDescriptor,
    H: std::borrow::Borrow<Surface<M>>,
{
    type Picture = H;

    fn import_picture(
        &mut self,
        _metadata: &FrameMetadata,
        handle: H,
    ) -> StatelessBackendResult<Self::Picture> {
        Ok(handle)
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

    /// Hold reference frames/object from being dropped while [`handle`]
    /// is processed.
    references: Vec<Rc<dyn Any>>,

    // VaBuffer where the coded output will be present after processing
    // is finished.
    coded_buf: EncCodedBuffer,

    /// Container for the request output. Moved from [`StatelessVideoEncoderBackend`] request.
    /// The output will be appended  to it
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
        Self {
            handle,
            references,
            coded_buf,
            coded_output,
            _phantom: Default::default(),
        }
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

    use libva::constants::VA_FOURCC_NV12;

    use super::*;
    use crate::encoder::FrameMetadata;
    use crate::FrameLayout;

    fn map_surface_nv12<'a, M: SurfaceMemoryDescriptor>(
        display: &Rc<Display>,
        surface: &'a Surface<M>,
    ) -> libva::Image<'a> {
        let image_fmts = display.query_image_formats().unwrap();
        let image_fmt = image_fmts
            .into_iter()
            .find(|f| f.fourcc == VA_FOURCC_NV12)
            .unwrap();

        libva::Image::create_from(surface, image_fmt, surface.size(), surface.size()).unwrap()
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
        display_resolution: Resolution,
        frame_layout: FrameLayout,
    }

    impl PooledFrameIterator {
        pub fn new(
            display: Rc<Display>,
            pool: VaSurfacePool<()>,
            display_resolution: Resolution,
            frame_layout: FrameLayout,
        ) -> Self {
            Self {
                counter: 0,
                display,
                pool,
                display_resolution,
                frame_layout,
            }
        }
    }

    impl Iterator for PooledFrameIterator {
        type Item = (FrameMetadata, PooledVaSurface<()>);

        fn next(&mut self) -> Option<Self::Item> {
            let handle = self.pool.get_surface().unwrap();

            let meta = FrameMetadata {
                display_resolution: self.display_resolution,
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
            display_resolution: Resolution,
            frame_layout: FrameLayout,
        ) -> Self {
            Self {
                raw_iterator,
                pool_iter: PooledFrameIterator::new(
                    display,
                    pool,
                    display_resolution,
                    frame_layout,
                ),
            }
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

    pub fn fill_test_frame(
        width: usize,
        height: usize,
        pitches: [u32; 3],
        offsets: [u32; 3],
        t: f32,
        raw: &mut [u8],
    ) {
        let (sin, cos) = f32::sin_cos(t);
        let (sin2, cos2) = (sin.powi(2), cos.powi(2));

        // Pick the dot position
        let dot_col = height as f32 * (1.1 + 2.0 * sin * cos) / 2.2;
        let dot_row = width as f32 * (1.1 + sin) / 2.2;
        let dot_size2 = (width.min(height) as f32 * 0.05).powi(2);

        let mut dst = &mut raw[offsets[0] as usize..];

        // Luma
        for row in 0..height {
            #[allow(clippy::needless_range_loop)]
            for col in 0..width {
                let dist = (dot_col - col as f32).powi(2) + (dot_row - row as f32).powi(2);

                let y = if dist < dot_size2 {
                    0
                } else {
                    255 * (row + col) / (width + height)
                };

                dst[col] = y as u8;
            }

            dst = &mut dst[pitches[0] as usize..];
        }

        // Advance to the offset of the chroma plane
        let mut dst = &mut raw[offsets[1] as usize..];

        // Chroma
        for row in 0..height / 2 {
            let row = row * 2;

            for col in 0..width / 2 {
                let col = col * 2;
                let dist = (dot_col - col as f32).powi(2) + (dot_row - row as f32).powi(2);

                let c = if dist < dot_size2 {
                    (128.0, 128.0)
                } else {
                    (
                        ((row * 255) / width) as f32 * sin2,
                        ((col * 255) / height) as f32 * cos2,
                    )
                };

                dst[col] = c.0 as u8;
                dst[col + 1] = c.1 as u8;
            }

            dst = &mut dst[pitches[1] as usize..];
        }
    }

    pub fn upload_test_frame<M: SurfaceMemoryDescriptor>(
        display: &Rc<Display>,
        surface: &Surface<M>,
        t: f32,
    ) {
        let mut image = map_surface_nv12(display, surface);

        let (width, height) = image.display_resolution();

        let offsets = image.image().offsets;
        let pitches = image.image().pitches;

        fill_test_frame(
            width as usize,
            height as usize,
            pitches,
            offsets,
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
    }

    impl TestFrameGenerator {
        pub fn new(
            max_count: u64,
            display: Rc<Display>,
            pool: VaSurfacePool<()>,
            display_resolution: Resolution,
            frame_layout: FrameLayout,
        ) -> Self {
            Self {
                counter: 0,
                max_count,
                pool_iter: PooledFrameIterator::new(
                    display.clone(),
                    pool,
                    display_resolution,
                    frame_layout,
                ),
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

            let t = 2.0 * std::f32::consts::PI * (meta.timestamp as f32) / (self.max_count as f32);
            upload_test_frame(&self.display, surface, t);

            Some((meta, handle))
        }
    }

    #[test]
    #[ignore]
    fn dump_generator_golden_frames() {
        use std::io::Write;

        const WIDTH: usize = 512;
        const HEIGHT: usize = 512;
        const COUNT: usize = 100;

        let mut raw = vec![0u8; WIDTH * HEIGHT + WIDTH * HEIGHT / 2];

        let pitches = [WIDTH as u32, WIDTH as u32, 0];
        let offsets = [0, WIDTH as u32 * HEIGHT as u32, 0];

        let mut out = std::fs::File::create("dump_generator_golden_frames.nv12").unwrap();

        for i in 0..=COUNT {
            let t = 2.0 * std::f32::consts::PI * (i as f32) / (COUNT as f32);

            fill_test_frame(WIDTH, HEIGHT, pitches, offsets, t, &mut raw[..]);

            out.write_all(&raw).unwrap();
            out.flush().unwrap();
        }
    }
}
