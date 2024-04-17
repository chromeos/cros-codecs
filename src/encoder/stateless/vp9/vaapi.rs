// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::any::Any;
use std::borrow::Borrow;
use std::rc::Rc;

use anyhow::Context;
use libva::constants::VA_INVALID_SURFACE;
use libva::BufferType;
use libva::Display;
use libva::EncPictureParameter;
use libva::EncPictureParameterBufferVP9;
use libva::EncSequenceParameter;
use libva::EncSequenceParameterBufferVP9;
use libva::Picture;
use libva::Surface;
use libva::SurfaceMemoryDescriptor;
use libva::VAProfile::VAProfileVP9Profile0;
use libva::VAProfile::VAProfileVP9Profile2;
use libva::VP9EncPicFlags;
use libva::VP9EncRefFlags;

use crate::backend::vaapi::encoder::tunings_to_libva_rc;
use crate::backend::vaapi::encoder::CodedOutputPromise;
use crate::backend::vaapi::encoder::Reconstructed;
use crate::backend::vaapi::encoder::VaapiBackend;
use crate::codec::vp9::parser::BitDepth;
use crate::codec::vp9::parser::FrameType;
use crate::codec::vp9::parser::InterpolationFilter;
use crate::codec::vp9::parser::ALTREF_FRAME;
use crate::codec::vp9::parser::GOLDEN_FRAME;
use crate::codec::vp9::parser::LAST_FRAME;
use crate::codec::vp9::parser::NUM_REF_FRAMES;
use crate::encoder::stateless::vp9::predictor::MAX_Q_IDX;
use crate::encoder::stateless::vp9::predictor::MIN_Q_IDX;
use crate::encoder::stateless::vp9::BackendRequest;
use crate::encoder::stateless::vp9::EncoderConfig;
use crate::encoder::stateless::vp9::ReferenceUse;
use crate::encoder::stateless::vp9::StatelessEncoder;
use crate::encoder::stateless::vp9::StatelessVP9EncoderBackend;
use crate::encoder::stateless::vp9::VP9;
use crate::encoder::stateless::ReadyPromise;
use crate::encoder::stateless::StatelessBackendResult;
use crate::encoder::stateless::StatelessVideoEncoderBackend;
use crate::encoder::EncodeResult;
use crate::encoder::RateControl;
use crate::BlockingMode;
use crate::Fourcc;
use crate::Resolution;

impl<M, Handle> StatelessVideoEncoderBackend<VP9> for VaapiBackend<M, Handle>
where
    M: SurfaceMemoryDescriptor,
    Handle: Borrow<Surface<M>>,
{
    type Picture = Handle;
    type Reconstructed = Reconstructed;
    type CodedPromise = CodedOutputPromise<M, Handle>;
    type ReconPromise = ReadyPromise<Self::Reconstructed>;
}

impl<M, Handle> StatelessVP9EncoderBackend for VaapiBackend<M, Handle>
where
    M: SurfaceMemoryDescriptor,
    Handle: Borrow<Surface<M>>,
{
    fn encode_frame(
        &mut self,
        request: BackendRequest<Self::Picture, Self::Reconstructed>,
    ) -> StatelessBackendResult<(Self::ReconPromise, Self::CodedPromise)> {
        let coded_buf = self.new_coded_buffer(&request.tunings.rate_control)?;
        let recon = self.new_scratch_picture()?;

        // Use bitrate from RateControl or ask driver to ignore
        let bits_per_second = request.tunings.rate_control.bitrate_target().unwrap_or(0) as u32;

        let seq_param = BufferType::EncSequenceParameter(EncSequenceParameter::VP9(
            EncSequenceParameterBufferVP9::new(
                request.input_meta.display_resolution.width,
                request.input_meta.display_resolution.height,
                0,
                10,
                2000,
                bits_per_second,
                1024,
            ),
        ));

        // From va_enc_vp9.h `ref_frame_ctrl_l0` documentation
        const LAST_FRAME_AS_REF: u32 = 0x01;
        const GOLDEN_FRAME_AS_REF: u32 = 0x02;
        const ALTREF_FRAME_AS_REF: u32 = 0x04;

        let mut references = Vec::<Rc<dyn Any>>::new();
        let mut reference_frames = [VA_INVALID_SURFACE; NUM_REF_FRAMES];

        let mut ref_frame_ctrl_l0 = 0;
        let mut ref_frame_ctrl_l1 = 0;

        let refs = [
            (&request.last_frame_ref, LAST_FRAME - 1, LAST_FRAME_AS_REF),
            (
                &request.golden_frame_ref,
                GOLDEN_FRAME - 1,
                GOLDEN_FRAME_AS_REF,
            ),
            (
                &request.altref_frame_ref,
                ALTREF_FRAME - 1,
                ALTREF_FRAME_AS_REF,
            ),
        ];

        for (r, ref_idx, ref_ctrl) in refs {
            let Some((ref_frame, ref_use)) = r else {
                continue;
            };

            reference_frames[request.header.ref_frame_idx[ref_idx] as usize] =
                ref_frame.surface_id();
            references.push(ref_frame.clone());

            match ref_use {
                ReferenceUse::Single => ref_frame_ctrl_l0 |= ref_ctrl,
                ReferenceUse::Compound => ref_frame_ctrl_l1 |= ref_ctrl,
                ReferenceUse::Hybrid => {
                    ref_frame_ctrl_l0 |= ref_ctrl;
                    ref_frame_ctrl_l1 |= ref_ctrl;
                }
            }
        }

        let force_kf =
            request.header.frame_type == FrameType::KeyFrame || request.input_meta.force_keyframe;

        let ref_flags = VP9EncRefFlags::new(
            // Force keyframe if requested
            force_kf as u32,
            ref_frame_ctrl_l0,
            ref_frame_ctrl_l1,
            request.header.ref_frame_idx[LAST_FRAME - 1] as u32,
            request.header.ref_frame_sign_bias[LAST_FRAME] as u32,
            request.header.ref_frame_idx[GOLDEN_FRAME - 1] as u32,
            request.header.ref_frame_sign_bias[GOLDEN_FRAME] as u32,
            request.header.ref_frame_idx[ALTREF_FRAME - 1] as u32,
            request.header.ref_frame_sign_bias[ALTREF_FRAME] as u32,
            0,
        );

        // From va_enc_vp9.h `mcomp_filter_type` documentation
        let mcomp_filter_type = match request.header.interpolation_filter {
            InterpolationFilter::EightTap => 0,
            InterpolationFilter::EightTapSmooth => 1,
            InterpolationFilter::EightTapSharp => 2,
            InterpolationFilter::Bilinear => 3,
            InterpolationFilter::Switchable => 4,
        };

        // TODO: show_existing_frame
        assert!(!request.header.show_existing_frame);

        // From va_enc_vp9.h `comp_prediction_mode` documentation
        const PRED_MODE_SINGLE: u32 = 0x00;
        // const PRED_MODE_COMPOUND: u32 = 0x01;
        const PRED_MODE_HYBRID: u32 = 0x02;

        let comp_prediction_mode = if ref_frame_ctrl_l1 != 0 {
            // Use hybrid prediction mode if any future reference frame are enabled
            PRED_MODE_HYBRID
        } else {
            PRED_MODE_SINGLE
        };

        let pic_flags = VP9EncPicFlags::new(
            request.header.frame_type as u32,
            request.header.show_frame as u32,
            request.header.error_resilient_mode as u32,
            request.header.intra_only as u32,
            request.header.allow_high_precision_mv as u32,
            mcomp_filter_type,
            request.header.frame_parallel_decoding_mode as u32,
            request.header.reset_frame_context as u32,
            request.header.refresh_frame_context as u32,
            request.header.frame_context_idx as u32,
            request.header.seg.enabled as u32,
            request.header.seg.temporal_update as u32,
            request.header.seg.update_map as u32,
            request.header.lossless as u32,
            comp_prediction_mode,
            1,
            0,
        );

        let pic_param = BufferType::EncPictureParameter(EncPictureParameter::VP9(
            EncPictureParameterBufferVP9::new(
                request.header.width,
                request.header.height,
                request.header.render_width,
                request.header.render_height,
                recon.surface_id(),
                reference_frames,
                coded_buf.id(),
                &ref_flags,
                &pic_flags,
                request.header.refresh_frame_flags,
                request.header.quant.base_q_idx,
                request.header.quant.delta_q_y_dc,
                request.header.quant.delta_q_uv_ac,
                request.header.quant.delta_q_uv_dc,
                request.header.lf.level,
                request.header.lf.sharpness,
                request.header.lf.ref_deltas,
                request.header.lf.mode_deltas,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                request.header.tile_rows_log2,
                request.header.tile_cols_log2,
                // Don't skip frames
                0,
                0,
                0,
            ),
        ));

        let rc_param =
            tunings_to_libva_rc::<{ MIN_Q_IDX as u32 }, { MAX_Q_IDX as u32 }>(&request.tunings)?;
        let rc_param =
            libva::BufferType::EncMiscParameter(libva::EncMiscParameter::RateControl(rc_param));

        let mut picture = Picture::new(
            request.input_meta.timestamp,
            Rc::clone(self.context()),
            request.input,
        );

        picture.add_buffer(self.context().create_buffer(seq_param)?);
        picture.add_buffer(self.context().create_buffer(pic_param)?);
        picture.add_buffer(self.context().create_buffer(rc_param)?);

        // Start processing the picture encoding
        let picture = picture.begin().context("picture begin")?;
        let picture = picture.render().context("picture render")?;
        let picture = picture.end().context("picture end")?;

        // libva will handle the synchronization of reconstructed surface with implicit fences.
        // Therefore return the reconstructed frame immediately.
        let reference_promise = ReadyPromise::from(recon);

        let bitstream_promise =
            CodedOutputPromise::new(picture, references, coded_buf, request.coded_output);

        Ok((reference_promise, bitstream_promise))
    }
}

impl<M, Handle> StatelessEncoder<Handle, VaapiBackend<M, Handle>>
where
    M: SurfaceMemoryDescriptor,
    Handle: Borrow<Surface<M>>,
{
    pub fn new_vaapi(
        display: Rc<Display>,
        config: EncoderConfig,
        fourcc: Fourcc,
        coded_size: Resolution,
        low_power: bool,
        blocking_mode: BlockingMode,
    ) -> EncodeResult<Self> {
        let bitrate_control = match config.initial_tunings.rate_control {
            RateControl::ConstantBitrate(_) => libva::constants::VA_RC_CBR,
            RateControl::ConstantQuality(_) => libva::constants::VA_RC_CQP,
        };

        let va_profile = match config.bit_depth {
            BitDepth::Depth8 => VAProfileVP9Profile0,
            BitDepth::Depth10 | BitDepth::Depth12 => VAProfileVP9Profile2,
        };

        let backend = VaapiBackend::new(
            display,
            va_profile,
            fourcc,
            coded_size,
            bitrate_control,
            low_power,
        )?;
        Self::new_vp9(backend, config, blocking_mode)
    }
}

#[cfg(test)]
pub(super) mod tests {
    use std::rc::Rc;

    use libva::constants::VA_RT_FORMAT_YUV420;
    use libva::constants::VA_RT_FORMAT_YUV420_10;
    use libva::Display;
    use libva::UsageHint;
    use libva::VAEntrypoint::VAEntrypointEncSliceLP;

    use super::*;
    use crate::backend::vaapi::encoder::tests::upload_test_frame_nv12;
    use crate::backend::vaapi::encoder::tests::TestFrameGenerator;
    use crate::backend::vaapi::encoder::VaapiBackend;
    use crate::backend::vaapi::surface_pool::PooledVaSurface;
    use crate::backend::vaapi::surface_pool::VaSurfacePool;
    use crate::codec::vp9::parser::Header;
    use crate::decoder::FramePool;
    use crate::encoder::simple_encode_loop;
    use crate::encoder::stateless::vp9::BackendRequest;
    use crate::encoder::stateless::vp9::EncoderConfig;
    use crate::encoder::stateless::vp9::StatelessEncoder;
    use crate::encoder::stateless::BackendPromise;
    use crate::encoder::stateless::StatelessEncoderBackendImport;
    use crate::encoder::FrameMetadata;
    use crate::encoder::Tunings;
    use crate::utils::IvfFileHeader;
    use crate::utils::IvfFrameHeader;
    use crate::FrameLayout;
    use crate::PlaneLayout;
    use crate::Resolution;

    #[test]
    // Ignore this test by default as it requires libva-compatible hardware.
    #[ignore]
    fn test_simple_encode_frame() {
        type Descriptor = ();
        type Surface = libva::Surface<Descriptor>;
        const WIDTH: u32 = 256;
        const HEIGHT: u32 = 256;
        let fourcc = b"NV12".into();

        let frame_layout = FrameLayout {
            format: (fourcc, 0),
            size: Resolution {
                width: WIDTH,
                height: HEIGHT,
            },
            planes: vec![
                PlaneLayout {
                    buffer_index: 0,
                    offset: 0,
                    stride: WIDTH as usize,
                },
                PlaneLayout {
                    buffer_index: 0,
                    offset: (WIDTH * HEIGHT) as usize,
                    stride: WIDTH as usize,
                },
            ],
        };

        let display = Display::open().unwrap();
        let entrypoints = display
            .query_config_entrypoints(VAProfileVP9Profile0)
            .unwrap();
        let low_power = entrypoints.contains(&VAEntrypointEncSliceLP);

        let mut backend = VaapiBackend::<Descriptor, Surface>::new(
            Rc::clone(&display),
            VAProfileVP9Profile0,
            fourcc,
            Resolution {
                width: WIDTH,
                height: HEIGHT,
            },
            libva::constants::VA_RC_CBR,
            low_power,
        )
        .unwrap();

        let mut surfaces = display
            .create_surfaces(
                VA_RT_FORMAT_YUV420,
                Some(frame_layout.format.0 .0),
                WIDTH,
                HEIGHT,
                Some(UsageHint::USAGE_HINT_ENCODER),
                vec![()],
            )
            .unwrap();

        let surface = surfaces.pop().unwrap();

        upload_test_frame_nv12(&display, &surface, 0.0);

        let input_meta = FrameMetadata {
            display_resolution: Resolution {
                width: WIDTH,
                height: HEIGHT,
            },
            layout: frame_layout,
            force_keyframe: false,
            timestamp: 0,
        };

        let pic = backend.import_picture(&input_meta, surface).unwrap();

        let header = Header {
            frame_type: FrameType::KeyFrame,
            show_frame: true,
            error_resilient_mode: false,
            width: WIDTH,
            height: HEIGHT,
            render_and_frame_size_different: false,
            render_width: WIDTH,
            render_height: HEIGHT,
            intra_only: true,
            refresh_frame_flags: 0x01,
            ref_frame_idx: [0, 0, 0],

            ..Default::default()
        };

        let request = BackendRequest {
            header,
            input: pic,
            input_meta,
            last_frame_ref: None,
            golden_frame_ref: None,
            altref_frame_ref: None,
            tunings: Tunings {
                rate_control: RateControl::ConstantBitrate(30_000),
                ..Default::default()
            },
            coded_output: Vec::new(),
        };

        let (_, output) = backend.encode_frame(request).unwrap();
        let output = output.sync().unwrap();

        let write_to_file = std::option_env!("CROS_CODECS_TEST_WRITE_TO_FILE") == Some("true");
        if write_to_file {
            use std::io::Write;

            let mut out = std::fs::File::create("test_simple_encode_frame.vp9.ivf").unwrap();

            let file_header =
                IvfFileHeader::new(IvfFileHeader::CODEC_VP9, WIDTH as u16, HEIGHT as u16, 30, 1);

            let frame_header = IvfFrameHeader {
                frame_size: output.len() as u32,
                timestamp: 0,
            };

            file_header.writo_into(&mut out).unwrap();
            frame_header.writo_into(&mut out).unwrap();

            out.write_all(&output).unwrap();
            out.flush().unwrap();
        }
    }

    #[test]
    // Ignore this test by default as it requires libva-compatible hardware.
    #[ignore]
    fn test_vaapi_encoder() {
        type VaapiVp9Encoder<'l> =
            StatelessEncoder<PooledVaSurface<()>, VaapiBackend<(), PooledVaSurface<()>>>;

        const WIDTH: usize = 512;
        const HEIGHT: usize = 512;
        const FRAME_COUNT: u64 = 100;

        let _ = env_logger::try_init();

        let display = libva::Display::open().unwrap();
        let entrypoints = display
            .query_config_entrypoints(VAProfileVP9Profile0)
            .unwrap();
        let low_power = entrypoints.contains(&VAEntrypointEncSliceLP);

        let config = EncoderConfig {
            resolution: Resolution {
                width: WIDTH as u32,
                height: HEIGHT as u32,
            },
            initial_tunings: Tunings {
                rate_control: RateControl::ConstantBitrate(200_000),
                framerate: 30,
                ..Default::default()
            },
            ..Default::default()
        };

        let frame_layout = FrameLayout {
            format: (b"NV12".into(), 0),
            size: Resolution {
                width: WIDTH as u32,
                height: HEIGHT as u32,
            },
            planes: vec![
                PlaneLayout {
                    buffer_index: 0,
                    offset: 0,
                    stride: WIDTH,
                },
                PlaneLayout {
                    buffer_index: 0,
                    offset: WIDTH * HEIGHT,
                    stride: WIDTH,
                },
            ],
        };

        let mut encoder = VaapiVp9Encoder::new_vaapi(
            Rc::clone(&display),
            config,
            frame_layout.format.0,
            frame_layout.size,
            low_power,
            BlockingMode::Blocking,
        )
        .unwrap();

        let mut pool = VaSurfacePool::new(
            Rc::clone(&display),
            VA_RT_FORMAT_YUV420,
            Some(UsageHint::USAGE_HINT_ENCODER),
            Resolution {
                width: WIDTH as u32,
                height: HEIGHT as u32,
            },
        );

        pool.add_frames(vec![(); 16]).unwrap();

        let mut frame_producer = TestFrameGenerator::new(
            FRAME_COUNT,
            display,
            pool,
            Resolution {
                width: WIDTH as u32,
                height: HEIGHT as u32,
            },
            frame_layout,
        );

        let mut bitstream = Vec::new();

        let file_header = IvfFileHeader::new(
            IvfFileHeader::CODEC_VP9,
            WIDTH as u16,
            HEIGHT as u16,
            30,
            FRAME_COUNT as u32,
        );

        file_header.writo_into(&mut bitstream).unwrap();

        simple_encode_loop(&mut encoder, &mut frame_producer, |coded| {
            let header = IvfFrameHeader {
                timestamp: coded.metadata.timestamp,
                frame_size: coded.bitstream.len() as u32,
            };

            header.writo_into(&mut bitstream).unwrap();
            bitstream.extend(coded.bitstream);
        })
        .unwrap();

        let write_to_file = std::option_env!("CROS_CODECS_TEST_WRITE_TO_FILE") == Some("true");
        if write_to_file {
            use std::io::Write;
            let mut out = std::fs::File::create("test_vaapi_encoder.vp9.ivf").unwrap();
            out.write_all(&bitstream).unwrap();
            out.flush().unwrap();
        }
    }

    #[test]
    // Ignore this test by default as it requires libva-compatible hardware.
    #[ignore]
    fn test_vaapi_encoder_p010() {
        type VaapiVp9Encoder<'l> =
            StatelessEncoder<PooledVaSurface<()>, VaapiBackend<(), PooledVaSurface<()>>>;

        const WIDTH: usize = 512;
        const HEIGHT: usize = 512;
        const FRAME_COUNT: u64 = 100;

        let _ = env_logger::try_init();

        let display = libva::Display::open().unwrap();
        let entrypoints = display
            .query_config_entrypoints(VAProfileVP9Profile2)
            .unwrap();
        let low_power = entrypoints.contains(&VAEntrypointEncSliceLP);

        let config = EncoderConfig {
            bit_depth: BitDepth::Depth10,
            resolution: Resolution {
                width: WIDTH as u32,
                height: HEIGHT as u32,
            },
            initial_tunings: Tunings {
                rate_control: RateControl::ConstantBitrate(200_000),
                framerate: 30,
                ..Default::default()
            },
            ..Default::default()
        };

        let frame_layout = FrameLayout {
            format: (b"P010".into(), 0),
            size: Resolution {
                width: WIDTH as u32,
                height: HEIGHT as u32,
            },
            planes: vec![
                PlaneLayout {
                    buffer_index: 0,
                    offset: 0,
                    stride: WIDTH,
                },
                PlaneLayout {
                    buffer_index: 0,
                    offset: WIDTH * HEIGHT,
                    stride: WIDTH,
                },
            ],
        };

        let mut encoder = VaapiVp9Encoder::new_vaapi(
            Rc::clone(&display),
            config,
            frame_layout.format.0,
            frame_layout.size,
            low_power,
            BlockingMode::Blocking,
        )
        .unwrap();

        let mut pool = VaSurfacePool::new(
            Rc::clone(&display),
            VA_RT_FORMAT_YUV420_10,
            Some(UsageHint::USAGE_HINT_ENCODER),
            Resolution {
                width: WIDTH as u32,
                height: HEIGHT as u32,
            },
        );

        pool.add_frames(vec![(); 16]).unwrap();

        let mut frame_producer = TestFrameGenerator::new(
            FRAME_COUNT,
            display,
            pool,
            Resolution {
                width: WIDTH as u32,
                height: HEIGHT as u32,
            },
            frame_layout,
        );

        let mut bitstream = Vec::new();

        let file_header = IvfFileHeader::new(
            IvfFileHeader::CODEC_VP9,
            WIDTH as u16,
            HEIGHT as u16,
            30,
            FRAME_COUNT as u32,
        );

        file_header.writo_into(&mut bitstream).unwrap();

        simple_encode_loop(&mut encoder, &mut frame_producer, |coded| {
            let header = IvfFrameHeader {
                timestamp: coded.metadata.timestamp,
                frame_size: coded.bitstream.len() as u32,
            };

            header.writo_into(&mut bitstream).unwrap();
            bitstream.extend(coded.bitstream);
        })
        .unwrap();

        let write_to_file = std::option_env!("CROS_CODECS_TEST_WRITE_TO_FILE") == Some("true");
        if write_to_file {
            use std::io::Write;
            let mut out = std::fs::File::create("test_vaapi_encoder_p010.vp9.ivf").unwrap();
            out.write_all(&bitstream).unwrap();
            out.flush().unwrap();
        }
    }
}
