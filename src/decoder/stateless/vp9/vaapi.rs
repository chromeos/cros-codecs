// Copyright 2022 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::rc::Rc;

use anyhow::anyhow;
use anyhow::Context;
use libva::Display;
use libva::Picture as VaPicture;
use libva::SegmentParameterVP9;
use libva::SurfaceMemoryDescriptor;

use crate::backend::vaapi::decoder::va_surface_id;
use crate::backend::vaapi::decoder::PoolCreationMode;
use crate::backend::vaapi::decoder::VaStreamInfo;
use crate::backend::vaapi::decoder::VaapiBackend;
use crate::backend::vaapi::decoder::VaapiPicture;
use crate::codec::vp9::parser::BitDepth;
use crate::codec::vp9::parser::Header;
use crate::codec::vp9::parser::Profile;
use crate::codec::vp9::parser::ALTREF_FRAME;
use crate::codec::vp9::parser::GOLDEN_FRAME;
use crate::codec::vp9::parser::LAST_FRAME;
use crate::codec::vp9::parser::MAX_SEGMENTS;
use crate::codec::vp9::parser::NUM_REF_FRAMES;
use crate::decoder::stateless::vp9::Segmentation;
use crate::decoder::stateless::vp9::StatelessVp9DecoderBackend;
use crate::decoder::stateless::vp9::Vp9;
use crate::decoder::stateless::NewPictureError;
use crate::decoder::stateless::NewPictureResult;
use crate::decoder::stateless::NewStatelessDecoderError;
use crate::decoder::stateless::StatelessBackendResult;
use crate::decoder::stateless::StatelessDecoder;
use crate::decoder::stateless::StatelessDecoderBackendPicture;
use crate::decoder::BlockingMode;

/// The number of surfaces to allocate for this codec.
const NUM_SURFACES: usize = 12;

/// Returns the RT format matching the input parameters.
fn get_rt_format(
    profile: Profile,
    bit_depth: BitDepth,
    subsampling_x: bool,
    subsampling_y: bool,
) -> anyhow::Result<u32> {
    match profile {
        Profile::Profile0 => Ok(libva::VA_RT_FORMAT_YUV420),
        Profile::Profile1 => {
            if subsampling_x && !subsampling_y {
                Ok(libva::VA_RT_FORMAT_YUV422)
            } else if !subsampling_x && !subsampling_y {
                Ok(libva::VA_RT_FORMAT_YUV444)
            } else {
                Err(anyhow!(
                    "Unsupported subsampling for profile 1: X: {:?} Y: {:?}",
                    subsampling_x,
                    subsampling_y
                ))
            }
        }
        Profile::Profile2 => match bit_depth {
            BitDepth::Depth8 => Err(anyhow!(
                "Unsupported bit depth for profile 2: {:?}",
                bit_depth
            )),
            BitDepth::Depth10 => Ok(libva::VA_RT_FORMAT_YUV420_10),
            BitDepth::Depth12 => Ok(libva::VA_RT_FORMAT_YUV420_12),
        },
        Profile::Profile3 => {
            if subsampling_x && !subsampling_y {
                match bit_depth {
                        BitDepth::Depth8 => Err(anyhow!(
                            "Unsupported (subsampling_x, subsampling_y, bit depth) combination for profile 3: ({:?}, {:?}, {:?})",
                            subsampling_x,
                            subsampling_y,
                            bit_depth
                        )),
                        BitDepth::Depth10 => Ok(libva::VA_RT_FORMAT_YUV422_10),
                        BitDepth::Depth12 => Ok(libva::VA_RT_FORMAT_YUV422_12),
                    }
            } else if !subsampling_x && !subsampling_y {
                match bit_depth {
                        BitDepth::Depth8 => Err(anyhow!(
                            "Unsupported (subsampling_x, subsampling_y, bit depth) combination for profile 3: ({:?}, {:?}, {:?})",
                            subsampling_x,
                            subsampling_y,
                            bit_depth
                        )),
                        BitDepth::Depth10 => Ok(libva::VA_RT_FORMAT_YUV444_10),
                        BitDepth::Depth12 => Ok(libva::VA_RT_FORMAT_YUV444_12),
                    }
            } else {
                Err(anyhow!(
                            "Unsupported (subsampling_x, subsampling_y, bit depth) combination for profile 3: ({:?}, {:?}, {:?})",
                            subsampling_x,
                            subsampling_y,
                            bit_depth
                        ))
            }
        }
    }
}

impl VaStreamInfo for &Header {
    fn va_profile(&self) -> anyhow::Result<i32> {
        Ok(match self.profile {
            Profile::Profile0 => libva::VAProfile::VAProfileVP9Profile0,
            Profile::Profile1 => libva::VAProfile::VAProfileVP9Profile1,
            Profile::Profile2 => libva::VAProfile::VAProfileVP9Profile2,
            Profile::Profile3 => libva::VAProfile::VAProfileVP9Profile3,
        })
    }

    fn rt_format(&self) -> anyhow::Result<u32> {
        get_rt_format(
            self.profile,
            self.bit_depth,
            self.subsampling_x,
            self.subsampling_y,
        )
    }

    fn min_num_surfaces(&self) -> usize {
        NUM_SURFACES
    }

    fn coded_size(&self) -> (u32, u32) {
        (self.width, self.height)
    }

    fn visible_rect(&self) -> ((u32, u32), (u32, u32)) {
        ((0, 0), self.coded_size())
    }
}

fn build_pic_param(
    hdr: &Header,
    reference_frames: [u32; NUM_REF_FRAMES],
) -> anyhow::Result<libva::BufferType> {
    let pic_fields = libva::VP9PicFields::new(
        hdr.subsampling_x as u32,
        hdr.subsampling_y as u32,
        hdr.frame_type as u32,
        hdr.show_frame as u32,
        hdr.error_resilient_mode as u32,
        hdr.intra_only as u32,
        hdr.allow_high_precision_mv as u32,
        hdr.interpolation_filter as u32,
        hdr.frame_parallel_decoding_mode as u32,
        hdr.reset_frame_context as u32,
        hdr.refresh_frame_context as u32,
        hdr.frame_context_idx as u32,
        hdr.seg.enabled as u32,
        hdr.seg.temporal_update as u32,
        hdr.seg.update_map as u32,
        hdr.ref_frame_idx[LAST_FRAME - 1] as u32,
        hdr.ref_frame_sign_bias[LAST_FRAME] as u32,
        hdr.ref_frame_idx[GOLDEN_FRAME - 1] as u32,
        hdr.ref_frame_sign_bias[GOLDEN_FRAME] as u32,
        hdr.ref_frame_idx[ALTREF_FRAME - 1] as u32,
        hdr.ref_frame_sign_bias[ALTREF_FRAME] as u32,
        hdr.lossless as u32,
    );

    let lf = &hdr.lf;
    let seg = &hdr.seg;

    let seg_pred_prob = if seg.temporal_update {
        seg.pred_probs
    } else {
        [0xff, 0xff, 0xff]
    };

    let pic_param = libva::PictureParameterBufferVP9::new(
        hdr.width.try_into().unwrap(),
        hdr.height.try_into().unwrap(),
        reference_frames,
        &pic_fields,
        lf.level,
        lf.sharpness,
        hdr.tile_rows_log2,
        hdr.tile_cols_log2,
        hdr.uncompressed_header_size_in_bytes.try_into().unwrap(),
        hdr.header_size_in_bytes,
        seg.tree_probs,
        seg_pred_prob,
        hdr.profile as u8,
        hdr.bit_depth as u8,
    );

    Ok(libva::BufferType::PictureParameter(
        libva::PictureParameter::VP9(pic_param),
    ))
}

fn build_slice_param(
    seg: &[Segmentation; MAX_SEGMENTS],
    slice_size: usize,
) -> anyhow::Result<libva::BufferType> {
    let seg_params: std::result::Result<[SegmentParameterVP9; MAX_SEGMENTS], _> = seg
        .iter()
        .map(|s| {
            let seg_flags = libva::VP9SegmentFlags::new(
                s.reference_frame_enabled as u16,
                s.reference_frame as u16,
                s.reference_skip_enabled as u16,
            );

            libva::SegmentParameterVP9::new(
                &seg_flags,
                s.lvl_lookup,
                s.luma_ac_quant_scale,
                s.luma_dc_quant_scale,
                s.chroma_ac_quant_scale,
                s.chroma_dc_quant_scale,
            )
        })
        .collect::<Vec<_>>()
        .try_into();

    let seg_params = match seg_params {
        Ok(seg_params) => seg_params,

        // Impossible, this is just a detour to collect into a fixed-size
        // array whose size is the same size of the `seg` argument.
        Err(_) => panic!("Invalid segment parameters"),
    };

    Ok(libva::BufferType::SliceParameter(
        libva::SliceParameter::VP9(libva::SliceParameterBufferVP9::new(
            slice_size as u32,
            0,
            libva::VA_SLICE_DATA_FLAG_ALL,
            seg_params,
        )),
    ))
}

impl<M: SurfaceMemoryDescriptor + 'static> StatelessDecoderBackendPicture<Vp9> for VaapiBackend<M> {
    type Picture = VaapiPicture<M>;
}

impl<M: SurfaceMemoryDescriptor + 'static> StatelessVp9DecoderBackend for VaapiBackend<M> {
    fn new_sequence(&mut self, header: &Header) -> StatelessBackendResult<()> {
        self.new_sequence(header, PoolCreationMode::Highest)
    }

    fn new_picture(&mut self, timestamp: u64) -> NewPictureResult<Self::Picture> {
        let highest_pool = self.highest_pool();
        let surface = highest_pool
            .get_surface()
            .ok_or(NewPictureError::OutOfOutputBuffers)?;
        let metadata = self.metadata_state.get_parsed()?;
        let context = &metadata.context;

        Ok(VaPicture::new(timestamp, Rc::clone(context), surface))
    }

    fn submit_picture(
        &mut self,
        mut picture: Self::Picture,
        hdr: &Header,
        reference_frames: &[Option<Self::Handle>; NUM_REF_FRAMES],
        bitstream: &[u8],
        segmentation: &[Segmentation; MAX_SEGMENTS],
    ) -> StatelessBackendResult<Self::Handle> {
        let metadata = self.metadata_state.get_parsed()?;
        let context = &metadata.context;

        let reference_frames: [u32; NUM_REF_FRAMES] = reference_frames
            .iter()
            .map(va_surface_id)
            .collect::<Vec<_>>()
            .try_into()
            .unwrap();

        let pic_param = context
            .create_buffer(build_pic_param(hdr, reference_frames)?)
            .context("while creating pic params buffer")?;

        let slice_param = context
            .create_buffer(build_slice_param(segmentation, bitstream.len())?)
            .context("while creating slice params buffer")?;

        let slice_data = context
            .create_buffer(libva::BufferType::SliceData(Vec::from(bitstream)))
            .context("while creating slice data buffer")?;

        // Add buffers with the parsed data.
        picture.add_buffer(pic_param);
        picture.add_buffer(slice_param);
        picture.add_buffer(slice_data);

        self.process_picture::<Vp9>(picture)
    }
}

impl<M: SurfaceMemoryDescriptor + 'static> StatelessDecoder<Vp9, VaapiBackend<M>> {
    // Creates a new instance of the decoder using the VAAPI backend.
    pub fn new_vaapi<S>(
        display: Rc<Display>,
        blocking_mode: BlockingMode,
    ) -> Result<Self, NewStatelessDecoderError>
    where
        M: From<S>,
        S: From<M>,
    {
        Self::new(VaapiBackend::new(display, true), blocking_mode)
    }
}

#[cfg(test)]
mod tests {
    use libva::BufferType;
    use libva::Display;
    use libva::PictureParameter;
    use libva::SliceParameter;

    use crate::bitstream_utils::IvfIterator;
    use crate::codec::vp9::parser::Parser;
    use crate::codec::vp9::parser::MAX_SEGMENTS;
    use crate::codec::vp9::parser::NUM_REF_FRAMES;
    use crate::decoder::stateless::tests::test_decode_stream;
    use crate::decoder::stateless::tests::TestStream;
    use crate::decoder::stateless::vp9::Segmentation;
    use crate::decoder::BlockingMode;
    use crate::utils::simple_playback_loop;
    use crate::utils::simple_playback_loop_owned_frames;
    use crate::DecodedFormat;

    use super::*;

    /// Run `test` using the vaapi decoder, in both blocking and non-blocking modes.
    fn test_decoder_vaapi(
        test: &TestStream,
        output_format: DecodedFormat,
        blocking_mode: BlockingMode,
    ) {
        let display = Display::open().unwrap();
        let decoder = StatelessDecoder::<Vp9, _>::new_vaapi::<()>(display, blocking_mode).unwrap();

        test_decode_stream(
            |d, s, c| {
                simple_playback_loop(
                    d,
                    IvfIterator::new(s),
                    c,
                    &mut simple_playback_loop_owned_frames,
                    output_format,
                    blocking_mode,
                )
            },
            decoder,
            test,
            true,
            false,
        );
    }

    #[test]
    // Ignore this test by default as it requires libva-compatible hardware.
    #[ignore]
    fn test_25fps_block() {
        use crate::decoder::stateless::vp9::tests::DECODE_TEST_25FPS;
        test_decoder_vaapi(
            &DECODE_TEST_25FPS,
            DecodedFormat::NV12,
            BlockingMode::Blocking,
        );
    }

    #[test]
    // Ignore this test by default as it requires libva-compatible hardware.
    #[ignore]
    fn test_25fps_nonblock() {
        use crate::decoder::stateless::vp9::tests::DECODE_TEST_25FPS;
        test_decoder_vaapi(
            &DECODE_TEST_25FPS,
            DecodedFormat::NV12,
            BlockingMode::NonBlocking,
        );
    }

    #[test]
    // Ignore this test by default as it requires libva-compatible hardware.
    #[ignore]
    fn show_existing_frame_block() {
        use crate::decoder::stateless::vp9::tests::DECODE_TEST_25FPS_SHOW_EXISTING_FRAME;
        test_decoder_vaapi(
            &DECODE_TEST_25FPS_SHOW_EXISTING_FRAME,
            DecodedFormat::NV12,
            BlockingMode::Blocking,
        );
    }

    #[test]
    // Ignore this test by default as it requires libva-compatible hardware.
    #[ignore]
    fn show_existing_frame_nonblock() {
        use crate::decoder::stateless::vp9::tests::DECODE_TEST_25FPS_SHOW_EXISTING_FRAME;
        test_decoder_vaapi(
            &DECODE_TEST_25FPS_SHOW_EXISTING_FRAME,
            DecodedFormat::NV12,
            BlockingMode::NonBlocking,
        );
    }

    #[test]
    // Ignore this test by default as it requires libva-compatible hardware.
    #[ignore]
    fn show_existing_frame2_block() {
        use crate::decoder::stateless::vp9::tests::DECODE_TEST_25FPS_SHOW_EXISTING_FRAME2;
        test_decoder_vaapi(
            &DECODE_TEST_25FPS_SHOW_EXISTING_FRAME2,
            DecodedFormat::NV12,
            BlockingMode::Blocking,
        );
    }

    #[test]
    // Ignore this test by default as it requires libva-compatible hardware.
    #[ignore]
    fn show_existing_frame2_nonblock() {
        use crate::decoder::stateless::vp9::tests::DECODE_TEST_25FPS_SHOW_EXISTING_FRAME2;
        test_decoder_vaapi(
            &DECODE_TEST_25FPS_SHOW_EXISTING_FRAME2,
            DecodedFormat::NV12,
            BlockingMode::NonBlocking,
        );
    }

    #[test]
    // Ignore this test by default as it requires libva-compatible hardware.
    #[ignore]
    fn test_resolution_change_500frames_block() {
        use crate::decoder::stateless::vp9::tests::DECODE_RESOLUTION_CHANGE_500FRAMES;
        let display = Display::open().unwrap();
        let decoder =
            StatelessDecoder::<Vp9, _>::new_vaapi::<()>(display, BlockingMode::Blocking).unwrap();

        // Skip CRC checking as they have not been generated properly?
        test_decode_stream(
            |d, s, c| {
                simple_playback_loop(
                    d,
                    IvfIterator::new(s),
                    c,
                    &mut simple_playback_loop_owned_frames,
                    DecodedFormat::NV12,
                    BlockingMode::Blocking,
                )
            },
            decoder,
            &DECODE_RESOLUTION_CHANGE_500FRAMES,
            false,
            false,
        );
    }

    #[test]
    // Ignore this test by default as it requires libva-compatible hardware.
    #[ignore]
    fn test_resolution_change_500frames_nonblock() {
        use crate::decoder::stateless::vp9::tests::DECODE_RESOLUTION_CHANGE_500FRAMES;
        let display = Display::open().unwrap();
        let decoder =
            StatelessDecoder::<Vp9, _>::new_vaapi::<()>(display, BlockingMode::Blocking).unwrap();

        // Skip CRC checking as they have not been generated properly?
        test_decode_stream(
            |d, s, c| {
                simple_playback_loop(
                    d,
                    IvfIterator::new(s),
                    c,
                    &mut simple_playback_loop_owned_frames,
                    DecodedFormat::NV12,
                    BlockingMode::NonBlocking,
                )
            },
            decoder,
            &DECODE_RESOLUTION_CHANGE_500FRAMES,
            false,
            false,
        );
    }

    #[test]
    /// Check that we are able to build the VA picture parameters from the stream properly.
    fn build_pic_params() {
        const TEST_STREAM: &[u8] = include_bytes!("../../../codec/vp9/test_data/test-25fps.vp9");
        let mut parser: Parser = Default::default();
        let mut segmentation: [Segmentation; MAX_SEGMENTS] = Default::default();
        let mut ivf_iter = IvfIterator::new(TEST_STREAM);

        // FRAME 0

        let packet = ivf_iter.next().unwrap();
        let mut frames = parser.parse_chunk(packet).unwrap();
        assert_eq!(frames.len(), 1);
        let frame = frames.remove(0);

        assert_eq!(frame.as_ref().len(), 10674);

        let pic_param = build_pic_param(
            &frame.header,
            [libva::VA_INVALID_SURFACE; NUM_REF_FRAMES],
        )
        .unwrap();
        let pic_param = match pic_param {
            BufferType::PictureParameter(PictureParameter::VP9(pic_param)) => pic_param,
            _ => panic!(),
        };

        Segmentation::update_segmentation(&mut segmentation, &frame.header);
        let slice_param = build_slice_param(&segmentation, frame.as_ref().len()).unwrap();
        let slice_param = match slice_param {
            BufferType::SliceParameter(SliceParameter::VP9(slice_param)) => slice_param,
            _ => panic!(),
        };

        assert_eq!(pic_param.inner().frame_width, 320);
        assert_eq!(pic_param.inner().frame_height, 240);
        assert_eq!(
            pic_param.inner().reference_frames,
            [libva::VA_INVALID_SURFACE; NUM_REF_FRAMES]
        );

        // Safe because this bitfield is initialized by the decoder.
        assert_eq!(unsafe { pic_param.inner().pic_fields.value }, unsafe {
            libva::VP9PicFields::new(
                1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            )
            .inner()
            .value
        });

        assert_eq!(pic_param.inner().filter_level, 9);
        assert_eq!(pic_param.inner().sharpness_level, 0);
        assert_eq!(pic_param.inner().log2_tile_rows, 0);
        assert_eq!(pic_param.inner().log2_tile_columns, 0);
        assert_eq!(pic_param.inner().frame_header_length_in_bytes, 18);
        assert_eq!(pic_param.inner().first_partition_size, 120);
        assert_eq!(pic_param.inner().mb_segment_tree_probs, [0; 7]);
        assert_eq!(pic_param.inner().segment_pred_probs, [0xff; 3]);
        assert_eq!(pic_param.inner().profile, 0);
        assert_eq!(pic_param.inner().bit_depth, 8);

        assert_eq!(slice_param.inner().slice_data_size, 10674);
        assert_eq!(slice_param.inner().slice_data_offset, 0);
        assert_eq!(
            slice_param.inner().slice_data_flag,
            libva::VA_SLICE_DATA_FLAG_ALL
        );

        for seg_param in &slice_param.inner().seg_param {
            // Safe because this bitfield is initialized by the decoder.
            assert_eq!(unsafe { seg_param.segment_flags.value }, 0);
            assert_eq!(seg_param.filter_level, [[10, 0], [9, 9], [8, 8], [8, 8]]);
            assert_eq!(seg_param.luma_ac_quant_scale, 72);
            assert_eq!(seg_param.luma_dc_quant_scale, 62);
            assert_eq!(seg_param.chroma_ac_quant_scale, 72);
            assert_eq!(seg_param.chroma_dc_quant_scale, 62);
        }

        // FRAME 1

        let packet = ivf_iter.next().unwrap();
        let mut frames = parser.parse_chunk(packet).unwrap();
        assert_eq!(frames.len(), 2);
        let frame = frames.remove(0);
        assert_eq!(frame.as_ref().len(), 2390);

        let pic_param = build_pic_param(&frame.header, [0; NUM_REF_FRAMES]).unwrap();
        let pic_param = match pic_param {
            BufferType::PictureParameter(PictureParameter::VP9(pic_param)) => pic_param,
            _ => panic!(),
        };

        Segmentation::update_segmentation(&mut segmentation, &frame.header);
        let slice_param = build_slice_param(&segmentation, frame.as_ref().len()).unwrap();
        let slice_param = match slice_param {
            BufferType::SliceParameter(SliceParameter::VP9(slice_param)) => slice_param,
            _ => panic!(),
        };

        assert_eq!(pic_param.inner().frame_width, 320);
        assert_eq!(pic_param.inner().frame_height, 240);
        assert_eq!(pic_param.inner().reference_frames, [0; NUM_REF_FRAMES]);
        // Safe because this bitfield is initialized by the decoder.
        assert_eq!(unsafe { pic_param.inner().pic_fields.value }, unsafe {
            libva::VP9PicFields::new(
                1, 1, 1, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 2, 0, 0,
            )
            .inner()
            .value
        });

        assert_eq!(pic_param.inner().filter_level, 15);
        assert_eq!(pic_param.inner().sharpness_level, 0);
        assert_eq!(pic_param.inner().log2_tile_rows, 0);
        assert_eq!(pic_param.inner().log2_tile_columns, 0);
        assert_eq!(pic_param.inner().frame_header_length_in_bytes, 11);
        assert_eq!(pic_param.inner().first_partition_size, 48);
        assert_eq!(pic_param.inner().mb_segment_tree_probs, [0; 7]);
        assert_eq!(pic_param.inner().segment_pred_probs, [0xff; 3]);
        assert_eq!(pic_param.inner().profile, 0);
        assert_eq!(pic_param.inner().bit_depth, 8);

        assert_eq!(slice_param.inner().slice_data_size, 2390);
        assert_eq!(slice_param.inner().slice_data_offset, 0);
        assert_eq!(
            slice_param.inner().slice_data_flag,
            libva::VA_SLICE_DATA_FLAG_ALL
        );

        for seg_param in &slice_param.inner().seg_param {
            // Safe because this bitfield is initialized by the decoder.
            assert_eq!(unsafe { seg_param.segment_flags.value }, 0);
            assert_eq!(
                seg_param.filter_level,
                [[16, 0], [15, 15], [14, 14], [14, 14]]
            );
            assert_eq!(seg_param.luma_ac_quant_scale, 136);
            assert_eq!(seg_param.luma_dc_quant_scale, 111);
            assert_eq!(seg_param.chroma_ac_quant_scale, 136);
            assert_eq!(seg_param.chroma_dc_quant_scale, 111);
        }

        // FRAME 2

        let frame = frames.remove(0);
        assert_eq!(frame.as_ref().len(), 108);

        let pic_param = build_pic_param(&frame.header, [0, 0, 1, 0, 0, 0, 0, 0]).unwrap();
        let pic_param = match pic_param {
            BufferType::PictureParameter(PictureParameter::VP9(pic_param)) => pic_param,
            _ => panic!(),
        };

        Segmentation::update_segmentation(&mut segmentation, &frame.header);
        let slice_param = build_slice_param(&segmentation, frame.as_ref().len()).unwrap();
        let slice_param = match slice_param {
            BufferType::SliceParameter(SliceParameter::VP9(slice_param)) => slice_param,
            _ => panic!(),
        };

        assert_eq!(pic_param.inner().frame_width, 320);
        assert_eq!(pic_param.inner().frame_height, 240);
        assert_eq!(pic_param.inner().reference_frames, [0, 0, 1, 0, 0, 0, 0, 0]);

        // Safe because this bitfield is initialized by the decoder.
        assert_eq!(unsafe { pic_param.inner().pic_fields.value }, unsafe {
            libva::VP9PicFields::new(
                1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 2, 1, 0,
            )
            .inner()
            .value
        });

        assert_eq!(pic_param.inner().filter_level, 36);
        assert_eq!(pic_param.inner().sharpness_level, 0);
        assert_eq!(pic_param.inner().log2_tile_rows, 0);
        assert_eq!(pic_param.inner().log2_tile_columns, 0);
        assert_eq!(pic_param.inner().frame_header_length_in_bytes, 10);
        assert_eq!(pic_param.inner().first_partition_size, 9);
        assert_eq!(pic_param.inner().mb_segment_tree_probs, [0; 7]);
        assert_eq!(pic_param.inner().segment_pred_probs, [0xff; 3]);
        assert_eq!(pic_param.inner().profile, 0);
        assert_eq!(pic_param.inner().bit_depth, 8);

        assert_eq!(slice_param.inner().slice_data_size, 108);
        assert_eq!(slice_param.inner().slice_data_offset, 0);
        assert_eq!(
            slice_param.inner().slice_data_flag,
            libva::VA_SLICE_DATA_FLAG_ALL
        );

        for seg_param in &slice_param.inner().seg_param {
            // Safe because this bitfield is initialized by the decoder.
            assert_eq!(unsafe { seg_param.segment_flags.value }, 0);
            assert_eq!(
                seg_param.filter_level,
                [[38, 0], [36, 36], [34, 34], [34, 34]]
            );
            assert_eq!(seg_param.luma_ac_quant_scale, 864);
            assert_eq!(seg_param.luma_dc_quant_scale, 489);
            assert_eq!(seg_param.chroma_ac_quant_scale, 864);
            assert_eq!(seg_param.chroma_dc_quant_scale, 489);
        }
    }
}
