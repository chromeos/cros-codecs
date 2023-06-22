// Copyright 2022 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::convert::TryFrom;
use std::rc::Rc;

use anyhow::Context;
use libva::BufferType;
use libva::Display;
use libva::IQMatrix;
use libva::IQMatrixBufferVP8;
use libva::Picture as VaPicture;
use libva::ProbabilityDataBufferVP8;
use libva::SurfaceMemoryDescriptor;

use crate::backend::vaapi::DecodedHandle as VADecodedHandle;
use crate::backend::vaapi::VaStreamInfo;
use crate::backend::vaapi::VaapiBackend;
use crate::codec::vp8::parser::Header;
use crate::codec::vp8::parser::MbLfAdjustments;
use crate::codec::vp8::parser::Segmentation;
use crate::decoder::stateless::vp8::Decoder;
use crate::decoder::stateless::vp8::StatelessVp8DecoderBackend;
use crate::decoder::stateless::StatelessBackendError;
use crate::decoder::stateless::StatelessBackendResult;
use crate::decoder::BlockingMode;
use crate::Resolution;

/// The number of surfaces to allocate for this codec. Same as GStreamer's vavp8dec.
const NUM_SURFACES: usize = 7;

impl VaStreamInfo for &Header {
    fn va_profile(&self) -> anyhow::Result<i32> {
        Ok(libva::VAProfile::VAProfileVP8Version0_3)
    }

    fn rt_format(&self) -> anyhow::Result<u32> {
        Ok(libva::constants::VA_RT_FORMAT_YUV420)
    }

    fn min_num_surfaces(&self) -> usize {
        NUM_SURFACES
    }

    fn coded_size(&self) -> (u32, u32) {
        (self.width() as u32, self.height() as u32)
    }

    fn visible_rect(&self) -> ((u32, u32), (u32, u32)) {
        ((0, 0), self.coded_size())
    }
}

/// A clamp such that min <= x <= max
fn clamp<T: PartialOrd>(x: T, low: T, high: T) -> T {
    if x > high {
        high
    } else if x < low {
        low
    } else {
        x
    }
}

fn build_iq_matrix(
    frame_hdr: &Header,
    segmentation: &Segmentation,
) -> anyhow::Result<libva::BufferType> {
    let mut quantization_index: [[u16; 6]; 4] = Default::default();

    for (i, quantization_index) in quantization_index.iter_mut().enumerate() {
        let mut qi_base: i16;

        if segmentation.segmentation_enabled {
            qi_base = i16::from(segmentation.quantizer_update_value[i]);
            if !segmentation.segment_feature_mode {
                qi_base += i16::from(frame_hdr.quant_indices().y_ac_qi);
            }
        } else {
            qi_base = i16::from(frame_hdr.quant_indices().y_ac_qi);
        }

        let mut qi = qi_base;
        quantization_index[0] = u16::try_from(clamp(qi, 0, 127))?;
        qi = qi_base + i16::from(frame_hdr.quant_indices().y_dc_delta);
        quantization_index[1] = u16::try_from(clamp(qi, 0, 127))?;
        qi = qi_base + i16::from(frame_hdr.quant_indices().y2_dc_delta);
        quantization_index[2] = u16::try_from(clamp(qi, 0, 127))?;
        qi = qi_base + i16::from(frame_hdr.quant_indices().y2_ac_delta);
        quantization_index[3] = u16::try_from(clamp(qi, 0, 127))?;
        qi = qi_base + i16::from(frame_hdr.quant_indices().uv_dc_delta);
        quantization_index[4] = u16::try_from(clamp(qi, 0, 127))?;
        qi = qi_base + i16::from(frame_hdr.quant_indices().uv_ac_delta);
        quantization_index[5] = u16::try_from(clamp(qi, 0, 127))?;
    }

    Ok(BufferType::IQMatrix(IQMatrix::VP8(IQMatrixBufferVP8::new(
        quantization_index,
    ))))
}

fn build_probability_table(frame_hdr: &Header) -> libva::BufferType {
    BufferType::Probability(ProbabilityDataBufferVP8::new(frame_hdr.coeff_prob()))
}

fn build_pic_param(
    frame_hdr: &Header,
    resolution: &Resolution,
    seg: &Segmentation,
    adj: &MbLfAdjustments,
    last: u32,
    golden: u32,
    alt: u32,
) -> anyhow::Result<libva::BufferType> {
    let mut loop_filter_level: [u8; 4] = Default::default();
    let mut loop_filter_deltas_ref_frame: [i8; 4] = Default::default();
    let mut loop_filter_deltas_mode: [i8; 4] = Default::default();

    for i in 0..4 {
        let mut level;
        if seg.segmentation_enabled {
            level = seg.lf_update_value[i];
            if !seg.segment_feature_mode {
                level += i8::try_from(frame_hdr.loop_filter_level())?;
            }
        } else {
            level = i8::try_from(frame_hdr.loop_filter_level())?;
        }

        loop_filter_level[i] = clamp(u8::try_from(level)?, 0, 63);
        loop_filter_deltas_ref_frame[i] = adj.ref_frame_delta[i];
        loop_filter_deltas_mode[i] = adj.mb_mode_delta[i];
    }

    let pic_fields = libva::VP8PicFields::new(
        u32::from(!frame_hdr.key_frame()),
        u32::from(frame_hdr.version()),
        u32::from(seg.segmentation_enabled),
        u32::from(seg.update_mb_segmentation_map),
        u32::from(seg.update_segment_feature_data),
        u32::from(frame_hdr.filter_type()),
        u32::from(frame_hdr.sharpness_level()),
        u32::from(adj.loop_filter_adj_enable),
        u32::from(adj.mode_ref_lf_delta_update),
        u32::from(frame_hdr.sign_bias_golden()),
        u32::from(frame_hdr.sign_bias_alternate()),
        u32::from(frame_hdr.mb_no_coeff_skip()),
        u32::from(frame_hdr.loop_filter_level() == 0),
    );

    let bool_coder_ctx = libva::BoolCoderContextVPX::new(
        u8::try_from(frame_hdr.bd_range())?,
        u8::try_from(frame_hdr.bd_value())?,
        u8::try_from(frame_hdr.bd_count())?,
    );

    let pic_param = libva::PictureParameterBufferVP8::new(
        resolution.width,
        resolution.height,
        last,
        golden,
        alt,
        &pic_fields,
        seg.segment_prob,
        loop_filter_level,
        loop_filter_deltas_ref_frame,
        loop_filter_deltas_mode,
        frame_hdr.prob_skip_false(),
        frame_hdr.prob_intra(),
        frame_hdr.prob_last(),
        frame_hdr.prob_golden(),
        frame_hdr.mode_probs().intra_16x16_prob,
        frame_hdr.mode_probs().intra_chroma_prob,
        frame_hdr.mv_prob(),
        &bool_coder_ctx,
    );

    Ok(libva::BufferType::PictureParameter(
        libva::PictureParameter::VP8(pic_param),
    ))
}

fn build_slice_param(frame_hdr: &Header, slice_size: usize) -> anyhow::Result<libva::BufferType> {
    let mut partition_size: [u32; 9] = Default::default();
    let num_of_partitions = (1 << frame_hdr.log2_nbr_of_dct_partitions()) + 1;

    partition_size[0] = frame_hdr.first_part_size() - ((frame_hdr.header_size() + 7) >> 3);

    partition_size[1..num_of_partitions]
        .clone_from_slice(&frame_hdr.partition_size()[..(num_of_partitions - 1)]);

    Ok(libva::BufferType::SliceParameter(
        libva::SliceParameter::VP8(libva::SliceParameterBufferVP8::new(
            u32::try_from(slice_size)?,
            u32::from(frame_hdr.data_chunk_size()),
            0,
            frame_hdr.header_size(),
            u8::try_from(num_of_partitions)?,
            partition_size,
        )),
    ))
}

impl<M: SurfaceMemoryDescriptor> StatelessVp8DecoderBackend<M> for VaapiBackend<Header, M> {
    fn new_sequence(&mut self, header: &Header) -> StatelessBackendResult<()> {
        self.new_sequence(header)
    }

    fn submit_picture(
        &mut self,
        picture: &Header,
        last_ref: Option<&Self::Handle>,
        golden_ref: Option<&Self::Handle>,
        alt_ref: Option<&Self::Handle>,
        bitstream: &[u8],
        segmentation: &Segmentation,
        mb_lf_adjust: &MbLfAdjustments,
        timestamp: u64,
    ) -> StatelessBackendResult<Self::Handle> {
        let last_ref = if let Some(last_ref) = last_ref {
            last_ref.borrow().surface_id()
        } else {
            libva::constants::VA_INVALID_SURFACE
        };

        let golden_ref = if let Some(golden_ref) = golden_ref {
            golden_ref.borrow().surface_id()
        } else {
            libva::constants::VA_INVALID_SURFACE
        };

        let alt_ref = if let Some(alt_ref) = alt_ref {
            alt_ref.borrow().surface_id()
        } else {
            libva::constants::VA_INVALID_SURFACE
        };

        let metadata = self.metadata_state.get_parsed_mut()?;
        let context = &metadata.context;
        let coded_resolution = self.surface_pool.borrow().coded_resolution();

        let iq_buffer = context
            .create_buffer(build_iq_matrix(picture, segmentation)?)
            .context("while creating IQ matrix buffer")?;

        let probs = context
            .create_buffer(build_probability_table(picture))
            .context("while creating probability table buffer")?;

        let pic_param = context
            .create_buffer(build_pic_param(
                picture,
                &coded_resolution,
                segmentation,
                mb_lf_adjust,
                last_ref,
                golden_ref,
                alt_ref,
            )?)
            .context("while creating pic params buffer")?;

        let slice_param = context
            .create_buffer(build_slice_param(picture, bitstream.len())?)
            .context("while creating slice params buffer")?;

        let slice_data = context
            .create_buffer(libva::BufferType::SliceData(Vec::from(bitstream)))
            .context("while creating slice data buffer")?;

        let surface = self
            .surface_pool
            .borrow_mut()
            .get_surface(&self.surface_pool)
            .ok_or(StatelessBackendError::OutOfResources)?;

        let mut va_picture = VaPicture::new(timestamp, Rc::clone(context), surface);

        // Add buffers with the parsed data.
        va_picture.add_buffer(iq_buffer);
        va_picture.add_buffer(probs);
        va_picture.add_buffer(pic_param);
        va_picture.add_buffer(slice_param);
        va_picture.add_buffer(slice_data);

        self.process_picture(va_picture)
    }
}

impl<M: SurfaceMemoryDescriptor + 'static> Decoder<VADecodedHandle<M>, M> {
    // Creates a new instance of the decoder using the VAAPI backend.
    pub fn new_vaapi<S>(display: Rc<Display>, blocking_mode: BlockingMode) -> anyhow::Result<Self>
    where
        M: From<S>,
        S: From<M>,
    {
        Self::new(
            Box::new(VaapiBackend::<Header, M>::new(display)),
            blocking_mode,
        )
    }
}

#[cfg(test)]
mod tests {
    use libva::BufferType;
    use libva::Display;
    use libva::IQMatrix;
    use libva::PictureParameter;
    use libva::SliceParameter;

    use crate::codec::vp8::parser::Parser;
    use crate::decoder::stateless::tests::test_decode_stream;
    use crate::decoder::stateless::tests::TestStream;
    use crate::decoder::stateless::vp8::Decoder;
    use crate::decoder::BlockingMode;
    use crate::utils::simple_playback_loop;
    use crate::utils::simple_playback_loop_owned_surfaces;
    use crate::utils::IvfIterator;
    use crate::DecodedFormat;
    use crate::Resolution;

    use super::*;

    /// Run `test` using the vaapi decoder, in both blocking and non-blocking modes.
    fn test_decoder_vaapi(
        test: &TestStream,
        output_format: DecodedFormat,
        blocking_mode: BlockingMode,
    ) {
        let display = Display::open().unwrap();
        let decoder = Decoder::new_vaapi::<()>(display, blocking_mode).unwrap();

        test_decode_stream(
            |d, s, c| {
                simple_playback_loop(
                    d,
                    IvfIterator::new(s),
                    c,
                    &mut simple_playback_loop_owned_surfaces,
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
        use crate::decoder::stateless::vp8::tests::DECODE_TEST_25FPS;
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
        use crate::decoder::stateless::vp8::tests::DECODE_TEST_25FPS;
        test_decoder_vaapi(
            &DECODE_TEST_25FPS,
            DecodedFormat::NV12,
            BlockingMode::NonBlocking,
        );
    }

    #[test]
    /// Check that we are able to build the VA picture parameters from the stream properly.
    fn build_pic_params() {
        const TEST_STREAM: &[u8] = include_bytes!("../../../codec/vp8/test_data/test-25fps.vp8");
        const TEST_25_FPS_VP8_STREAM_PROBABILITY_TABLE_0: &[u8] =
            include_bytes!("../../../codec/vp8/test_data/test-25fps-vp8-probability-table-0.bin");
        const TEST_25_FPS_VP8_STREAM_PROBABILITY_TABLE_1: &[u8] =
            include_bytes!("../../../codec/vp8/test_data/test-25fps-vp8-probability-table-1.bin");
        const TEST_25_FPS_VP8_STREAM_PROBABILITY_TABLE_2: &[u8] =
            include_bytes!("../../../codec/vp8/test_data/test-25fps-vp8-probability-table-2.bin");

        let mut parser: Parser = Default::default();
        let mut ivf_iter = IvfIterator::new(TEST_STREAM);

        // FRAME 0

        let packet = ivf_iter.next().unwrap();
        let frame = parser.parse_frame(packet).unwrap();

        assert_eq!(frame.size(), 14788);

        let resolution = Resolution {
            width: frame.header.width() as u32,
            height: frame.header.height() as u32,
        };

        let pic_param = build_pic_param(
            &frame.header,
            &resolution,
            parser.segmentation(),
            parser.mb_lf_adjust(),
            libva::constants::VA_INVALID_SURFACE,
            libva::constants::VA_INVALID_SURFACE,
            libva::constants::VA_INVALID_SURFACE,
        )
        .unwrap();
        let pic_param = match pic_param {
            BufferType::PictureParameter(PictureParameter::VP8(pic_param)) => pic_param,
            _ => panic!(),
        };

        let iq_matrix = build_iq_matrix(&frame.header, parser.segmentation()).unwrap();
        let iq_matrix = match iq_matrix {
            BufferType::IQMatrix(IQMatrix::VP8(iq_matrix)) => iq_matrix,
            _ => panic!(),
        };

        let prob_table = build_probability_table(&frame.header);
        let prob_table = match prob_table {
            BufferType::Probability(prob_table) => prob_table,
            _ => panic!(),
        };

        let slice_param = build_slice_param(&frame.header, frame.size()).unwrap();
        let slice_param = match slice_param {
            BufferType::SliceParameter(SliceParameter::VP8(slice_param)) => slice_param,
            _ => panic!(),
        };

        assert_eq!(iq_matrix.inner().quantization_index, [[4; 6]; 4]);
        for i in 0..4 {
            for j in 0..8 {
                for k in 0..3 {
                    for l in 0..11 {
                        const OFF_I: usize = 8 * 3 * 11;
                        const OFF_J: usize = 3 * 11;
                        const OFF_K: usize = 11;
                        // maybe std::transmute?
                        assert_eq!(
                            prob_table.inner().dct_coeff_probs[i][j][k][l],
                            TEST_25_FPS_VP8_STREAM_PROBABILITY_TABLE_0
                                [(i * OFF_I) + (j * OFF_J) + (k * OFF_K) + l]
                        );
                    }
                }
            }
        }

        assert_eq!(pic_param.inner().frame_width, 320);
        assert_eq!(pic_param.inner().frame_height, 240);
        assert_eq!(
            pic_param.inner().last_ref_frame,
            libva::constants::VA_INVALID_SURFACE
        );
        assert_eq!(
            pic_param.inner().golden_ref_frame,
            libva::constants::VA_INVALID_SURFACE
        );
        assert_eq!(
            pic_param.inner().alt_ref_frame,
            libva::constants::VA_INVALID_SURFACE
        );
        assert_eq!(
            pic_param.inner().out_of_loop_frame,
            libva::constants::VA_INVALID_SURFACE
        );

        // Safe because this bitfield is initialized by the decoder.
        assert_eq!(unsafe { pic_param.inner().pic_fields.value }, unsafe {
            libva::VP8PicFields::new(0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1)
                .inner()
                .value
        });

        assert_eq!(pic_param.inner().mb_segment_tree_probs, [0; 3]);
        assert_eq!(pic_param.inner().loop_filter_level, [0; 4]);
        assert_eq!(
            pic_param.inner().loop_filter_deltas_ref_frame,
            [2, 0, -2, -2]
        );
        assert_eq!(pic_param.inner().loop_filter_deltas_mode, [4, -2, 2, 4]);
        assert_eq!(pic_param.inner().prob_skip_false, 0xbe);
        assert_eq!(pic_param.inner().prob_intra, 0);
        assert_eq!(pic_param.inner().prob_last, 0);
        assert_eq!(pic_param.inner().prob_gf, 0);

        assert_eq!(pic_param.inner().y_mode_probs, [0x91, 0x9c, 0xa3, 0x80]);
        assert_eq!(pic_param.inner().uv_mode_probs, [0x8e, 0x72, 0xb7]);
        assert_eq!(
            pic_param.inner().mv_probs[0],
            [
                0xa2, 0x80, 0xe1, 0x92, 0xac, 0x93, 0xd6, 0x27, 0x9c, 0x80, 0x81, 0x84, 0x4b, 0x91,
                0xb2, 0xce, 0xef, 0xfe, 0xfe
            ]
        );
        assert_eq!(
            pic_param.inner().mv_probs[1],
            [
                0xa4, 0x80, 0xcc, 0xaa, 0x77, 0xeb, 0x8c, 0xe6, 0xe4, 0x80, 0x82, 0x82, 0x4a, 0x94,
                0xb4, 0xcb, 0xec, 0xfe, 0xfe,
            ]
        );

        assert_eq!(pic_param.inner().bool_coder_ctx.range, 0xfc);
        assert_eq!(pic_param.inner().bool_coder_ctx.value, 0x39);
        assert_eq!(pic_param.inner().bool_coder_ctx.count, 0x0);

        assert_eq!(
            slice_param.inner(),
            libva::SliceParameterBufferVP8::new(
                14788,
                10,
                0,
                3040,
                2,
                [926, 13472, 0, 0, 0, 0, 0, 0, 0],
            )
            .inner(),
        );

        // FRAME 1

        let packet = ivf_iter.next().unwrap();
        let frame = parser.parse_frame(packet).unwrap();

        assert_eq!(frame.size(), 257);

        let pic_param = build_pic_param(
            &frame.header,
            &resolution,
            parser.segmentation(),
            parser.mb_lf_adjust(),
            0,
            0,
            0,
        )
        .unwrap();
        let pic_param = match pic_param {
            BufferType::PictureParameter(PictureParameter::VP8(pic_param)) => pic_param,
            _ => panic!(),
        };

        let iq_matrix = build_iq_matrix(&frame.header, parser.segmentation()).unwrap();
        let iq_matrix = match iq_matrix {
            BufferType::IQMatrix(IQMatrix::VP8(iq_matrix)) => iq_matrix,
            _ => panic!(),
        };

        let prob_table = build_probability_table(&frame.header);
        let prob_table = match prob_table {
            BufferType::Probability(prob_table) => prob_table,
            _ => panic!(),
        };

        let slice_param = build_slice_param(&frame.header, frame.size()).unwrap();
        let slice_param = match slice_param {
            BufferType::SliceParameter(SliceParameter::VP8(slice_param)) => slice_param,
            _ => panic!(),
        };

        assert_eq!(iq_matrix.inner().quantization_index, [[0x7f; 6]; 4]);
        for i in 0..4 {
            for j in 0..8 {
                for k in 0..3 {
                    for l in 0..11 {
                        const OFF_I: usize = 8 * 3 * 11;
                        const OFF_J: usize = 3 * 11;
                        const OFF_K: usize = 11;
                        // maybe std::transmute?
                        assert_eq!(
                            prob_table.inner().dct_coeff_probs[i][j][k][l],
                            TEST_25_FPS_VP8_STREAM_PROBABILITY_TABLE_1
                                [(i * OFF_I) + (j * OFF_J) + (k * OFF_K) + l]
                        );
                    }
                }
            }
        }
        assert_eq!(pic_param.inner().frame_width, 320);
        assert_eq!(pic_param.inner().frame_height, 240);
        assert_eq!(pic_param.inner().last_ref_frame, 0);
        assert_eq!(pic_param.inner().golden_ref_frame, 0);
        assert_eq!(pic_param.inner().alt_ref_frame, 0);
        assert_eq!(
            pic_param.inner().out_of_loop_frame,
            libva::constants::VA_INVALID_SURFACE
        );

        // Safe because this bitfield is initialized by the decoder.
        assert_eq!(unsafe { pic_param.inner().pic_fields.value }, unsafe {
            libva::VP8PicFields::new(1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0)
                .inner()
                .value
        });

        assert_eq!(pic_param.inner().mb_segment_tree_probs, [0; 3]);
        assert_eq!(pic_param.inner().loop_filter_level, [44; 4]);
        assert_eq!(
            pic_param.inner().loop_filter_deltas_ref_frame,
            [2, 0, -2, -2]
        );
        assert_eq!(pic_param.inner().loop_filter_deltas_mode, [4, -2, 2, 4]);
        assert_eq!(pic_param.inner().prob_skip_false, 0x11);
        assert_eq!(pic_param.inner().prob_intra, 0x2a);
        assert_eq!(pic_param.inner().prob_last, 0xff);
        assert_eq!(pic_param.inner().prob_gf, 0x80);

        assert_eq!(pic_param.inner().y_mode_probs, [0x70, 0x56, 0x8c, 0x25]);
        assert_eq!(pic_param.inner().uv_mode_probs, [0xa2, 0x65, 0xcc]);
        assert_eq!(
            pic_param.inner().mv_probs[0],
            [
                0xa2, 0x80, 0xe1, 0x92, 0xac, 0x93, 0xd6, 0x27, 0x9c, 0x80, 0x81, 0x84, 0x4b, 0x91,
                0xb2, 0xce, 0xef, 0xfe, 0xfe,
            ]
        );
        assert_eq!(
            pic_param.inner().mv_probs[1],
            [
                0xa4, 0x80, 0xcc, 0xaa, 0x77, 0xeb, 0x8c, 0xe6, 0xe4, 0x80, 0x82, 0x82, 0x4a, 0x94,
                0xb4, 0xcb, 0xec, 0xfe, 0xfe,
            ]
        );

        assert_eq!(pic_param.inner().bool_coder_ctx.range, 0xde);
        assert_eq!(pic_param.inner().bool_coder_ctx.value, 0x39);
        assert_eq!(pic_param.inner().bool_coder_ctx.count, 0x7);

        assert_eq!(
            slice_param.inner(),
            libva::SliceParameterBufferVP8::new(257, 3, 0, 129, 2, [143, 94, 0, 0, 0, 0, 0, 0, 0],)
                .inner()
        );

        // FRAME 2

        let packet = ivf_iter.next().unwrap();
        let frame = parser.parse_frame(packet).unwrap();

        assert_eq!(frame.size(), 131);

        let pic_param = build_pic_param(
            &frame.header,
            &resolution,
            parser.segmentation(),
            parser.mb_lf_adjust(),
            1,
            0,
            0,
        )
        .unwrap();
        let pic_param = match pic_param {
            BufferType::PictureParameter(PictureParameter::VP8(pic_param)) => pic_param,
            _ => panic!(),
        };

        let iq_matrix = build_iq_matrix(&frame.header, parser.segmentation()).unwrap();
        let iq_matrix = match iq_matrix {
            BufferType::IQMatrix(IQMatrix::VP8(iq_matrix)) => iq_matrix,
            _ => panic!(),
        };

        let prob_table = build_probability_table(&frame.header);
        let prob_table = match prob_table {
            BufferType::Probability(prob_table) => prob_table,
            _ => panic!(),
        };

        let slice_param = build_slice_param(&frame.header, frame.size()).unwrap();
        let slice_param = match slice_param {
            BufferType::SliceParameter(SliceParameter::VP8(slice_param)) => slice_param,
            _ => panic!(),
        };

        assert_eq!(iq_matrix.inner().quantization_index, [[0x7f; 6]; 4]);
        for i in 0..4 {
            for j in 0..8 {
                for k in 0..3 {
                    for l in 0..11 {
                        const OFF_I: usize = 8 * 3 * 11;
                        const OFF_J: usize = 3 * 11;
                        const OFF_K: usize = 11;
                        // maybe std::transmute?
                        assert_eq!(
                            prob_table.inner().dct_coeff_probs[i][j][k][l],
                            TEST_25_FPS_VP8_STREAM_PROBABILITY_TABLE_2
                                [(i * OFF_I) + (j * OFF_J) + (k * OFF_K) + l]
                        );
                    }
                }
            }
        }
        assert_eq!(pic_param.inner().frame_width, 320);
        assert_eq!(pic_param.inner().frame_height, 240);
        assert_eq!(pic_param.inner().last_ref_frame, 1);
        assert_eq!(pic_param.inner().golden_ref_frame, 0);
        assert_eq!(pic_param.inner().alt_ref_frame, 0);
        assert_eq!(
            pic_param.inner().out_of_loop_frame,
            libva::constants::VA_INVALID_SURFACE
        );

        // Safe because this bitfield is initialized by the decoder.
        assert_eq!(unsafe { pic_param.inner().pic_fields.value }, unsafe {
            libva::VP8PicFields::new(1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0)
                .inner()
                .value
        });

        assert_eq!(pic_param.inner().mb_segment_tree_probs, [0; 3]);
        assert_eq!(pic_param.inner().loop_filter_level, [28; 4]);
        assert_eq!(
            pic_param.inner().loop_filter_deltas_ref_frame,
            [2, 0, -2, -2]
        );
        assert_eq!(pic_param.inner().loop_filter_deltas_mode, [4, -2, 2, 4]);
        assert_eq!(pic_param.inner().prob_skip_false, 0x6);
        assert_eq!(pic_param.inner().prob_intra, 0x1);
        assert_eq!(pic_param.inner().prob_last, 0xf8);
        assert_eq!(pic_param.inner().prob_gf, 0xff);

        assert_eq!(pic_param.inner().y_mode_probs, [0x70, 0x56, 0x8c, 0x25]);
        assert_eq!(pic_param.inner().uv_mode_probs, [0xa2, 0x65, 0xcc]);
        assert_eq!(
            pic_param.inner().mv_probs[0],
            [
                0xa2, 0x80, 0xe1, 0x92, 0xac, 0x93, 0xd6, 0x27, 0x9c, 0x80, 0x81, 0x84, 0x4b, 0x91,
                0xb2, 0xce, 0xef, 0xfe, 0xfe,
            ]
        );
        assert_eq!(
            pic_param.inner().mv_probs[1],
            [
                0xa4, 0x80, 0xcc, 0xaa, 0x77, 0xeb, 0x8c, 0xe6, 0xe4, 0x80, 0x82, 0x82, 0x4a, 0x94,
                0xb4, 0xcb, 0xec, 0xfe, 0xfe,
            ]
        );

        assert_eq!(pic_param.inner().bool_coder_ctx.range, 0xb1);
        assert_eq!(pic_param.inner().bool_coder_ctx.value, 0xd);
        assert_eq!(pic_param.inner().bool_coder_ctx.count, 0x2);

        assert_eq!(
            slice_param.inner(),
            libva::SliceParameterBufferVP8::new(131, 3, 0, 86, 2, [66, 51, 0, 0, 0, 0, 0, 0, 0],)
                .inner()
        );
    }
}
