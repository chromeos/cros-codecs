// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use crate::codec::vp8::parser::Header;
use crate::codec::vp8::parser::MbLfAdjustments;
use crate::codec::vp8::parser::Segmentation;

use v4l2r::bindings::v4l2_ctrl_vp8_frame;
use v4l2r::bindings::V4L2_VP8_COEFF_PROB_CNT;
use v4l2r::bindings::V4L2_VP8_FRAME_FLAG_EXPERIMENTAL;
use v4l2r::bindings::V4L2_VP8_FRAME_FLAG_KEY_FRAME;
use v4l2r::bindings::V4L2_VP8_FRAME_FLAG_MB_NO_SKIP_COEFF;
use v4l2r::bindings::V4L2_VP8_FRAME_FLAG_SHOW_FRAME;
use v4l2r::bindings::V4L2_VP8_FRAME_FLAG_SIGN_BIAS_ALT;
use v4l2r::bindings::V4L2_VP8_FRAME_FLAG_SIGN_BIAS_GOLDEN;
use v4l2r::bindings::V4L2_VP8_LF_ADJ_ENABLE;
use v4l2r::bindings::V4L2_VP8_LF_DELTA_UPDATE;
use v4l2r::bindings::V4L2_VP8_LF_FILTER_TYPE_SIMPLE;
use v4l2r::bindings::V4L2_VP8_MV_PROB_CNT;
use v4l2r::bindings::V4L2_VP8_SEGMENT_FLAG_DELTA_VALUE_MODE;
use v4l2r::bindings::V4L2_VP8_SEGMENT_FLAG_ENABLED;
use v4l2r::bindings::V4L2_VP8_SEGMENT_FLAG_UPDATE_FEATURE_DATA;
use v4l2r::bindings::V4L2_VP8_SEGMENT_FLAG_UPDATE_MAP;

use v4l2r::controls::codec::Vp8Frame;
use v4l2r::controls::SafeExtControl;

#[derive(Default)]
pub struct V4l2CtrlVp8FrameParams {
    handle: v4l2_ctrl_vp8_frame,
}

impl V4l2CtrlVp8FrameParams {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn set_loop_filter_params(
        &mut self,
        hdr: &Header,
        mb_lf_adjust: &MbLfAdjustments,
    ) -> &mut Self {
        self.handle.lf.sharpness_level = hdr.sharpness_level;
        self.handle.lf.level = hdr.loop_filter_level;

        let mut flags: u32 = 0;
        if hdr.filter_type {
            flags |= V4L2_VP8_LF_FILTER_TYPE_SIMPLE;
        }
        if mb_lf_adjust.loop_filter_adj_enable {
            flags |= V4L2_VP8_LF_ADJ_ENABLE;
        }
        if mb_lf_adjust.mode_ref_lf_delta_update {
            flags |= V4L2_VP8_LF_DELTA_UPDATE;
        }
        self.handle.lf.flags = flags;

        for i in 0..4 {
            self.handle.lf.ref_frm_delta[i] = mb_lf_adjust.ref_frame_delta[i];
            self.handle.lf.mb_mode_delta[i] = mb_lf_adjust.mb_mode_delta[i];
        }

        self
    }

    pub fn set_quantization_params(&mut self, hdr: &Header) -> &mut Self {
        self.handle.quant.y_ac_qi =
            u8::try_from(hdr.quant_indices.y_ac_qi).expect("Value out of range for u8");

        self.handle.quant.y_dc_delta =
            i8::try_from(hdr.quant_indices.y_dc_delta).expect("Value out of range for u8");
        self.handle.quant.y2_dc_delta =
            i8::try_from(hdr.quant_indices.y2_dc_delta).expect("Value out of range for u8");
        self.handle.quant.y2_ac_delta =
            i8::try_from(hdr.quant_indices.y2_ac_delta).expect("Value out of range for u8");
        self.handle.quant.uv_dc_delta =
            i8::try_from(hdr.quant_indices.uv_dc_delta).expect("Value out of range for u8");
        self.handle.quant.uv_ac_delta =
            i8::try_from(hdr.quant_indices.uv_ac_delta).expect("Value out of range for u8");
        self
    }

    pub fn set_segmentation_params(&mut self, segmentation: &Segmentation) -> &mut Self {
        let mut flags: u32 = 0;

        if segmentation.segmentation_enabled {
            flags |= V4L2_VP8_SEGMENT_FLAG_ENABLED;
        }
        if segmentation.update_mb_segmentation_map {
            flags |= V4L2_VP8_SEGMENT_FLAG_UPDATE_MAP;
        }
        if segmentation.update_segment_feature_data {
            flags |= V4L2_VP8_SEGMENT_FLAG_UPDATE_FEATURE_DATA;
        }
        if segmentation.segment_feature_mode == false {
            flags |= V4L2_VP8_SEGMENT_FLAG_DELTA_VALUE_MODE;
        }
        self.handle.segment.flags = flags;

        for i in 0..4 {
            self.handle.segment.quant_update[i] = segmentation.quantizer_update_value[i];
            self.handle.segment.lf_update[i] = segmentation.lf_update_value[i];
        }

        for i in 0..3 {
            self.handle.segment.segment_probs[i] = segmentation.segment_prob[i];
        }

        self.handle.segment.padding = 0;

        self
    }

    pub fn set_entropy_params(&mut self, hdr: &Header) -> &mut Self {
        self.handle.entropy.coeff_probs = hdr.coeff_prob;
        self.handle.entropy.y_mode_probs = hdr.mode_probs.intra_16x16_prob;
        self.handle.entropy.uv_mode_probs = hdr.mode_probs.intra_chroma_prob;
        self.handle.entropy.mv_probs = hdr.mv_prob;
        self
    }
}

impl From<&V4l2CtrlVp8FrameParams> for SafeExtControl<Vp8Frame> {
    fn from(decode_params: &V4l2CtrlVp8FrameParams) -> Self {
        SafeExtControl::<Vp8Frame>::from(decode_params.handle)
    }
}
