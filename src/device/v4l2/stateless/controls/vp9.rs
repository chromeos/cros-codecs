// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::marker::PhantomData;

use crate::codec::vp9::parser::ColorRange;
use crate::codec::vp9::parser::FrameType;
use crate::codec::vp9::parser::Header;
use crate::codec::vp9::parser::LoopFilterParams;
use crate::codec::vp9::parser::QuantizationParams;
use crate::codec::vp9::parser::SegmentationParams;
use crate::codec::vp9::parser::MAX_SEGMENTS;
use crate::codec::vp9::parser::SEG_LVL_MAX;

use v4l2r::bindings::v4l2_ctrl_vp9_frame;
use v4l2r::bindings::v4l2_ext_control;
use v4l2r::bindings::v4l2_ext_control__bindgen_ty_1;
use v4l2r::bindings::v4l2_vp9_loop_filter;
use v4l2r::bindings::v4l2_vp9_quantization;
use v4l2r::bindings::v4l2_vp9_segmentation;
use v4l2r::bindings::V4L2_CID_STATELESS_VP9_FRAME;
use v4l2r::bindings::V4L2_VP9_FRAME_FLAG_ALLOW_HIGH_PREC_MV;
use v4l2r::bindings::V4L2_VP9_FRAME_FLAG_COLOR_RANGE_FULL_SWING;
use v4l2r::bindings::V4L2_VP9_FRAME_FLAG_ERROR_RESILIENT;
use v4l2r::bindings::V4L2_VP9_FRAME_FLAG_INTRA_ONLY;
use v4l2r::bindings::V4L2_VP9_FRAME_FLAG_KEY_FRAME;
use v4l2r::bindings::V4L2_VP9_FRAME_FLAG_PARALLEL_DEC_MODE;
use v4l2r::bindings::V4L2_VP9_FRAME_FLAG_REFRESH_FRAME_CTX;
use v4l2r::bindings::V4L2_VP9_FRAME_FLAG_SHOW_FRAME;
use v4l2r::bindings::V4L2_VP9_FRAME_FLAG_X_SUBSAMPLING;
use v4l2r::bindings::V4L2_VP9_FRAME_FLAG_Y_SUBSAMPLING;
use v4l2r::bindings::V4L2_VP9_LOOP_FILTER_FLAG_DELTA_ENABLED;
use v4l2r::bindings::V4L2_VP9_LOOP_FILTER_FLAG_DELTA_UPDATE;
use v4l2r::bindings::V4L2_VP9_REFERENCE_MODE_SINGLE_REFERENCE;
use v4l2r::bindings::V4L2_VP9_SEGMENTATION_FLAG_ABS_OR_DELTA_UPDATE;
use v4l2r::bindings::V4L2_VP9_SEGMENTATION_FLAG_ENABLED;
use v4l2r::bindings::V4L2_VP9_SEGMENTATION_FLAG_TEMPORAL_UPDATE;
use v4l2r::bindings::V4L2_VP9_SEGMENTATION_FLAG_UPDATE_DATA;
use v4l2r::bindings::V4L2_VP9_SEGMENTATION_FLAG_UPDATE_MAP;
use v4l2r::controls::AsV4l2ControlSlice;

impl From<&LoopFilterParams> for v4l2_vp9_loop_filter {
    fn from(lf: &LoopFilterParams) -> Self {
        let mut ret = Self {
            level: lf.level,
            sharpness: lf.sharpness,
            flags: 0,
            ..Default::default()
        };

        if lf.delta_enabled {
            ret.flags |= V4L2_VP9_LOOP_FILTER_FLAG_DELTA_ENABLED as u8;
        }
        if lf.delta_update {
            ret.flags |= V4L2_VP9_LOOP_FILTER_FLAG_DELTA_UPDATE as u8;
        }

        ret.ref_deltas.clone_from_slice(&lf.ref_deltas);
        ret.mode_deltas.clone_from_slice(&lf.mode_deltas);

        ret
    }
}

impl From<&QuantizationParams> for v4l2_vp9_quantization {
    fn from(quant: &QuantizationParams) -> Self {
        Self {
            base_q_idx: quant.base_q_idx,
            delta_q_y_dc: quant.delta_q_y_dc,
            delta_q_uv_dc: quant.delta_q_uv_dc,
            delta_q_uv_ac: quant.delta_q_uv_ac,
            ..Default::default()
        }
    }
}

impl From<&SegmentationParams> for v4l2_vp9_segmentation {
    fn from(seg_params: &SegmentationParams) -> Self {
        let mut ret = Self {
            ..Default::default()
        };

        if seg_params.enabled {
            ret.flags |= V4L2_VP9_SEGMENTATION_FLAG_ENABLED as u8;
        }
        if seg_params.update_map {
            ret.flags |= V4L2_VP9_SEGMENTATION_FLAG_UPDATE_MAP as u8;
        }
        if seg_params.temporal_update {
            ret.flags |= V4L2_VP9_SEGMENTATION_FLAG_TEMPORAL_UPDATE as u8;
        }
        if seg_params.update_data {
            ret.flags |= V4L2_VP9_SEGMENTATION_FLAG_UPDATE_DATA as u8;
        }
        if seg_params.abs_or_delta_update {
            ret.flags |= V4L2_VP9_SEGMENTATION_FLAG_ABS_OR_DELTA_UPDATE as u8;
        }

        for i in 0..MAX_SEGMENTS {
            ret.feature_data[i].clone_from_slice(&seg_params.feature_data[i]);
            let mut feature_enabled = 0u8;
            for j in 0..SEG_LVL_MAX {
                feature_enabled |= (seg_params.feature_enabled[i][j] as u8) << j;
            }
            ret.feature_enabled[i] = feature_enabled;
        }

        ret.tree_probs.clone_from_slice(&seg_params.tree_probs);
        ret.pred_probs.clone_from_slice(&seg_params.pred_probs);

        ret
    }
}

impl From<&Header> for v4l2_ctrl_vp9_frame {
    fn from(hdr: &Header) -> Self {
        let mut ret = Self {
            lf: v4l2_vp9_loop_filter::from(&hdr.lf),
            quant: v4l2_vp9_quantization::from(&hdr.quant),
            seg: v4l2_vp9_segmentation::from(&hdr.seg),
            compressed_header_size: hdr.header_size_in_bytes,
            uncompressed_header_size: hdr.uncompressed_header_size_in_bytes,
            frame_width_minus_1: (hdr.width - 1) as u16,
            frame_height_minus_1: (hdr.height - 1) as u16,
            render_width_minus_1: (hdr.render_width - 1) as u16,
            render_height_minus_1: (hdr.render_height - 1) as u16,
            reset_frame_context: hdr.reset_frame_context,
            frame_context_idx: hdr.frame_context_idx,
            profile: hdr.profile as u8,
            bit_depth: hdr.bit_depth as u8,
            interpolation_filter: hdr.interpolation_filter as u8,
            tile_cols_log2: hdr.tile_cols_log2,
            tile_rows_log2: hdr.tile_rows_log2,
            ..Default::default()
        };

        // The MTK V4L2 stateless driver ignores the reference_mode field current.
        // TODO: Implement this properly for future drivers?
        ret.reference_mode = V4L2_VP9_REFERENCE_MODE_SINGLE_REFERENCE as u8;

        for i in 0..4 {
            ret.ref_frame_sign_bias |= ((hdr.ref_frame_sign_bias[i] != 0) as u8) << i;
        }

        if hdr.frame_type == FrameType::KeyFrame {
            ret.flags |= V4L2_VP9_FRAME_FLAG_KEY_FRAME;
        }
        if hdr.show_frame {
            ret.flags |= V4L2_VP9_FRAME_FLAG_SHOW_FRAME;
        }
        if hdr.error_resilient_mode {
            ret.flags |= V4L2_VP9_FRAME_FLAG_ERROR_RESILIENT;
        }
        if hdr.intra_only {
            ret.flags |= V4L2_VP9_FRAME_FLAG_INTRA_ONLY;
        }
        if hdr.allow_high_precision_mv {
            ret.flags |= V4L2_VP9_FRAME_FLAG_ALLOW_HIGH_PREC_MV;
        }
        if hdr.refresh_frame_context {
            ret.flags |= V4L2_VP9_FRAME_FLAG_REFRESH_FRAME_CTX;
        }
        if hdr.frame_parallel_decoding_mode {
            ret.flags |= V4L2_VP9_FRAME_FLAG_PARALLEL_DEC_MODE;
        }
        if hdr.subsampling_x {
            ret.flags |= V4L2_VP9_FRAME_FLAG_X_SUBSAMPLING;
        }
        if hdr.subsampling_y {
            ret.flags |= V4L2_VP9_FRAME_FLAG_Y_SUBSAMPLING;
        }
        if hdr.color_range == ColorRange::FullSwing {
            ret.flags |= V4L2_VP9_FRAME_FLAG_COLOR_RANGE_FULL_SWING;
        }

        ret
    }
}

// Cargo-culted from v4l2r's control.rs file. v4l2r does not currently support
// VP9 controls, so we work around the issue by making our own "control" struct
// and just implementing the AsV4l2ControlSlice trait.
pub struct Vp9V4l2Control(v4l2_ext_control, PhantomData<v4l2_ctrl_vp9_frame>);

impl From<&Header> for Vp9V4l2Control {
    fn from(hdr: &Header) -> Self {
        let payload = Box::new(v4l2_ctrl_vp9_frame::from(hdr));

        Self(
            v4l2_ext_control {
                id: V4L2_CID_STATELESS_VP9_FRAME,
                size: std::mem::size_of::<v4l2_ctrl_vp9_frame>() as u32,
                __bindgen_anon_1: v4l2_ext_control__bindgen_ty_1 {
                    p_vp9_frame: Box::into_raw(payload),
                },
                ..Default::default()
            },
            PhantomData,
        )
    }
}

impl AsV4l2ControlSlice for &mut Vp9V4l2Control {
    fn as_v4l2_control_slice(&mut self) -> &mut [v4l2_ext_control] {
         std::slice::from_mut(&mut self.0)
    }
}

impl Drop for Vp9V4l2Control {
    fn drop(&mut self) {
        // Invariant: p_vp9_frame contains a pointer to a non-NULL v4l2_ctrl_vp9_frame object.
        unsafe {
            let _ = Box::from_raw(self.0.__bindgen_anon_1.p_vp9_frame);
        }
    }
}
