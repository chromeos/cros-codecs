// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::marker::PhantomData;

use v4l2r::bindings::v4l2_ctrl_vp9_frame;
use v4l2r::bindings::v4l2_ext_control;
use v4l2r::bindings::v4l2_ext_control__bindgen_ty_1;
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

use crate::codec::vp9::parser::ColorRange;
use crate::codec::vp9::parser::FrameType;
use crate::codec::vp9::parser::Header;
use crate::codec::vp9::parser::LAST_FRAME;
use crate::codec::vp9::parser::MAX_REF_FRAMES;
use crate::codec::vp9::parser::MAX_SEGMENTS;
use crate::codec::vp9::parser::SEG_LVL_MAX;

#[derive(Default)]
pub struct V4l2CtrlVp9FrameParams {
    handle: v4l2_ctrl_vp9_frame,
}

impl V4l2CtrlVp9FrameParams {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn set_loop_filter_params(&mut self, hdr: &Header) -> &mut Self {
        self.handle.lf.level = hdr.lf.level;
        self.handle.lf.sharpness = hdr.lf.sharpness;
        self.handle.lf.flags = 0;

        if hdr.lf.delta_enabled {
            self.handle.lf.flags |= V4L2_VP9_LOOP_FILTER_FLAG_DELTA_ENABLED as u8;
        }
        if hdr.lf.delta_update {
            self.handle.lf.flags |= V4L2_VP9_LOOP_FILTER_FLAG_DELTA_UPDATE as u8;
        }

        self.handle.lf.ref_deltas.clone_from_slice(&hdr.lf.ref_deltas);
        self.handle.lf.mode_deltas.clone_from_slice(&hdr.lf.mode_deltas);

        self
    }

    pub fn set_quantization_params(&mut self, hdr: &Header) -> &mut Self {
        self.handle.quant.base_q_idx = hdr.quant.base_q_idx;
        self.handle.quant.delta_q_y_dc = hdr.quant.delta_q_y_dc;
        self.handle.quant.delta_q_uv_dc = hdr.quant.delta_q_uv_dc;
        self.handle.quant.delta_q_uv_ac = hdr.quant.delta_q_uv_ac;
        self
    }

    pub fn set_segmentation_params(&mut self, hdr: &Header) -> &mut Self {
        if hdr.seg.enabled {
            self.handle.seg.flags |= V4L2_VP9_SEGMENTATION_FLAG_ENABLED as u8;
        }
        if hdr.seg.update_map {
            self.handle.seg.flags |= V4L2_VP9_SEGMENTATION_FLAG_UPDATE_MAP as u8;
        }
        if hdr.seg.temporal_update {
            self.handle.seg.flags |= V4L2_VP9_SEGMENTATION_FLAG_TEMPORAL_UPDATE as u8;
        }
        if hdr.seg.update_data {
            self.handle.seg.flags |= V4L2_VP9_SEGMENTATION_FLAG_UPDATE_DATA as u8;
        }
        if hdr.seg.abs_or_delta_update {
            self.handle.seg.flags |= V4L2_VP9_SEGMENTATION_FLAG_ABS_OR_DELTA_UPDATE as u8;
        }

        for i in 0..MAX_SEGMENTS {
            self.handle.seg.feature_data[i].clone_from_slice(&hdr.seg.feature_data[i]);
            let mut feature_enabled = 0u8;
            for j in 0..SEG_LVL_MAX {
                feature_enabled |= (hdr.seg.feature_enabled[i][j] as u8) << j;
            }
            self.handle.seg.feature_enabled[i] = feature_enabled;
        }

        self.handle.seg.tree_probs.clone_from_slice(&hdr.seg.tree_probs);
        self.handle.seg.pred_probs.clone_from_slice(&hdr.seg.pred_probs);

        self
    }

    pub fn set_frame_params(
        &mut self,
        hdr: &Header,
        last_frame_ts: u64,
        golden_frame_ts: u64,
        alt_frame_ts: u64,
    ) -> &mut Self {
        self.handle.compressed_header_size = hdr.header_size_in_bytes;
        self.handle.uncompressed_header_size = hdr.uncompressed_header_size_in_bytes;
        self.handle.frame_width_minus_1 = (hdr.width - 1) as u16;
        self.handle.frame_height_minus_1 = (hdr.height - 1) as u16;
        self.handle.render_width_minus_1 = (hdr.render_width - 1) as u16;
        self.handle.render_height_minus_1 = (hdr.render_height - 1) as u16;
        self.handle.reset_frame_context = hdr.reset_frame_context;
        self.handle.frame_context_idx = hdr.frame_context_idx;
        self.handle.profile = hdr.profile as u8;
        self.handle.bit_depth = hdr.bit_depth as u8;
        self.handle.interpolation_filter = hdr.interpolation_filter as u8;
        self.handle.tile_cols_log2 = hdr.tile_cols_log2;
        self.handle.tile_rows_log2 = hdr.tile_rows_log2;

        // The MTK V4L2 stateless driver ignores the reference_mode field current.
        // TODO: Implement this properly for future drivers?
        self.handle.reference_mode = V4L2_VP9_REFERENCE_MODE_SINGLE_REFERENCE as u8;

        for i in 0..(MAX_REF_FRAMES - LAST_FRAME) {
            self.handle.ref_frame_sign_bias |=
                ((hdr.ref_frame_sign_bias[i + LAST_FRAME] != 0) as u8) << i;
        }

        if hdr.frame_type == FrameType::KeyFrame {
            self.handle.flags |= V4L2_VP9_FRAME_FLAG_KEY_FRAME;
        }
        if hdr.show_frame {
            self.handle.flags |= V4L2_VP9_FRAME_FLAG_SHOW_FRAME;
        }
        if hdr.error_resilient_mode {
            self.handle.flags |= V4L2_VP9_FRAME_FLAG_ERROR_RESILIENT;
        }
        if hdr.intra_only {
            self.handle.flags |= V4L2_VP9_FRAME_FLAG_INTRA_ONLY;
        }
        if hdr.allow_high_precision_mv {
            self.handle.flags |= V4L2_VP9_FRAME_FLAG_ALLOW_HIGH_PREC_MV;
        }
        if hdr.refresh_frame_context {
            self.handle.flags |= V4L2_VP9_FRAME_FLAG_REFRESH_FRAME_CTX;
        }
        if hdr.frame_parallel_decoding_mode {
            self.handle.flags |= V4L2_VP9_FRAME_FLAG_PARALLEL_DEC_MODE;
        }
        if hdr.subsampling_x {
            self.handle.flags |= V4L2_VP9_FRAME_FLAG_X_SUBSAMPLING;
        }
        if hdr.subsampling_y {
            self.handle.flags |= V4L2_VP9_FRAME_FLAG_Y_SUBSAMPLING;
        }
        if hdr.color_range == ColorRange::FullSwing {
            self.handle.flags |= V4L2_VP9_FRAME_FLAG_COLOR_RANGE_FULL_SWING;
        }

        self.handle.last_frame_ts = last_frame_ts * 1000;
        self.handle.golden_frame_ts = golden_frame_ts * 1000;
        self.handle.alt_frame_ts = alt_frame_ts * 1000;

        self
    }
}

// Cargo-culted from v4l2r's control.rs file. v4l2r does not currently support
// VP9 controls, so we work around the issue by making our own "control" struct
// and just implementing the AsV4l2ControlSlice trait.
pub struct Vp9V4l2Control(v4l2_ext_control, PhantomData<v4l2_ctrl_vp9_frame>);

impl From<&V4l2CtrlVp9FrameParams> for Vp9V4l2Control {
    fn from(decode_params: &V4l2CtrlVp9FrameParams) -> Self {
        let payload = Box::new(decode_params.handle);

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
