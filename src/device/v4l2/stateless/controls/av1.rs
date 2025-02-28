// Copyright 2025 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::marker::PhantomData;

use v4l2r::bindings::v4l2_ctrl_av1_film_grain;
use v4l2r::bindings::v4l2_ctrl_av1_frame;
use v4l2r::bindings::v4l2_ctrl_av1_sequence;
use v4l2r::bindings::v4l2_ctrl_av1_tile_group_entry;
use v4l2r::bindings::v4l2_ext_control;
use v4l2r::bindings::v4l2_ext_control__bindgen_ty_1;
use v4l2r::bindings::V4L2_AV1_AR_COEFFS_SIZE;
use v4l2r::bindings::V4L2_AV1_FILM_GRAIN_FLAG_APPLY_GRAIN;
use v4l2r::bindings::V4L2_AV1_FILM_GRAIN_FLAG_CHROMA_SCALING_FROM_LUMA;
use v4l2r::bindings::V4L2_AV1_FILM_GRAIN_FLAG_CLIP_TO_RESTRICTED_RANGE;
use v4l2r::bindings::V4L2_AV1_FILM_GRAIN_FLAG_OVERLAP;
use v4l2r::bindings::V4L2_AV1_FILM_GRAIN_FLAG_UPDATE_GRAIN;
use v4l2r::bindings::V4L2_AV1_FRAME_FLAG_ALLOW_HIGH_PRECISION_MV;
use v4l2r::bindings::V4L2_AV1_FRAME_FLAG_ALLOW_INTRABC;
use v4l2r::bindings::V4L2_AV1_FRAME_FLAG_ALLOW_SCREEN_CONTENT_TOOLS;
use v4l2r::bindings::V4L2_AV1_FRAME_FLAG_ALLOW_WARPED_MOTION;
use v4l2r::bindings::V4L2_AV1_FRAME_FLAG_BUFFER_REMOVAL_TIME_PRESENT;
use v4l2r::bindings::V4L2_AV1_FRAME_FLAG_DISABLE_CDF_UPDATE;
use v4l2r::bindings::V4L2_AV1_FRAME_FLAG_DISABLE_FRAME_END_UPDATE_CDF;
use v4l2r::bindings::V4L2_AV1_FRAME_FLAG_ERROR_RESILIENT_MODE;
use v4l2r::bindings::V4L2_AV1_FRAME_FLAG_FORCE_INTEGER_MV;
use v4l2r::bindings::V4L2_AV1_FRAME_FLAG_FRAME_REFS_SHORT_SIGNALING;
use v4l2r::bindings::V4L2_AV1_FRAME_FLAG_FRAME_SIZE_OVERRIDE;
use v4l2r::bindings::V4L2_AV1_FRAME_FLAG_IS_MOTION_MODE_SWITCHABLE;
use v4l2r::bindings::V4L2_AV1_FRAME_FLAG_REDUCED_TX_SET;
use v4l2r::bindings::V4L2_AV1_FRAME_FLAG_REFERENCE_SELECT;
use v4l2r::bindings::V4L2_AV1_FRAME_FLAG_SHOWABLE_FRAME;
use v4l2r::bindings::V4L2_AV1_FRAME_FLAG_SHOW_FRAME;
use v4l2r::bindings::V4L2_AV1_FRAME_FLAG_SKIP_MODE_ALLOWED;
use v4l2r::bindings::V4L2_AV1_FRAME_FLAG_SKIP_MODE_PRESENT;
use v4l2r::bindings::V4L2_AV1_FRAME_FLAG_USE_REF_FRAME_MVS;
use v4l2r::bindings::V4L2_AV1_FRAME_FLAG_USE_SUPERRES;
use v4l2r::bindings::V4L2_AV1_GLOBAL_MOTION_FLAG_IS_GLOBAL;
use v4l2r::bindings::V4L2_AV1_GLOBAL_MOTION_FLAG_IS_ROT_ZOOM;
use v4l2r::bindings::V4L2_AV1_GLOBAL_MOTION_FLAG_IS_TRANSLATION;
use v4l2r::bindings::V4L2_AV1_LOOP_RESTORATION_FLAG_USES_CHROMA_LR;
use v4l2r::bindings::V4L2_AV1_LOOP_RESTORATION_FLAG_USES_LR;
use v4l2r::bindings::V4L2_AV1_MAX_NUM_CB_POINTS;
use v4l2r::bindings::V4L2_AV1_MAX_NUM_CR_POINTS;
use v4l2r::bindings::V4L2_AV1_MAX_NUM_Y_POINTS;
use v4l2r::bindings::V4L2_AV1_MAX_OPERATING_POINTS;
use v4l2r::bindings::V4L2_AV1_NUM_PLANES_MAX;
use v4l2r::bindings::V4L2_AV1_TOTAL_REFS_PER_FRAME;
use v4l2r::bindings::V4L2_CID_STATELESS_AV1_FILM_GRAIN;
use v4l2r::bindings::V4L2_CID_STATELESS_AV1_FRAME;
use v4l2r::controls::AsV4l2ControlSlice;

use crate::codec::av1::parser::FrameHeaderObu;
use crate::codec::av1::parser::GlobalMotionParams;
use crate::codec::av1::parser::LoopRestorationParams;

#[derive(Default)]
pub struct V4l2CtrlAv1FilmGrainParams {
    handle: v4l2_ctrl_av1_film_grain,
}

impl V4l2CtrlAv1FilmGrainParams {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn set_film_grain_params(&mut self, hdr: &FrameHeaderObu) -> &mut Self {
        let fg = &hdr.film_grain_params;

        if fg.apply_grain {
            log::warn!("Film grain is not officially supported yet.")
        }

        self.handle.flags = 0;
        if fg.apply_grain {
            self.handle.flags |= V4L2_AV1_FILM_GRAIN_FLAG_APPLY_GRAIN as u8;
        }

        if fg.update_grain {
            self.handle.flags |= V4L2_AV1_FILM_GRAIN_FLAG_UPDATE_GRAIN as u8;
        }

        if fg.chroma_scaling_from_luma {
            self.handle.flags |= V4L2_AV1_FILM_GRAIN_FLAG_CHROMA_SCALING_FROM_LUMA as u8;
        }

        if fg.overlap_flag {
            self.handle.flags |= V4L2_AV1_FILM_GRAIN_FLAG_OVERLAP as u8;
        }

        if fg.clip_to_restricted_range {
            self.handle.flags |= V4L2_AV1_FILM_GRAIN_FLAG_CLIP_TO_RESTRICTED_RANGE as u8;
        }

        self.handle.cr_mult = fg.cr_mult;
        self.handle.grain_seed = fg.grain_seed;
        self.handle.film_grain_params_ref_idx = fg.film_grain_params_ref_idx;
        self.handle.num_y_points = fg.num_y_points;
        self.handle
            .point_y_value
            .copy_from_slice(&fg.point_y_scaling[0..V4L2_AV1_MAX_NUM_Y_POINTS as usize]);
        self.handle
            .point_y_scaling
            .copy_from_slice(&fg.point_y_scaling[0..V4L2_AV1_MAX_NUM_Y_POINTS as usize]);
        self.handle.num_cb_points = fg.num_cb_points;
        self.handle
            .point_cb_value
            .copy_from_slice(&fg.point_cb_value[0..V4L2_AV1_MAX_NUM_CB_POINTS as usize]);
        self.handle
            .point_cb_scaling
            .copy_from_slice(&fg.point_cb_scaling[0..V4L2_AV1_MAX_NUM_CB_POINTS as usize]);
        self.handle.num_cr_points = fg.num_cr_points;
        self.handle
            .point_cr_value
            .copy_from_slice(&fg.point_cr_value[0..V4L2_AV1_MAX_NUM_CR_POINTS as usize]);
        self.handle
            .point_cr_scaling
            .copy_from_slice(&fg.point_cr_scaling[0..V4L2_AV1_MAX_NUM_CR_POINTS as usize]);
        self.handle.grain_scaling_minus_8 = fg.grain_scaling_minus_8;
        self.handle.ar_coeff_lag = fg.ar_coeff_lag as u8;
        self.handle
            .ar_coeffs_y_plus_128
            .copy_from_slice(&fg.ar_coeffs_y_plus_128[0..V4L2_AV1_AR_COEFFS_SIZE as usize]);
        self.handle
            .ar_coeffs_cb_plus_128
            .copy_from_slice(&fg.ar_coeffs_cb_plus_128[0..V4L2_AV1_AR_COEFFS_SIZE as usize]);
        self.handle
            .ar_coeffs_cr_plus_128
            .copy_from_slice(&fg.ar_coeffs_cr_plus_128[0..V4L2_AV1_AR_COEFFS_SIZE as usize]);
        self.handle.ar_coeff_shift_minus_6 = fg.ar_coeff_shift_minus_6;
        self.handle.grain_scale_shift = fg.grain_scale_shift;
        self.handle.cb_mult = fg.cb_mult;
        self.handle.cb_luma_mult = fg.cb_luma_mult;
        self.handle.cr_luma_mult = fg.cr_luma_mult;
        self.handle.cb_offset = fg.cb_offset;
        self.handle.cr_offset = fg.cr_offset;
        self
    }
}

#[derive(Default)]
pub struct V4l2CtrlAv1FrameParams {
    handle: v4l2_ctrl_av1_frame,
}

impl V4l2CtrlAv1FrameParams {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn set_loop_restoration_params(
        &mut self,
        loop_restoration: &LoopRestorationParams,
    ) -> &mut Self {
        if loop_restoration.uses_lr {
            self.handle.loop_restoration.flags |= V4L2_AV1_LOOP_RESTORATION_FLAG_USES_LR as u8;
        }
        if loop_restoration.uses_chroma_lr {
            self.handle.loop_restoration.flags |=
                V4L2_AV1_LOOP_RESTORATION_FLAG_USES_CHROMA_LR as u8;
        }
        self.handle.loop_restoration.lr_unit_shift = loop_restoration.lr_unit_shift;
        self.handle.loop_restoration.lr_uv_shift = loop_restoration.lr_uv_shift;
        for i in 0..V4L2_AV1_NUM_PLANES_MAX as usize {
            self.handle.loop_restoration.frame_restoration_type[i] = 1;
            self.handle.loop_restoration.loop_restoration_size[i] = 1;
        }
        self
    }

    pub fn set_global_motion_params(&mut self, global_motion: &GlobalMotionParams) -> &mut Self {
        for i in 0..V4L2_AV1_TOTAL_REFS_PER_FRAME as usize {
            if global_motion.is_global[i] {
                self.handle.global_motion.flags[i] |= V4L2_AV1_GLOBAL_MOTION_FLAG_IS_GLOBAL as u8;
            }
            if global_motion.is_rot_zoom[i] {
                self.handle.global_motion.flags[i] |= V4L2_AV1_GLOBAL_MOTION_FLAG_IS_ROT_ZOOM as u8;
            }
            if global_motion.is_translation[i] {
                self.handle.global_motion.flags[i] |=
                    V4L2_AV1_GLOBAL_MOTION_FLAG_IS_TRANSLATION as u8;
            }
            self.handle.global_motion.type_[i] = global_motion.gm_type[i] as u32;
            self.handle.global_motion.params[i].copy_from_slice(&global_motion.gm_params[i][0..6]);
            self.handle.global_motion.invalid |= (global_motion.warp_valid[i] as u8) << i;
        }
        self
    }

    pub fn set_frame_params(&mut self, hdr: &FrameHeaderObu) -> &mut Self {
        self.handle.superres_denom = hdr.superres_denom as u8;
        for i in 0..2 as usize {
            self.handle.skip_mode_frame[i] = hdr.skip_mode_frame[i] as u8;
        }
        self.handle.primary_ref_frame = hdr.primary_ref_frame as u8;
        if hdr.show_frame {
            self.handle.flags |= V4L2_AV1_FRAME_FLAG_SHOW_FRAME;
        }
        if hdr.showable_frame {
            self.handle.flags |= V4L2_AV1_FRAME_FLAG_SHOWABLE_FRAME;
        }
        if hdr.error_resilient_mode {
            self.handle.flags |= V4L2_AV1_FRAME_FLAG_ERROR_RESILIENT_MODE;
        }
        if hdr.disable_cdf_update {
            self.handle.flags |= V4L2_AV1_FRAME_FLAG_DISABLE_CDF_UPDATE;
        }
        if hdr.allow_screen_content_tools != 0 {
            self.handle.flags |= V4L2_AV1_FRAME_FLAG_ALLOW_SCREEN_CONTENT_TOOLS;
        }
        if hdr.force_integer_mv != 0 {
            self.handle.flags |= V4L2_AV1_FRAME_FLAG_FORCE_INTEGER_MV;
        }
        if hdr.allow_intrabc {
            self.handle.flags |= V4L2_AV1_FRAME_FLAG_ALLOW_INTRABC;
        }
        if hdr.use_superres {
            self.handle.flags |= V4L2_AV1_FRAME_FLAG_USE_SUPERRES;
        }
        if hdr.allow_high_precision_mv {
            self.handle.flags |= V4L2_AV1_FRAME_FLAG_ALLOW_HIGH_PRECISION_MV;
        }
        if hdr.is_motion_mode_switchable {
            self.handle.flags |= V4L2_AV1_FRAME_FLAG_IS_MOTION_MODE_SWITCHABLE;
        }
        if hdr.use_ref_frame_mvs {
            self.handle.flags |= V4L2_AV1_FRAME_FLAG_USE_REF_FRAME_MVS;
        }
        if hdr.disable_frame_end_update_cdf {
            self.handle.flags |= V4L2_AV1_FRAME_FLAG_DISABLE_FRAME_END_UPDATE_CDF;
        }
        if hdr.allow_warped_motion {
            self.handle.flags |= V4L2_AV1_FRAME_FLAG_ALLOW_WARPED_MOTION;
        }
        if hdr.reference_select {
            self.handle.flags |= V4L2_AV1_FRAME_FLAG_REFERENCE_SELECT;
        }
        if hdr.reduced_tx_set {
            self.handle.flags |= V4L2_AV1_FRAME_FLAG_REDUCED_TX_SET;
        }
        if hdr.skip_mode_present {
            self.handle.flags |= V4L2_AV1_FRAME_FLAG_SKIP_MODE_ALLOWED;
        }
        if hdr.skip_mode_present {
            self.handle.flags |= V4L2_AV1_FRAME_FLAG_SKIP_MODE_PRESENT;
        }
        if hdr.frame_size_override_flag {
            self.handle.flags |= V4L2_AV1_FRAME_FLAG_FRAME_SIZE_OVERRIDE;
        }
        if hdr.buffer_removal_time_present_flag {
            self.handle.flags |= V4L2_AV1_FRAME_FLAG_BUFFER_REMOVAL_TIME_PRESENT;
            self.handle.buffer_removal_time.copy_from_slice(
                &hdr.buffer_removal_time[0..V4L2_AV1_MAX_OPERATING_POINTS as usize],
            );
        }
        if hdr.frame_refs_short_signaling {
            self.handle.flags |= V4L2_AV1_FRAME_FLAG_FRAME_REFS_SHORT_SIGNALING;
        }
        self.handle.frame_type = hdr.frame_type as u32;
        self.handle.order_hint = hdr.order_hint;
        self.handle.upscaled_width = hdr.upscaled_width;
        self.handle.interpolation_filter = hdr.interpolation_filter as u32;
        self.handle.tx_mode = hdr.tx_mode as u32;
        self.handle.frame_width_minus_1 = hdr.frame_width - 1;
        self.handle.frame_height_minus_1 = hdr.frame_height - 1;
        self.handle.render_width_minus_1 = hdr.render_width as u16 - 1;
        self.handle.render_height_minus_1 = hdr.render_height as u16 - 1;
        self.handle.current_frame_id = hdr.current_frame_id;
        self.handle
            .order_hints
            .copy_from_slice(&hdr.order_hints[0..V4L2_AV1_TOTAL_REFS_PER_FRAME as usize]);
        self
    }
}

#[derive(Default)]
pub struct V4l2CtrlAv1SequenceParams {
    #[allow(dead_code)]
    handle: v4l2_ctrl_av1_sequence,
}

impl V4l2CtrlAv1SequenceParams {
    pub fn new() -> Self {
        Default::default()
    }
}

#[derive(Default)]
pub struct V4l2CtrlAv1TileGroupEntryParams {
    #[allow(dead_code)]
    handle: v4l2_ctrl_av1_tile_group_entry,
}

impl V4l2CtrlAv1TileGroupEntryParams {
    pub fn new() -> Self {
        Default::default()
    }
}

// Cargo-culted from v4l2r's control.rs file. v4l2r does not currently support
// VP9 controls, so we work around the issue by making our own "control" struct
// and just implementing the AsV4l2ControlSlice trait.

pub struct Av1V4l2FilmGrainCtrl(v4l2_ext_control, PhantomData<v4l2_ctrl_av1_film_grain>);

impl From<&V4l2CtrlAv1FilmGrainParams> for Av1V4l2FilmGrainCtrl {
    fn from(decode_params: &V4l2CtrlAv1FilmGrainParams) -> Self {
        let payload = Box::new(decode_params.handle);

        Self(
            v4l2_ext_control {
                id: V4L2_CID_STATELESS_AV1_FILM_GRAIN,
                size: std::mem::size_of::<v4l2_ctrl_av1_film_grain>() as u32,
                __bindgen_anon_1: v4l2_ext_control__bindgen_ty_1 {
                    p_av1_film_grain: Box::into_raw(payload),
                },
                ..Default::default()
            },
            PhantomData,
        )
    }
}

impl AsV4l2ControlSlice for &mut Av1V4l2FilmGrainCtrl {
    fn as_v4l2_control_slice(&mut self) -> &mut [v4l2_ext_control] {
        std::slice::from_mut(&mut self.0)
    }
}

impl Drop for Av1V4l2FilmGrainCtrl {
    fn drop(&mut self) {
        // Invariant: p_av1_film_grain contains a pointer to a non-NULL
        // v4l2_ctrl_av1_film_grain object.
        unsafe {
            let _ = Box::from_raw(self.0.__bindgen_anon_1.p_av1_film_grain);
        }
    }
}

pub struct Av1V4l2FrameCtrl(v4l2_ext_control, PhantomData<v4l2_ctrl_av1_frame>);

impl From<&V4l2CtrlAv1FrameParams> for Av1V4l2FrameCtrl {
    fn from(decode_params: &V4l2CtrlAv1FrameParams) -> Self {
        let payload = Box::new(decode_params.handle);

        Self(
            v4l2_ext_control {
                id: V4L2_CID_STATELESS_AV1_FRAME,
                size: std::mem::size_of::<v4l2_ctrl_av1_frame>() as u32,
                __bindgen_anon_1: v4l2_ext_control__bindgen_ty_1 {
                    p_av1_frame: Box::into_raw(payload),
                },
                ..Default::default()
            },
            PhantomData,
        )
    }
}

impl AsV4l2ControlSlice for &mut Av1V4l2FrameCtrl {
    fn as_v4l2_control_slice(&mut self) -> &mut [v4l2_ext_control] {
        std::slice::from_mut(&mut self.0)
    }
}

impl Drop for Av1V4l2FrameCtrl {
    fn drop(&mut self) {
        // Invariant: p_av1_frame contains a pointer to a non-NULL v4l2_ctrl_av1_frame object.
        unsafe {
            let _ = Box::from_raw(self.0.__bindgen_anon_1.p_av1_frame);
        }
    }
}
