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
use v4l2r::bindings::V4L2_AV1_MAX_NUM_CB_POINTS;
use v4l2r::bindings::V4L2_AV1_MAX_NUM_CR_POINTS;
use v4l2r::bindings::V4L2_AV1_MAX_NUM_Y_POINTS;
use v4l2r::bindings::V4L2_CID_STATELESS_AV1_FILM_GRAIN;
use v4l2r::controls::AsV4l2ControlSlice;

use crate::codec::av1::parser::FrameHeaderObu;

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
    #[allow(dead_code)]
    handle: v4l2_ctrl_av1_frame,
}

impl V4l2CtrlAv1FrameParams {
    pub fn new() -> Self {
        Default::default()
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
