// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
use std::fmt;
use std::io::Write;

use crate::codec::h265::nalu_writer::NaluWriter;
use crate::codec::h265::nalu_writer::NaluWriterError;
use crate::codec::h265::parser::HrdParams;
use crate::codec::h265::parser::NaluType;
use crate::codec::h265::parser::Pps;
use crate::codec::h265::parser::ProfileTierLevel;
use crate::codec::h265::parser::ScalingLists;
use crate::codec::h265::parser::ShortTermRefPicSet;
use crate::codec::h265::parser::Sps;
use crate::codec::h265::parser::SublayerHrdParameters;
use crate::codec::h265::parser::Vps;

mod private {
    pub trait NaluStruct {}
}

impl private::NaluStruct for Vps {}

impl private::NaluStruct for Sps {}

impl private::NaluStruct for Pps {}

#[derive(Debug)]
pub enum SynthesizerError {
    Unsupported,
    NaluWriter(NaluWriterError),
}

impl fmt::Display for SynthesizerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SynthesizerError::Unsupported => write!(f, "tried to synthesize unsupported settings"),
            SynthesizerError::NaluWriter(x) => write!(f, "{}", x.to_string()),
        }
    }
}

impl From<NaluWriterError> for SynthesizerError {
    fn from(err: NaluWriterError) -> Self {
        SynthesizerError::NaluWriter(err)
    }
}

pub type SynthesizerResult<T> = Result<T, SynthesizerError>;

/// A helper to output typed NALUs to [`std::io::Write`] using [`NaluWriter`].
pub struct Synthesizer<'n, N: private::NaluStruct, W: Write> {
    writer: NaluWriter<W>,
    nalu: &'n N,
}

/// Extended Sample Aspect Ratio - H.265 Table E.1
const EXTENDED_SAR: u32 = 255;

impl<N: private::NaluStruct, W: Write> Synthesizer<'_, N, W> {
    fn u<T: Into<u32>>(&mut self, bits: usize, value: T) -> SynthesizerResult<()> {
        self.writer.write_u(bits, value)?;
        Ok(())
    }

    fn f<T: Into<u32>>(&mut self, bits: usize, value: T) -> SynthesizerResult<()> {
        self.writer.write_f(bits, value)?;
        Ok(())
    }

    fn ue<T: Into<u32>>(&mut self, value: T) -> SynthesizerResult<()> {
        self.writer.write_ue(value)?;
        Ok(())
    }

    fn se<T: Into<i32>>(&mut self, value: T) -> SynthesizerResult<()> {
        self.writer.write_se(value)?;
        Ok(())
    }

    /// Writes `bits` zero bits, e.g. for the reserved fields of
    /// profile_tier_level().
    fn reserved_zero_bits(&mut self, bits: usize) -> SynthesizerResult<()> {
        for _ in 0..bits {
            self.f(1, 0u32)?;
        }

        Ok(())
    }

    fn profile_tier_level(
        &mut self,
        ptl: &ProfileTierLevel,
        profile_present_flag: bool,
        max_sub_layers_minus1: u8,
    ) -> SynthesizerResult<()> {
        // H.265 7.3.3
        if profile_present_flag {
            self.u(2, ptl.general_profile_space)?;
            self.u(1, ptl.general_tier_flag)?;
            self.u(5, ptl.general_profile_idc)?;

            for i in 0..32 {
                self.u(1, ptl.general_profile_compatibility_flag[i])?;
            }

            self.u(1, ptl.general_progressive_source_flag)?;
            self.u(1, ptl.general_interlaced_source_flag)?;
            self.u(1, ptl.general_non_packed_constraint_flag)?;
            self.u(1, ptl.general_frame_only_constraint_flag)?;

            if ptl.general_profile_idc == 4
                || ptl.general_profile_compatibility_flag[4]
                || ptl.general_profile_idc == 5
                || ptl.general_profile_compatibility_flag[5]
                || ptl.general_profile_idc == 6
                || ptl.general_profile_compatibility_flag[6]
                || ptl.general_profile_idc == 7
                || ptl.general_profile_compatibility_flag[7]
                || ptl.general_profile_idc == 8
                || ptl.general_profile_compatibility_flag[8]
                || ptl.general_profile_idc == 9
                || ptl.general_profile_compatibility_flag[9]
                || ptl.general_profile_idc == 10
                || ptl.general_profile_compatibility_flag[10]
                || ptl.general_profile_idc == 11
                || ptl.general_profile_compatibility_flag[11]
            {
                self.u(1, ptl.general_max_12bit_constraint_flag)?;
                self.u(1, ptl.general_max_10bit_constraint_flag)?;
                self.u(1, ptl.general_max_8bit_constraint_flag)?;
                self.u(1, ptl.general_max_422chroma_constraint_flag)?;
                self.u(1, ptl.general_max_420chroma_constraint_flag)?;
                self.u(1, ptl.general_max_monochrome_constraint_flag)?;
                self.u(1, ptl.general_intra_constraint_flag)?;
                self.u(1, ptl.general_one_picture_only_constraint_flag)?;
                self.u(1, ptl.general_lower_bit_rate_constraint_flag)?;

                if ptl.general_profile_idc == 5
                    || ptl.general_profile_compatibility_flag[5]
                    || ptl.general_profile_idc == 9
                    || ptl.general_profile_compatibility_flag[9]
                    || ptl.general_profile_idc == 10
                    || ptl.general_profile_compatibility_flag[10]
                    || ptl.general_profile_idc == 11
                    || ptl.general_profile_compatibility_flag[11]
                {
                    self.u(1, ptl.general_max_14bit_constraint_flag)?;
                    // general_reserved_zero_33bits
                    self.reserved_zero_bits(33)?;
                } else {
                    // general_reserved_zero_34bits
                    self.reserved_zero_bits(34)?;
                }
            } else if ptl.general_profile_idc == 2 || ptl.general_profile_compatibility_flag[2] {
                // general_reserved_zero_7bits
                self.reserved_zero_bits(7)?;
                self.u(1, ptl.general_one_picture_only_constraint_flag)?;
                // general_reserved_zero_35bits
                self.reserved_zero_bits(35)?;
            } else {
                // general_reserved_zero_43bits
                self.reserved_zero_bits(43)?;
            }

            if ptl.general_profile_idc == 1
                || ptl.general_profile_compatibility_flag[1]
                || ptl.general_profile_idc == 2
                || ptl.general_profile_compatibility_flag[2]
                || ptl.general_profile_idc == 3
                || ptl.general_profile_compatibility_flag[3]
                || ptl.general_profile_idc == 4
                || ptl.general_profile_compatibility_flag[4]
                || ptl.general_profile_idc == 5
                || ptl.general_profile_compatibility_flag[5]
                || ptl.general_profile_idc == 9
                || ptl.general_profile_compatibility_flag[9]
                || ptl.general_profile_idc == 11
                || ptl.general_profile_compatibility_flag[11]
            {
                self.u(1, ptl.general_inbld_flag)?;
            } else {
                // general_reserved_zero_bit
                self.reserved_zero_bits(1)?;
            }
        }

        self.u(8, ptl.general_level_idc as u8)?;

        // Sub-layer profile and level information is not synthesized: the
        // parser gates the whole sub-layer profile section on
        // sub_layer_level_present_flag, so a round-trip is only possible for
        // the single sub-layer case.
        if max_sub_layers_minus1 > 0 {
            return Err(SynthesizerError::Unsupported);
        }

        Ok(())
    }

    /// Writes a single scaling list, always signalling it explicitly, i.e. with
    /// `scaling_list_pred_mode_flag` equal to 1. See H.265 7.3.4.
    fn scaling_list(&mut self, list: &[u8], dc_coef_minus8: Option<i16>) -> SynthesizerResult<()> {
        self.u(1, /* scaling_list_pred_mode_flag */ true)?;

        let mut next_coef = 8i32;

        if let Some(dc_coef_minus8) = dc_coef_minus8 {
            self.se(dc_coef_minus8)?;
            next_coef = i32::from(dc_coef_minus8) + 8;
        }

        for coef in list {
            let coef = i32::from(*coef);
            // The decoding process is (7-42), i.e. modulo 256 arithmetic, so
            // the delta is normalized into the [-128, 127] range.
            let delta = (coef - next_coef + 128).rem_euclid(256) - 128;
            self.se(delta)?;
            next_coef = coef;
        }

        Ok(())
    }

    fn scaling_list_data(&mut self, sl: &ScalingLists) -> SynthesizerResult<()> {
        // H.265 7.3.4
        for size_id in 0..4 {
            let mut matrix_id = 0usize;
            while matrix_id < 6 {
                match size_id {
                    0 => self.scaling_list(&sl.scaling_list_4x4[matrix_id], None)?,
                    1 => self.scaling_list(&sl.scaling_list_8x8[matrix_id], None)?,
                    2 => self.scaling_list(
                        &sl.scaling_list_16x16[matrix_id],
                        Some(sl.scaling_list_dc_coef_minus8_16x16[matrix_id]),
                    )?,
                    _ => self.scaling_list(
                        &sl.scaling_list_32x32[matrix_id],
                        Some(sl.scaling_list_dc_coef_minus8_32x32[matrix_id]),
                    )?,
                }

                matrix_id += if size_id == 3 { 3 } else { 1 };
            }
        }

        Ok(())
    }

    /// Writes a `st_ref_pic_set()` as an explicit list of deltas, i.e. with
    /// `inter_ref_pic_set_prediction_flag` equal to 0. See H.265 7.3.7.
    fn short_term_ref_pic_set(
        &mut self,
        st: &ShortTermRefPicSet,
        st_rps_idx: u8,
    ) -> SynthesizerResult<()> {
        if st.inter_ref_pic_set_prediction_flag {
            // The parser resolves inter RPS prediction into absolute values, so
            // the original syntax elements cannot be recovered.
            return Err(SynthesizerError::Unsupported);
        }

        if st_rps_idx != 0 {
            self.u(1, /* inter_ref_pic_set_prediction_flag */ false)?;
        }

        self.ue(st.num_negative_pics)?;
        self.ue(st.num_positive_pics)?;

        for i in 0..usize::from(st.num_negative_pics) {
            let prev = if i == 0 { 0 } else { st.delta_poc_s0[i - 1] };
            // (7-67): DeltaPocS0[i] = DeltaPocS0[i - 1] -
            //         (delta_poc_s0_minus1[i] + 1)
            let delta_poc_s0_minus1 = prev - st.delta_poc_s0[i] - 1;
            if delta_poc_s0_minus1 < 0 {
                return Err(SynthesizerError::Unsupported);
            }
            self.ue(delta_poc_s0_minus1 as u32)?;
            self.u(1, st.used_by_curr_pic_s0[i])?;
        }

        for i in 0..usize::from(st.num_positive_pics) {
            let prev = if i == 0 { 0 } else { st.delta_poc_s1[i - 1] };
            // (7-68): DeltaPocS1[i] = DeltaPocS1[i - 1] +
            //         (delta_poc_s1_minus1[i] + 1)
            let delta_poc_s1_minus1 = st.delta_poc_s1[i] - prev - 1;
            if delta_poc_s1_minus1 < 0 {
                return Err(SynthesizerError::Unsupported);
            }
            self.ue(delta_poc_s1_minus1 as u32)?;
            self.u(1, st.used_by_curr_pic_s1[i])?;
        }

        Ok(())
    }

    fn sub_layer_hrd_parameters(
        &mut self,
        hrd: &SublayerHrdParameters,
        cpb_cnt: u32,
        sub_pic_hrd_params_present_flag: bool,
    ) -> SynthesizerResult<()> {
        // H.265 E.2.3
        for i in 0..cpb_cnt as usize {
            self.ue(hrd.bit_rate_value_minus1[i])?;
            self.ue(hrd.cpb_size_value_minus1[i])?;
            if sub_pic_hrd_params_present_flag {
                self.ue(hrd.cpb_size_du_value_minus1[i])?;
                self.ue(hrd.bit_rate_du_value_minus1[i])?;
            }

            self.u(1, hrd.cbr_flag[i])?;
        }

        Ok(())
    }

    fn hrd_parameters(
        &mut self,
        hrd: &HrdParams,
        common_inf_present_flag: bool,
        max_num_sublayers_minus1: u8,
    ) -> SynthesizerResult<()> {
        // H.265 E.2.2
        if common_inf_present_flag {
            self.u(1, hrd.nal_hrd_parameters_present_flag)?;
            self.u(1, hrd.vcl_hrd_parameters_present_flag)?;

            if hrd.nal_hrd_parameters_present_flag || hrd.vcl_hrd_parameters_present_flag {
                self.u(1, hrd.sub_pic_hrd_params_present_flag)?;
                if hrd.sub_pic_hrd_params_present_flag {
                    self.u(8, hrd.tick_divisor_minus2)?;
                    self.u(5, hrd.du_cpb_removal_delay_increment_length_minus1)?;
                    self.u(1, hrd.sub_pic_cpb_params_in_pic_timing_sei_flag)?;
                    self.u(5, hrd.dpb_output_delay_du_length_minus1)?;
                }

                self.u(4, hrd.bit_rate_scale)?;
                self.u(4, hrd.cpb_size_scale)?;
                if hrd.sub_pic_hrd_params_present_flag {
                    self.u(4, hrd.cpb_size_du_scale)?;
                }

                self.u(5, hrd.initial_cpb_removal_delay_length_minus1)?;
                self.u(5, hrd.au_cpb_removal_delay_length_minus1)?;
                self.u(5, hrd.dpb_output_delay_length_minus1)?;
            }
        }

        for i in 0..=usize::from(max_num_sublayers_minus1) {
            self.u(1, hrd.fixed_pic_rate_general_flag[i])?;
            if !hrd.fixed_pic_rate_general_flag[i] {
                self.u(1, hrd.fixed_pic_rate_within_cvs_flag[i])?;
            }

            if hrd.fixed_pic_rate_within_cvs_flag[i] {
                self.ue(hrd.elemental_duration_in_tc_minus1[i])?;
            } else {
                self.u(1, hrd.low_delay_hrd_flag[i])?;
            }

            if !hrd.low_delay_hrd_flag[i] {
                self.ue(hrd.cpb_cnt_minus1[i])?;
            }

            if hrd.nal_hrd_parameters_present_flag {
                self.sub_layer_hrd_parameters(
                    &hrd.nal_hrd[i],
                    hrd.cpb_cnt_minus1[i] + 1,
                    hrd.sub_pic_hrd_params_present_flag,
                )?;
            }

            if hrd.vcl_hrd_parameters_present_flag {
                self.sub_layer_hrd_parameters(
                    &hrd.vcl_hrd[i],
                    hrd.cpb_cnt_minus1[i] + 1,
                    hrd.sub_pic_hrd_params_present_flag,
                )?;
            }
        }

        Ok(())
    }

    fn rbsp_trailing_bits(&mut self) -> SynthesizerResult<()> {
        self.f(1, 1u32)?;

        while !self.writer.aligned() {
            self.f(1, 0u32)?;
        }

        Ok(())
    }
}

impl<'n, W: Write> Synthesizer<'n, Vps, W> {
    /// Writes a VPS NALU with the given `nuh_temporal_id_plus1`. `nuh_layer_id`
    /// is always zero, as multi-layer coding is not supported.
    pub fn synthesize(
        nuh_temporal_id_plus1: u8,
        vps: &'n Vps,
        writer: W,
        ep_enabled: bool,
    ) -> SynthesizerResult<()> {
        let mut s = Self { writer: NaluWriter::<W>::new(writer, ep_enabled), nalu: vps };

        s.writer.write_header(NaluType::VpsNut as u8, 0, nuh_temporal_id_plus1)?;
        s.video_parameter_set_rbsp()?;
        s.rbsp_trailing_bits()
    }

    fn video_parameter_set_rbsp(&mut self) -> SynthesizerResult<()> {
        // H.265 7.3.2.1
        self.u(4, self.nalu.video_parameter_set_id)?;
        self.u(1, self.nalu.base_layer_internal_flag)?;
        self.u(1, self.nalu.base_layer_available_flag)?;
        self.u(6, self.nalu.max_layers_minus1)?;
        self.u(3, self.nalu.max_sub_layers_minus1)?;
        self.u(1, self.nalu.temporal_id_nesting_flag)?;
        self.f(16, /* vps_reserved_0xffff_16bits */ 0xffffu32)?;

        self.profile_tier_level(
            &self.nalu.profile_tier_level,
            true,
            self.nalu.max_sub_layers_minus1,
        )?;

        self.u(1, self.nalu.sub_layer_ordering_info_present_flag)?;

        let start = if self.nalu.sub_layer_ordering_info_present_flag {
            0
        } else {
            self.nalu.max_sub_layers_minus1
        };

        for i in usize::from(start)..=usize::from(self.nalu.max_sub_layers_minus1) {
            self.ue(self.nalu.max_dec_pic_buffering_minus1[i])?;
            self.ue(self.nalu.max_num_reorder_pics[i])?;
            self.ue(self.nalu.max_latency_increase_plus1[i])?;
        }

        self.u(6, self.nalu.max_layer_id)?;
        self.ue(self.nalu.num_layer_sets_minus1)?;

        for _ in 1..=self.nalu.num_layer_sets_minus1 {
            for _ in 0..=self.nalu.max_layer_id {
                // The parser discards layer_id_included_flag[i][j].
                self.u(1, /* layer_id_included_flag */ false)?;
            }
        }

        self.u(1, self.nalu.timing_info_present_flag)?;
        if self.nalu.timing_info_present_flag {
            self.u(32, self.nalu.num_units_in_tick)?;
            self.u(32, self.nalu.time_scale)?;

            self.u(1, self.nalu.poc_proportional_to_timing_flag)?;
            if self.nalu.poc_proportional_to_timing_flag {
                self.ue(self.nalu.num_ticks_poc_diff_one_minus1)?;
            }

            self.ue(self.nalu.num_hrd_parameters)?;

            for i in 0..self.nalu.num_hrd_parameters as usize {
                self.ue(self.nalu.hrd_layer_set_idx[i])?;
                if i > 0 {
                    self.u(1, self.nalu.cprms_present_flag[i])?;
                }

                self.hrd_parameters(
                    &self.nalu.hrd_parameters[i],
                    self.nalu.cprms_present_flag[i],
                    self.nalu.max_sub_layers_minus1,
                )?;
            }
        }

        if self.nalu.extension_flag {
            // vps_extension_data_flag is not retained by the parser.
            return Err(SynthesizerError::Unsupported);
        }

        self.u(1, /* vps_extension_flag */ false)?;

        Ok(())
    }
}

impl<'n, W: Write> Synthesizer<'n, Sps, W> {
    /// Writes a SPS NALU with the given `nuh_temporal_id_plus1`. `nuh_layer_id`
    /// is always zero, as multi-layer coding is not supported.
    pub fn synthesize(
        nuh_temporal_id_plus1: u8,
        sps: &'n Sps,
        writer: W,
        ep_enabled: bool,
    ) -> SynthesizerResult<()> {
        let mut s = Self { writer: NaluWriter::<W>::new(writer, ep_enabled), nalu: sps };

        s.writer.write_header(NaluType::SpsNut as u8, 0, nuh_temporal_id_plus1)?;
        s.seq_parameter_set_rbsp()?;
        s.rbsp_trailing_bits()
    }

    fn vui_parameters(&mut self) -> SynthesizerResult<()> {
        // H.265 E.2.1
        let vui = &self.nalu.vui_parameters;

        self.u(1, vui.aspect_ratio_info_present_flag)?;
        if vui.aspect_ratio_info_present_flag {
            self.u(8, vui.aspect_ratio_idc)?;
            if vui.aspect_ratio_idc == EXTENDED_SAR {
                self.u(16, vui.sar_width)?;
                self.u(16, vui.sar_height)?;
            }
        }

        self.u(1, vui.overscan_info_present_flag)?;
        if vui.overscan_info_present_flag {
            self.u(1, vui.overscan_appropriate_flag)?;
        }

        self.u(1, vui.video_signal_type_present_flag)?;
        if vui.video_signal_type_present_flag {
            self.u(3, vui.video_format)?;
            self.u(1, vui.video_full_range_flag)?;
            self.u(1, vui.colour_description_present_flag)?;
            if vui.colour_description_present_flag {
                self.u(8, vui.colour_primaries)?;
                self.u(8, vui.transfer_characteristics)?;
                self.u(8, vui.matrix_coeffs)?;
            }
        }

        self.u(1, vui.chroma_loc_info_present_flag)?;
        if vui.chroma_loc_info_present_flag {
            self.ue(vui.chroma_sample_loc_type_top_field)?;
            self.ue(vui.chroma_sample_loc_type_bottom_field)?;
        }

        self.u(1, vui.neutral_chroma_indication_flag)?;
        self.u(1, vui.field_seq_flag)?;
        self.u(1, vui.frame_field_info_present_flag)?;

        self.u(1, vui.default_display_window_flag)?;
        if vui.default_display_window_flag {
            self.ue(vui.def_disp_win_left_offset)?;
            self.ue(vui.def_disp_win_right_offset)?;
            self.ue(vui.def_disp_win_top_offset)?;
            self.ue(vui.def_disp_win_bottom_offset)?;
        }

        self.u(1, vui.timing_info_present_flag)?;
        if vui.timing_info_present_flag {
            self.u(32, vui.num_units_in_tick)?;
            self.u(32, vui.time_scale)?;

            self.u(1, vui.poc_proportional_to_timing_flag)?;
            if vui.poc_proportional_to_timing_flag {
                self.ue(vui.num_ticks_poc_diff_one_minus1)?;
            }

            self.u(1, vui.hrd_parameters_present_flag)?;
            if vui.hrd_parameters_present_flag {
                self.hrd_parameters(&vui.hrd, true, self.nalu.max_sub_layers_minus1)?;
            }
        }

        self.u(1, vui.bitstream_restriction_flag)?;
        if vui.bitstream_restriction_flag {
            self.u(1, vui.tiles_fixed_structure_flag)?;
            self.u(1, vui.motion_vectors_over_pic_boundaries_flag)?;
            self.u(1, vui.restricted_ref_pic_lists_flag)?;
            self.ue(vui.min_spatial_segmentation_idc)?;
            self.ue(vui.max_bytes_per_pic_denom)?;
            self.ue(vui.max_bits_per_min_cu_denom)?;
            self.ue(vui.log2_max_mv_length_horizontal)?;
            self.ue(vui.log2_max_mv_length_vertical)?;
        }

        Ok(())
    }

    fn sps_range_extension(&mut self) -> SynthesizerResult<()> {
        // H.265 7.3.2.2.2
        let ext = &self.nalu.range_extension;

        self.u(1, ext.transform_skip_rotation_enabled_flag)?;
        self.u(1, ext.transform_skip_context_enabled_flag)?;
        self.u(1, ext.implicit_rdpcm_enabled_flag)?;
        self.u(1, ext.explicit_rdpcm_enabled_flag)?;
        self.u(1, ext.extended_precision_processing_flag)?;
        self.u(1, ext.intra_smoothing_disabled_flag)?;
        self.u(1, ext.high_precision_offsets_enabled_flag)?;
        self.u(1, ext.persistent_rice_adaptation_enabled_flag)?;
        self.u(1, ext.cabac_bypass_alignment_enabled_flag)?;

        Ok(())
    }

    fn seq_parameter_set_rbsp(&mut self) -> SynthesizerResult<()> {
        // H.265 7.3.2.2.1
        self.u(4, self.nalu.video_parameter_set_id)?;
        self.u(3, self.nalu.max_sub_layers_minus1)?;
        self.u(1, self.nalu.temporal_id_nesting_flag)?;

        self.profile_tier_level(
            &self.nalu.profile_tier_level,
            true,
            self.nalu.max_sub_layers_minus1,
        )?;

        self.ue(self.nalu.seq_parameter_set_id)?;
        self.ue(self.nalu.chroma_format_idc)?;
        if self.nalu.chroma_format_idc == 3 {
            self.u(1, self.nalu.separate_colour_plane_flag)?;
        }

        self.ue(self.nalu.pic_width_in_luma_samples)?;
        self.ue(self.nalu.pic_height_in_luma_samples)?;

        self.u(1, self.nalu.conformance_window_flag)?;
        if self.nalu.conformance_window_flag {
            self.ue(self.nalu.conf_win_left_offset)?;
            self.ue(self.nalu.conf_win_right_offset)?;
            self.ue(self.nalu.conf_win_top_offset)?;
            self.ue(self.nalu.conf_win_bottom_offset)?;
        }

        self.ue(self.nalu.bit_depth_luma_minus8)?;
        self.ue(self.nalu.bit_depth_chroma_minus8)?;
        self.ue(self.nalu.log2_max_pic_order_cnt_lsb_minus4)?;

        self.u(1, self.nalu.sub_layer_ordering_info_present_flag)?;

        let start = if self.nalu.sub_layer_ordering_info_present_flag {
            0
        } else {
            self.nalu.max_sub_layers_minus1
        };

        for i in usize::from(start)..=usize::from(self.nalu.max_sub_layers_minus1) {
            self.ue(self.nalu.max_dec_pic_buffering_minus1[i])?;
            self.ue(self.nalu.max_num_reorder_pics[i])?;
            self.ue(self.nalu.max_latency_increase_plus1[i])?;
        }

        self.ue(self.nalu.log2_min_luma_coding_block_size_minus3)?;
        self.ue(self.nalu.log2_diff_max_min_luma_coding_block_size)?;
        self.ue(self.nalu.log2_min_luma_transform_block_size_minus2)?;
        self.ue(self.nalu.log2_diff_max_min_luma_transform_block_size)?;
        self.ue(self.nalu.max_transform_hierarchy_depth_inter)?;
        self.ue(self.nalu.max_transform_hierarchy_depth_intra)?;

        self.u(1, self.nalu.scaling_list_enabled_flag)?;
        if self.nalu.scaling_list_enabled_flag {
            self.u(1, self.nalu.scaling_list_data_present_flag)?;
            if self.nalu.scaling_list_data_present_flag {
                self.scaling_list_data(&self.nalu.scaling_list)?;
            }
        }

        self.u(1, self.nalu.amp_enabled_flag)?;
        self.u(1, self.nalu.sample_adaptive_offset_enabled_flag)?;

        self.u(1, self.nalu.pcm_enabled_flag)?;
        if self.nalu.pcm_enabled_flag {
            self.u(4, self.nalu.pcm_sample_bit_depth_luma_minus1)?;
            self.u(4, self.nalu.pcm_sample_bit_depth_chroma_minus1)?;
            self.ue(self.nalu.log2_min_pcm_luma_coding_block_size_minus3)?;
            self.ue(self.nalu.log2_diff_max_min_pcm_luma_coding_block_size)?;
            self.u(1, self.nalu.pcm_loop_filter_disabled_flag)?;
        }

        self.ue(self.nalu.num_short_term_ref_pic_sets)?;
        for i in 0..usize::from(self.nalu.num_short_term_ref_pic_sets) {
            let st =
                self.nalu.short_term_ref_pic_set.get(i).ok_or(SynthesizerError::Unsupported)?;
            self.short_term_ref_pic_set(st, i as u8)?;
        }

        self.u(1, self.nalu.long_term_ref_pics_present_flag)?;
        if self.nalu.long_term_ref_pics_present_flag {
            self.ue(self.nalu.num_long_term_ref_pics_sps)?;

            let bits = usize::from(self.nalu.log2_max_pic_order_cnt_lsb_minus4) + 4;
            for i in 0..usize::from(self.nalu.num_long_term_ref_pics_sps) {
                self.u(bits, self.nalu.lt_ref_pic_poc_lsb_sps[i])?;
                self.u(1, self.nalu.used_by_curr_pic_lt_sps_flag[i])?;
            }
        }

        self.u(1, self.nalu.temporal_mvp_enabled_flag)?;
        self.u(1, self.nalu.strong_intra_smoothing_enabled_flag)?;

        self.u(1, self.nalu.vui_parameters_present_flag)?;
        if self.nalu.vui_parameters_present_flag {
            self.vui_parameters()?;
        }

        self.u(1, self.nalu.extension_present_flag)?;
        if self.nalu.extension_present_flag {
            if self.nalu.scc_extension_flag {
                // The screen content coding extension is out of scope.
                return Err(SynthesizerError::Unsupported);
            }

            self.u(1, self.nalu.range_extension_flag)?;
            if self.nalu.range_extension_flag {
                self.sps_range_extension()?;
            }

            self.u(1, /* sps_multilayer_extension_flag */ false)?;
            self.u(1, /* sps_3d_extension_flag */ false)?;
            self.u(1, /* sps_scc_extension_flag */ false)?;
            self.u(4, /* sps_extension_4bits */ 0u32)?;
        }

        Ok(())
    }
}

impl<'n, W: Write> Synthesizer<'n, Pps, W> {
    /// Writes a PPS NALU with the given `nuh_temporal_id_plus1`. `nuh_layer_id`
    /// is always zero, as multi-layer coding is not supported.
    pub fn synthesize(
        nuh_temporal_id_plus1: u8,
        pps: &'n Pps,
        writer: W,
        ep_enabled: bool,
    ) -> SynthesizerResult<()> {
        let mut s = Self { writer: NaluWriter::<W>::new(writer, ep_enabled), nalu: pps };

        s.writer.write_header(NaluType::PpsNut as u8, 0, nuh_temporal_id_plus1)?;
        s.pic_parameter_set_rbsp()?;
        s.rbsp_trailing_bits()
    }

    fn pps_range_extension(&mut self) -> SynthesizerResult<()> {
        // H.265 7.3.2.3.2
        let ext = &self.nalu.range_extension;

        if self.nalu.transform_skip_enabled_flag {
            self.ue(ext.log2_max_transform_skip_block_size_minus2)?;
        }

        self.u(1, ext.cross_component_prediction_enabled_flag)?;
        self.u(1, ext.chroma_qp_offset_list_enabled_flag)?;
        if ext.chroma_qp_offset_list_enabled_flag {
            self.ue(ext.diff_cu_chroma_qp_offset_depth)?;
            self.ue(ext.chroma_qp_offset_list_len_minus1)?;
            for i in 0..=ext.chroma_qp_offset_list_len_minus1 as usize {
                self.se(ext.cb_qp_offset_list[i])?;
                self.se(ext.cr_qp_offset_list[i])?;
            }
        }

        self.ue(ext.log2_sao_offset_scale_luma)?;
        self.ue(ext.log2_sao_offset_scale_chroma)?;

        Ok(())
    }

    fn pic_parameter_set_rbsp(&mut self) -> SynthesizerResult<()> {
        // H.265 7.3.2.3.1
        self.ue(self.nalu.pic_parameter_set_id)?;
        self.ue(self.nalu.seq_parameter_set_id)?;
        self.u(1, self.nalu.dependent_slice_segments_enabled_flag)?;
        self.u(1, self.nalu.output_flag_present_flag)?;
        self.u(3, self.nalu.num_extra_slice_header_bits)?;
        self.u(1, self.nalu.sign_data_hiding_enabled_flag)?;
        self.u(1, self.nalu.cabac_init_present_flag)?;
        self.ue(self.nalu.num_ref_idx_l0_default_active_minus1)?;
        self.ue(self.nalu.num_ref_idx_l1_default_active_minus1)?;
        self.se(self.nalu.init_qp_minus26)?;
        self.u(1, self.nalu.constrained_intra_pred_flag)?;
        self.u(1, self.nalu.transform_skip_enabled_flag)?;

        self.u(1, self.nalu.cu_qp_delta_enabled_flag)?;
        if self.nalu.cu_qp_delta_enabled_flag {
            self.ue(self.nalu.diff_cu_qp_delta_depth)?;
        }

        self.se(self.nalu.cb_qp_offset)?;
        self.se(self.nalu.cr_qp_offset)?;
        self.u(1, self.nalu.slice_chroma_qp_offsets_present_flag)?;
        self.u(1, self.nalu.weighted_pred_flag)?;
        self.u(1, self.nalu.weighted_bipred_flag)?;
        self.u(1, self.nalu.transquant_bypass_enabled_flag)?;
        self.u(1, self.nalu.tiles_enabled_flag)?;
        self.u(1, self.nalu.entropy_coding_sync_enabled_flag)?;

        if self.nalu.tiles_enabled_flag {
            if !self.nalu.uniform_spacing_flag {
                // The parser consumes column_width_minus1 and row_height_minus1
                // to derive the tile layout, so the original values cannot be
                // recovered.
                return Err(SynthesizerError::Unsupported);
            }

            self.ue(self.nalu.num_tile_columns_minus1)?;
            self.ue(self.nalu.num_tile_rows_minus1)?;
            self.u(1, /* uniform_spacing_flag */ true)?;
            self.u(1, self.nalu.loop_filter_across_tiles_enabled_flag)?;
        }

        self.u(1, self.nalu.loop_filter_across_slices_enabled_flag)?;

        self.u(1, self.nalu.deblocking_filter_control_present_flag)?;
        if self.nalu.deblocking_filter_control_present_flag {
            self.u(1, self.nalu.deblocking_filter_override_enabled_flag)?;
            self.u(1, self.nalu.deblocking_filter_disabled_flag)?;
            if !self.nalu.deblocking_filter_disabled_flag {
                self.se(self.nalu.beta_offset_div2)?;
                self.se(self.nalu.tc_offset_div2)?;
            }
        }

        self.u(1, self.nalu.scaling_list_data_present_flag)?;
        if self.nalu.scaling_list_data_present_flag {
            self.scaling_list_data(&self.nalu.scaling_list)?;
        }

        self.u(1, self.nalu.lists_modification_present_flag)?;
        self.ue(self.nalu.log2_parallel_merge_level_minus2)?;
        self.u(1, self.nalu.slice_segment_header_extension_present_flag)?;

        self.u(1, self.nalu.extension_present_flag)?;
        if self.nalu.extension_present_flag {
            if self.nalu.scc_extension_flag {
                // The screen content coding extension is out of scope.
                return Err(SynthesizerError::Unsupported);
            }

            self.u(1, self.nalu.range_extension_flag)?;
            if self.nalu.range_extension_flag {
                self.pps_range_extension()?;
            }

            self.u(1, /* pps_multilayer_extension_flag */ false)?;
            self.u(1, /* pps_3d_extension_flag */ false)?;
            self.u(1, /* pps_scc_extension_flag */ false)?;
            self.u(4, /* pps_extension_4bits */ 0u32)?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;
    use std::rc::Rc;

    use super::*;
    use crate::codec::h265::parser::Level;
    use crate::codec::h265::parser::Nalu;
    use crate::codec::h265::parser::Parser;
    use crate::codec::h265::parser::PpsBuilder;
    use crate::codec::h265::parser::Profile;
    use crate::codec::h265::parser::ShortTermRefPicSet;
    use crate::codec::h265::parser::SpsBuilder;
    use crate::codec::h265::parser::VpsBuilder;

    /// Synthesizes `vps`, `sps` and `pps` into a single Annex B buffer, parses
    /// it back and returns the parsed structures.
    fn roundtrip(
        vps: &Vps,
        sps: &Sps,
        pps: &Pps,
        ep_enabled: bool,
    ) -> (Vec<u8>, Rc<Vps>, Rc<Sps>, Rc<Pps>) {
        let mut buf = Vec::<u8>::new();

        Synthesizer::<'_, Vps, _>::synthesize(1, vps, &mut buf, ep_enabled).unwrap();
        Synthesizer::<'_, Sps, _>::synthesize(1, sps, &mut buf, ep_enabled).unwrap();
        Synthesizer::<'_, Pps, _>::synthesize(1, pps, &mut buf, ep_enabled).unwrap();

        let mut cursor = Cursor::new(&buf[..]);
        let mut parser = Parser::default();

        let nalu = Nalu::next(&mut cursor).unwrap();
        let vps = Rc::new(parser.parse_vps(&nalu).unwrap().clone());

        let nalu = Nalu::next(&mut cursor).unwrap();
        let sps = Rc::new(parser.parse_sps(&nalu).unwrap().clone());

        let nalu = Nalu::next(&mut cursor).unwrap();
        let pps = Rc::new(parser.parse_pps(&nalu).unwrap().clone());

        (buf, vps, sps, pps)
    }

    fn default_vps() -> Rc<Vps> {
        VpsBuilder::new()
            .video_parameter_set_id(0)
            .general_profile(Profile::Main)
            .general_level_idc(Level::L4)
            .max_dec_pic_buffering_minus1(1)
            .max_num_reorder_pics(0)
            .build()
    }

    fn default_sps(vps: Rc<Vps>) -> Rc<Sps> {
        SpsBuilder::new(vps)
            .seq_parameter_set_id(0)
            .resolution(1920, 1080)
            .max_dec_pic_buffering_minus1(1)
            .max_num_reorder_pics(0)
            .amp_enabled_flag(true)
            .sample_adaptive_offset_enabled_flag(true)
            .temporal_mvp_enabled_flag(true)
            .strong_intra_smoothing_enabled_flag(true)
            .build()
    }

    #[test]
    fn synthesize_vps_sps_pps() {
        let vps = default_vps();
        let sps = default_sps(Rc::clone(&vps));
        let pps = PpsBuilder::new(Rc::clone(&sps))
            .pic_parameter_set_id(0)
            .init_qp_minus26(0)
            .cu_qp_delta_enabled_flag(true)
            .deblocking_filter_control_present_flag(true)
            .build();

        let (_, vps2, sps2, pps2) = roundtrip(&vps, &sps, &pps, true);

        assert_eq!(*vps, *vps2);
        assert_eq!(*sps, *sps2);
        assert_eq!(*pps, *pps2);
    }

    /// The conformance window must compensate for the coded size being aligned
    /// to the minimum coding block size.
    #[test]
    fn synthesize_sps_conformance_window() {
        let vps = default_vps();
        let sps = SpsBuilder::new(Rc::clone(&vps)).resolution(1920, 1082).build();

        assert_eq!(sps.pic_height_in_luma_samples, 1088);
        assert!(sps.conformance_window_flag);
        assert_eq!(sps.visible_rectangle().max.y, 1082);

        let pps = PpsBuilder::new(Rc::clone(&sps)).build();
        let (_, _, sps2, _) = roundtrip(&vps, &sps, &pps, true);

        assert_eq!(*sps, *sps2);
        assert_eq!(sps2.visible_rectangle().max.y, 1082);
    }

    /// A SPS carrying VUI timing information, a long term reference picture set
    /// and a short term reference picture set list.
    #[test]
    fn synthesize_sps_ref_pic_sets_and_vui() {
        let vps = default_vps();

        let mut st = ShortTermRefPicSet { num_negative_pics: 1, ..Default::default() };
        st.delta_poc_s0[0] = -1;
        st.used_by_curr_pic_s0[0] = true;
        st.num_delta_pocs = 1;

        let sps = SpsBuilder::new(Rc::clone(&vps))
            .resolution(320, 240)
            .max_dec_pic_buffering_minus1(2)
            .short_term_ref_pic_set(st.clone())
            .long_term_ref_pics_present_flag(true)
            .timing_info(1, 60, true)
            .build();

        assert_eq!(sps.num_short_term_ref_pic_sets, 1);

        let pps = PpsBuilder::new(Rc::clone(&sps)).build();
        let (_, _, sps2, _) = roundtrip(&vps, &sps, &pps, true);

        assert_eq!(*sps, *sps2);
        assert_eq!(sps2.short_term_ref_pic_set[0], st);
        assert_eq!(sps2.vui_parameters.num_units_in_tick, 1);
        assert_eq!(sps2.vui_parameters.time_scale, 60);
    }

    /// A VPS carrying timing information, which contains long runs of zero
    /// bits, so that emulation prevention bytes are actually emitted.
    #[test]
    fn synthesize_emulation_prevention() {
        let vps = VpsBuilder::new()
            .general_profile(Profile::Main)
            .general_level_idc(Level::L4)
            .max_dec_pic_buffering_minus1(1)
            .timing_info(1, 60)
            .build();
        let sps = default_sps(Rc::clone(&vps));
        let pps = PpsBuilder::new(Rc::clone(&sps)).build();

        let (buf, vps2, sps2, pps2) = roundtrip(&vps, &sps, &pps, true);

        // Emulation prevention bytes were actually needed here, otherwise this
        // test would not be testing anything.
        assert!(buf.windows(3).any(|w| w == [0x00, 0x00, 0x03]));

        assert_eq!(*vps, *vps2);
        assert_eq!(*sps, *sps2);
        assert_eq!(*pps, *pps2);
    }

    #[test]
    fn synthesize_scaling_lists() {
        let vps = default_vps();
        let mut scaling_list = ScalingLists::default();

        for (i, list) in scaling_list.scaling_list_4x4.iter_mut().enumerate() {
            for (j, coef) in list.iter_mut().enumerate() {
                *coef = (16 + i + j) as u8;
            }
        }

        for (i, list) in scaling_list.scaling_list_16x16.iter_mut().enumerate() {
            for (j, coef) in list.iter_mut().enumerate() {
                *coef = (16 + i * j) as u8;
            }
            scaling_list.scaling_list_dc_coef_minus8_16x16[i] = i as i16;
        }

        let sps = SpsBuilder::new(Rc::clone(&vps))
            .resolution(320, 240)
            .scaling_list(scaling_list.clone())
            .build();
        let pps = PpsBuilder::new(Rc::clone(&sps)).scaling_list(scaling_list.clone()).build();

        let (_, _, sps2, pps2) = roundtrip(&vps, &sps, &pps, true);

        assert_eq!(sps2.scaling_list, scaling_list);
        assert_eq!(pps2.scaling_list, scaling_list);
        assert_eq!(*sps, *sps2);
        assert_eq!(*pps, *pps2);
    }

    /// Re-synthesizing the parameter sets of a real bitstream must produce the
    /// exact same NALUs, which checks the synthesizer against streams that were
    /// not produced by this crate.
    #[test]
    fn synthesize_parameter_sets_from_stream() {
        const STREAM_BEAR: &[u8] = include_bytes!("test_data/bear.h265");
        const STREAM_TEST25FPS: &[u8] = include_bytes!("test_data/test-25fps.h265");
        const STREAM_BBB: &[u8] = include_bytes!("test_data/bbb.h265");

        for stream in [STREAM_BEAR, STREAM_TEST25FPS, STREAM_BBB] {
            let mut cursor = Cursor::new(stream);
            let mut parser = Parser::default();
            let mut num_checked = 0;

            while let Ok(nalu) = Nalu::next(&mut cursor) {
                let mut buf = Vec::<u8>::new();
                let temporal_id_plus1 = nalu.header.nuh_temporal_id_plus1;

                match nalu.header.type_ {
                    NaluType::VpsNut => {
                        let vps = parser.parse_vps(&nalu).unwrap();
                        Synthesizer::<'_, Vps, _>::synthesize(
                            temporal_id_plus1,
                            vps,
                            &mut buf,
                            true,
                        )
                        .unwrap();
                    }
                    NaluType::SpsNut => {
                        let sps = parser.parse_sps(&nalu).unwrap();
                        Synthesizer::<'_, Sps, _>::synthesize(
                            temporal_id_plus1,
                            sps,
                            &mut buf,
                            true,
                        )
                        .unwrap();
                    }
                    NaluType::PpsNut => {
                        let pps = parser.parse_pps(&nalu).unwrap();
                        Synthesizer::<'_, Pps, _>::synthesize(
                            temporal_id_plus1,
                            pps,
                            &mut buf,
                            true,
                        )
                        .unwrap();
                    }
                    _ => continue,
                }

                // Skip the start code, which may be three bytes long in the
                // original stream.
                assert_eq!(&buf[4..], nalu.as_ref(), "{:?} mismatch", nalu.header.type_);
                num_checked += 1;
            }

            assert!(num_checked >= 3);
        }
    }

    /// A PPS exercising every field a CQP/CBR encoder is likely to set.
    #[test]
    fn synthesize_pps_fields() {
        let vps = default_vps();
        let sps = default_sps(Rc::clone(&vps));
        let pps = PpsBuilder::new(Rc::clone(&sps))
            .pic_parameter_set_id(3)
            .dependent_slice_segments_enabled_flag(true)
            .output_flag_present_flag(true)
            .num_extra_slice_header_bits(2)
            .sign_data_hiding_enabled_flag(true)
            .cabac_init_present_flag(true)
            .num_ref_idx_l0_default_active_minus1(1)
            .num_ref_idx_l1_default_active_minus1(2)
            .init_qp_minus26(-4)
            .constrained_intra_pred_flag(true)
            .transform_skip_enabled_flag(true)
            .cu_qp_delta_enabled_flag(true)
            .diff_cu_qp_delta_depth(2)
            .cb_qp_offset(-3)
            .cr_qp_offset(5)
            .slice_chroma_qp_offsets_present_flag(true)
            .weighted_pred_flag(true)
            .weighted_bipred_flag(true)
            .transquant_bypass_enabled_flag(true)
            .entropy_coding_sync_enabled_flag(true)
            .loop_filter_across_slices_enabled_flag(true)
            .deblocking_filter_control_present_flag(true)
            .deblocking_filter_override_enabled_flag(true)
            .deblocking_filter_offsets(-2, 3)
            .lists_modification_present_flag(true)
            .log2_parallel_merge_level_minus2(1)
            .slice_segment_header_extension_present_flag(true)
            .build();

        let (_, _, _, pps2) = roundtrip(&vps, &sps, &pps, true);

        assert_eq!(*pps, *pps2);
    }
}
