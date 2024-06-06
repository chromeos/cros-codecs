// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use crate::codec::h264::parser::Pps;
use crate::codec::h264::parser::SliceHeader;
use crate::codec::h264::parser::Sps;
use crate::codec::h264::picture::Field;
use crate::codec::h264::picture::IsIdr;
use crate::codec::h264::picture::PictureData;
use crate::codec::h264::picture::RcPictureData;
use crate::codec::h264::picture::Reference;

use v4l2r::bindings::v4l2_ctrl_h264_decode_params;
use v4l2r::bindings::v4l2_ctrl_h264_pps;
use v4l2r::bindings::v4l2_ctrl_h264_scaling_matrix;
use v4l2r::bindings::v4l2_ctrl_h264_sps;
use v4l2r::bindings::v4l2_h264_dpb_entry;
use v4l2r::bindings::v4l2_stateless_h264_decode_mode_V4L2_STATELESS_H264_DECODE_MODE_FRAME_BASED as V4L2_STATELESS_H264_DECODE_MODE_FRAME_BASED;
use v4l2r::bindings::v4l2_stateless_h264_decode_mode_V4L2_STATELESS_H264_DECODE_MODE_SLICE_BASED as V4L2_STATELESS_H264_DECODE_MODE_SLICE_BASED;
use v4l2r::bindings::V4L2_H264_DECODE_PARAM_FLAG_BOTTOM_FIELD;
use v4l2r::bindings::V4L2_H264_DECODE_PARAM_FLAG_FIELD_PIC;
use v4l2r::bindings::V4L2_H264_DECODE_PARAM_FLAG_IDR_PIC;
use v4l2r::bindings::V4L2_H264_DPB_ENTRY_FLAG_ACTIVE;
use v4l2r::bindings::V4L2_H264_DPB_ENTRY_FLAG_LONG_TERM;
use v4l2r::bindings::V4L2_H264_DPB_ENTRY_FLAG_VALID;
use v4l2r::bindings::V4L2_H264_FRAME_REF;
use v4l2r::bindings::V4L2_H264_PPS_FLAG_BOTTOM_FIELD_PIC_ORDER_IN_FRAME_PRESENT;
use v4l2r::bindings::V4L2_H264_PPS_FLAG_CONSTRAINED_INTRA_PRED;
use v4l2r::bindings::V4L2_H264_PPS_FLAG_DEBLOCKING_FILTER_CONTROL_PRESENT;
use v4l2r::bindings::V4L2_H264_PPS_FLAG_ENTROPY_CODING_MODE;
use v4l2r::bindings::V4L2_H264_PPS_FLAG_REDUNDANT_PIC_CNT_PRESENT;
use v4l2r::bindings::V4L2_H264_PPS_FLAG_SCALING_MATRIX_PRESENT;
use v4l2r::bindings::V4L2_H264_PPS_FLAG_TRANSFORM_8X8_MODE;
use v4l2r::bindings::V4L2_H264_PPS_FLAG_WEIGHTED_PRED;
use v4l2r::bindings::V4L2_H264_SPS_CONSTRAINT_SET0_FLAG;
use v4l2r::bindings::V4L2_H264_SPS_CONSTRAINT_SET1_FLAG;
use v4l2r::bindings::V4L2_H264_SPS_CONSTRAINT_SET2_FLAG;
use v4l2r::bindings::V4L2_H264_SPS_CONSTRAINT_SET3_FLAG;
use v4l2r::bindings::V4L2_H264_SPS_CONSTRAINT_SET4_FLAG;
use v4l2r::bindings::V4L2_H264_SPS_CONSTRAINT_SET5_FLAG;
use v4l2r::bindings::V4L2_H264_SPS_FLAG_DELTA_PIC_ORDER_ALWAYS_ZERO;
use v4l2r::bindings::V4L2_H264_SPS_FLAG_DIRECT_8X8_INFERENCE;
use v4l2r::bindings::V4L2_H264_SPS_FLAG_FRAME_MBS_ONLY;
use v4l2r::bindings::V4L2_H264_SPS_FLAG_GAPS_IN_FRAME_NUM_VALUE_ALLOWED;
use v4l2r::bindings::V4L2_H264_SPS_FLAG_MB_ADAPTIVE_FRAME_FIELD;
use v4l2r::bindings::V4L2_H264_SPS_FLAG_QPPRIME_Y_ZERO_TRANSFORM_BYPASS;
use v4l2r::bindings::V4L2_H264_SPS_FLAG_SEPARATE_COLOUR_PLANE;
use v4l2r::controls::codec::H264DecodeMode;
use v4l2r::controls::codec::H264DecodeParams;
use v4l2r::controls::codec::H264Pps;
use v4l2r::controls::codec::H264ScalingMatrix;
use v4l2r::controls::codec::H264Sps;
use v4l2r::controls::SafeExtControl;

impl From<&Sps> for v4l2_ctrl_h264_sps {
    fn from(sps: &Sps) -> Self {
        let mut constraint_set_flags: u32 = 0;
        if sps.constraint_set0_flag {
            constraint_set_flags |= V4L2_H264_SPS_CONSTRAINT_SET0_FLAG;
        }
        if sps.constraint_set1_flag {
            constraint_set_flags |= V4L2_H264_SPS_CONSTRAINT_SET1_FLAG;
        }
        if sps.constraint_set2_flag {
            constraint_set_flags |= V4L2_H264_SPS_CONSTRAINT_SET2_FLAG;
        }
        if sps.constraint_set3_flag {
            constraint_set_flags |= V4L2_H264_SPS_CONSTRAINT_SET3_FLAG;
        }
        if sps.constraint_set4_flag {
            constraint_set_flags |= V4L2_H264_SPS_CONSTRAINT_SET4_FLAG;
        }
        if sps.constraint_set5_flag {
            constraint_set_flags |= V4L2_H264_SPS_CONSTRAINT_SET5_FLAG;
        }
        let mut flags: u32 = 0;
        if sps.separate_colour_plane_flag {
            flags |= V4L2_H264_SPS_FLAG_SEPARATE_COLOUR_PLANE;
        }
        if sps.qpprime_y_zero_transform_bypass_flag {
            flags |= V4L2_H264_SPS_FLAG_QPPRIME_Y_ZERO_TRANSFORM_BYPASS;
        }
        if sps.delta_pic_order_always_zero_flag {
            flags |= V4L2_H264_SPS_FLAG_DELTA_PIC_ORDER_ALWAYS_ZERO;
        }
        if sps.gaps_in_frame_num_value_allowed_flag {
            flags |= V4L2_H264_SPS_FLAG_GAPS_IN_FRAME_NUM_VALUE_ALLOWED;
        }
        if sps.frame_mbs_only_flag {
            flags |= V4L2_H264_SPS_FLAG_FRAME_MBS_ONLY;
        }
        if sps.mb_adaptive_frame_field_flag {
            flags |= V4L2_H264_SPS_FLAG_MB_ADAPTIVE_FRAME_FIELD;
        }
        if sps.direct_8x8_inference_flag {
            flags |= V4L2_H264_SPS_FLAG_DIRECT_8X8_INFERENCE;
        }
        Self {
            profile_idc: sps.profile_idc,
            constraint_set_flags: constraint_set_flags as u8,
            level_idc: sps.level_idc as u8,
            seq_parameter_set_id: sps.seq_parameter_set_id,
            chroma_format_idc: sps.chroma_format_idc,
            bit_depth_luma_minus8: sps.bit_depth_luma_minus8,
            bit_depth_chroma_minus8: sps.bit_depth_chroma_minus8,
            log2_max_frame_num_minus4: sps.log2_max_frame_num_minus4,
            pic_order_cnt_type: sps.pic_order_cnt_type,
            log2_max_pic_order_cnt_lsb_minus4: sps.log2_max_pic_order_cnt_lsb_minus4,
            max_num_ref_frames: sps.max_num_ref_frames as u8,
            num_ref_frames_in_pic_order_cnt_cycle: sps.num_ref_frames_in_pic_order_cnt_cycle,
            offset_for_ref_frame: sps.offset_for_ref_frame,
            offset_for_non_ref_pic: sps.offset_for_non_ref_pic,
            offset_for_top_to_bottom_field: sps.offset_for_top_to_bottom_field,
            pic_width_in_mbs_minus1: sps.pic_width_in_mbs_minus1 as u16,
            pic_height_in_map_units_minus1: sps.pic_height_in_map_units_minus1 as u16,
            flags,
            ..Default::default()
        }
    }
}

impl From<&Pps> for v4l2_ctrl_h264_pps {
    fn from(pps: &Pps) -> Self {
        let mut flags: u32 = 0;
        if pps.entropy_coding_mode_flag {
            flags |= V4L2_H264_PPS_FLAG_ENTROPY_CODING_MODE;
        }
        if pps.bottom_field_pic_order_in_frame_present_flag {
            flags |= V4L2_H264_PPS_FLAG_BOTTOM_FIELD_PIC_ORDER_IN_FRAME_PRESENT;
        }
        if pps.weighted_pred_flag {
            flags |= V4L2_H264_PPS_FLAG_WEIGHTED_PRED;
        }
        if pps.deblocking_filter_control_present_flag {
            flags |= V4L2_H264_PPS_FLAG_DEBLOCKING_FILTER_CONTROL_PRESENT;
        }
        if pps.constrained_intra_pred_flag {
            flags |= V4L2_H264_PPS_FLAG_CONSTRAINED_INTRA_PRED;
        }
        if pps.redundant_pic_cnt_present_flag {
            flags |= V4L2_H264_PPS_FLAG_REDUNDANT_PIC_CNT_PRESENT;
        }
        if pps.transform_8x8_mode_flag {
            flags |= V4L2_H264_PPS_FLAG_TRANSFORM_8X8_MODE;
        }
        if pps.pic_scaling_matrix_present_flag {
            flags |= V4L2_H264_PPS_FLAG_SCALING_MATRIX_PRESENT;
        }
        Self {
            pic_parameter_set_id: pps.pic_parameter_set_id,
            seq_parameter_set_id: pps.seq_parameter_set_id,
            num_slice_groups_minus1: pps.num_slice_groups_minus1 as u8,
            num_ref_idx_l0_default_active_minus1: pps.num_ref_idx_l0_default_active_minus1,
            num_ref_idx_l1_default_active_minus1: pps.num_ref_idx_l1_default_active_minus1,
            weighted_bipred_idc: pps.weighted_bipred_idc,
            pic_init_qp_minus26: pps.pic_init_qp_minus26,
            pic_init_qs_minus26: pps.pic_init_qs_minus26,
            chroma_qp_index_offset: pps.chroma_qp_index_offset,
            second_chroma_qp_index_offset: pps.second_chroma_qp_index_offset,
            flags: flags as u16,
            ..Default::default()
        }
    }
}

pub struct V4l2CtrlH264DpbEntry {
    pub timestamp: u64,
    pub pic: RcPictureData,
}

impl From<&V4l2CtrlH264DpbEntry> for v4l2_h264_dpb_entry {
    fn from(dpb: &V4l2CtrlH264DpbEntry) -> Self {
        let pic: &PictureData = &dpb.pic.borrow();
        // TODO     DCHECK_EQ(pic->field, H264Picture::FIELD_NONE)
        // TODO         << "Interlacing not supported";

        let (frame_num, pic_num): (u16, u32) = match pic.reference() {
            Reference::LongTerm => (pic.long_term_pic_num as u16, pic.long_term_frame_idx),
            _ => (pic.frame_num as u16, pic.pic_num as u32),
        };

        let mut flags: u32 = V4L2_H264_DPB_ENTRY_FLAG_VALID;
        if pic.nal_ref_idc != 0 {
            flags |= V4L2_H264_DPB_ENTRY_FLAG_ACTIVE;
        }
        if matches!(pic.reference(), Reference::LongTerm) {
            flags |= V4L2_H264_DPB_ENTRY_FLAG_LONG_TERM;
        }

        Self {
            reference_ts: dpb.timestamp * 1000, // usec to nsec
            frame_num,
            pic_num,
            fields: V4L2_H264_FRAME_REF as u8,
            top_field_order_cnt: pic.top_field_order_cnt,
            bottom_field_order_cnt: pic.bottom_field_order_cnt,
            flags,
            ..Default::default()
        }
    }
}

#[derive(Default)]
pub struct V4l2CtrlH264Sps {
    handle: v4l2_ctrl_h264_sps,
}

impl V4l2CtrlH264Sps {
    pub fn new() -> Self {
        Default::default()
    }
    pub fn set(&mut self, sps: &Sps) -> &mut Self {
        self.handle = v4l2_ctrl_h264_sps::from(sps);
        self
    }
}

impl From<&V4l2CtrlH264Sps> for SafeExtControl<H264Sps> {
    fn from(sps: &V4l2CtrlH264Sps) -> Self {
        SafeExtControl::<H264Sps>::from(sps.handle)
    }
}

#[derive(Default)]
pub struct V4l2CtrlH264Pps {
    handle: v4l2_ctrl_h264_pps,
}

impl V4l2CtrlH264Pps {
    pub fn new() -> Self {
        Default::default()
    }
    pub fn set(&mut self, pps: &Pps) -> &mut Self {
        self.handle = v4l2_ctrl_h264_pps::from(pps);
        self
    }
}

impl From<&V4l2CtrlH264Pps> for SafeExtControl<H264Pps> {
    fn from(pps: &V4l2CtrlH264Pps) -> Self {
        SafeExtControl::<H264Pps>::from(pps.handle)
    }
}

#[derive(Default)]
pub struct V4l2CtrlH264ScalingMatrix {
    handle: v4l2_ctrl_h264_scaling_matrix,
}

impl V4l2CtrlH264ScalingMatrix {
    pub fn new() -> Self {
        Default::default()
    }
    pub fn set(&mut self) -> &mut Self {
        todo!()
    }
}

impl From<&V4l2CtrlH264ScalingMatrix> for SafeExtControl<H264ScalingMatrix> {
    fn from(scaling_matrix: &V4l2CtrlH264ScalingMatrix) -> Self {
        SafeExtControl::<H264ScalingMatrix>::from(scaling_matrix.handle)
    }
}

#[derive(Default)]
pub struct V4l2CtrlH264DecodeParams {
    handle: v4l2_ctrl_h264_decode_params,
}

impl V4l2CtrlH264DecodeParams {
    pub fn new() -> Self {
        Default::default()
    }
    pub fn set_picture_data(&mut self, pic: &PictureData) -> &mut Self {
        self.handle.top_field_order_cnt = pic.top_field_order_cnt;
        self.handle.bottom_field_order_cnt = pic.bottom_field_order_cnt;
        self.handle.flags |= match pic.field {
            Field::Top => V4L2_H264_DECODE_PARAM_FLAG_FIELD_PIC,
            Field::Bottom => {
                V4L2_H264_DECODE_PARAM_FLAG_FIELD_PIC | V4L2_H264_DECODE_PARAM_FLAG_BOTTOM_FIELD
            }
            _ => 0,
        };
        self.handle.flags |= match pic.is_idr {
            IsIdr::Yes { idr_pic_id: _ } => V4L2_H264_DECODE_PARAM_FLAG_IDR_PIC,
            _ => 0,
        };
        self.handle.nal_ref_idc = pic.nal_ref_idc as u16;
        self
    }
    pub fn set_dpb_entries(&mut self, dpb: Vec<V4l2CtrlH264DpbEntry>) -> &mut Self {
        for i in 0..dpb.len() {
            self.handle.dpb[i] = v4l2_h264_dpb_entry::from(&dpb[i]);
        }
        self
    }
    pub fn set_slice_header(&mut self, slice_header: &SliceHeader) -> &mut Self {
        self.handle.frame_num = slice_header.frame_num;
        self.handle.idr_pic_id = slice_header.idr_pic_id;
        self.handle.pic_order_cnt_lsb = slice_header.pic_order_cnt_lsb;
        self.handle.delta_pic_order_cnt_bottom = slice_header.delta_pic_order_cnt_bottom;
        self.handle.delta_pic_order_cnt0 = slice_header.delta_pic_order_cnt[0];
        self.handle.delta_pic_order_cnt1 = slice_header.delta_pic_order_cnt[1];
        self.handle.dec_ref_pic_marking_bit_size = slice_header.dec_ref_pic_marking_bit_size as u32;
        self.handle.pic_order_cnt_bit_size = slice_header.pic_order_cnt_bit_size as u32;
        self
    }
}

impl From<&V4l2CtrlH264DecodeParams> for SafeExtControl<H264DecodeParams> {
    fn from(decode_params: &V4l2CtrlH264DecodeParams) -> Self {
        SafeExtControl::<H264DecodeParams>::from(decode_params.handle)
    }
}

pub enum V4l2CtrlH264DecodeMode {
    SliceBased = V4L2_STATELESS_H264_DECODE_MODE_SLICE_BASED as isize,
    FrameBased = V4L2_STATELESS_H264_DECODE_MODE_FRAME_BASED as isize,
}

impl From<V4l2CtrlH264DecodeMode> for SafeExtControl<H264DecodeMode> {
    fn from(decode_mode: V4l2CtrlH264DecodeMode) -> Self {
        SafeExtControl::<H264DecodeMode>::from_value(decode_mode as i32)
    }
}
