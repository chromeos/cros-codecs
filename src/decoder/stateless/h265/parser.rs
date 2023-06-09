// Copyright 2023 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

//! An Annex B h.265 parser.
//!
//! Parses VPSs, SPSs, PPSs and Slices from NALUs.

use std::collections::BTreeMap;

use anyhow::anyhow;
use anyhow::Context;
use bitreader::BitReader;
use bytes::Buf;
use enumn::N;

use crate::utils::nalu;
use crate::utils::nalu::Header;
use crate::utils::nalu_reader::NaluReader;

// Given the max VPS id.
const MAX_VPS_COUNT: usize = 16;
// Given the max SPS id.
const MAX_SPS_COUNT: usize = 16;
// Given the max PPS id.
const MAX_PPS_COUNT: usize = 64;
// 7.4.7.1
const MAX_REF_IDX_ACTIVE: u32 = 15;

// 7.4.3.2.1:
// num_short_term_ref_pic_sets specifies the number of st_ref_pic_set( ) syntax
// structures included in the SPS. The value of num_short_term_ref_pic_sets
// shall be in the range of 0 to 64, inclusive.
// NOTE 5 – A decoder should allocate memory for a total number of
// num_short_term_ref_pic_sets + 1 st_ref_pic_set( ) syntax structures since
// there may be a st_ref_pic_set( ) syntax structure directly signalled in the
// slice headers of a current picture. A st_ref_pic_set( ) syntax structure
// directly signalled in the slice headers of a current picture has an index
// equal to num_short_term_ref_pic_sets.
const MAX_SHORT_TERM_REF_PIC_SETS: usize = 65;

// From table 7-5.
const DEFAULT_SCALING_LIST_0: [u8; 16] = [0; 16];

// From Table 7-6.
const DEFAULT_SCALING_LIST_1: [u8; 64] = [
    16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 17, 16, 17, 16, 17, 18, 17, 18, 18, 17, 18, 21, 19, 20,
    21, 20, 19, 21, 24, 22, 22, 24, 24, 22, 22, 24, 25, 25, 27, 30, 27, 25, 25, 29, 31, 35, 35, 31,
    29, 36, 41, 44, 41, 36, 47, 54, 54, 47, 65, 70, 65, 88, 88, 115,
];

// From Table 7-6.
const DEFAULT_SCALING_LIST_2: [u8; 64] = [
    16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 17, 17, 17, 17, 17, 18, 18, 18, 18, 18, 18, 20, 20, 20,
    20, 20, 20, 20, 24, 24, 24, 24, 24, 24, 24, 24, 25, 25, 25, 25, 25, 25, 25, 28, 28, 28, 28, 28,
    28, 33, 33, 33, 33, 33, 41, 41, 41, 41, 54, 54, 54, 71, 71, 91,
];

/// Table 7-1 – NAL unit type codes and NAL unit type classes
#[derive(N, Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub enum NaluType {
    #[default]
    TrailN = 0,
    TrailR = 1,
    TsaN = 2,
    TsaR = 3,
    StsaN = 4,
    StsaR = 5,
    RadlN = 6,
    RadlR = 7,
    RaslN = 8,
    RaslR = 9,
    RsvVclN10 = 10,
    RsvVclR11 = 11,
    RsvVclN12 = 12,
    RsvVclR13 = 13,
    RsvVclN14 = 14,
    RsvVclR15 = 15,
    BlaWLp = 16,
    BlaWRadl = 17,
    BlaNLp = 18,
    IdrWRadl = 19,
    IdrNLp = 20,
    CraNut = 21,
    RsvIrapVcl22 = 22,
    RsvIrapVcl23 = 23,
    RsvVcl24 = 24,
    RsvVcl25 = 25,
    RsvVcl26 = 26,
    RsvVcl27 = 27,
    RsvVcl28 = 28,
    RsvVcl29 = 29,
    RsvVcl30 = 30,
    RsvVcl31 = 31,
    VpsNut = 32,
    SpsNut = 33,
    PpsNut = 34,
    AudNut = 35,
    EosNut = 36,
    EobNut = 37,
    FdNut = 38,
    PrefixSeiNut = 39,
    SuffixSeiNut = 40,
    RsvNvcl41 = 41,
    RsvNvcl42 = 42,
    RsvNvcl43 = 43,
    RsvNvcl44 = 44,
    RsvNvcl45 = 45,
    RsvNvcl46 = 46,
    RsvNvcl47 = 47,
}

impl NaluType {
    /// Whether this is an IDR NALU.
    pub fn is_idr(&self) -> bool {
        matches!(self, Self::IdrWRadl | Self::IdrNLp)
    }

    /// Whether this is an IRAP NALU.
    pub fn is_irap(&self) -> bool {
        let type_ = *self as u32;
        type_ >= Self::BlaWLp as u32 && type_ <= Self::RsvIrapVcl23 as u32
    }

    /// Whether this is a BLA NALU.
    pub fn is_bla(&self) -> bool {
        let type_ = *self as u32;
        type_ >= Self::BlaWLp as u32 && type_ <= Self::BlaNLp as u32
    }

    /// Whether this is a CRA NALU.
    pub fn is_cra(&self) -> bool {
        matches!(self, Self::CraNut)
    }

    /// Whether this is a RADL NALU.
    pub fn is_radl(&self) -> bool {
        matches!(self, Self::RadlN | Self::RadlR)
    }

    /// Whether this is a RASL NALU.
    pub fn is_rasl(&self) -> bool {
        matches!(self, Self::RaslN | Self::RaslR)
    }

    //// Whether this is a SLNR NALU.
    pub fn is_slnr(&self) -> bool {
        matches!(self, Self::RsvVclN10 | Self::RsvVclN12 | Self::RsvVclN14)
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct NaluHeader {
    /// The NALU type.
    type_: NaluType,
    /// Specifies the identifier of the layer to which a VCL NAL unit belongs or
    /// the identifier of a layer to which a non-VCL NAL unit applies.
    nuh_layer_id: u8,
    /// Minus 1 specifies a temporal identifier for the NAL unit. The value of
    /// nuh_temporal_id_plus1 shall not be equal to 0.
    nuh_temporal_id_plus1: u8,
}

impl NaluHeader {
    pub fn type_(&self) -> NaluType {
        self.type_
    }

    pub fn layer_id(&self) -> u8 {
        self.nuh_layer_id
    }

    pub fn temporal_id_plus1(&self) -> u8 {
        self.nuh_temporal_id_plus1
    }
}

impl Header for NaluHeader {
    fn parse<T: AsRef<[u8]>>(cursor: &std::io::Cursor<T>) -> anyhow::Result<Self> {
        let data = &cursor.chunk()[0..2];
        let mut r = BitReader::new(data);

        // Skip forbidden_zero_bit
        r.skip(1)?;

        Ok(Self {
            type_: NaluType::n(r.read_u32(6)?).ok_or(anyhow!("Invalid NALU type"))?,
            nuh_layer_id: r.read_u8(6)?,
            nuh_temporal_id_plus1: r.read_u8(3)?,
        })
    }

    fn is_end(&self) -> bool {
        matches!(self.type_, NaluType::EosNut | NaluType::EobNut)
    }

    fn len(&self) -> usize {
        // 7.3.1.2
        2
    }
}

pub type Nalu<T> = nalu::Nalu<T, NaluHeader>;

/// H265 levels as defined by table A.8.
/// general_level_idc and sub_layer_level_idc[ OpTid ] shall be set equal to a
/// value of 30 times the level number specified in Table A.8
#[derive(N, Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub enum Level {
    #[default]
    L1 = 30,
    L2 = 60,
    L2_1 = 63,
    L3 = 90,
    L3_1 = 93,
    L4 = 120,
    L4_1 = 123,
    L5 = 150,
    L5_1 = 153,
    L5_2 = 156,
    L6 = 180,
    L6_1 = 183,
    L6_2 = 186,
}

/// A H.265 Video Parameter Set.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Vps {
    /// Identifies the VPS for reference by other syntax elements.
    video_parameter_set_id: u8,
    /// If vps_base_layer_internal_flag is equal to 1 and
    /// vps_base_layer_available_flag is equal to 1, the base layer is present
    /// in the bitstream.
    base_layer_internal_flag: bool,
    /// See `base_layer_internal_flag`.
    base_layer_available_flag: bool,
    /// Plus 1 specifies the maximum allowed number of layers in each CVS
    /// referring to the VPS.
    max_layers_minus1: u8,
    /// Plus 1 specifies the maximum number of temporal sub-layers that may be
    /// present in each CVS referring to the VPS.
    max_sub_layers_minus1: u8,
    /// When vps_max_sub_layers_minus1 is greater than 0, specifies whether
    /// inter prediction is additionally restricted for CVSs referring to the
    /// VPS.
    temporal_id_nesting_flag: bool,
    /// ProfileTierLevel() data.
    profile_tier_level: ProfileTierLevel,
    /// When true, specifies that vps_max_dec_pic_buffering_minus1[ i ],
    /// vps_max_num_reorder_pics[ i ] and vps_max_latency_increase_plus1[ i ]
    /// are present for vps_max_sub_layers_ minus1 + 1 sub-layers.
    /// vps_sub_layer_ordering_info_present_flag equal to 0 specifies that the
    /// values of vps_max_dec_pic_buffering_minus1[ vps_max_sub_layers_minus1 ],
    /// vps_max_num_reorder_pics[ vps_max_sub_ layers_minus1 ] and
    /// vps_max_latency_increase_plus1[ vps_max_sub_layers_minus1 ] apply to all
    /// sub-layers
    sub_layer_ordering_info_present_flag: bool,
    /// max_dec_pic_buffering_minus1[i] plus 1 specifies the maximum required
    /// size of the decoded picture buffer for the CVS in units of picture
    /// storage buffers when HighestTid is equal to i.
    max_dec_pic_buffering_minus1: [u32; 7],
    /// Indicates the maximum allowed number of pictures with PicOutputFlag
    /// equal to 1 that can precede any picture with PicOutputFlag equal to 1 in
    /// the CVS in decoding order and follow that picture with PicOutputFlag
    /// equal to 1 in output order when HighestTid is equal to i.
    max_num_reorder_pics: [u32; 7],
    /// When true, max_latency_increase_plus1[i] is used to compute the value of
    /// VpsMaxLatencyPictures[ i ], which specifies the maximum number of
    /// pictures with PicOutputFlag equal to 1 that can precede any picture with
    /// PicOutputFlag equal to 1 in the CVS in output order and follow that
    /// picture with PicOutputFlag equal to 1 in decoding order when HighestTid
    /// is equal to i.
    max_latency_increase_plus1: [u32; 7],
    /// Specifies the maximum allowed value of nuh_layer_id of all NAL units in
    /// each CVS referring to the VPS.
    max_layer_id: u8,
    /// num_layer_sets_minus1 plus 1 specifies the number of layer sets that are
    /// specified by the VPS.
    num_layer_sets_minus1: u32,
    /// When true, specifies that num_units_in_tick, time_scale,
    /// poc_proportional_to_timing_flag and num_hrd_parameters are present in
    /// the VPS.
    timing_info_present_flag: bool,
    /// The number of time units of a clock operating at the frequency
    /// vps_time_scale Hz that corresponds to one increment (called a clock
    /// tick) of a clock tick counter. The value of vps_num_units_in_tick shall
    /// be greater than 0. A clock tick, in units of seconds, is equal to the
    /// quotient of vps_num_units_in_tick divided by vps_time_scale. For
    /// example, when the picture rate of a video signal is 25 Hz,
    /// vps_time_scale may be equal to 27 000 000 and vps_num_units_in_tick may
    /// be equal to 1 080 000, and consequently a clock tick may be 0.04
    /// seconds.
    num_units_in_tick: u32,
    /// The number of time units that pass in one second. For example, a time
    /// coordinate system that measures time using a 27 MHz clock has a
    /// vps_time_scale of 27 000 000.
    time_scale: u32,
    /// When true, indicates that the picture order count value for each picture
    /// in the CVS that is not the first picture in the CVS, in decoding order,
    /// is proportional to the output time of the picture relative to the output
    /// time of the first picture in the CVS.  When false, indicates that the
    /// picture order count value for each picture in the CVS that is not the
    /// first picture in the CVS, in decoding order, may or may not be
    /// proportional to the output time of the picture relative to the output
    /// time of the first picture in the CVS.
    poc_proportional_to_timing_flag: bool,
    /// num_ticks_poc_diff_one_minus1 plus 1 specifies the number of clock ticks
    /// corresponding to a difference of picture order count values equal to 1.
    num_ticks_poc_diff_one_minus1: u32,
    /// Specifies the number of hrd_parameters( ) syntax structures present in
    /// the VPS RBSP before the vps_extension_flag syntax element.
    num_hrd_parameters: u32,
    /// hrd_layer_set_idx[ i ] specifies the index, into the list of layer sets
    /// specified by the VPS, of the layer set to which the i-th hrd_parameters(
    /// ) syntax structure in the VPS applies.
    hrd_layer_set_idx: Vec<u16>,
    /// cprms_present_flag[ i ] equal to true specifies that the HRD parameters
    /// that are common for all sub-layers are present in the i-th
    /// hrd_parameters( ) syntax structure in the VPS. cprms_present_flag[ i ]
    /// equal to false specifies that the HRD parameters that are common for all
    /// sub-layers are not present in the i-th hrd_parameters( ) syntax
    /// structure in the VPS and are derived to be the same as the ( i − 1 )-th
    /// hrd_parameters( ) syntax structure in the VPS. cprms_present_flag[ 0 ]
    /// is inferred to be equal to true.
    cprms_present_flag: Vec<bool>,
    /// The hrd_parameters() data.
    hrd_parameters: Vec<HrdParams>,
    /// When false, specifies that no vps_extension_data_flag syntax elements
    /// are present in the VPS RBSP syntax structure. When true, specifies that
    /// there are vps_extension_data_flag syntax elements present in the VPS
    /// RBSP syntax structure. Decoders conforming to a profile specified in
    /// Annex A but not supporting the INBLD capability specified in Annex F
    /// shall ignore all data that follow the value 1 for vps_extension_flag in
    /// a VPS NAL unit.
    extension_flag: bool,
}

impl Vps {
    pub fn video_parameter_set_id(&self) -> u8 {
        self.video_parameter_set_id
    }

    pub fn base_layer_internal_flag(&self) -> bool {
        self.base_layer_internal_flag
    }

    pub fn base_layer_available_flag(&self) -> bool {
        self.base_layer_available_flag
    }

    pub fn max_layers_minus1(&self) -> u8 {
        self.max_layers_minus1
    }

    pub fn max_sub_layers_minus1(&self) -> u8 {
        self.max_sub_layers_minus1
    }

    pub fn temporal_id_nesting_flag(&self) -> bool {
        self.temporal_id_nesting_flag
    }

    pub fn profile_tier_level(&self) -> &ProfileTierLevel {
        &self.profile_tier_level
    }

    pub fn sub_layer_ordering_info_present_flag(&self) -> bool {
        self.sub_layer_ordering_info_present_flag
    }

    pub fn max_dec_pic_buffering_minus1(&self) -> [u32; 7] {
        self.max_dec_pic_buffering_minus1
    }

    pub fn max_num_reorder_pics(&self) -> [u32; 7] {
        self.max_num_reorder_pics
    }

    pub fn max_latency_increase_plus1(&self) -> [u32; 7] {
        self.max_latency_increase_plus1
    }

    pub fn max_layer_id(&self) -> u8 {
        self.max_layer_id
    }

    pub fn num_layer_sets_minus1(&self) -> u32 {
        self.num_layer_sets_minus1
    }

    pub fn timing_info_present_flag(&self) -> bool {
        self.timing_info_present_flag
    }

    pub fn num_units_in_tick(&self) -> u32 {
        self.num_units_in_tick
    }

    pub fn time_scale(&self) -> u32 {
        self.time_scale
    }

    pub fn poc_proportional_to_timing_flag(&self) -> bool {
        self.poc_proportional_to_timing_flag
    }

    pub fn num_ticks_poc_diff_one_minus1(&self) -> u32 {
        self.num_ticks_poc_diff_one_minus1
    }

    pub fn num_hrd_parameters(&self) -> u32 {
        self.num_hrd_parameters
    }

    pub fn hrd_layer_set_idx(&self) -> &[u16] {
        self.hrd_layer_set_idx.as_ref()
    }

    pub fn cprms_present_flag(&self) -> &[bool] {
        self.cprms_present_flag.as_ref()
    }

    pub fn hrd_parameters(&self) -> &[HrdParams] {
        self.hrd_parameters.as_ref()
    }

    pub fn extension_flag(&self) -> bool {
        self.extension_flag
    }
}

impl Default for Vps {
    fn default() -> Self {
        Self {
            video_parameter_set_id: Default::default(),
            base_layer_internal_flag: Default::default(),
            base_layer_available_flag: Default::default(),
            max_layers_minus1: Default::default(),
            max_sub_layers_minus1: Default::default(),
            temporal_id_nesting_flag: Default::default(),
            profile_tier_level: Default::default(),
            sub_layer_ordering_info_present_flag: Default::default(),
            max_dec_pic_buffering_minus1: Default::default(),
            max_num_reorder_pics: Default::default(),
            max_latency_increase_plus1: Default::default(),
            max_layer_id: Default::default(),
            num_layer_sets_minus1: Default::default(),
            timing_info_present_flag: Default::default(),
            num_units_in_tick: Default::default(),
            time_scale: Default::default(),
            poc_proportional_to_timing_flag: Default::default(),
            num_ticks_poc_diff_one_minus1: Default::default(),
            num_hrd_parameters: Default::default(),
            hrd_layer_set_idx: Default::default(),
            cprms_present_flag: vec![true],
            hrd_parameters: Default::default(),
            extension_flag: Default::default(),
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct ProfileTierLevel {
    /// Specifies the context for the interpretation of general_profile_idc and
    /// general_profile_compatibility_flag[ j ] for all values of j in the range
    /// of 0 to 31, inclusive.
    general_profile_space: u8,
    /// Specifies the tier context for the interpretation of general_level_idc
    /// as specified in Annex A.
    general_tier_flag: bool,
    /// When general_profile_space is equal to 0, indicates a profile to which
    /// the CVS conforms as specified in Annex A. Bitstreams shall not contain
    /// values of general_profile_idc other than those specified in Annex A.
    /// Other values of general_profile_idc are reserved for future use by ITU-T
    /// | ISO/IEC.
    general_profile_idc: u8,
    /// general_profile_compatibility_flag[ j ] equal to true, when
    /// general_profile_space is false, indicates that the CVS conforms to the
    /// profile indicated by general_profile_idc equal to j as specified in
    /// Annex A.
    general_profile_compatibility_flag: [bool; 32],
    /// general_progressive_source_flag and general_interlaced_source_flag are
    /// interpreted as follows:
    ///
    /// –If general_progressive_source_flag is true and
    /// general_interlaced_source_flag is false, the source scan type of the
    /// pictures in the CVS should be interpreted as progressive only.
    ///
    /// –Otherwise, if general_progressive_source_flag is false and
    /// general_interlaced_source_flag is true, the source scan type of the
    /// pictures in the CVS should be interpreted as interlaced only.
    ///
    /// –Otherwise, if general_progressive_source_flag is false and
    /// general_interlaced_source_flag is false, the source scan type of the
    /// pictures in the CVS should be interpreted as unknown or unspecified.
    ///
    /// –Otherwise (general_progressive_source_flag is true and
    /// general_interlaced_source_flag is true), the source scan type of each
    /// picture in the CVS is indicated at the picture level using the syntax
    /// element source_scan_type in a picture timing SEI message.
    general_progressive_source_flag: bool,
    /// See `general_progressive_source_flag`.
    general_interlaced_source_flag: bool,
    /// If true, specifies that there are no frame packing arrangement SEI
    /// messages, segmented rectangular frame packing arrangement SEI messages,
    /// equirectangular projection SEI messages, or cubemap projection SEI
    /// messages present in the CVS. If false, indicates that there may or may
    /// not be one or more frame packing arrangement SEI messages, segmented
    /// rectangular frame packing arrangement SEI messages, equirectangular
    /// projection SEI messages, or cubemap projection SEI messages present in
    /// the CVS.
    general_non_packed_constraint_flag: bool,
    /// When true, specifies that field_seq_flag is false. When false, indicates
    /// that field_seq_flag may or may not be false.
    general_frame_only_constraint_flag: bool,
    /// See Annex A.
    general_max_12bit_constraint_flag: bool,
    /// See Annex A.
    general_max_10bit_constraint_flag: bool,
    /// See Annex A.
    general_max_8bit_constraint_flag: bool,
    /// See Annex A.
    general_max_422chroma_constraint_flag: bool,
    /// See Annex A.
    general_max_420chroma_constraint_flag: bool,
    /// See Annex A.
    general_max_monochrome_constraint_flag: bool,
    /// See Annex A.
    general_intra_constraint_flag: bool,
    /// See Annex A.
    general_lower_bit_rate_constraint_flag: bool,
    /// See Annex A.
    general_max_14bit_constraint_flag: bool,
    /// See Annex A.
    general_one_picture_only_constraint_flag: bool,
    /// When true, specifies that the INBLD capability as specified in Annex F
    /// is required for decoding of the layer to which the profile_tier_level( )
    /// syntax structure applies. When false, specifies that the INBLD
    /// capability as specified in Annex F is not required for decoding of the
    /// layer to which the profile_tier_level( ) syntax structure applies.
    general_inbld_flag: bool,
    /// Indicates a level to which the CVS conforms as specified in Annex A.
    general_level_idc: u8,
    /// Sub-layer syntax element.
    sub_layer_profile_present_flag: [bool; 6],
    /// Sub-layer syntax element.
    sub_layer_level_present_flag: [bool; 6],
    /// Sub-layer syntax element.
    sub_layer_profile_space: [u8; 6],
    /// Sub-layer syntax element.
    sub_layer_tier_flag: [bool; 6],
    /// Sub-layer syntax element.
    sub_layer_profile_idc: [u8; 6],
    /// Sub-layer syntax element.
    sub_layer_profile_compatibility_flag: [[bool; 32]; 6],
    /// Sub-layer syntax element.
    sub_layer_progressive_source_flag: [bool; 6],
    /// Sub-layer syntax element.
    sub_layer_interlaced_source_flag: [bool; 6],
    /// Sub-layer syntax element.
    sub_layer_non_packed_constraint_flag: [bool; 6],
    /// Sub-layer syntax element.
    sub_layer_frame_only_constraint_flag: [bool; 6],
    /// Sub-layer syntax element.
    sub_layer_max_12bit_constraint_flag: [bool; 6],
    /// Sub-layer syntax element.
    sub_layer_max_10bit_constraint_flag: [bool; 6],
    /// Sub-layer syntax element.
    sub_layer_max_8bit_constraint_flag: [bool; 6],
    /// Sub-layer syntax element.
    sub_layer_max_422chroma_constraint_flag: [bool; 6],
    /// Sub-layer syntax element.
    sub_layer_max_420chroma_constraint_flag: [bool; 6],
    /// Sub-layer syntax element.
    sub_layer_max_monochrome_constraint_flag: [bool; 6],
    /// Sub-layer syntax element.
    sub_layer_intra_constraint_flag: [bool; 6],
    /// Sub-layer syntax element.
    sub_layer_one_picture_only_constraint_flag: [bool; 6],
    /// Sub-layer syntax element.
    sub_layer_lower_bit_rate_constraint_flag: [bool; 6],
    /// Sub-layer syntax element.
    sub_layer_max_14bit_constraint_flag: [bool; 6],
    /// Sub-layer syntax element.
    sub_layer_inbld_flag: [bool; 6],
    /// Sub-layer syntax element.
    sub_layer_level_idc: [u8; 6],
}

impl ProfileTierLevel {
    pub fn general_profile_space(&self) -> u8 {
        self.general_profile_space
    }

    pub fn general_tier_flag(&self) -> bool {
        self.general_tier_flag
    }

    pub fn general_profile_idc(&self) -> u8 {
        self.general_profile_idc
    }

    pub fn general_profile_compatibility_flag(&self) -> [bool; 32] {
        self.general_profile_compatibility_flag
    }

    pub fn general_progressive_source_flag(&self) -> bool {
        self.general_progressive_source_flag
    }

    pub fn general_interlaced_source_flag(&self) -> bool {
        self.general_interlaced_source_flag
    }

    pub fn general_non_packed_constraint_flag(&self) -> bool {
        self.general_non_packed_constraint_flag
    }

    pub fn general_frame_only_constraint_flag(&self) -> bool {
        self.general_frame_only_constraint_flag
    }

    pub fn general_max_12bit_constraint_flag(&self) -> bool {
        self.general_max_12bit_constraint_flag
    }

    pub fn general_max_10bit_constraint_flag(&self) -> bool {
        self.general_max_10bit_constraint_flag
    }

    pub fn general_max_8bit_constraint_flag(&self) -> bool {
        self.general_max_8bit_constraint_flag
    }

    pub fn general_max_422chroma_constraint_flag(&self) -> bool {
        self.general_max_422chroma_constraint_flag
    }

    pub fn general_max_420chroma_constraint_flag(&self) -> bool {
        self.general_max_420chroma_constraint_flag
    }

    pub fn general_max_monochrome_constraint_flag(&self) -> bool {
        self.general_max_monochrome_constraint_flag
    }

    pub fn general_intra_constraint_flag(&self) -> bool {
        self.general_intra_constraint_flag
    }

    pub fn general_lower_bit_rate_constraint_flag(&self) -> bool {
        self.general_lower_bit_rate_constraint_flag
    }

    pub fn general_max_14bit_constraint_flag(&self) -> bool {
        self.general_max_14bit_constraint_flag
    }

    pub fn general_one_picture_only_constraint_flag(&self) -> bool {
        self.general_one_picture_only_constraint_flag
    }

    pub fn general_inbld_flag(&self) -> bool {
        self.general_inbld_flag
    }

    pub fn general_level_idc(&self) -> u8 {
        self.general_level_idc
    }

    pub fn sub_layer_profile_present_flag(&self) -> [bool; 6] {
        self.sub_layer_profile_present_flag
    }

    pub fn sub_layer_level_present_flag(&self) -> [bool; 6] {
        self.sub_layer_level_present_flag
    }

    pub fn sub_layer_profile_space(&self) -> [u8; 6] {
        self.sub_layer_profile_space
    }

    pub fn sub_layer_tier_flag(&self) -> [bool; 6] {
        self.sub_layer_tier_flag
    }

    pub fn sub_layer_profile_idc(&self) -> [u8; 6] {
        self.sub_layer_profile_idc
    }

    pub fn sub_layer_profile_compatibility_flag(&self) -> [[bool; 32]; 6] {
        self.sub_layer_profile_compatibility_flag
    }

    pub fn sub_layer_progressive_source_flag(&self) -> [bool; 6] {
        self.sub_layer_progressive_source_flag
    }

    pub fn sub_layer_interlaced_source_flag(&self) -> [bool; 6] {
        self.sub_layer_interlaced_source_flag
    }

    pub fn sub_layer_non_packed_constraint_flag(&self) -> [bool; 6] {
        self.sub_layer_non_packed_constraint_flag
    }

    pub fn sub_layer_frame_only_constraint_flag(&self) -> [bool; 6] {
        self.sub_layer_frame_only_constraint_flag
    }

    pub fn sub_layer_max_12bit_constraint_flag(&self) -> [bool; 6] {
        self.sub_layer_max_12bit_constraint_flag
    }

    pub fn sub_layer_max_10bit_constraint_flag(&self) -> [bool; 6] {
        self.sub_layer_max_10bit_constraint_flag
    }

    pub fn sub_layer_max_8bit_constraint_flag(&self) -> [bool; 6] {
        self.sub_layer_max_8bit_constraint_flag
    }

    pub fn sub_layer_max_422chroma_constraint_flag(&self) -> [bool; 6] {
        self.sub_layer_max_422chroma_constraint_flag
    }

    pub fn sub_layer_max_420chroma_constraint_flag(&self) -> [bool; 6] {
        self.sub_layer_max_420chroma_constraint_flag
    }

    pub fn sub_layer_max_monochrome_constraint_flag(&self) -> [bool; 6] {
        self.sub_layer_max_monochrome_constraint_flag
    }

    pub fn sub_layer_intra_constraint_flag(&self) -> [bool; 6] {
        self.sub_layer_intra_constraint_flag
    }

    pub fn sub_layer_one_picture_only_constraint_flag(&self) -> [bool; 6] {
        self.sub_layer_one_picture_only_constraint_flag
    }

    pub fn sub_layer_lower_bit_rate_constraint_flag(&self) -> [bool; 6] {
        self.sub_layer_lower_bit_rate_constraint_flag
    }

    pub fn sub_layer_max_14bit_constraint_flag(&self) -> [bool; 6] {
        self.sub_layer_max_14bit_constraint_flag
    }

    pub fn sub_layer_inbld_flag(&self) -> [bool; 6] {
        self.sub_layer_inbld_flag
    }

    pub fn sub_layer_level_idc(&self) -> [u8; 6] {
        self.sub_layer_level_idc
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct SpsRangeExtension {
    transform_skip_rotation_enabled_flag: bool,
    transform_skip_context_enabled_flag: bool,
    implicit_rdpcm_enabled_flag: bool,
    explicit_rdpcm_enabled_flag: bool,
    extended_precision_processing_flag: bool,
    intra_smoothing_disabled_flag: bool,
    high_precision_offsets_enabled_flag: bool,
    persistent_rice_adaptation_enabled_flag: bool,
    cabac_bypass_alignment_enabled_flag: bool,
}

impl SpsRangeExtension {
    pub fn transform_skip_rotation_enabled_flag(&self) -> bool {
        self.transform_skip_rotation_enabled_flag
    }

    pub fn transform_skip_context_enabled_flag(&self) -> bool {
        self.transform_skip_context_enabled_flag
    }

    pub fn implicit_rdpcm_enabled_flag(&self) -> bool {
        self.implicit_rdpcm_enabled_flag
    }

    pub fn explicit_rdpcm_enabled_flag(&self) -> bool {
        self.explicit_rdpcm_enabled_flag
    }

    pub fn extended_precision_processing_flag(&self) -> bool {
        self.extended_precision_processing_flag
    }

    pub fn intra_smoothing_disabled_flag(&self) -> bool {
        self.intra_smoothing_disabled_flag
    }

    pub fn high_precision_offsets_enabled_flag(&self) -> bool {
        self.high_precision_offsets_enabled_flag
    }

    pub fn persistent_rice_adaptation_enabled_flag(&self) -> bool {
        self.persistent_rice_adaptation_enabled_flag
    }

    pub fn cabac_bypass_alignment_enabled_flag(&self) -> bool {
        self.cabac_bypass_alignment_enabled_flag
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SpsSccExtension {
    /// When set, specifies that a picture in the CVS may be included in a
    /// reference picture list of a slice of the picture itself.  When not set,
    /// specifies that a picture in the CVS is never included in a reference
    /// picture list of a slice of the picture itself.
    curr_pic_ref_enabled_flag: bool,
    /// When set, specifies that the decoding process for palette mode may be
    /// used for intra blocks. When not set, specifies that the decoding process
    /// for palette mode is not applied.
    palette_mode_enabled_flag: bool,
    /// Specifies the maximum allowed palette size.
    palette_max_size: u8,
    /// Specifies the difference between the maximum allowed palette predictor
    /// size and the maximum allowed palette size.
    delta_palette_max_predictor_size: u8,
    /// When set, specifies that the sequence palette predictors are initialized
    /// using the sps_palette_predictor_initializers. When not set, specifies
    /// that the entries in the sequence palette predictor are initialized to 0.
    palette_predictor_initializers_present_flag: bool,
    /// num_palette_predictor_initializers_minus1 plus 1 specifies the number of
    /// entries in the sequence palette predictor initializer.
    num_palette_predictor_initializer_minus1: u8,
    /// palette_predictor_initializer[ comp ][ i ] specifies the value of the
    /// comp-th component of the i-th palette entry in the SPS that is used to
    /// initialize the array PredictorPaletteEntries.
    palette_predictor_initializer: [[u32; 128]; 3],
    /// Controls the presence and inference of the use_integer_mv_flag that
    /// specifies the resolution of motion vectors for inter prediction.
    motion_vector_resolution_control_idc: u8,
    /// When set, specifies that the intra boundary filtering process is
    /// unconditionally disabled for intra prediction.  If not set, specifies
    /// that the intra boundary filtering process may be used.
    intra_boundary_filtering_disabled_flag: bool,
}

impl SpsSccExtension {
    pub fn curr_pic_ref_enabled_flag(&self) -> bool {
        self.curr_pic_ref_enabled_flag
    }

    pub fn palette_mode_enabled_flag(&self) -> bool {
        self.palette_mode_enabled_flag
    }

    pub fn palette_max_size(&self) -> u8 {
        self.palette_max_size
    }

    pub fn delta_palette_max_predictor_size(&self) -> u8 {
        self.delta_palette_max_predictor_size
    }

    pub fn palette_predictor_initializers_present_flag(&self) -> bool {
        self.palette_predictor_initializers_present_flag
    }

    pub fn num_palette_predictor_initializer_minus1(&self) -> u8 {
        self.num_palette_predictor_initializer_minus1
    }

    pub fn palette_predictor_initializer(&self) -> [[u32; 128]; 3] {
        self.palette_predictor_initializer
    }

    pub fn motion_vector_resolution_control_idc(&self) -> u8 {
        self.motion_vector_resolution_control_idc
    }

    pub fn intra_boundary_filtering_disabled_flag(&self) -> bool {
        self.intra_boundary_filtering_disabled_flag
    }
}

impl Default for SpsSccExtension {
    fn default() -> Self {
        Self {
            curr_pic_ref_enabled_flag: Default::default(),
            palette_mode_enabled_flag: Default::default(),
            palette_max_size: Default::default(),
            delta_palette_max_predictor_size: Default::default(),
            palette_predictor_initializers_present_flag: Default::default(),
            num_palette_predictor_initializer_minus1: Default::default(),
            palette_predictor_initializer: [[0; 128]; 3],
            motion_vector_resolution_control_idc: Default::default(),
            intra_boundary_filtering_disabled_flag: Default::default(),
        }
    }
}

/// A H.265 Sequence Parameter Set.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Sps {
    /// Specifies the value of the vps_video_parameter_set_id of the active VPS.
    video_parameter_set_id: u8,
    /// `max_sub_layers_minus1` plus 1 specifies the maximum number of temporal
    /// sub-layers that may be present in each CVS referring to the SPS.
    max_sub_layers_minus1: u8,
    /// When sps_max_sub_layers_minus1 is greater than 0, specifies whether
    /// inter prediction is additionally restricted for CVSs referring to the
    /// SPS.
    temporal_id_nesting_flag: bool,
    /// profile_tier_level() data.
    profile_tier_level: ProfileTierLevel,
    /// Provides an identifier for the SPS for reference by other syntax
    /// elements.
    seq_parameter_set_id: u8,
    /// Specifies the chroma sampling relative to the luma sampling as specified
    /// in clause 6.2.
    chroma_format_idc: u8,
    /// When true, specifies that the three colour components of the 4:4:4
    /// chroma format are coded separately. When false, specifies that the
    /// colour components are not coded separately.
    separate_colour_plane_flag: bool,
    /// Specifies the width of each decoded picture in units of luma samples.
    pic_width_in_luma_samples: u16,
    /// Specifies the height of each decoded picture in units of luma samples.
    pic_height_in_luma_samples: u16,
    /// When true, indicates that the conformance cropping window offset
    /// parameters follow next in the SPS. When false, indicates that the
    /// conformance cropping window offset parameters are not present.
    conformance_window_flag: bool,
    /* if conformance_window_flag */
    /// Specify the samples of the pictures in the CVS that are output from the
    /// decoding process, in terms of a rectangular region specified in picture
    /// coordinates for output.
    conf_win_left_offset: u32,
    conf_win_right_offset: u32,
    conf_win_top_offset: u32,
    conf_win_bottom_offset: u32,

    /// Specifies the bit depth of the samples of the luma array BitDepthY and
    /// the value of the luma quantization parameter range offset QpBdOffsetY.
    bit_depth_luma_minus8: u8,
    /// Specifies the bit depth of the samples of the chroma arrays BitDepthC
    /// and the value of the chroma quantization parameter range offset
    /// QpBdOffsetC.
    bit_depth_chroma_minus8: u8,
    /// Specifies the value of the variable MaxPicOrderCntLsb that is used in
    /// the decoding process for picture order count.
    log2_max_pic_order_cnt_lsb_minus4: u8,
    /// When true, specifies that max_dec_pic_buffering_minus1[ i ],
    /// max_num_reorder_pics[ i ] and max_latency_increase_plus1[ i ] are
    /// present for max_sub_layers_minus1 + 1 sub- layers. When false, specifies
    /// that the values of max_dec_pic_ buffering_minus1[ max_sub_layers_minus1
    /// ], max_num_reorder_pics[ max_sub_layers_minus1 ] and max_
    /// latency_increase_plus1[ max_sub_layers_minus1 ] apply to all sub-layers.
    sub_layer_ordering_info_present_flag: bool,
    /// max_dec_pic_buffering_minus1[ i ] plus 1 specifies the maximum required
    /// size of the decoded picture buffer for the CVS in units of picture
    /// storage buffers when HighestTid is equal to i.
    max_dec_pic_buffering_minus1: [u8; 7],
    /// max_num_reorder_pics[ i ] indicates the maximum allowed number of
    /// pictures with PicOutputFlag equal to 1 that can precede any picture with
    /// PicOutputFlag equal to 1 in the CVS in decoding order and follow that
    /// picture with PicOutputFlag equal to 1 in output order when HighestTid is
    /// equal to i.
    max_num_reorder_pics: [u8; 7],
    /// max_latency_increase_plus1[ i ] not equal to 0 is used to compute the
    /// value of SpsMaxLatencyPictures[ i ], which specifies the maximum number
    /// of pictures with PicOutputFlag equal to 1 that can precede any picture
    /// with PicOutputFlag equal to 1 in the CVS in output order and follow that
    /// picture with PicOutputFlag equal to 1 in decoding order when HighestTid
    /// is equal to i.
    max_latency_increase_plus1: [u8; 7],
    /// min_luma_coding_block_size_minus3 plus 3 specifies the minimum luma
    /// coding block size.
    log2_min_luma_coding_block_size_minus3: u8,
    /// Specifies the difference between the maximum and minimum luma coding
    /// block size.
    log2_diff_max_min_luma_coding_block_size: u8,
    /// min_luma_transform_block_size_minus2 plus 2 specifies the minimum luma
    /// transform block size.
    log2_min_luma_transform_block_size_minus2: u8,
    /// Specifies the difference between the maximum and minimum luma transform
    /// block size.
    log2_diff_max_min_luma_transform_block_size: u8,
    /// Specifies the maximum hierarchy depth for transform units of coding
    /// units coded in inter prediction mode.
    max_transform_hierarchy_depth_inter: u8,
    /// Specifies the maximum hierarchy depth for transform units of coding
    /// units coded in intra prediction mode.
    max_transform_hierarchy_depth_intra: u8,
    /// When true, specifies that a scaling list is used for the scaling process
    /// for transform coefficients. When false, specifies that scaling list is
    /// not used for the scaling process for transform coefficients.
    scaling_list_enabled_flag: bool,
    /* if scaling_list_enabled_flag */
    /// When true, specifies that the scaling_list_data( ) syntax structure is
    /// present in the SPS. When false, specifies that the scaling_list_data( )
    /// syntax structure is not present in the SPS.
    scaling_list_data_present_flag: bool,
    /// The scaling_list_data() syntax data.
    scaling_list: ScalingLists,
    /// When true, specifies that asymmetric motion partitions, i.e., PartMode
    /// equal to PART_2NxnU, PART_2NxnD, PART_nLx2N or PART_nRx2N, may be used
    /// in CTBs. When false, specifies that asymmetric motion partitions cannot
    /// be used in CTBs.
    amp_enabled_flag: bool,
    /// When true, specifies that the sample adaptive offset process is applied
    /// to the reconstructed picture after the deblocking filter process.  When
    /// false, specifies that the sample adaptive offset process is not applied
    /// to the reconstructed picture after the deblocking filter process.
    sample_adaptive_offset_enabled_flag: bool,
    /// When false, specifies that PCM-related syntax
    /// (pcm_sample_bit_depth_luma_minus1, pcm_sample_ bit_depth_chroma_minus1,
    /// log2_min_pcm_luma_coding_block_size_minus3, log2_diff_max_min_pcm_luma_
    /// coding_block_size, pcm_loop_filter_disabled_flag, pcm_flag,
    /// pcm_alignment_zero_bit syntax elements and pcm_sample( ) syntax
    /// structure) is not present in the CVS.
    pcm_enabled_flag: bool,

    /* if pcm_enabled_flag */
    pcm_sample_bit_depth_luma_minus1: u8,
    /// Specifies the number of bits used to represent each of PCM sample values
    /// of the luma component.
    pcm_sample_bit_depth_chroma_minus1: u8,
    /// Specifies the number of bits used to represent each of PCM sample values
    /// of the chroma components.
    log2_min_pcm_luma_coding_block_size_minus3: u8,
    /// Specifies the difference between the maximum and minimum size of coding
    /// blocks with pcm_flag equal to true.
    log2_diff_max_min_pcm_luma_coding_block_size: u8,
    /// Specifies whether the loop filter process is disabled on reconstructed
    /// samples in a coding unit with pcm_flag equal to true as follows:
    ///
    /// – If pcm_loop_filter_disabled_flag is set, the deblocking filter and
    /// sample adaptive offset filter processes on the reconstructed samples in
    /// a coding unit with pcm_flag set are disabled.
    ///
    /// – Otherwise (pcm_loop_filter_disabled_flag value is not set), the
    /// deblocking filter and sample adaptive offset filter processes on the
    /// reconstructed samples in a coding unit with pcm_flag set are not
    /// disabled.
    pcm_loop_filter_disabled_flag: bool,
    /// Specifies the number of st_ref_pic_set( ) syntax structures included in
    /// the SPS.
    num_short_term_ref_pic_sets: u8,
    /// the st_ref_pic_set() data.
    short_term_ref_pic_set: Vec<ShortTermRefPicSet>,
    /// If unset, specifies that no long-term reference picture is used for
    /// inter prediction of any coded picture in the CVS.
    /// If set, specifies that long-term reference pictures may be used for
    /// inter prediction of one or more coded pictures in the CVS.
    long_term_ref_pics_present_flag: bool,

    /* if long_term_ref_pics_present_flag */
    /// Specifies the number of candidate long-term reference pictures that are
    /// specified in the SPS.
    num_long_term_ref_pics_sps: u8,
    /// lt_ref_pic_poc_lsb_sps[ i ] specifies the picture order count modulo
    /// MaxPicOrderCntLsb of the i-th candidate long-term reference picture
    /// specified in the SPS.
    lt_ref_pic_poc_lsb_sps: Vec<u32>,
    /// used_by_curr_pic_lt_sps_flag[ i ] equal to false specifies that the i-th
    /// candidate long-term reference picture specified in the SPS is not used
    /// for reference by a picture that includes in its long-term reference
    /// picture set (RPS) the i-th candidate long-term reference picture
    /// specified in the SPS.
    used_by_curr_pic_lt_sps_flag: Vec<bool>,
    /// When set, specifies that slice_temporal_mvp_enabled_flag is present in
    /// the slice headers of non-IDR pictures in the CVS. When not set,
    /// specifies that slice_temporal_mvp_enabled_flag is not present in slice
    /// headers and that temporal motion vector predictors are not used in the
    /// CVS.
    temporal_mvp_enabled_flag: bool,
    /// When set, specifies that bi-linear interpolation is conditionally used
    /// in the intraprediction filtering process in the CVS as specified in
    /// clause 8.4.4.2.3.
    strong_intra_smoothing_enabled_flag: bool,
    /// When set, specifies that the vui_parameters( ) syntax structure as
    /// specified in Annex E is present. When not set, specifies that the
    /// vui_parameters( ) syntax structure as specified in Annex E is not
    /// present.
    vui_parameters_present_flag: bool,
    /// The vui_parameters() data.
    vui_parameters: VuiParams,
    /// When set, specifies that the syntax elements sps_range_extension_flag,
    /// sps_multilayer_extension_flag, sps_3d_extension_flag,
    /// sps_scc_extension_flag, and sps_extension_4bits are present in the SPS
    /// RBSP syntax structure. When not set, specifies that these syntax
    /// elements are not present.
    extension_present_flag: bool,

    range_extension_flag: bool,
    /// The sps_range_extension() data.
    range_extension: SpsRangeExtension,
    /// When set, specifies that the sps_scc_extension( ) syntax structure is
    /// present in the SPS RBSP syntax structure. When not set, specifies that
    /// this syntax structure is not present
    scc_extension_flag: bool,
    /// The sps_scc_extension() data.
    scc_extension: SpsSccExtension,

    // Internal H265 variables. Computed from the bitstream.
    /// Equivalent to MinCbLog2SizeY in the specification.
    min_cb_log2_size_y: u32,
    /// Equivalent to CtbLog2SizeY in the specification.
    ctb_log2_size_y: u32,
    /// Equivalent to CtbSizeY in the specification.
    ctb_size_y: u32,
    /// Equivalent to PicHeightInCtbsY in the specification.
    pic_height_in_ctbs_y: u32,
    /// Equivalent to PicWidthInCtbsY in the specification.
    pic_width_in_ctbs_y: u32,
    /// Equivalent to PicSizeInCtbsY in the specification.
    pic_size_in_ctbs_y: u32,
    /// Equivalent to ChromaArrayType in the specification.
    chroma_array_type: u8,
    /// Equivalent to WpOffsetHalfRangeY in the specification.
    wp_offset_half_range_y: u32,
    /// Equivalent to WpOffsetHalfRangeC in the specification.
    wp_offset_half_range_c: u32,
    /// Equivalent to MaxTbLog2SizeY in the specification.
    max_tb_log2_size_y: u32,
}

impl Sps {
    pub fn video_parameter_set_id(&self) -> u8 {
        self.video_parameter_set_id
    }

    pub fn max_sub_layers_minus1(&self) -> u8 {
        self.max_sub_layers_minus1
    }

    pub fn temporal_id_nesting_flag(&self) -> bool {
        self.temporal_id_nesting_flag
    }

    pub fn profile_tier_level(&self) -> &ProfileTierLevel {
        &self.profile_tier_level
    }

    pub fn seq_parameter_set_id(&self) -> u8 {
        self.seq_parameter_set_id
    }

    pub fn chroma_format_idc(&self) -> u8 {
        self.chroma_format_idc
    }

    pub fn separate_colour_plane_flag(&self) -> bool {
        self.separate_colour_plane_flag
    }

    pub fn pic_width_in_luma_samples(&self) -> u16 {
        self.pic_width_in_luma_samples
    }

    pub fn pic_height_in_luma_samples(&self) -> u16 {
        self.pic_height_in_luma_samples
    }

    pub fn conformance_window_flag(&self) -> bool {
        self.conformance_window_flag
    }

    pub fn conf_win_left_offset(&self) -> u32 {
        self.conf_win_left_offset
    }

    pub fn conf_win_right_offset(&self) -> u32 {
        self.conf_win_right_offset
    }

    pub fn conf_win_top_offset(&self) -> u32 {
        self.conf_win_top_offset
    }

    pub fn conf_win_bottom_offset(&self) -> u32 {
        self.conf_win_bottom_offset
    }

    pub fn bit_depth_luma_minus8(&self) -> u8 {
        self.bit_depth_luma_minus8
    }

    pub fn bit_depth_chroma_minus8(&self) -> u8 {
        self.bit_depth_chroma_minus8
    }

    pub fn log2_max_pic_order_cnt_lsb_minus4(&self) -> u8 {
        self.log2_max_pic_order_cnt_lsb_minus4
    }

    pub fn sub_layer_ordering_info_present_flag(&self) -> bool {
        self.sub_layer_ordering_info_present_flag
    }

    pub fn max_dec_pic_buffering_minus1(&self) -> [u8; 7] {
        self.max_dec_pic_buffering_minus1
    }

    pub fn max_num_reorder_pics(&self) -> [u8; 7] {
        self.max_num_reorder_pics
    }

    pub fn max_latency_increase_plus1(&self) -> [u8; 7] {
        self.max_latency_increase_plus1
    }

    pub fn log2_min_luma_coding_block_size_minus3(&self) -> u8 {
        self.log2_min_luma_coding_block_size_minus3
    }

    pub fn log2_diff_max_min_luma_coding_block_size(&self) -> u8 {
        self.log2_diff_max_min_luma_coding_block_size
    }

    pub fn log2_min_luma_transform_block_size_minus2(&self) -> u8 {
        self.log2_min_luma_transform_block_size_minus2
    }

    pub fn log2_diff_max_min_luma_transform_block_size(&self) -> u8 {
        self.log2_diff_max_min_luma_transform_block_size
    }

    pub fn max_transform_hierarchy_depth_inter(&self) -> u8 {
        self.max_transform_hierarchy_depth_inter
    }

    pub fn max_transform_hierarchy_depth_intra(&self) -> u8 {
        self.max_transform_hierarchy_depth_intra
    }

    pub fn scaling_list_enabled_flag(&self) -> bool {
        self.scaling_list_enabled_flag
    }

    pub fn scaling_list_data_present_flag(&self) -> bool {
        self.scaling_list_data_present_flag
    }

    pub fn scaling_list(&self) -> &ScalingLists {
        &self.scaling_list
    }

    pub fn amp_enabled_flag(&self) -> bool {
        self.amp_enabled_flag
    }

    pub fn sample_adaptive_offset_enabled_flag(&self) -> bool {
        self.sample_adaptive_offset_enabled_flag
    }

    pub fn pcm_enabled_flag(&self) -> bool {
        self.pcm_enabled_flag
    }

    pub fn pcm_sample_bit_depth_luma_minus1(&self) -> u8 {
        self.pcm_sample_bit_depth_luma_minus1
    }

    pub fn pcm_sample_bit_depth_chroma_minus1(&self) -> u8 {
        self.pcm_sample_bit_depth_chroma_minus1
    }

    pub fn log2_min_pcm_luma_coding_block_size_minus3(&self) -> u8 {
        self.log2_min_pcm_luma_coding_block_size_minus3
    }

    pub fn log2_diff_max_min_pcm_luma_coding_block_size(&self) -> u8 {
        self.log2_diff_max_min_pcm_luma_coding_block_size
    }

    pub fn pcm_loop_filter_disabled_flag(&self) -> bool {
        self.pcm_loop_filter_disabled_flag
    }

    pub fn num_short_term_ref_pic_sets(&self) -> u8 {
        self.num_short_term_ref_pic_sets
    }

    pub fn short_term_ref_pic_set(&self) -> &[ShortTermRefPicSet] {
        self.short_term_ref_pic_set.as_ref()
    }

    pub fn long_term_ref_pics_present_flag(&self) -> bool {
        self.long_term_ref_pics_present_flag
    }

    pub fn num_long_term_ref_pics_sps(&self) -> u8 {
        self.num_long_term_ref_pics_sps
    }

    pub fn lt_ref_pic_poc_lsb_sps(&self) -> &[u32] {
        self.lt_ref_pic_poc_lsb_sps.as_ref()
    }

    pub fn used_by_curr_pic_lt_sps_flag(&self) -> &[bool] {
        self.used_by_curr_pic_lt_sps_flag.as_ref()
    }

    pub fn temporal_mvp_enabled_flag(&self) -> bool {
        self.temporal_mvp_enabled_flag
    }

    pub fn strong_intra_smoothing_enabled_flag(&self) -> bool {
        self.strong_intra_smoothing_enabled_flag
    }

    pub fn vui_parameters_present_flag(&self) -> bool {
        self.vui_parameters_present_flag
    }

    pub fn vui_parameters(&self) -> &VuiParams {
        &self.vui_parameters
    }

    pub fn extension_present_flag(&self) -> bool {
        self.extension_present_flag
    }

    pub fn range_extension(&self) -> &SpsRangeExtension {
        &self.range_extension
    }

    pub fn min_cb_log2_size_y(&self) -> u32 {
        self.min_cb_log2_size_y
    }

    pub fn ctb_log2_size_y(&self) -> u32 {
        self.ctb_log2_size_y
    }

    pub fn ctb_size_y(&self) -> u32 {
        self.ctb_size_y
    }

    pub fn pic_height_in_ctbs_y(&self) -> u32 {
        self.pic_height_in_ctbs_y
    }

    pub fn pic_width_in_ctbs_y(&self) -> u32 {
        self.pic_width_in_ctbs_y
    }

    pub fn pic_size_in_ctbs_y(&self) -> u32 {
        self.pic_size_in_ctbs_y
    }

    pub fn chroma_array_type(&self) -> u8 {
        self.chroma_array_type
    }

    pub fn wp_offset_half_range_y(&self) -> u32 {
        self.wp_offset_half_range_y
    }

    pub fn wp_offset_half_range_c(&self) -> u32 {
        self.wp_offset_half_range_c
    }

    pub fn max_tb_log2_size_y(&self) -> u32 {
        self.max_tb_log2_size_y
    }

    pub fn scc_extension(&self) -> &SpsSccExtension {
        &self.scc_extension
    }

    pub fn scc_extension_flag(&self) -> bool {
        self.scc_extension_flag
    }

    pub fn range_extension_flag(&self) -> bool {
        self.range_extension_flag
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PpsSccExtension {
    /// When set, specifies that a picture referring to the PPS may be included
    /// in a reference picture list of a slice of the picture itself.  If not
    /// set, specifies that a picture referring to the PPS is never included in
    /// a reference picture list of a slice of the picture itself.
    curr_pic_ref_enabled_flag: bool,
    /// When set, specifies that an adaptive colour transform may be applied to
    /// the residual in the decoding process. When not set, specifies that
    /// adaptive colour transform is not applied to the residual.
    residual_adaptive_colour_transform_enabled_flag: bool,
    /// When set, specifies that slice_act_y_qp_offset, slice_act_cb_qp_offset,
    /// slice_act_cr_qp_offset are present in the slice header.  When not set,
    /// specifies that slice_act_y_qp_offset, slice_act_cb_qp_offset,
    /// slice_act_cr_qp_offset are not present in the slice header.
    slice_act_qp_offsets_present_flag: bool,
    /// See the specificartion for more details.
    act_y_qp_offset_plus5: i8,
    /// See the specificartion for more details.
    act_cb_qp_offset_plus5: i8,
    /// See the specificartion for more details.
    act_cr_qp_offset_plus3: i8,
    /// When set, specifies that the palette predictor initializers used for the
    /// pictures referring to the PPS are derived based on the palette predictor
    /// initializers specified by the PPS. If not set, specifies that the
    /// palette predictor initializers used for the pictures referring to the
    /// PPS are inferred to be equal to those specified by the active SPS.
    palette_predictor_initializers_present_flag: bool,
    /// Specifies the number of entries in the picture palette predictor
    /// initializer.
    num_palette_predictor_initializers: u8,
    /// When set, specifies that the pictures that refer to this PPS are
    /// monochrome. If not set, specifies that the pictures that refer to this
    /// PPS have multiple components.
    monochrome_palette_flag: bool,
    /// luma_bit_depth_entry_minus8 plus 8 specifies the bit depth of the luma
    /// component of the entries of the palette predictor initializer.
    luma_bit_depth_entry_minus8: u8,
    /// chroma_bit_depth_entry_minus8 plus 8 specifies the bit depth of the
    /// chroma components of the entries of the palette predictor initializer.
    chroma_bit_depth_entry_minus8: u8,
    /// pps_palette_predictor_initializer[ comp ][ i ] specifies the value of
    /// the comp-th component of the i-th palette entry in the PPS that is used
    /// to initialize the array PredictorPaletteEntries.
    palette_predictor_initializer: [[u8; 128]; 3],
}

impl PpsSccExtension {
    pub fn curr_pic_ref_enabled_flag(&self) -> bool {
        self.curr_pic_ref_enabled_flag
    }

    pub fn residual_adaptive_colour_transform_enabled_flag(&self) -> bool {
        self.residual_adaptive_colour_transform_enabled_flag
    }

    pub fn slice_act_qp_offsets_present_flag(&self) -> bool {
        self.slice_act_qp_offsets_present_flag
    }

    pub fn act_y_qp_offset_plus5(&self) -> i8 {
        self.act_y_qp_offset_plus5
    }

    pub fn act_cb_qp_offset_plus5(&self) -> i8 {
        self.act_cb_qp_offset_plus5
    }

    pub fn act_cr_qp_offset_plus3(&self) -> i8 {
        self.act_cr_qp_offset_plus3
    }

    pub fn palette_predictor_initializers_present_flag(&self) -> bool {
        self.palette_predictor_initializers_present_flag
    }

    pub fn num_palette_predictor_initializers(&self) -> u8 {
        self.num_palette_predictor_initializers
    }

    pub fn monochrome_palette_flag(&self) -> bool {
        self.monochrome_palette_flag
    }

    pub fn luma_bit_depth_entry_minus8(&self) -> u8 {
        self.luma_bit_depth_entry_minus8
    }

    pub fn chroma_bit_depth_entry_minus8(&self) -> u8 {
        self.chroma_bit_depth_entry_minus8
    }

    pub fn palette_predictor_initializer(&self) -> [[u8; 128]; 3] {
        self.palette_predictor_initializer
    }
}

impl Default for PpsSccExtension {
    fn default() -> Self {
        Self {
            curr_pic_ref_enabled_flag: Default::default(),
            residual_adaptive_colour_transform_enabled_flag: Default::default(),
            slice_act_qp_offsets_present_flag: Default::default(),
            act_y_qp_offset_plus5: Default::default(),
            act_cb_qp_offset_plus5: Default::default(),
            act_cr_qp_offset_plus3: Default::default(),
            palette_predictor_initializers_present_flag: Default::default(),
            num_palette_predictor_initializers: Default::default(),
            monochrome_palette_flag: Default::default(),
            luma_bit_depth_entry_minus8: Default::default(),
            chroma_bit_depth_entry_minus8: Default::default(),
            palette_predictor_initializer: [[0; 128]; 3],
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct PpsRangeExtension {
    /// log2_max_transform_skip_block_size_minus2 plus 2 specifies the maximum
    /// transform block size for which transform_skip_flag may be present in
    /// coded pictures referring to the PPS. When not present, the value of
    /// log2_max_transform_skip_block_size_minus2 is inferred to be equal to 0.
    /// When present, the value of log2_max_transform_skip_block_size_minus2
    /// shall be less than or equal to MaxTbLog2SizeY − 2.
    log2_max_transform_skip_block_size_minus2: u32,
    /// When set, specifies that log2_res_scale_abs_plus1 and
    /// res_scale_sign_flag may be present in the transform unit syntax for
    /// pictures referring to the PPS. When not set, specifies that
    /// log2_res_scale_abs_plus1 and res_scale_sign_flag are not present for
    /// pictures referring to the PPS.
    cross_component_prediction_enabled_flag: bool,
    /// When set, specifies that the cu_chroma_qp_offset_flag may be present in
    /// the transform unit syntax. When not set, specifies that the
    /// cu_chroma_qp_offset_flag is not present in the transform unit syntax.
    chroma_qp_offset_list_enabled_flag: bool,
    /// Specifies the difference between the luma CTB size and the minimum luma
    /// coding block size of coding units that convey cu_chroma_qp_offset_flag.
    diff_cu_chroma_qp_offset_depth: u32,
    /// chroma_qp_offset_list_len_minus1 plus 1 specifies the number of
    /// cb_qp_offset_list[ i ] and cr_qp_offset_list[ i ] syntax elements that
    /// are present in the PPS.
    chroma_qp_offset_list_len_minus1: u32,
    /// Specify offsets used in the derivation of Qp′Cb and Qp′Cr, respectively.
    cb_qp_offset_list: [i32; 6],
    /// Specify offsets used in the derivation of Qp′Cb and Qp′Cr, respectively.
    cr_qp_offset_list: [i32; 6],
    /// The base 2 logarithm of the scaling parameter that is used to scale
    /// sample adaptive offset (SAO) offset values for luma samples.
    log2_sao_offset_scale_luma: u32,
    /// The base 2 logarithm of the scaling parameter that is used to scale SAO
    /// offset values for chroma samples.
    log2_sao_offset_scale_chroma: u32,
}

impl PpsRangeExtension {
    pub fn log2_max_transform_skip_block_size_minus2(&self) -> u32 {
        self.log2_max_transform_skip_block_size_minus2
    }

    pub fn cross_component_prediction_enabled_flag(&self) -> bool {
        self.cross_component_prediction_enabled_flag
    }

    pub fn chroma_qp_offset_list_enabled_flag(&self) -> bool {
        self.chroma_qp_offset_list_enabled_flag
    }

    pub fn diff_cu_chroma_qp_offset_depth(&self) -> u32 {
        self.diff_cu_chroma_qp_offset_depth
    }

    pub fn chroma_qp_offset_list_len_minus1(&self) -> u32 {
        self.chroma_qp_offset_list_len_minus1
    }

    pub fn cb_qp_offset_list(&self) -> [i32; 6] {
        self.cb_qp_offset_list
    }

    pub fn cr_qp_offset_list(&self) -> [i32; 6] {
        self.cr_qp_offset_list
    }

    pub fn log2_sao_offset_scale_luma(&self) -> u32 {
        self.log2_sao_offset_scale_luma
    }

    pub fn log2_sao_offset_scale_chroma(&self) -> u32 {
        self.log2_sao_offset_scale_chroma
    }
}

/// A H.265 Picture Parameter Set.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Pps {
    /// Identifies the PPS for reference by other syntax elements.
    pic_parameter_set_id: u8,
    /// Specifies the value of sps_seq_parameter_set_id for the active SPS.
    seq_parameter_set_id: u8,
    /// When set, specifies the presence of the syntax element
    /// dependent_slice_segment_flag in the slice segment headers for coded
    /// pictures referring to the PPS. When not set, specifies the absence of
    /// the syntax element dependent_slice_segment_flag in the slice segment
    /// headers for coded pictures referring to the PPS.
    dependent_slice_segments_enabled_flag: bool,
    /// When set, indicates that the pic_output_flag syntax element is present
    /// in the associated slice headers. When not set, indicates that the
    /// pic_output_flag syntax element is not present in the associated slice
    /// headers.
    output_flag_present_flag: bool,
    /// Specifies the number of extra slice header bits that are present in the
    /// slice header RBSP for coded pictures referring to the PPS.
    num_extra_slice_header_bits: u8,
    /// When not set, specifies that sign bit hiding is disabled. Whens set,
    /// specifies that sign bit hiding is enabled.
    sign_data_hiding_enabled_flag: bool,
    /// When set, specifies that cabac_init_flag is present in slice headers
    /// referring to the PPS. When not set, specifies that cabac_init_flag is
    /// not present in slice headers referring to the PPS.
    cabac_init_present_flag: bool,
    /// Specifies the inferred value of num_ref_idx_l0_active_minus1 for P and B
    /// slices with num_ref_idx_active_override_flag not set.
    num_ref_idx_l0_default_active_minus1: u8,
    /// Specifies the inferred value of num_ref_idx_l1_active_minus1 for B
    /// slices with num_ref_idx_active_override_flag not set.
    num_ref_idx_l1_default_active_minus1: u8,
    /// init_qp_minus26 plus 26 specifies the initial value of SliceQpY for each
    /// slice referring to the PPS. The initial value of SliceQpY is modified at
    /// the slice segment layer when a non-zero value of slice_qp_delta is
    /// decoded.
    init_qp_minus26: i8,
    /// When not set, specifies that intra prediction allows usage of residual
    /// data and decoded samples of neighbouring coding blocks coded using
    /// either intra or inter prediction modes. When set, specifies constrained
    /// intra prediction, in which case intra prediction only uses residual data
    /// and decoded samples from neighbouring coding blocks coded using intra
    /// prediction modes.
    constrained_intra_pred_flag: bool,
    /// When set, specifies that transform_skip_flag may be present in the
    /// residual coding syntax. When not set, specifies that transform_skip_flag
    /// is not present in the residual coding syntax.
    transform_skip_enabled_flag: bool,
    /// When set, specifies that the diff_cu_qp_delta_depth syntax element is
    /// present in the PPS and that cu_qp_delta_abs may be present in the
    /// transform unit syntax and the palette syntax. When not set, specifies
    /// that the diff_cu_qp_delta_depth syntax element is not present in the PPS
    /// and that cu_qp_delta_abs is not present in the transform unit syntax and
    /// the palette syntax.
    cu_qp_delta_enabled_flag: bool,

    /*if cu_qp_delta_enabled_flag */
    /// Specifies the difference between the luma CTB size and the minimum luma
    /// coding block size of coding units that convey cu_qp_delta_abs and
    /// cu_qp_delta_sign_flag.
    diff_cu_qp_delta_depth: u8,
    /// Specifies the offsets to the luma quantization parameter Qp′Y used for
    /// deriving Qp′Cb and Qp′Cr, respectively.
    cb_qp_offset: i8,
    /// Specifies the offsets to the luma quantization parameter Qp′Y used for
    /// deriving Qp′Cb and Qp′Cr, respectively.
    cr_qp_offset: i8,
    /// When set, indicates that the slice_cb_qp_offset and slice_cr_qp_offset
    /// syntax elements are present in the associated slice headers.  When not
    /// set, indicates that these syntax elements are not present in the
    /// associated slice headers. When ChromaArrayType is equal to 0,
    /// pps_slice_chroma_qp_offsets_present_flag shall be equal to 0
    slice_chroma_qp_offsets_present_flag: bool,
    /// When not set, specifies that weighted prediction is not applied to P
    /// slices. When set, specifies that weighted prediction is applied to P
    /// slices.
    weighted_pred_flag: bool,
    /// When not set, specifies that the default weighted prediction is applied
    /// to B slices. When set, specifies that weighted prediction is applied to
    /// B slices.
    weighted_bipred_flag: bool,
    /// When set, specifies that `cu_transquant_bypass_flag` is present, When
    /// not set, specifies that `cu_transquant_bypass_flag` is not present.
    transquant_bypass_enabled_flag: bool,
    /// When set, specifies that there is more than one tile in each picture
    /// referring to the PPS. When not set, specifies that there is only one
    /// tile in each picture referring to the PPS.
    tiles_enabled_flag: bool,
    /// When set, specifies that a specific synchronization process for context
    /// variables, and when applicable, Rice parameter initialization states and
    /// palette predictor variables, is invoked before decoding the CTU which
    /// includes the first CTB of a row of CTBs in each tile in each picture
    /// referring to the PPS, and a specific storage process for context
    /// variables, and when applicable, Rice parameter initialization states and
    /// palette predictor variables, is invoked after decoding the CTU which
    /// includes the second CTB of a row of CTBs in each tile in each picture
    /// referring to the PPS. When not set, specifies that no specific
    /// synchronization process for context variables, and when applicable, Rice
    /// parameter initialization states and palette predictor variables, is
    /// required to be invoked before decoding the CTU which includes the first
    /// CTB of a row of CTBs in each tile in each picture referring to the PPS,
    /// and no specific storage process for context variables, and when
    /// applicable, Rice parameter initialization states and palette predictor
    /// variables, is required to be invoked after decoding the CTU which
    /// includes the second CTB of a row of CTBs in each tile in each picture
    /// referring to the PPS.
    entropy_coding_sync_enabled_flag: bool,
    /// num_tile_columns_minus1 plus 1 specifies the number of tile columns
    /// partitioning the picture.
    num_tile_columns_minus1: u8,
    /// num_tile_rows_minus1 plus 1 specifies the number of tile rows
    /// partitioning the picture.
    num_tile_rows_minus1: u8,
    /// When set, specifies that tile column boundaries and likewise tile row
    /// boundaries are distributed uniformly across the picture.  When not set,
    /// specifies that tile column boundaries and likewise tile row boundaries
    /// are not distributed uniformly across the picture but signalled
    /// explicitly using the syntax elements column_width_minus1[ i ] and
    /// row_height_minus1[ i ].
    uniform_spacing_flag: bool,
    /// column_width_minus1[ i ] plus 1 specifies the width of the i-th tile
    /// column in units of CTBs.
    column_width_minus1: [u32; 19],
    /// row_height_minus1[ i ] plus 1 specifies the height of the i-th tile row
    /// in units of CTBs.
    row_height_minus1: [u32; 21],
    /// When set, specifies that in-loop filtering operations may be performed
    /// across tile boundaries in pictures referring to the PPS.  When not set,
    /// specifies that in-loop filtering operations are not performed across
    /// tile boundaries in pictures referring to the PPS. The in-loop filtering
    /// operations include the deblocking filter and sample adaptive offset
    /// filter operations.
    loop_filter_across_tiles_enabled_flag: bool,
    /// When set, specifies that in-loop filtering operations may be performed
    /// across left and upper boundaries of slices referring to the PPS.  When
    /// not set, specifies that in-loop filtering operations are not performed
    /// across left and upper boundaries of slices referring to the PPS. The in-
    /// loop filtering operations include the deblocking filter and sample
    /// adaptive offset filter operations.
    loop_filter_across_slices_enabled_flag: bool,
    /// When set, specifies the presence of deblocking filter control syntax
    /// elements in the PPS. When not set, specifies the absence of deblocking
    /// filter control syntax elements in the PPS.
    deblocking_filter_control_present_flag: bool,
    /// When set, specifies the presence of deblocking_filter_override_flag in
    /// the slice headers for pictures referring to the PPS.  When not set,
    /// specifies the absence of deblocking_filter_override_flag in the slice
    /// headers for pictures referring to the PPS.
    deblocking_filter_override_enabled_flag: bool,
    /// When set, specifies that the operation of deblocking filter is not
    /// applied for slices referring to the PPS in which
    /// slice_deblocking_filter_disabled_flag is not present.  When not set,
    /// specifies that the operation of the deblocking filter is applied for
    /// slices referring to the PPS in which
    /// slice_deblocking_filter_disabled_flag is not present.
    deblocking_filter_disabled_flag: bool,
    /// Specify the default deblocking parameter offsets for β and tC (divided
    /// by 2) that are applied for slices referring to the PPS, unless the
    /// default deblocking parameter offsets are overridden by the deblocking
    /// parameter offsets present in the slice headers of the slices referring
    /// to the PPS.
    beta_offset_div2: i8,
    /// Specify the default deblocking parameter offsets for β and tC (divided
    /// by 2) that are applied for slices referring to the PPS, unless the
    /// default deblocking parameter offsets are overridden by the deblocking
    /// parameter offsets present in the slice headers of the slices referring
    /// to the PPS.
    tc_offset_div2: i8,
    /// When set, specifies that the scaling list data used for the pictures
    /// referring to the PPS are derived based on the scaling lists specified by
    /// the active SPS and the scaling lists specified by the PPS.
    /// pps_scaling_list_data_present_flag equal to 0 specifies that the scaling
    /// list data used for the pictures referring to the PPS are inferred to be
    /// equal to those specified by the active SPS.
    scaling_list_data_present_flag: bool,
    /// The scaling list data.
    scaling_list: ScalingLists,
    /// When set, specifies that the syntax structure
    /// ref_pic_lists_modification( ) is present in the slice segment header.
    /// When not set, specifies that the syntax structure
    /// ref_pic_lists_modification( ) is not present in the slice segment header
    lists_modification_present_flag: bool,
    /// log2_parallel_merge_level_minus2 plus 2 specifies the value of the
    /// variable Log2ParMrgLevel, which is used in the derivation process for
    /// luma motion vectors for merge mode as specified in clause 8.5.3.2.2 and
    /// the derivation process for spatial merging candidates as specified in
    /// clause 8.5.3.2.3.
    log2_parallel_merge_level_minus2: u8,
    /// When not set, specifies that no slice segment header extension syntax
    /// elements are present in the slice segment headers for coded pictures
    /// referring to the PPS. When set, specifies that slice segment header
    /// extension syntax elements are present in the slice segment headers for
    /// coded pictures referring to the PPS.
    slice_segment_header_extension_present_flag: bool,
    /// When set, specifies that the syntax elements pps_range_extension_flag,
    /// pps_multilayer_extension_flag, pps_3d_extension_flag,
    /// pps_scc_extension_flag, and pps_extension_4bits are present in the
    /// picture parameter set RBSP syntax structure. When not set, specifies
    /// that these syntax elements are not present.
    extension_present_flag: bool,
    /// When setspecifies that the pps_range_extension( ) syntax structure is
    /// present in the PPS RBSP syntax structure. When not set, specifies that
    /// this syntax structure is not present.
    range_extension_flag: bool,
    /// The range extension data.
    range_extension: PpsRangeExtension,

    scc_extension_flag: bool,
    /// The SCC extension data.
    scc_extension: PpsSccExtension,

    // Internal variables.
    /// Equivalent to QpBdOffsetY in the specification.
    qp_bd_offset_y: u32,

    /// The nuh_temporal_id_plus1 - 1 of the associated NALU.
    temporal_id: u8,
}

impl Pps {
    pub fn pic_parameter_set_id(&self) -> u8 {
        self.pic_parameter_set_id
    }

    pub fn seq_parameter_set_id(&self) -> u8 {
        self.seq_parameter_set_id
    }

    pub fn dependent_slice_segments_enabled_flag(&self) -> bool {
        self.dependent_slice_segments_enabled_flag
    }

    pub fn output_flag_present_flag(&self) -> bool {
        self.output_flag_present_flag
    }

    pub fn num_extra_slice_header_bits(&self) -> u8 {
        self.num_extra_slice_header_bits
    }

    pub fn sign_data_hiding_enabled_flag(&self) -> bool {
        self.sign_data_hiding_enabled_flag
    }

    pub fn cabac_init_present_flag(&self) -> bool {
        self.cabac_init_present_flag
    }

    pub fn num_ref_idx_l0_default_active_minus1(&self) -> u8 {
        self.num_ref_idx_l0_default_active_minus1
    }

    pub fn num_ref_idx_l1_default_active_minus1(&self) -> u8 {
        self.num_ref_idx_l1_default_active_minus1
    }

    pub fn init_qp_minus26(&self) -> i8 {
        self.init_qp_minus26
    }

    pub fn constrained_intra_pred_flag(&self) -> bool {
        self.constrained_intra_pred_flag
    }

    pub fn transform_skip_enabled_flag(&self) -> bool {
        self.transform_skip_enabled_flag
    }

    pub fn cu_qp_delta_enabled_flag(&self) -> bool {
        self.cu_qp_delta_enabled_flag
    }

    pub fn diff_cu_qp_delta_depth(&self) -> u8 {
        self.diff_cu_qp_delta_depth
    }

    pub fn cb_qp_offset(&self) -> i8 {
        self.cb_qp_offset
    }

    pub fn cr_qp_offset(&self) -> i8 {
        self.cr_qp_offset
    }

    pub fn slice_chroma_qp_offsets_present_flag(&self) -> bool {
        self.slice_chroma_qp_offsets_present_flag
    }

    pub fn weighted_pred_flag(&self) -> bool {
        self.weighted_pred_flag
    }

    pub fn weighted_bipred_flag(&self) -> bool {
        self.weighted_bipred_flag
    }

    pub fn transquant_bypass_enabled_flag(&self) -> bool {
        self.transquant_bypass_enabled_flag
    }

    pub fn tiles_enabled_flag(&self) -> bool {
        self.tiles_enabled_flag
    }

    pub fn entropy_coding_sync_enabled_flag(&self) -> bool {
        self.entropy_coding_sync_enabled_flag
    }

    pub fn num_tile_columns_minus1(&self) -> u8 {
        self.num_tile_columns_minus1
    }

    pub fn num_tile_rows_minus1(&self) -> u8 {
        self.num_tile_rows_minus1
    }

    pub fn uniform_spacing_flag(&self) -> bool {
        self.uniform_spacing_flag
    }

    pub fn column_width_minus1(&self) -> [u32; 19] {
        self.column_width_minus1
    }

    pub fn row_height_minus1(&self) -> [u32; 21] {
        self.row_height_minus1
    }

    pub fn loop_filter_across_tiles_enabled_flag(&self) -> bool {
        self.loop_filter_across_tiles_enabled_flag
    }

    pub fn loop_filter_across_slices_enabled_flag(&self) -> bool {
        self.loop_filter_across_slices_enabled_flag
    }

    pub fn deblocking_filter_control_present_flag(&self) -> bool {
        self.deblocking_filter_control_present_flag
    }

    pub fn deblocking_filter_override_enabled_flag(&self) -> bool {
        self.deblocking_filter_override_enabled_flag
    }

    pub fn deblocking_filter_disabled_flag(&self) -> bool {
        self.deblocking_filter_disabled_flag
    }

    pub fn beta_offset_div2(&self) -> i8 {
        self.beta_offset_div2
    }

    pub fn tc_offset_div2(&self) -> i8 {
        self.tc_offset_div2
    }

    pub fn scaling_list_data_present_flag(&self) -> bool {
        self.scaling_list_data_present_flag
    }

    pub fn scaling_list(&self) -> &ScalingLists {
        &self.scaling_list
    }

    pub fn lists_modification_present_flag(&self) -> bool {
        self.lists_modification_present_flag
    }

    pub fn log2_parallel_merge_level_minus2(&self) -> u8 {
        self.log2_parallel_merge_level_minus2
    }

    pub fn slice_segment_header_extension_present_flag(&self) -> bool {
        self.slice_segment_header_extension_present_flag
    }

    pub fn extension_present_flag(&self) -> bool {
        self.extension_present_flag
    }

    pub fn range_extension_flag(&self) -> bool {
        self.range_extension_flag
    }

    pub fn range_extension(&self) -> &PpsRangeExtension {
        &self.range_extension
    }

    pub fn qp_bd_offset_y(&self) -> u32 {
        self.qp_bd_offset_y
    }

    pub fn scc_extension(&self) -> &PpsSccExtension {
        &self.scc_extension
    }

    pub fn scc_extension_flag(&self) -> bool {
        self.scc_extension_flag
    }

    pub fn temporal_id(&self) -> u8 {
        self.temporal_id
    }
}

impl Default for Pps {
    fn default() -> Self {
        Self {
            pic_parameter_set_id: Default::default(),
            seq_parameter_set_id: Default::default(),
            dependent_slice_segments_enabled_flag: Default::default(),
            output_flag_present_flag: Default::default(),
            num_extra_slice_header_bits: Default::default(),
            sign_data_hiding_enabled_flag: Default::default(),
            cabac_init_present_flag: Default::default(),
            num_ref_idx_l0_default_active_minus1: Default::default(),
            num_ref_idx_l1_default_active_minus1: Default::default(),
            init_qp_minus26: Default::default(),
            constrained_intra_pred_flag: Default::default(),
            transform_skip_enabled_flag: Default::default(),
            cu_qp_delta_enabled_flag: Default::default(),
            diff_cu_qp_delta_depth: Default::default(),
            cb_qp_offset: Default::default(),
            cr_qp_offset: Default::default(),
            slice_chroma_qp_offsets_present_flag: Default::default(),
            weighted_pred_flag: Default::default(),
            weighted_bipred_flag: Default::default(),
            transquant_bypass_enabled_flag: Default::default(),
            tiles_enabled_flag: Default::default(),
            entropy_coding_sync_enabled_flag: Default::default(),
            num_tile_columns_minus1: Default::default(),
            num_tile_rows_minus1: Default::default(),
            uniform_spacing_flag: true,
            column_width_minus1: Default::default(),
            row_height_minus1: Default::default(),
            loop_filter_across_tiles_enabled_flag: true,
            loop_filter_across_slices_enabled_flag: Default::default(),
            deblocking_filter_control_present_flag: Default::default(),
            deblocking_filter_override_enabled_flag: Default::default(),
            deblocking_filter_disabled_flag: Default::default(),
            beta_offset_div2: Default::default(),
            tc_offset_div2: Default::default(),
            scaling_list_data_present_flag: Default::default(),
            scaling_list: Default::default(),
            lists_modification_present_flag: Default::default(),
            log2_parallel_merge_level_minus2: Default::default(),
            slice_segment_header_extension_present_flag: Default::default(),
            extension_present_flag: Default::default(),
            range_extension_flag: Default::default(),
            range_extension: Default::default(),
            qp_bd_offset_y: Default::default(),
            scc_extension: Default::default(),
            scc_extension_flag: Default::default(),
            temporal_id: Default::default(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ScalingLists {
    /// plus 8 specifies the value of the variable ScalingFactor[ 2 ][ matrixId
    /// ] [ 0 ][ 0 ] for the scaling list for the 16x16 size.
    scaling_list_dc_coef_minus8_16x16: [i16; 6],
    /// plus 8 specifies the value of the variable ScalingFactor[ 3 ][ matrixId
    /// ][ 0 ][ 0 ] for the scaling list for the 32x32 size.
    scaling_list_dc_coef_minus8_32x32: [i16; 2],
    /// The 4x4 scaling list.
    scaling_list_4x4: [[u8; 16]; 6],
    /// The 8x8 scaling list.
    scaling_list_8x8: [[u8; 64]; 6],
    /// The 16x16 scaling list.
    scaling_list_16x16: [[u8; 64]; 6],
    /// The 32x32 scaling list.
    scaling_list_32x32: [[u8; 64]; 2],
}

impl ScalingLists {
    pub fn scaling_list_dc_coef_minus8_16x16(&self) -> [i16; 6] {
        self.scaling_list_dc_coef_minus8_16x16
    }

    pub fn scaling_list_dc_coef_minus8_32x32(&self) -> [i16; 2] {
        self.scaling_list_dc_coef_minus8_32x32
    }

    pub fn scaling_list_4x4(&self) -> [[u8; 16]; 6] {
        self.scaling_list_4x4
    }

    pub fn scaling_list_8x8(&self) -> [[u8; 64]; 6] {
        self.scaling_list_8x8
    }

    pub fn scaling_list_16x16(&self) -> [[u8; 64]; 6] {
        self.scaling_list_16x16
    }

    pub fn scaling_list_32x32(&self) -> [[u8; 64]; 2] {
        self.scaling_list_32x32
    }
}

impl Default for ScalingLists {
    fn default() -> Self {
        Self {
            scaling_list_dc_coef_minus8_16x16: Default::default(),
            scaling_list_dc_coef_minus8_32x32: Default::default(),
            scaling_list_4x4: Default::default(),
            scaling_list_8x8: [[0; 64]; 6],
            scaling_list_16x16: [[0; 64]; 6],
            scaling_list_32x32: [[0; 64]; 2],
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct RefPicListModification {
    /// Whenset, indicates that reference picture list 0 is specified explicitly
    /// by a list of list_entry_l0[ i ] values.  When not set, indicates that
    /// reference picture list 0 is determined implicitly.
    ref_pic_list_modification_flag_l0: bool,
    /// list_entry_l0[ i ] specifies the index of the reference picture in
    /// RefPicListTemp0 to be placed at the current position of reference
    /// picture list 0.
    list_entry_l0: Vec<u32>,
    /// Whenset, indicates that reference picture list 1 is specified explicitly
    /// by a list of list_entry_l1[ i ] values.  When not set, indicates that
    /// reference picture list 1 is determined implicitly.
    ref_pic_list_modification_flag_l1: bool,
    /// list_entry_l1[ i ] specifies the index of the reference picture in
    /// RefPicListTemp1 to be placed at the current position of reference
    /// picture list 1.
    list_entry_l1: Vec<u32>,
}

impl RefPicListModification {
    pub fn ref_pic_list_modification_flag_l0(&self) -> bool {
        self.ref_pic_list_modification_flag_l0
    }

    pub fn list_entry_l0(&self) -> &[u32] {
        self.list_entry_l0.as_ref()
    }

    pub fn ref_pic_list_modification_flag_l1(&self) -> bool {
        self.ref_pic_list_modification_flag_l1
    }

    pub fn list_entry_l1(&self) -> &[u32] {
        self.list_entry_l1.as_ref()
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct PredWeightTable {
    /// The base 2 logarithm of the denominator for all luma weighting factors.
    luma_log2_weight_denom: u8,
    /// The difference of the base 2 logarithm of the denominator for all chroma
    /// weighting factors.
    delta_chroma_log2_weight_denom: i8,
    /// luma_weight_l0_flag[ i ] set specifies that weighting factors for the
    /// luma component of list 0 prediction using RefPicList0[ i ] are present.
    /// luma_weight_l0_flag[ i ] not set specifies that these weighting factors
    /// are not present.
    luma_weight_l0_flag: [bool; 15],
    /// chroma_weight_l0_flag[ i ] set specifies that weighting factors for the
    /// chroma prediction values of list 0 prediction using RefPicList0[ i ] are
    /// present. chroma_weight_l0_flag[ i ] not set specifies that these
    /// weighting factors are not present.
    chroma_weight_l0_flag: [bool; 15],
    /// delta_luma_weight_l0[ i ] is the difference of the weighting factor
    /// applied to the luma prediction value for list 0 prediction using
    /// RefPicList0[ i ].
    delta_luma_weight_l0: [i8; 15],
    /// luma_offset_l0[ i ] is the additive offset applied to the luma
    /// prediction value for list 0 prediction using RefPicList0[ i ].
    luma_offset_l0: [i8; 15],
    /// delta_chroma_weight_l0[ i ][ j ] is the difference of the weighting
    /// factor applied to the chroma prediction values for list 0 prediction
    /// using RefPicList0[ i ] with j equal to 0 for Cb and j equal to 1 for Cr.
    delta_chroma_weight_l0: [[i8; 2]; 15],
    /// delta_chroma_offset_l0[ i ][ j ] is the difference of the additive
    /// offset applied to the chroma prediction values for list 0 prediction
    /// using RefPicList0[ i ] with j equal to 0 for Cb and j equal to 1 for Cr.
    delta_chroma_offset_l0: [[i16; 2]; 15],

    // luma_weight_l1_flag[ i ], chroma_weight_l1_flag[ i ],
    // delta_luma_weight_l1[ i ], luma_offset_l1[ i ], delta_chroma_weight_l1[ i
    // ][ j ] and delta_chroma_offset_l1[ i ][ j ] have the same
    // semanticsasluma_weight_l0_flag[ i ], chroma_weight_l0_flag[ i ],
    // delta_luma_weight_l0[ i ], luma_offset_l0[ i ], delta_chroma_weight_l0[ i
    // ][ j ] and delta_chroma_offset_l0[ i ][ j ], respectively, with l0, L0,
    // list 0 and List0 replaced by l1, L1, list 1 and List1, respectively.
    luma_weight_l1_flag: [bool; 15],
    chroma_weight_l1_flag: [bool; 15],
    delta_luma_weight_l1: [i8; 15],
    luma_offset_l1: [i8; 15],

    delta_chroma_weight_l1: [[i8; 2]; 15],
    delta_chroma_offset_l1: [[i16; 2]; 15],

    // Calculated.
    /// Same as ChromaLog2WeightDenom in the specification.
    chroma_log2_weight_denom: u8,
}

impl PredWeightTable {
    pub fn luma_log2_weight_denom(&self) -> u8 {
        self.luma_log2_weight_denom
    }

    pub fn delta_chroma_log2_weight_denom(&self) -> i8 {
        self.delta_chroma_log2_weight_denom
    }

    pub fn luma_weight_l0_flag(&self) -> [bool; 15] {
        self.luma_weight_l0_flag
    }

    pub fn chroma_weight_l0_flag(&self) -> [bool; 15] {
        self.chroma_weight_l0_flag
    }

    pub fn delta_luma_weight_l0(&self) -> [i8; 15] {
        self.delta_luma_weight_l0
    }

    pub fn luma_offset_l0(&self) -> [i8; 15] {
        self.luma_offset_l0
    }

    pub fn delta_chroma_weight_l0(&self) -> [[i8; 2]; 15] {
        self.delta_chroma_weight_l0
    }

    pub fn delta_chroma_offset_l0(&self) -> [[i16; 2]; 15] {
        self.delta_chroma_offset_l0
    }

    pub fn luma_weight_l1_flag(&self) -> [bool; 15] {
        self.luma_weight_l1_flag
    }

    pub fn chroma_weight_l1_flag(&self) -> [bool; 15] {
        self.chroma_weight_l1_flag
    }

    pub fn delta_luma_weight_l1(&self) -> [i8; 15] {
        self.delta_luma_weight_l1
    }

    pub fn luma_offset_l1(&self) -> [i8; 15] {
        self.luma_offset_l1
    }

    pub fn delta_chroma_weight_l1(&self) -> [[i8; 2]; 15] {
        self.delta_chroma_weight_l1
    }

    pub fn delta_chroma_offset_l1(&self) -> [[i16; 2]; 15] {
        self.delta_chroma_offset_l1
    }

    pub fn chroma_log2_weight_denom(&self) -> u8 {
        self.chroma_log2_weight_denom
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ShortTermRefPicSet {
    /// When set, specifies that the stRpsIdx-th candidate short-term RPS is
    /// predicted from another candidate short-term RPS, which is referred to as
    /// the source candidate short-term RPS.
    inter_ref_pic_set_prediction_flag: bool,
    /// delta_idx_minus1 plus 1 specifies the difference between the value of
    /// stRpsIdx and the index, into the list of the candidate short-term RPSs
    /// specified in the SPS, of the source candidate short-term RPS.
    delta_idx_minus1: u8,
    /// delta_rps_sign and abs_delta_rps_minus1 together specify the value of
    /// the variable deltaRps.
    delta_rps_sign: bool,
    /// delta_rps_sign and abs_delta_rps_minus1 together specify the value of
    /// the variable deltaRps.
    abs_delta_rps_minus1: u16,
    /// specifies the number of entries in the stRpsIdx-th candidate short-term
    /// RPS that have picture order count values less than the picture order
    /// count value of the current picture.
    num_negative_pics: u8,
    /// specifies the number of entries in the stRpsIdx-th candidate short-term
    /// RPS that have picture order count values greater than the picture order
    /// count value of the current picture.
    num_positive_pics: u8,
    /// Same as UsedByCurrPicS0 in the specification.
    used_by_curr_pic_s0: [bool; MAX_SHORT_TERM_REF_PIC_SETS],
    /// Same as UsedByCurrPicS1 in the specification.
    used_by_curr_pic_s1: [bool; MAX_SHORT_TERM_REF_PIC_SETS],
    /// Same as DeltaPocS0 in the specification.
    delta_poc_s0: [i32; MAX_SHORT_TERM_REF_PIC_SETS],
    /// Same as DeltaPocS1 in the specification.
    delta_poc_s1: [i32; MAX_SHORT_TERM_REF_PIC_SETS],
    /// Same as NumDeltaPocs in the specification.
    num_delta_pocs: u32,
}

impl ShortTermRefPicSet {
    pub fn inter_ref_pic_set_prediction_flag(&self) -> bool {
        self.inter_ref_pic_set_prediction_flag
    }

    pub fn delta_idx_minus1(&self) -> u8 {
        self.delta_idx_minus1
    }

    pub fn delta_rps_sign(&self) -> bool {
        self.delta_rps_sign
    }

    pub fn abs_delta_rps_minus1(&self) -> u16 {
        self.abs_delta_rps_minus1
    }

    pub fn num_negative_pics(&self) -> u8 {
        self.num_negative_pics
    }

    pub fn num_positive_pics(&self) -> u8 {
        self.num_positive_pics
    }

    pub fn used_by_curr_pic_s0(&self) -> [bool; MAX_SHORT_TERM_REF_PIC_SETS] {
        self.used_by_curr_pic_s0
    }

    pub fn used_by_curr_pic_s1(&self) -> [bool; MAX_SHORT_TERM_REF_PIC_SETS] {
        self.used_by_curr_pic_s1
    }

    pub fn delta_poc_s0(&self) -> [i32; MAX_SHORT_TERM_REF_PIC_SETS] {
        self.delta_poc_s0
    }

    pub fn delta_poc_s1(&self) -> [i32; MAX_SHORT_TERM_REF_PIC_SETS] {
        self.delta_poc_s1
    }

    pub fn num_delta_pocs(&self) -> u32 {
        self.num_delta_pocs
    }
}

impl Default for ShortTermRefPicSet {
    fn default() -> Self {
        Self {
            inter_ref_pic_set_prediction_flag: Default::default(),
            delta_idx_minus1: Default::default(),
            delta_rps_sign: Default::default(),
            abs_delta_rps_minus1: Default::default(),
            num_negative_pics: Default::default(),
            num_positive_pics: Default::default(),
            used_by_curr_pic_s0: [false; MAX_SHORT_TERM_REF_PIC_SETS],
            used_by_curr_pic_s1: [false; MAX_SHORT_TERM_REF_PIC_SETS],
            delta_poc_s0: [0; MAX_SHORT_TERM_REF_PIC_SETS],
            delta_poc_s1: [0; MAX_SHORT_TERM_REF_PIC_SETS],
            num_delta_pocs: Default::default(),
        }
    }
}

#[derive(N, Clone, Copy, Debug, PartialEq, Eq)]
/// See table 7-7 in the specification.
pub enum SliceType {
    B = 0,
    P = 1,
    I = 2,
}

impl SliceType {
    /// Whether this is a P slice. See table 7-7 in the specification.
    pub fn is_p(&self) -> bool {
        matches!(self, SliceType::P)
    }

    /// Whether this is a B slice. See table 7-7 in the specification.
    pub fn is_b(&self) -> bool {
        matches!(self, SliceType::B)
    }

    /// Whether this is an I slice. See table 7-7 in the specification.
    pub fn is_i(&self) -> bool {
        matches!(self, SliceType::I)
    }
}

impl Default for SliceType {
    fn default() -> Self {
        Self::P
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SliceHeader {
    /// When set, specifies that the slice segment is the first slice segment of
    /// the picture in decoding order. When not set, specifies that the slice
    /// segment is not the first slice segment of the picture in decoding order.
    first_slice_segment_in_pic_flag: bool,
    /// Affects the output of previously-decoded pictures in the decoded picture
    /// buffer after the decoding of an IDR or a BLA picture that is not the
    /// first picture in the bitstream as specified in Annex C.
    no_output_of_prior_pics_flag: bool,
    /// Specifies the value of pps_pic_parameter_set_id for the PPS in use.
    pic_parameter_set_id: u8,
    /// When set, specifies that the value of each slice segment header syntax
    /// element that is not present is inferred to be equal to the value of the
    /// corresponding slice segment header syntax element in the slice header.
    dependent_slice_segment_flag: bool,
    /// Specifies the address of the first CTB in the slice segment, in CTB
    /// raster scan of a picture.
    segment_address: u32,
    /// Specifies the coding type of the slice according to Table 7-7.
    type_: SliceType,
    /// Affects the decoded picture output and removal processes as specified in
    /// Annex C.
    pic_output_flag: bool,
    /// Specifies the colour plane associated with the current slice RBSP when
    /// separate_colour_plane_flag is set. The value of colour_plane_id shall be
    /// in the range of 0 to 2, inclusive. colour_plane_id values 0, 1 and 2
    /// correspond to the Y, Cb and Cr planes, respectively.
    colour_plane_id: u8,
    /// Specifies the picture order count modulo MaxPicOrderCntLsb for the
    /// current picture. The length of the slice_pic_order_cnt_lsb syntax
    /// element is log2_max_pic_order_cnt_lsb_minus4 + 4 bits.
    pic_order_cnt_lsb: u16,
    /// When set, specifies that the short-term RPS of the current picture is
    /// derived based on one of the st_ref_pic_set( ) syntax structures in the
    /// active SPS that is identified by the syntax element
    /// short_term_ref_pic_set_idx in the slice header.  When not set, specifies
    /// that the short-term RPS of the current picture is derived based on the
    /// st_ref_pic_set( ) syntax structure that is directly included in the
    /// slice headers of the current picture.
    short_term_ref_pic_set_sps_flag: bool,
    /// The st_ref_pic_set() data.
    short_term_ref_pic_set: ShortTermRefPicSet,
    /// Specifies the index, into the list of the st_ref_pic_set( ) syntax
    /// structures included in the active SPS, of the st_ref_pic_set( ) syntax
    /// structure that is used for derivation of the short-term RPS of the
    /// current picture.
    short_term_ref_pic_set_idx: u8,
    /// Specifies the number of entries in the long-term RPS of the current
    /// picture that are derived based on the candidate long-term reference
    /// pictures specified in the active SPS.
    num_long_term_sps: u8,
    /// Specifies the number of entries in the long-term RPS of the current
    /// picture that are directly signalled in the slice header.
    num_long_term_pics: u8,
    /// lt_idx_sps[ i ] specifies an index, into the list of candidate long-term
    /// reference pictures specified in the active SPS, of the i-th entry in the
    /// long-term RPS of the current picture.
    lt_idx_sps: [u8; 16],
    /// poc_lsb_lt[ i ] specifies the value of the picture order count modulo
    /// MaxPicOrderCntLsb of the i-th entry in the long-term RPS of the current
    /// picture.
    poc_lsb_lt: [u32; 16],
    /// Same as UsedByCurrPicLt in the specification.
    used_by_curr_pic_lt: [bool; 16],
    /// When set, specifies that that delta_poc_msb_cycle_lt[i] is present.
    delta_poc_msb_present_flag: [bool; 16],
    /// Same as DeltaPocMsbCycleLt in the specification.
    delta_poc_msb_cycle_lt: [u32; 16],
    /// Specifies whether temporal motion vector predictors can be used for
    /// inter prediction. If slice_temporal_mvp_enabled_flag is not set, the
    /// syntax elements of the current picture shall be constrained such that no
    /// temporal motion vector predictor is used in decoding of the current
    /// picture. Otherwise (slice_temporal_mvp_enabled_flag is set), temporal
    /// motion vector predictors may be used in decoding of the current picture.
    temporal_mvp_enabled_flag: bool,
    /// When set, specifies that SAO is enabled for the luma component in the
    /// current slice; slice_sao_luma_flag not set specifies that SAO is
    /// disabled for the luma component in the current slice.
    sao_luma_flag: bool,
    /// When set, specifies that SAO is enabled for the chroma component in the
    /// current slice; When not set, specifies that SAO is disabled for the
    /// chroma component in the current slice.
    sao_chroma_flag: bool,
    /// When set, specifies that the syntax element num_ref_idx_l0_active_minus1
    /// is present for P and B slices and that the syntax element
    /// num_ref_idx_l1_active_minus1 is present for B slices. When not set,
    /// specifies that the syntax elements num_ref_idx_l0_active_minus1 and
    /// num_ref_idx_l1_active_minus1 are not present.
    num_ref_idx_active_override_flag: bool,
    /// Specifies the maximum reference index for
    /// reference picture list 0 that may be used to decode the slice.
    num_ref_idx_l0_active_minus1: u8,
    /// Specifies the maximum reference index for reference picture list 1 that
    /// may be used to decode the slice.
    num_ref_idx_l1_active_minus1: u8,
    /// The RefPicListModification data.
    ref_pic_list_modification: RefPicListModification,
    /// When set, indicates that the mvd_coding( x0, y0, 1 ) syntax structure is
    /// not parsed and MvdL1[ x0 ][ y0 ][ compIdx ] is set equal to 0 for
    /// compIdx = 0..1. When not set, indicates that the mvd_coding( x0, y0, 1 )
    /// syntax structure is parsed.
    mvd_l1_zero_flag: bool,
    /// Specifies the method for determining the initialization table used in
    /// the initialization process for context variables.
    cabac_init_flag: bool,
    /// When set, specifies that the collocated picture used for temporal motion
    /// vector prediction is derived from reference picture list 0.  When not
    /// set, specifies that the collocated picture used for temporal motion
    /// vector prediction is derived from reference picture list 1.
    collocated_from_l0_flag: bool,
    /// Specifies the reference index of the collocated picture used for
    /// temporal motion vector prediction.
    collocated_ref_idx: u8,
    /// The PredWeightTable data.
    pred_weight_table: PredWeightTable,
    /// Specifies the maximum number of merging motion vector prediction (MVP)
    /// candidates supported in the slice subtracted from 5.
    five_minus_max_num_merge_cand: u8,
    /// Specifies that the resolution of motion vectors for inter prediction in
    /// the current slice is integer. When not set, specifies
    /// that the resolution of motion vectors for inter prediction in the
    /// current slice that refer to pictures other than the current picture is
    /// fractional with quarter-sample precision in units of luma samples.
    use_integer_mv_flag: bool,
    /// Specifies the initial value of QpY to be used for the coding blocks in
    /// the slice until modified by the value of CuQpDeltaVal in the coding unit
    /// layer.
    qp_delta: i8,
    /// Specifies a difference to be added to the value of pps_cb_qp_offset when
    /// determining the value of the Qp′Cb quantization parameter.
    cb_qp_offset: i8,
    /// Specifies a difference to be added to the value of pps_cb_qr_offset when
    /// determining the value of the Qp′Cr quantization parameter.
    cr_qp_offset: i8,
    /// Specifies offsets to the quantization parameter values qP derived in
    /// clause 8.6.2 for luma, Cb, and Cr components, respectively.
    slice_act_y_qp_offset: i8,
    /// Specifies offsets to the quantization parameter values qP derived in
    /// clause 8.6.2 for luma, Cb, and Cr components, respectively.
    slice_act_cb_qp_offset: i8,
    /// Specifies offsets to the quantization parameter values qP derived in
    /// clause 8.6.2 for luma, Cb, and Cr components, respectively.
    slice_act_cr_qp_offset: i8,
    /// When set, specifies that the cu_chroma_qp_offset_flag may be present in
    /// the transform unit syntax. When not set, specifies that the
    /// cu_chroma_qp_offset_flag is not present in the transform unit syntax.
    cu_chroma_qp_offset_enabled_flag: bool,
    /// When set, specifies that deblocking parameters are present in the slice
    /// header. When not set, specifies that deblocking parameters are not
    /// present in the slice header.
    deblocking_filter_override_flag: bool,
    /// When set, specifies that the operation of the deblocking filter is not
    /// applied for the current slice. When not set, specifies that the
    /// operation of the deblocking filter is applied for the current slice.
    deblocking_filter_disabled_flag: bool,
    /// Specifies the deblocking parameter offsets for β and tC (divided by 2)
    /// for the current slice.
    beta_offset_div2: i8,
    /// Specifies the deblocking parameter offsets for β and tC (divided by 2)
    /// for the current slice.
    tc_offset_div2: i8,
    /// When set, specifies that in-loop filtering operations may be performed
    /// across the left and upper boundaries of the current slice.  When not
    /// set, specifies that in-loop operations are not performed across left and
    /// upper boundaries of the current slice. The in-loop filtering operations
    /// include the deblocking filter and sample adaptive offset filter.
    loop_filter_across_slices_enabled_flag: bool,
    /// Specifies the number of entry_point_offset_minus1[ i ] syntax elements
    /// in the slice header.
    num_entry_point_offsets: u32,
    /// offset_len_minus1 plus 1 specifies the length, in bits, of the
    /// entry_point_offset_minus1[ i ] syntax elements.
    offset_len_minus1: u8,
    /// entry_point_offset_minus1[ i ] plus 1 specifies the i-th entry point
    /// offset in bytes, and is represented by offset_len_minus1 plus 1 bits.
    /// The slice segment data that follow the slice segment header consists of
    /// num_entry_point_offsets + 1 subsets, with subset index values ranging
    /// from 0 to num_entry_point_offsets, inclusive. See the specification for
    /// more details.
    entry_point_offset_minus1: [u32; 32],
    /// Same as NumPicTotalCurr in the specification.
    num_pic_total_curr: u32,
    // Size of slice_header() in bits.
    header_bit_size: u32,
    // Number of emulation prevention bytes (EPB) in this slice_header().
    n_emulation_prevention_bytes: u32,
    /// Same as CurrRpsIdx in the specification.
    curr_rps_idx: u8,
}

impl SliceHeader {
    pub fn first_slice_segment_in_pic_flag(&self) -> bool {
        self.first_slice_segment_in_pic_flag
    }

    pub fn no_output_of_prior_pics_flag(&self) -> bool {
        self.no_output_of_prior_pics_flag
    }

    pub fn pic_parameter_set_id(&self) -> u8 {
        self.pic_parameter_set_id
    }

    pub fn dependent_slice_segment_flag(&self) -> bool {
        self.dependent_slice_segment_flag
    }

    pub fn segment_address(&self) -> u32 {
        self.segment_address
    }

    pub fn type_(&self) -> SliceType {
        self.type_
    }

    pub fn pic_output_flag(&self) -> bool {
        self.pic_output_flag
    }

    pub fn colour_plane_id(&self) -> u8 {
        self.colour_plane_id
    }

    pub fn pic_order_cnt_lsb(&self) -> u16 {
        self.pic_order_cnt_lsb
    }

    pub fn short_term_ref_pic_set_sps_flag(&self) -> bool {
        self.short_term_ref_pic_set_sps_flag
    }

    pub fn short_term_ref_pic_sets(&self) -> &ShortTermRefPicSet {
        &self.short_term_ref_pic_set
    }

    pub fn short_term_ref_pic_set_idx(&self) -> u8 {
        self.short_term_ref_pic_set_idx
    }

    pub fn num_long_term_sps(&self) -> u8 {
        self.num_long_term_sps
    }

    pub fn num_long_term_pics(&self) -> u8 {
        self.num_long_term_pics
    }

    pub fn lt_idx_sps(&self) -> [u8; 16] {
        self.lt_idx_sps
    }

    pub fn poc_lsb_lt(&self) -> [u32; 16] {
        self.poc_lsb_lt
    }

    pub fn used_by_curr_pic_lt(&self) -> [bool; 16] {
        self.used_by_curr_pic_lt
    }

    pub fn delta_poc_msb_present_flag(&self) -> [bool; 16] {
        self.delta_poc_msb_present_flag
    }

    pub fn delta_poc_msb_cycle_lt(&self) -> [u32; 16] {
        self.delta_poc_msb_cycle_lt
    }

    pub fn temporal_mvp_enabled_flag(&self) -> bool {
        self.temporal_mvp_enabled_flag
    }

    pub fn sao_luma_flag(&self) -> bool {
        self.sao_luma_flag
    }

    pub fn sao_chroma_flag(&self) -> bool {
        self.sao_chroma_flag
    }

    pub fn num_ref_idx_active_override_flag(&self) -> bool {
        self.num_ref_idx_active_override_flag
    }

    pub fn num_ref_idx_l0_active_minus1(&self) -> u8 {
        self.num_ref_idx_l0_active_minus1
    }

    pub fn num_ref_idx_l1_active_minus1(&self) -> u8 {
        self.num_ref_idx_l1_active_minus1
    }

    pub fn ref_pic_list_modification(&self) -> &RefPicListModification {
        &self.ref_pic_list_modification
    }

    pub fn mvd_l1_zero_flag(&self) -> bool {
        self.mvd_l1_zero_flag
    }

    pub fn cabac_init_flag(&self) -> bool {
        self.cabac_init_flag
    }

    pub fn collocated_from_l0_flag(&self) -> bool {
        self.collocated_from_l0_flag
    }

    pub fn collocated_ref_idx(&self) -> u8 {
        self.collocated_ref_idx
    }

    pub fn pred_weight_table(&self) -> &PredWeightTable {
        &self.pred_weight_table
    }

    pub fn five_minus_max_num_merge_cand(&self) -> u8 {
        self.five_minus_max_num_merge_cand
    }

    pub fn use_integer_mv_flag(&self) -> bool {
        self.use_integer_mv_flag
    }

    pub fn qp_delta(&self) -> i8 {
        self.qp_delta
    }

    pub fn cb_qp_offset(&self) -> i8 {
        self.cb_qp_offset
    }

    pub fn cr_qp_offset(&self) -> i8 {
        self.cr_qp_offset
    }

    pub fn slice_act_y_qp_offset(&self) -> i8 {
        self.slice_act_y_qp_offset
    }

    pub fn slice_act_cb_qp_offset(&self) -> i8 {
        self.slice_act_cb_qp_offset
    }

    pub fn slice_act_cr_qp_offset(&self) -> i8 {
        self.slice_act_cr_qp_offset
    }

    pub fn cu_chroma_qp_offset_enabled_flag(&self) -> bool {
        self.cu_chroma_qp_offset_enabled_flag
    }

    pub fn deblocking_filter_override_flag(&self) -> bool {
        self.deblocking_filter_override_flag
    }

    pub fn deblocking_filter_disabled_flag(&self) -> bool {
        self.deblocking_filter_disabled_flag
    }

    pub fn beta_offset_div2(&self) -> i8 {
        self.beta_offset_div2
    }

    pub fn tc_offset_div2(&self) -> i8 {
        self.tc_offset_div2
    }

    pub fn loop_filter_across_slices_enabled_flag(&self) -> bool {
        self.loop_filter_across_slices_enabled_flag
    }

    pub fn num_entry_point_offsets(&self) -> u32 {
        self.num_entry_point_offsets
    }

    pub fn offset_len_minus1(&self) -> u8 {
        self.offset_len_minus1
    }

    pub fn entry_point_offset_minus1(&self) -> [u32; 32] {
        self.entry_point_offset_minus1
    }

    pub fn num_pic_total_curr(&self) -> u32 {
        self.num_pic_total_curr
    }

    pub fn header_bit_size(&self) -> u32 {
        self.header_bit_size
    }

    pub fn n_emulation_prevention_bytes(&self) -> u32 {
        self.n_emulation_prevention_bytes
    }

    pub fn curr_rps_idx(&self) -> u8 {
        self.curr_rps_idx
    }

    pub fn short_term_ref_pic_set(&self) -> &ShortTermRefPicSet {
        &self.short_term_ref_pic_set
    }
}

impl Default for SliceHeader {
    fn default() -> Self {
        Self {
            first_slice_segment_in_pic_flag: Default::default(),
            no_output_of_prior_pics_flag: Default::default(),
            pic_parameter_set_id: Default::default(),
            dependent_slice_segment_flag: Default::default(),
            segment_address: Default::default(),
            type_: Default::default(),
            pic_output_flag: true,
            colour_plane_id: Default::default(),
            pic_order_cnt_lsb: Default::default(),
            short_term_ref_pic_set_sps_flag: Default::default(),
            short_term_ref_pic_set: Default::default(),
            short_term_ref_pic_set_idx: Default::default(),
            num_long_term_sps: Default::default(),
            num_long_term_pics: Default::default(),
            lt_idx_sps: Default::default(),
            poc_lsb_lt: Default::default(),
            used_by_curr_pic_lt: Default::default(),
            delta_poc_msb_present_flag: Default::default(),
            delta_poc_msb_cycle_lt: Default::default(),
            temporal_mvp_enabled_flag: Default::default(),
            sao_luma_flag: Default::default(),
            sao_chroma_flag: Default::default(),
            num_ref_idx_active_override_flag: Default::default(),
            num_ref_idx_l0_active_minus1: Default::default(),
            num_ref_idx_l1_active_minus1: Default::default(),
            ref_pic_list_modification: Default::default(),
            mvd_l1_zero_flag: Default::default(),
            cabac_init_flag: Default::default(),
            collocated_from_l0_flag: true,
            collocated_ref_idx: Default::default(),
            pred_weight_table: Default::default(),
            five_minus_max_num_merge_cand: Default::default(),
            use_integer_mv_flag: Default::default(),
            qp_delta: Default::default(),
            cb_qp_offset: Default::default(),
            cr_qp_offset: Default::default(),
            slice_act_y_qp_offset: Default::default(),
            slice_act_cb_qp_offset: Default::default(),
            slice_act_cr_qp_offset: Default::default(),
            cu_chroma_qp_offset_enabled_flag: Default::default(),
            deblocking_filter_override_flag: Default::default(),
            deblocking_filter_disabled_flag: Default::default(),
            beta_offset_div2: Default::default(),
            tc_offset_div2: Default::default(),
            loop_filter_across_slices_enabled_flag: Default::default(),
            num_entry_point_offsets: Default::default(),
            offset_len_minus1: Default::default(),
            entry_point_offset_minus1: Default::default(),
            num_pic_total_curr: Default::default(),
            header_bit_size: Default::default(),
            n_emulation_prevention_bytes: Default::default(),
            curr_rps_idx: Default::default(),
        }
    }
}

/// A H265 slice. An integer number of macroblocks or macroblock pairs ordered
/// consecutively in the raster scan within a particular slice group
pub struct Slice<T> {
    /// The slice header.
    header: SliceHeader,
    /// The NAL unit backing this slice.
    nalu: Nalu<T>,
}

impl<T> Slice<T> {
    /// Get a reference to the slice's header.
    pub fn header(&self) -> &SliceHeader {
        &self.header
    }

    /// Get a reference to the slice's nalu.
    pub fn nalu(&self) -> &Nalu<T> {
        &self.nalu
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct SublayerHrdParameters {
    // NOTE: The value of CpbCnt is cpb_cnt_minus1[i] + 1, and cpb_cnt_minus1
    // ranges from 0..=31
    /// bit_rate_value_minus1[ i ] (together with bit_rate_scale) specifies the
    /// maximum input bit rate for the i-th CPB when the CPB operates at the
    /// access unit level
    bit_rate_value_minus1: [u32; 32],
    /// cpb_size_value_minus1[ i ] is used together with cpb_size_scale to
    /// specify the i-th CPB size when the CPB operates at the access unit
    /// level.
    cpb_size_value_minus1: [u32; 32],
    /// cpb_size_du_value_minus1[ i ] is used together with cpb_size_du_scale to
    /// specify the i-th CPB size when the CPB operates at sub-picture level.
    cpb_size_du_value_minus1: [u32; 32],
    /// bit_rate_du_value_minus1[ i ] (together with bit_rate_scale) specifies
    /// the maximum input bit rate for the i-th CPB when the CPB operates at the
    /// sub-picture level.
    bit_rate_du_value_minus1: [u32; 32],
    /// cbr_flag[ i ] not set specifies that to decode this CVS by the HRD using
    /// the i-th CPB specification.
    cbr_flag: [bool; 32],
}

impl SublayerHrdParameters {
    pub fn cpb_size_value_minus1(&self) -> [u32; 32] {
        self.cpb_size_value_minus1
    }

    pub fn cpb_size_du_value_minus1(&self) -> [u32; 32] {
        self.cpb_size_du_value_minus1
    }

    pub fn bit_rate_du_value_minus1(&self) -> [u32; 32] {
        self.bit_rate_du_value_minus1
    }

    pub fn cbr_flag(&self) -> [bool; 32] {
        self.cbr_flag
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct HrdParams {
    /// When set, specifies that NAL HRD parameters (pertaining to the Type II
    /// bitstream conformance point) are present in the hrd_parameters( ) syntax
    /// structure. When not set, specifies that NAL HRD parameters are not
    /// present in the hrd_parameters( ) syntax structure.
    nal_hrd_parameters_present_flag: bool,
    /// When set, specifies that VCL HRD parameters (pertaining to the Type I
    /// bitstream conformance point) are present in the hrd_parameters( ) syntax
    /// structure. When not set, specifies that VCL HRD parameters are not
    /// present in the hrd_parameters( ) syntax structure.
    vcl_hrd_parameters_present_flag: bool,
    /// When set, specifies that sub-picture level HRD parameters are present
    /// and the HRD may operate at access unit level or sub-picture level. When
    /// not set, specifies that sub-picture level HRD parameters are not present
    /// and the HRD operates at access unit level.
    sub_pic_hrd_params_present_flag: bool,
    /// Used to specify the clock sub-tick. A clock sub-tick is the minimum
    /// interval of time that can be represented in the coded data when
    /// sub_pic_hrd_params_present_flag is set.
    tick_divisor_minus2: u8,
    /// du_cpb_removal_delay_increment_length_minus1 plus 1 specifies the
    /// length, in bits, of the du_cpb_removal_delay_increment_minus1[ i ] and
    /// du_common_cpb_removal_delay_increment_minus1 syntax elements of the
    /// picture timing SEI message and the du_spt_cpb_removal_delay_increment
    /// syntax element in the decoding unit information SEI message.
    du_cpb_removal_delay_increment_length_minus1: u8,
    /// When set, specifies that sub-picture level CPB removal delay parameters
    /// are present in picture timing SEI messages and no decoding unit
    /// information SEI message is available (in the CVS or provided through
    /// external means not specified in this Specification).  When not set,
    /// specifies that sub-picture level CPB removal delay parameters are
    /// present in decoding unit information SEI messages and picture timing SEI
    /// messages do not include sub-picture level CPB removal delay parameters.
    sub_pic_cpb_params_in_pic_timing_sei_flag: bool,
    /// dpb_output_delay_du_length_minus1 plus 1 specifies the length, in bits,
    /// of the pic_dpb_output_du_delay syntax element in the picture timing SEI
    /// message and the pic_spt_dpb_output_du_delay syntax element in the
    /// decoding unit information SEI message.
    dpb_output_delay_du_length_minus1: u8,
    /// Together with bit_rate_value_minus1[ i ], specifies the maximum input
    /// bit rate of the i-th CPB.
    bit_rate_scale: u8,
    /// Together with cpb_size_du_value_minus1[ i ], specifies the CPB size of
    /// the i-th CPB when the CPB operates at sub-picture level.
    cpb_size_scale: u8,
    /// Together with cpb_size_du_value_minus1[ i ], specifies the CPB size of
    /// the i-th CPB when the CPB operates at sub-picture level.
    cpb_size_du_scale: u8,
    /// initial_cpb_removal_delay_length_minus1 plus 1 specifies the length, in
    /// bits, of the nal_initial_cpb_removal_delay[ i ],
    /// nal_initial_cpb_removal_offset[ i ], vcl_initial_cpb_removal_delay[ i ]
    /// and vcl_initial_cpb_removal_offset[ i ] syntax elements of the buffering
    /// period SEI message.
    initial_cpb_removal_delay_length_minus1: u8,
    /// au_cpb_removal_delay_length_minus1 plus 1 specifies the length, in bits,
    /// of the cpb_delay_offset syntax element in the buffering period SEI
    /// message and the au_cpb_removal_delay_minus1 syntax element in the
    /// picture timing SEI message.
    au_cpb_removal_delay_length_minus1: u8,
    /// dpb_output_delay_length_minus1 plus 1 specifies the length, in bits, of
    /// the dpb_delay_offset syntax element in the buffering period SEI message
    /// and the pic_dpb_output_delay syntax element in the picture timing SEI
    /// message.
    dpb_output_delay_length_minus1: u8,
    /// fixed_pic_rate_general_flag[ i ] set indicates that, when HighestTid is
    /// equal to i, the temporal distance between the HRD output times of
    /// consecutive pictures in output order is constrained as specified in the
    /// specification. fixed_pic_rate_general_flag[ i ] not set indicates that
    /// this constraint may not apply.
    fixed_pic_rate_general_flag: [bool; 7],
    /// fixed_pic_rate_within_cvs_flag[ i ] set indicates that, when HighestTid
    /// is equal to i, the temporal distance between the HRD output times of
    /// consecutive pictures in output order is constrained as specified in the
    /// specification. fixed_pic_rate_within_cvs_flag[ i ] not set indicates
    /// that this constraint may not apply.
    fixed_pic_rate_within_cvs_flag: [bool; 7],
    /// elemental_duration_in_tc_minus1[ i ] plus 1 (when present) specifies,
    /// when HighestTid is equal to i, the temporal distance, in clock ticks,
    /// between the elemental units that specify the HRD output times of
    /// consecutive pictures in output order as specified in the specification.
    elemental_duration_in_tc_minus1: [u32; 7],
    /// low_delay_hrd_flag[ i ] specifies the HRD operational mode, when
    /// HighestTid is equal to i, as specified in Annex C or clause F.13.
    low_delay_hrd_flag: [bool; 7],
    /// cpb_cnt_minus1[ i ] plus 1 specifies the number of alternative CPB
    /// specifications in the bitstream of the CVS when HighestTid is equal to
    /// i.
    cpb_cnt_minus1: [u32; 7],
    /// The NAL HRD data.
    nal_hrd: [SublayerHrdParameters; 7],
    /// The VCL HRD data.
    vcl_hrd: [SublayerHrdParameters; 7],
}

impl HrdParams {
    pub fn nal_hrd_parameters_present_flag(&self) -> bool {
        self.nal_hrd_parameters_present_flag
    }

    pub fn vcl_hrd_parameters_present_flag(&self) -> bool {
        self.vcl_hrd_parameters_present_flag
    }

    pub fn sub_pic_hrd_params_present_flag(&self) -> bool {
        self.sub_pic_hrd_params_present_flag
    }

    pub fn tick_divisor_minus2(&self) -> u8 {
        self.tick_divisor_minus2
    }

    pub fn du_cpb_removal_delay_increment_length_minus1(&self) -> u8 {
        self.du_cpb_removal_delay_increment_length_minus1
    }

    pub fn sub_pic_cpb_params_in_pic_timing_sei_flag(&self) -> bool {
        self.sub_pic_cpb_params_in_pic_timing_sei_flag
    }

    pub fn dpb_output_delay_du_length_minus1(&self) -> u8 {
        self.dpb_output_delay_du_length_minus1
    }

    pub fn bit_rate_scale(&self) -> u8 {
        self.bit_rate_scale
    }

    pub fn cpb_size_scale(&self) -> u8 {
        self.cpb_size_scale
    }

    pub fn cpb_size_du_scale(&self) -> u8 {
        self.cpb_size_du_scale
    }

    pub fn initial_cpb_removal_delay_length_minus1(&self) -> u8 {
        self.initial_cpb_removal_delay_length_minus1
    }

    pub fn au_cpb_removal_delay_length_minus1(&self) -> u8 {
        self.au_cpb_removal_delay_length_minus1
    }

    pub fn dpb_output_delay_length_minus1(&self) -> u8 {
        self.dpb_output_delay_length_minus1
    }

    pub fn fixed_pic_rate_general_flag(&self) -> [bool; 7] {
        self.fixed_pic_rate_general_flag
    }

    pub fn fixed_pic_rate_within_cvs_flag(&self) -> [bool; 7] {
        self.fixed_pic_rate_within_cvs_flag
    }

    pub fn elemental_duration_in_tc_minus1(&self) -> [u32; 7] {
        self.elemental_duration_in_tc_minus1
    }

    pub fn low_delay_hrd_flag(&self) -> [bool; 7] {
        self.low_delay_hrd_flag
    }

    pub fn cpb_cnt_minus1(&self) -> [u32; 7] {
        self.cpb_cnt_minus1
    }

    pub fn nal_hrd(&self) -> &[SublayerHrdParameters; 7] {
        &self.nal_hrd
    }

    pub fn vcl_hrd(&self) -> &[SublayerHrdParameters; 7] {
        &self.vcl_hrd
    }
}

impl Default for HrdParams {
    fn default() -> Self {
        Self {
            initial_cpb_removal_delay_length_minus1: 23,
            au_cpb_removal_delay_length_minus1: 23,
            dpb_output_delay_du_length_minus1: 23,
            nal_hrd_parameters_present_flag: Default::default(),
            vcl_hrd_parameters_present_flag: Default::default(),
            sub_pic_hrd_params_present_flag: Default::default(),
            tick_divisor_minus2: Default::default(),
            du_cpb_removal_delay_increment_length_minus1: Default::default(),
            sub_pic_cpb_params_in_pic_timing_sei_flag: Default::default(),
            bit_rate_scale: Default::default(),
            cpb_size_scale: Default::default(),
            cpb_size_du_scale: Default::default(),
            dpb_output_delay_length_minus1: Default::default(),
            fixed_pic_rate_general_flag: Default::default(),
            fixed_pic_rate_within_cvs_flag: Default::default(),
            elemental_duration_in_tc_minus1: Default::default(),
            low_delay_hrd_flag: Default::default(),
            cpb_cnt_minus1: Default::default(),
            nal_hrd: Default::default(),
            vcl_hrd: Default::default(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VuiParams {
    /// When set, specifies that aspect_ratio_idc is present.  When not set,
    /// specifies that aspect_ratio_idc is not present.
    aspect_ratio_info_present_flag: bool,
    /// Specifies the value of the sample aspect ratio of the luma samples.
    aspect_ratio_idc: u32,
    /// Indicates the horizontal size of the sample aspect ratio (in arbitrary
    /// units).
    sar_width: u32,
    /// Indicates the vertical size of the sample aspect ratio (in arbitrary
    /// units).
    sar_height: u32,
    /// When set, specifies that the overscan_appropriate_flag is present. When
    /// not set, the preferred display method for the video signal is
    /// unspecified.
    overscan_info_present_flag: bool,
    /// When set indicates that the cropped decoded pictures output are suitable
    /// for display using overscan. When not set, indicates that the cropped
    /// decoded pictures output contain visually important information in the
    /// entire region out to the edges of the conformance cropping window of the
    /// picture, such that the cropped decoded pictures output should not be
    /// displayed using overscan.
    overscan_appropriate_flag: bool,
    /// When set, specifies that video_format, video_full_range_flag and
    /// colour_description_present_flag are present.  When not set, specify that
    /// video_format, video_full_range_flag and colour_description_present_flag
    /// are not present.
    video_signal_type_present_flag: bool,
    /// Indicates the representation of the pictures as specified in Table E.2,
    /// before being coded in accordance with this Specification.
    video_format: u8,
    /// Indicates the black level and range of the luma and chroma signals as
    /// derived from E′Y, E′PB, and E′PR or E′R, E′G, and E′B real-valued
    /// component signals.
    video_full_range_flag: bool,
    /// When set, specifies that colour_primaries, transfer_characteristics, and
    /// matrix_coeffs are present. When not set, specifies that
    /// colour_primaries, transfer_characteristics, and matrix_coeffs are not
    /// present.
    colour_description_present_flag: bool,
    /// Indicates the chromaticity coordinates of the source primaries as
    /// specified in Table E.3 in terms of the CIE 1931 definition of x and y as
    /// specified in ISO 11664-1.
    colour_primaries: u32,
    /// See table E.4 in the specification.
    transfer_characteristics: u32,
    /// Describes the matrix coefficients used in deriving luma and chroma
    /// signals from the green, blue, and red, or Y, Z, and X primaries, as
    /// specified in Table E.5.
    matrix_coeffs: u32,
    /// When true, specifies that chroma_sample_loc_type_top_field and
    /// chroma_sample_loc_type_bottom_field are present. When false, specifies
    /// that chroma_sample_loc_type_top_field and
    /// chroma_sample_loc_type_bottom_field are not present.
    chroma_loc_info_present_flag: bool,
    /// See the specification for more details.
    chroma_sample_loc_type_top_field: u32,
    /// See the specification for more details.
    chroma_sample_loc_type_bottom_field: u32,
    /// When true, indicates that the value of all decoded chroma samples is
    /// equal to 1 << ( BitDepthC − 1 ). When false, provides no indication of
    /// decoded chroma sample values.
    neutral_chroma_indication_flag: bool,
    /// When true, indicates that the CVS conveys pictures that represent
    /// fields, and specifies that a picture timing SEI message shall be present
    /// in every access unit of the current CVS. When false, indicates that the
    /// CVS conveys pictures that represent frames and that a picture timing SEI
    /// message may or may not be present in any access unit of the current CVS.
    field_seq_flag: bool,
    /// When true, specifies that picture timing SEI messages are present for
    /// every picture and include the pic_struct, source_scan_type and
    /// duplicate_flag syntax elements. When false, specifies that the
    /// pic_struct syntax element is not present in picture timing SEI messages.
    frame_field_info_present_flag: bool,
    /// When true, indicates that the default display window parameters follow
    /// next in the VUI. When false, indicates that the default display window
    /// parameters are not present.
    default_display_window_flag: bool,
    /// Specifies the samples of the pictures in the CVS that are within the
    /// default display window, in terms of a rectangular region specified in
    /// picture coordinates for display.
    def_disp_win_left_offset: u32,
    /// Specifies the samples of the pictures in the CVS that are within the
    /// default display window, in terms of a rectangular region specified in
    /// picture coordinates for display.
    def_disp_win_right_offset: u32,
    /// Specifies the samples of the pictures in the CVS that are within the
    /// default display window, in terms of a rectangular region specified in
    /// picture coordinates for display.
    def_disp_win_top_offset: u32,
    /// Specifies the samples of the pictures in the CVS that are within the
    /// default display window, in terms of a rectangular region specified in
    /// picture coordinates for display.
    def_disp_win_bottom_offset: u32,
    /// When set, specifies that vui_num_units_in_tick, vui_time_scale,
    /// vui_poc_proportional_to_timing_flag and vui_hrd_parameters_present_flag
    /// are present in the vui_parameters( ) syntax structure.  When not set,
    /// specifies that vui_num_units_in_tick, vui_time_scale,
    /// vui_poc_proportional_to_timing_flag and vui_hrd_parameters_present_flag
    /// are not present in the vui_parameters( ) syntax structure
    timing_info_present_flag: bool,
    /// The number of time units of a clock operating at the frequency
    /// vui_time_scale Hz that corresponds to one increment (called a clock
    /// tick) of a clock tick counter.
    num_units_in_tick: u32,
    /// Is the number of time units that pass in one second. For example, a time
    /// coordinate system that measures time using a 27 MHz clock has a
    /// vui_time_scale of 27 000 000.
    time_scale: u32,
    /// When set, indicates that the picture order count value for each picture
    /// in the CVS that is not the first picture in the CVS, in decoding order,
    /// is proportional to the output time of the picture relative to the output
    /// time of the first picture in the CVS.  When not set, indicates that the
    /// picture order count value for each picture in the CVS that is not the
    /// first picture in the CVS, in decoding order, may or may not be
    /// proportional to the output time of the picture relative to the output
    /// time of the first picture in the CVS.
    poc_proportional_to_timing_flag: bool,
    /// vui_num_ticks_poc_diff_one_minus1 plus 1 specifies the number of clock
    /// ticks corresponding to a difference of picture order count values equal
    /// to 1.
    num_ticks_poc_diff_one_minus1: u32,
    /// When set, specifies that the syntax structure hrd_parameters( ) is
    /// present in the vui_parameters( ) syntax structure.  When not set,
    /// specifies that the syntax structure hrd_parameters( ) is not present in
    /// the vui_parameters( ) syntax structure.
    hrd_parameters_present_flag: bool,
    /// The hrd_parameters() data.
    hrd: HrdParams,
    /// When set, specifies that the bitstream restriction parameters for the
    /// CVS are present. When not set, specifies that the bitstream restriction
    /// parameters for the CVS are not present.
    bitstream_restriction_flag: bool,
    /// When set, indicates that each PPS that is active in the CVS has the same
    /// value of the syntax elements num_tile_columns_minus1,
    /// num_tile_rows_minus1, uniform_spacing_flag, column_width_minus1[ i ],
    /// row_height_minus1[ i ] and loop_filter_across_tiles_enabled_flag, when
    /// present. When not set, indicates that tiles syntax elements in different
    /// PPSs may or may not have the same value
    tiles_fixed_structure_flag: bool,
    /// When not set, indicates that no sample outside the picture boundaries
    /// and no sample at a fractional sample position for which the sample value
    /// is derived using one or more samples outside the picture boundaries is
    /// used for inter prediction of any sample.  When set, indicates that one
    /// or more samples outside the picture boundaries may be used in inter
    /// prediction.
    motion_vectors_over_pic_boundaries_flag: bool,
    /// When set, indicates that all P and B slices (when present) that belong
    /// to the same picture have an identical reference picture list 0 and that
    /// all B slices (when present) that belong to the same picture have an
    /// identical reference picture list 1.
    restricted_ref_pic_lists_flag: bool,
    /// When not equal to 0, establishes a bound on the maximum possible size of
    /// distinct coded spatial segmentation regions in the pictures of the CVS.
    min_spatial_segmentation_idc: u32,
    /// Indicates a number of bytes not exceeded by the sum of the sizes of the
    /// VCL NAL units associated with any coded picture in the CVS.
    max_bytes_per_pic_denom: u32,
    /// Indicates an upper bound for the number of coded bits of coding_unit( )
    /// data for anycoding block in any picture of the CVS.
    max_bits_per_min_cu_denom: u32,
    /// Indicate the maximum absolute value of a decoded horizontal and vertical
    /// motion vector component, respectively, in quarter luma sample units, for
    /// all pictures in the CVS.
    log2_max_mv_length_horizontal: u32,
    /// Indicate the maximum absolute value of a decoded horizontal and vertical
    /// motion vector component, respectively, in quarter luma sample units, for
    /// all pictures in the CVS.
    log2_max_mv_length_vertical: u32,
}

impl VuiParams {
    pub fn aspect_ratio_info_present_flag(&self) -> bool {
        self.aspect_ratio_info_present_flag
    }

    pub fn aspect_ratio_idc(&self) -> u32 {
        self.aspect_ratio_idc
    }

    pub fn sar_width(&self) -> u32 {
        self.sar_width
    }

    pub fn sar_height(&self) -> u32 {
        self.sar_height
    }

    pub fn overscan_info_present_flag(&self) -> bool {
        self.overscan_info_present_flag
    }

    pub fn overscan_appropriate_flag(&self) -> bool {
        self.overscan_appropriate_flag
    }

    pub fn video_signal_type_present_flag(&self) -> bool {
        self.video_signal_type_present_flag
    }

    pub fn video_format(&self) -> u8 {
        self.video_format
    }

    pub fn video_full_range_flag(&self) -> bool {
        self.video_full_range_flag
    }

    pub fn colour_description_present_flag(&self) -> bool {
        self.colour_description_present_flag
    }

    pub fn colour_primaries(&self) -> u32 {
        self.colour_primaries
    }

    pub fn transfer_characteristics(&self) -> u32 {
        self.transfer_characteristics
    }

    pub fn matrix_coeffs(&self) -> u32 {
        self.matrix_coeffs
    }

    pub fn chroma_loc_info_present_flag(&self) -> bool {
        self.chroma_loc_info_present_flag
    }

    pub fn chroma_sample_loc_type_top_field(&self) -> u32 {
        self.chroma_sample_loc_type_top_field
    }

    pub fn chroma_sample_loc_type_bottom_field(&self) -> u32 {
        self.chroma_sample_loc_type_bottom_field
    }

    pub fn neutral_chroma_indication_flag(&self) -> bool {
        self.neutral_chroma_indication_flag
    }

    pub fn field_seq_flag(&self) -> bool {
        self.field_seq_flag
    }

    pub fn frame_field_info_present_flag(&self) -> bool {
        self.frame_field_info_present_flag
    }

    pub fn default_display_window_flag(&self) -> bool {
        self.default_display_window_flag
    }

    pub fn def_disp_win_left_offset(&self) -> u32 {
        self.def_disp_win_left_offset
    }

    pub fn def_disp_win_right_offset(&self) -> u32 {
        self.def_disp_win_right_offset
    }

    pub fn def_disp_win_top_offset(&self) -> u32 {
        self.def_disp_win_top_offset
    }

    pub fn def_disp_win_bottom_offset(&self) -> u32 {
        self.def_disp_win_bottom_offset
    }

    pub fn timing_info_present_flag(&self) -> bool {
        self.timing_info_present_flag
    }

    pub fn num_units_in_tick(&self) -> u32 {
        self.num_units_in_tick
    }

    pub fn time_scale(&self) -> u32 {
        self.time_scale
    }

    pub fn poc_proportional_to_timing_flag(&self) -> bool {
        self.poc_proportional_to_timing_flag
    }

    pub fn num_ticks_poc_diff_one_minus1(&self) -> u32 {
        self.num_ticks_poc_diff_one_minus1
    }

    pub fn hrd_parameters_present_flag(&self) -> bool {
        self.hrd_parameters_present_flag
    }

    pub fn hrd(&self) -> &HrdParams {
        &self.hrd
    }

    pub fn bitstream_restriction_flag(&self) -> bool {
        self.bitstream_restriction_flag
    }

    pub fn tiles_fixed_structure_flag(&self) -> bool {
        self.tiles_fixed_structure_flag
    }

    pub fn motion_vectors_over_pic_boundaries_flag(&self) -> bool {
        self.motion_vectors_over_pic_boundaries_flag
    }

    pub fn restricted_ref_pic_lists_flag(&self) -> bool {
        self.restricted_ref_pic_lists_flag
    }

    pub fn min_spatial_segmentation_idc(&self) -> u32 {
        self.min_spatial_segmentation_idc
    }

    pub fn max_bytes_per_pic_denom(&self) -> u32 {
        self.max_bytes_per_pic_denom
    }

    pub fn max_bits_per_min_cu_denom(&self) -> u32 {
        self.max_bits_per_min_cu_denom
    }

    pub fn log2_max_mv_length_horizontal(&self) -> u32 {
        self.log2_max_mv_length_horizontal
    }

    pub fn log2_max_mv_length_vertical(&self) -> u32 {
        self.log2_max_mv_length_vertical
    }
}

impl Default for VuiParams {
    fn default() -> Self {
        Self {
            aspect_ratio_info_present_flag: Default::default(),
            aspect_ratio_idc: Default::default(),
            sar_width: Default::default(),
            sar_height: Default::default(),
            overscan_info_present_flag: Default::default(),
            overscan_appropriate_flag: Default::default(),
            video_signal_type_present_flag: Default::default(),
            video_format: 5,
            video_full_range_flag: Default::default(),
            colour_description_present_flag: Default::default(),
            colour_primaries: 2,
            transfer_characteristics: 2,
            matrix_coeffs: 2,
            chroma_loc_info_present_flag: Default::default(),
            chroma_sample_loc_type_top_field: Default::default(),
            chroma_sample_loc_type_bottom_field: Default::default(),
            neutral_chroma_indication_flag: Default::default(),
            field_seq_flag: Default::default(),
            frame_field_info_present_flag: Default::default(),
            default_display_window_flag: Default::default(),
            def_disp_win_left_offset: Default::default(),
            def_disp_win_right_offset: Default::default(),
            def_disp_win_top_offset: Default::default(),
            def_disp_win_bottom_offset: Default::default(),
            timing_info_present_flag: Default::default(),
            num_units_in_tick: Default::default(),
            time_scale: Default::default(),
            poc_proportional_to_timing_flag: Default::default(),
            num_ticks_poc_diff_one_minus1: Default::default(),
            hrd_parameters_present_flag: Default::default(),
            hrd: Default::default(),
            bitstream_restriction_flag: Default::default(),
            tiles_fixed_structure_flag: Default::default(),
            motion_vectors_over_pic_boundaries_flag: true,
            restricted_ref_pic_lists_flag: Default::default(),
            min_spatial_segmentation_idc: Default::default(),
            max_bytes_per_pic_denom: 2,
            max_bits_per_min_cu_denom: 1,
            log2_max_mv_length_horizontal: 15,
            log2_max_mv_length_vertical: 15,
        }
    }
}

#[derive(Debug, Default)]
pub struct Parser {
    active_vpses: BTreeMap<u8, Vps>,
    active_spses: BTreeMap<u8, Sps>,
    active_ppses: BTreeMap<u8, Pps>,
}

impl Parser {
    /// Parse a VPS NALU.
    pub fn parse_vps<T: AsRef<[u8]>>(&mut self, nalu: &Nalu<T>) -> anyhow::Result<&Vps> {
        if !matches!(nalu.header().type_, NaluType::VpsNut) {
            return Err(anyhow!(
                "Invalid NALU type, expected {:?}, got {:?}",
                NaluType::VpsNut,
                nalu.header().type_
            ));
        }

        let data = nalu.as_ref();
        let header = nalu.header();
        let hdr_len = header.len();
        // Skip the header
        let mut r = NaluReader::new(&data[hdr_len..]);

        let mut vps = Vps {
            video_parameter_set_id: r.read_bits(4)?,
            base_layer_internal_flag: r.read_bit()?,
            base_layer_available_flag: r.read_bit()?,
            max_layers_minus1: r.read_bits(6)?,
            max_sub_layers_minus1: r.read_bits(3)?,
            temporal_id_nesting_flag: r.read_bit()?,
            ..Default::default()
        };

        r.skip_bits(16)?; // vps_reserved_0xffff_16bits

        let ptl = &mut vps.profile_tier_level;
        Self::parse_profile_tier_level(ptl, &mut r, true, vps.max_sub_layers_minus1)?;

        vps.sub_layer_ordering_info_present_flag = r.read_bit()?;

        let start = if vps.sub_layer_ordering_info_present_flag {
            0
        } else {
            vps.max_sub_layers_minus1
        } as usize;

        for i in start..=usize::from(vps.max_sub_layers_minus1) {
            vps.max_dec_pic_buffering_minus1[i] = r.read_ue_max(15)?;
            vps.max_num_reorder_pics[i] = r.read_ue_max(vps.max_dec_pic_buffering_minus1[i])?;
            vps.max_latency_increase_plus1[i] = r.read_ue()?;

            if i > 0 {
                if vps.max_dec_pic_buffering_minus1[i] >= vps.max_dec_pic_buffering_minus1[i - 1] {
                    return Err(anyhow!(
                        "Invalid max_dec_pic_buffering_minus1[{}]: {}",
                        i,
                        vps.max_dec_pic_buffering_minus1[i]
                    ));
                }

                if vps.max_num_reorder_pics[i] >= vps.max_num_reorder_pics[i - 1] {
                    return Err(anyhow!(
                        "Invalid max_num_reorder_pics[{}]: {}",
                        i,
                        vps.max_num_reorder_pics[i]
                    ));
                }
            }
        }

        // vps_sub_layer_ordering_info_present_flag equal to 0 specifies that
        // the values of vps_max_dec_pic_buffering_minus1[
        // vps_max_sub_layers_minus1 ], vps_max_num_reorder_pics[ vps_max_sub_
        // layers_minus1 ] and vps_max_latency_increase_plus1[
        // vps_max_sub_layers_minus1 ] apply to all sub-layers
        if !vps.sub_layer_ordering_info_present_flag {
            let max_num_sublayers = usize::from(vps.max_sub_layers_minus1);
            for i in 0..max_num_sublayers {
                vps.max_dec_pic_buffering_minus1[i] =
                    vps.max_dec_pic_buffering_minus1[max_num_sublayers];

                vps.max_num_reorder_pics[i] = vps.max_num_reorder_pics[max_num_sublayers];

                vps.max_latency_increase_plus1[i] =
                    vps.max_latency_increase_plus1[max_num_sublayers];
            }
        }

        vps.max_layer_id = r.read_bits(6)?;
        if vps.max_layer_id > 62 {
            return Err(anyhow!("Invalid max_layer_id {}", vps.max_layer_id));
        }

        vps.num_layer_sets_minus1 = r.read_ue_max(1023)?;

        for _ in 1..=vps.num_layer_sets_minus1 {
            for _ in 0..=vps.max_layer_id {
                // Skip layer_id_included_flag[i][j] for now.
                r.skip_bits(1)?;
            }
        }

        vps.timing_info_present_flag = r.read_bit()?;

        if vps.timing_info_present_flag {
            vps.num_units_in_tick = r.read_bits::<u32>(31)? << 1;
            vps.num_units_in_tick |= r.read_bits::<u32>(1)?;

            vps.time_scale = r.read_bits::<u32>(31)? << 1;
            vps.time_scale |= r.read_bits::<u32>(1)?;

            vps.poc_proportional_to_timing_flag = r.read_bit()?;
            if vps.poc_proportional_to_timing_flag {
                vps.num_ticks_poc_diff_one_minus1 = r.read_ue()?;
            }

            vps.num_hrd_parameters = r.read_ue()?;

            for i in 0..vps.num_hrd_parameters as usize {
                vps.hrd_layer_set_idx.push(r.read_ue()?);
                if i > 0 {
                    vps.cprms_present_flag.push(r.read_bit()?);
                }

                let mut hrd = HrdParams::default();
                Self::parse_hrd_parameters(
                    vps.cprms_present_flag[i],
                    vps.max_sub_layers_minus1,
                    &mut hrd,
                    &mut r,
                )?;

                vps.hrd_parameters.push(hrd);
            }
        }

        vps.extension_flag = r.read_bit()?;

        let key = vps.video_parameter_set_id;
        self.active_vpses.insert(key, vps);

        if self.active_spses.keys().len() > MAX_VPS_COUNT {
            return Err(anyhow!(
                "Broken data: Number of active SPSs > MAX_SPS_COUNT"
            ));
        }

        Ok(self.get_vps(key).unwrap())
    }

    fn parse_profile_tier_level<T: AsRef<[u8]>>(
        ptl: &mut ProfileTierLevel,
        r: &mut NaluReader<T>,
        profile_present_flag: bool,
        sps_max_sub_layers_minus_1: u8,
    ) -> anyhow::Result<()> {
        if profile_present_flag {
            ptl.general_profile_space = r.read_bits(2)?;
            ptl.general_tier_flag = r.read_bit()?;
            ptl.general_profile_idc = r.read_bits(5)?;

            for i in 0..32 {
                ptl.general_profile_compatibility_flag[i] = r.read_bit()?;
            }

            ptl.general_progressive_source_flag = r.read_bit()?;
            ptl.general_interlaced_source_flag = r.read_bit()?;
            ptl.general_non_packed_constraint_flag = r.read_bit()?;
            ptl.general_frame_only_constraint_flag = r.read_bit()?;

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
                ptl.general_max_12bit_constraint_flag = r.read_bit()?;
                ptl.general_max_10bit_constraint_flag = r.read_bit()?;
                ptl.general_max_8bit_constraint_flag = r.read_bit()?;
                ptl.general_max_422chroma_constraint_flag = r.read_bit()?;
                ptl.general_max_420chroma_constraint_flag = r.read_bit()?;
                ptl.general_max_monochrome_constraint_flag = r.read_bit()?;
                ptl.general_intra_constraint_flag = r.read_bit()?;
                ptl.general_one_picture_only_constraint_flag = r.read_bit()?;
                ptl.general_lower_bit_rate_constraint_flag = r.read_bit()?;
                if ptl.general_profile_idc == 5
                    || ptl.general_profile_compatibility_flag[5]
                    || ptl.general_profile_idc == 9
                    || ptl.general_profile_compatibility_flag[9]
                    || ptl.general_profile_idc == 10
                    || ptl.general_profile_compatibility_flag[10]
                    || ptl.general_profile_idc == 11
                    || ptl.general_profile_compatibility_flag[11]
                {
                    ptl.general_max_14bit_constraint_flag = r.read_bit()?;
                    // Skip general_reserved_zero_33bits
                    r.skip_bits(31)?;
                    r.skip_bits(2)?;
                } else {
                    // Skip general_reserved_zero_34bits
                    r.skip_bits(31)?;
                    r.skip_bits(3)?;
                }
            } else if ptl.general_profile_idc == 2 || ptl.general_profile_compatibility_flag[2] {
                // Skip general_reserved_zero_7bits
                r.skip_bits(7)?;
                ptl.general_one_picture_only_constraint_flag = r.read_bit()?;
                // Skip general_reserved_zero_35bits
                r.skip_bits(31)?;
                r.skip_bits(4)?;
            } else {
                r.skip_bits(31)?;
                r.skip_bits(12)?;
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
                ptl.general_inbld_flag = r.read_bit()?;
            } else {
                r.skip_bits(1)?;
            }
        }

        ptl.general_level_idc = r.read_bits(8)?;

        for i in 0..sps_max_sub_layers_minus_1 as usize {
            ptl.sub_layer_profile_present_flag[i] = r.read_bit()?;
            ptl.sub_layer_level_present_flag[i] = r.read_bit()?;
        }

        if sps_max_sub_layers_minus_1 > 0 {
            for _ in sps_max_sub_layers_minus_1..8 {
                r.skip_bits(2)?;
            }
        }

        for i in 0..sps_max_sub_layers_minus_1 as usize {
            if ptl.sub_layer_level_present_flag[i] {
                ptl.sub_layer_profile_space[i] = r.read_bits(2)?;
                ptl.sub_layer_tier_flag[i] = r.read_bit()?;
                ptl.sub_layer_profile_idc[i] = r.read_bits(5)?;
                for j in 0..32 {
                    ptl.sub_layer_profile_compatibility_flag[i][j] = r.read_bit()?;
                }
                ptl.sub_layer_progressive_source_flag[i] = r.read_bit()?;
                ptl.sub_layer_interlaced_source_flag[i] = r.read_bit()?;
                ptl.sub_layer_non_packed_constraint_flag[i] = r.read_bit()?;
                ptl.sub_layer_frame_only_constraint_flag[i] = r.read_bit()?;

                if ptl.sub_layer_profile_idc[i] == 4
                    || ptl.sub_layer_profile_compatibility_flag[i][4]
                    || ptl.sub_layer_profile_idc[i] == 5
                    || ptl.sub_layer_profile_compatibility_flag[i][5]
                    || ptl.sub_layer_profile_idc[i] == 6
                    || ptl.sub_layer_profile_compatibility_flag[i][6]
                    || ptl.sub_layer_profile_idc[i] == 7
                    || ptl.sub_layer_profile_compatibility_flag[i][7]
                    || ptl.sub_layer_profile_idc[i] == 8
                    || ptl.sub_layer_profile_compatibility_flag[i][8]
                    || ptl.sub_layer_profile_idc[i] == 9
                    || ptl.sub_layer_profile_compatibility_flag[i][9]
                    || ptl.sub_layer_profile_idc[i] == 10
                    || ptl.sub_layer_profile_compatibility_flag[i][10]
                    || ptl.sub_layer_profile_idc[i] == 11
                    || ptl.sub_layer_profile_compatibility_flag[i][11]
                {
                    ptl.sub_layer_max_12bit_constraint_flag[i] = r.read_bit()?;
                    ptl.sub_layer_max_10bit_constraint_flag[i] = r.read_bit()?;
                    ptl.sub_layer_max_8bit_constraint_flag[i] = r.read_bit()?;
                    ptl.sub_layer_max_422chroma_constraint_flag[i] = r.read_bit()?;
                    ptl.sub_layer_max_420chroma_constraint_flag[i] = r.read_bit()?;
                    ptl.sub_layer_max_monochrome_constraint_flag[i] = r.read_bit()?;
                    ptl.sub_layer_intra_constraint_flag[i] = r.read_bit()?;
                    ptl.sub_layer_one_picture_only_constraint_flag[i] = r.read_bit()?;
                    ptl.sub_layer_lower_bit_rate_constraint_flag[i] = r.read_bit()?;

                    if ptl.sub_layer_profile_idc[i] == 5
                        || ptl.sub_layer_profile_compatibility_flag[i][5]
                        || ptl.sub_layer_profile_idc[i] == 9
                        || ptl.sub_layer_profile_compatibility_flag[i][9]
                        || ptl.sub_layer_profile_idc[i] == 10
                        || ptl.sub_layer_profile_compatibility_flag[i][10]
                        || ptl.sub_layer_profile_idc[i] == 11
                        || ptl.sub_layer_profile_compatibility_flag[i][11]
                    {
                        ptl.sub_layer_max_14bit_constraint_flag[i] = r.read_bit()?;
                        r.skip_bits(33)?;
                    } else {
                        r.skip_bits(34)?;
                    }
                } else if ptl.sub_layer_profile_idc[i] == 2
                    || ptl.sub_layer_profile_compatibility_flag[i][2]
                {
                    r.skip_bits(7)?;
                    ptl.sub_layer_one_picture_only_constraint_flag[i] = r.read_bit()?;
                    r.skip_bits(35)?;
                } else {
                    r.skip_bits(43)?;
                }

                if ptl.sub_layer_profile_idc[i] == 1
                    || ptl.sub_layer_profile_compatibility_flag[i][1]
                    || ptl.sub_layer_profile_idc[i] == 2
                    || ptl.sub_layer_profile_compatibility_flag[i][2]
                    || ptl.sub_layer_profile_idc[i] == 3
                    || ptl.sub_layer_profile_compatibility_flag[i][3]
                    || ptl.sub_layer_profile_idc[i] == 4
                    || ptl.sub_layer_profile_compatibility_flag[i][4]
                    || ptl.sub_layer_profile_idc[i] == 5
                    || ptl.sub_layer_profile_compatibility_flag[i][5]
                    || ptl.sub_layer_profile_idc[i] == 9
                    || ptl.sub_layer_profile_compatibility_flag[i][9]
                    || ptl.sub_layer_profile_idc[i] == 11
                    || ptl.sub_layer_profile_compatibility_flag[i][11]
                {
                    ptl.sub_layer_inbld_flag[i] = r.read_bit()?;
                } else {
                    r.skip_bits(1)?;
                }

                if ptl.sub_layer_level_present_flag[i] {
                    ptl.sub_layer_level_idc[i] = r.read_bits(8)?;
                }
            }
        }
        Ok(())
    }

    fn fill_default_scaling_list(
        sl: &mut ScalingLists,
        size_id: i32,
        matrix_id: i32,
    ) -> anyhow::Result<()> {
        if size_id == 0 {
            sl.scaling_list_4x4[matrix_id as usize] = DEFAULT_SCALING_LIST_0;
            return Ok(());
        }

        let dst = match size_id {
            1 => &mut sl.scaling_list_8x8[matrix_id as usize],
            2 => &mut sl.scaling_list_16x16[matrix_id as usize],
            3 => &mut sl.scaling_list_32x32[matrix_id as usize],
            _ => return Err(anyhow!("Invalid size_id {}", size_id)),
        };

        let src = if matrix_id < 3 {
            &DEFAULT_SCALING_LIST_1
        } else if matrix_id < 5 {
            &DEFAULT_SCALING_LIST_2
        } else {
            return Err(anyhow!("Invalid matrix_id {}", matrix_id));
        };

        *dst = *src;

        //  When scaling_list_pred_mode_flag[ sizeId ][ matrixId ] is equal to
        //  0, scaling_list_pred_matrix_id_ delta[ sizeId ][ matrixId ] is equal
        //  to 0 and sizeId is greater than 1, the value of
        //  scaling_list_dc_coef_minus8[ sizeId − 2 ][ matrixId ] is inferred to
        //  be equal to 8.
        //
        // Since we are using a slightly different layout here, with two
        // different field names (i.e. 16x16, and 32x32), we must differentiate
        // between size_id == 2 or size_id == 3.
        if size_id == 2 {
            sl.scaling_list_dc_coef_minus8_16x16[matrix_id as usize] = 8;
        } else if size_id == 3 {
            sl.scaling_list_dc_coef_minus8_32x32[matrix_id as usize] = 8;
        }

        Ok(())
    }

    fn parse_scaling_list_data<T: AsRef<[u8]>>(
        sl: &mut ScalingLists,
        r: &mut NaluReader<T>,
    ) -> anyhow::Result<()> {
        // 7.4.5
        for size_id in 0..4 {
            let step = if size_id == 3 { 3 } else { 1 };
            for matrix_id in (0..6).step_by(step) {
                let scaling_list_pred_mode_flag = r.read_bit()?;
                // If scaling_list_pred_matrix_id_delta[ sizeId ][ matrixId ] is
                // equal to 0, the scaling list is inferred from the default
                // scaling list ScalingList[ sizeId ][ matrixId ][ i ] as specified
                // in Table 7-5 and Table 7-6 for i = 0..Min( 63, ( 1 << ( 4 + (
                // sizeId << 1 ) ) ) − 1 ).
                if !scaling_list_pred_mode_flag {
                    let scaling_list_pred_matrix_id_delta: u32 = r.read_ue()?;
                    if scaling_list_pred_matrix_id_delta == 0 {
                        Self::fill_default_scaling_list(sl, size_id, matrix_id)?;
                    } else {
                        // Equation 7-42
                        let factor = if size_id == 3 { 3 } else { 1 };
                        let ref_matrix_id =
                            matrix_id as u32 - scaling_list_pred_matrix_id_delta * factor;
                        if size_id == 0 {
                            sl.scaling_list_4x4[matrix_id as usize] =
                                sl.scaling_list_4x4[ref_matrix_id as usize];
                        } else {
                            let src = match size_id {
                                1 => sl.scaling_list_8x8[ref_matrix_id as usize],
                                2 => sl.scaling_list_16x16[ref_matrix_id as usize],
                                3 => sl.scaling_list_32x32[ref_matrix_id as usize],
                                _ => return Err(anyhow!("Invalid size_id {}", size_id)),
                            };

                            let dst = match size_id {
                                1 => &mut sl.scaling_list_8x8[matrix_id as usize],
                                2 => &mut sl.scaling_list_16x16[matrix_id as usize],
                                3 => &mut sl.scaling_list_32x32[matrix_id as usize],
                                _ => return Err(anyhow!("Invalid size_id {}", size_id)),
                            };

                            *dst = src;

                            if size_id == 2 {
                                sl.scaling_list_dc_coef_minus8_16x16[matrix_id as usize] =
                                    sl.scaling_list_dc_coef_minus8_16x16[ref_matrix_id as usize];
                            } else if size_id == 3 {
                                sl.scaling_list_dc_coef_minus8_32x32[matrix_id as usize] =
                                    sl.scaling_list_dc_coef_minus8_32x32[ref_matrix_id as usize];
                            }
                        }
                    }
                } else {
                    let mut next_coef = 8;
                    let coef_num = std::cmp::min(64, 1 << (4 + (size_id << 1)));

                    if size_id > 1 {
                        if size_id == 2 {
                            sl.scaling_list_dc_coef_minus8_16x16[matrix_id as usize] =
                                r.read_se_bounded(-7, 247)?;
                        } else if size_id == 3 {
                            sl.scaling_list_dc_coef_minus8_32x32[matrix_id as usize] =
                                r.read_se_bounded(-7, 247)?;
                        }

                        next_coef += 8;
                    }

                    for i in 0..coef_num as usize {
                        let scaling_list_delta_coef: i32 = r.read_se_bounded(-128, 127)?;
                        let next_coef = ((next_coef + scaling_list_delta_coef + 256) % 256) as u8;
                        match size_id {
                            0 => sl.scaling_list_4x4[matrix_id as usize][i] = next_coef,
                            1 => sl.scaling_list_8x8[matrix_id as usize][i] = next_coef,
                            2 => sl.scaling_list_16x16[matrix_id as usize][i] = next_coef,
                            3 => sl.scaling_list_32x32[matrix_id as usize][i] = next_coef,
                            _ => return Err(anyhow!("Invalid size_id {}", size_id)),
                        }
                    }
                }
            }
        }
        Ok(())
    }

    fn parse_short_term_ref_pic_set<T: AsRef<[u8]>>(
        sps: &Sps,
        st: &mut ShortTermRefPicSet,
        r: &mut NaluReader<T>,
        st_rps_idx: u8,
    ) -> anyhow::Result<()> {
        if st_rps_idx != 0 {
            st.inter_ref_pic_set_prediction_flag = r.read_bit()?;
        }

        // (7-59)
        if st.inter_ref_pic_set_prediction_flag {
            if st_rps_idx == sps.num_short_term_ref_pic_sets {
                st.delta_idx_minus1 = r.read_ue_max(st_rps_idx as u32 - 1)?;
            }

            st.delta_rps_sign = r.read_bit()?;
            // The value of abs_delta_rps_minus1 shall be in the range of 0 to
            // 2^15 − 1, inclusive.
            st.abs_delta_rps_minus1 = r.read_ue_max(32767)?;

            let ref_rps_idx = st_rps_idx - (st.delta_idx_minus1 + 1);
            let delta_rps =
                ((1 - 2 * st.delta_rps_sign as u16) * (st.abs_delta_rps_minus1 + 1)) as i32;

            let ref_st = sps
                .short_term_ref_pic_set
                .get(usize::from(ref_rps_idx))
                .ok_or(anyhow!("Invalid ref_rps_idx"))?;

            let mut used_by_curr_pic_flag = [false; 64];
            let mut use_delta_flag = [false; 64];

            for j in 0..ref_st.num_delta_pocs as usize {
                used_by_curr_pic_flag[j] = r.read_bit()?;
                if !used_by_curr_pic_flag[j] {
                    use_delta_flag[j] = r.read_bit()?;
                }
            }

            // (7-61)
            let mut i = 0;
            for j in (0..usize::from(ref_st.num_positive_pics) - 1).rev() {
                let d_poc = ref_st.delta_poc_s1[j] + delta_rps;
                if d_poc < 0 && use_delta_flag[usize::from(ref_st.num_negative_pics) + j] {
                    st.delta_poc_s0[i] = d_poc;
                    st.used_by_curr_pic_s0[i] =
                        used_by_curr_pic_flag[usize::from(ref_st.num_negative_pics) + j];

                    i += 1;
                }
            }

            if delta_rps < 0 && use_delta_flag[ref_st.num_delta_pocs as usize] {
                st.delta_poc_s0[i] = delta_rps;
                st.used_by_curr_pic_s0[i] = used_by_curr_pic_flag[ref_st.num_delta_pocs as usize];
            }

            // Let's *not* change the original algorithm in any way.
            #[allow(clippy::needless_range_loop)]
            for j in 0..ref_st.num_negative_pics as usize {
                let d_poc = ref_st.delta_poc_s0[j] + delta_rps;
                if d_poc < 0 && use_delta_flag[j] {
                    st.used_by_curr_pic_s0[i] = used_by_curr_pic_flag[j];

                    i += 1;
                }
            }

            st.num_negative_pics = i as u8;

            // (7-62)
            let mut i = 0;
            for j in (0..usize::from(ref_st.num_positive_pics) - 1).rev() {
                let d_poc = *ref_st.delta_poc_s1.get(j).ok_or(anyhow!("Invalid data"))?;
                if d_poc > 0 && use_delta_flag[ref_st.num_negative_pics as usize + j] {
                    st.delta_poc_s1[i] = d_poc;
                    st.used_by_curr_pic_s1[i] =
                        used_by_curr_pic_flag[ref_st.num_negative_pics as usize + j];

                    i += 1;
                }
            }
        } else {
            st.num_negative_pics = r.read_ue_max(u32::from(
                sps.max_dec_pic_buffering_minus1[usize::from(sps.max_sub_layers_minus1)],
            ))?;

            st.num_positive_pics = r.read_ue_max(u32::from(
                sps.max_dec_pic_buffering_minus1[usize::from(sps.max_sub_layers_minus1)]
                    - st.num_negative_pics,
            ))?;

            for i in 0..usize::from(st.num_negative_pics) {
                let delta_poc_s0_minus1: u32 = r.read_ue_max(32767)?;

                if i == 0 {
                    st.delta_poc_s0[i] = -(delta_poc_s0_minus1 as i32 + 1);
                } else {
                    st.delta_poc_s0[i] = st.delta_poc_s0[i - 1] - (delta_poc_s0_minus1 as i32 + 1);
                }

                st.used_by_curr_pic_s0[i] = r.read_bit()?;
            }

            for i in 0..usize::from(st.num_positive_pics) {
                let delta_poc_s1_minus1: u32 = r.read_ue_max(32767)?;

                if i == 0 {
                    st.delta_poc_s1[i] = delta_poc_s1_minus1 as i32 + 1;
                } else {
                    st.delta_poc_s1[i] = st.delta_poc_s1[i - 1] + (delta_poc_s1_minus1 as i32 + 1);
                }

                st.used_by_curr_pic_s1[i] = r.read_bit()?;
            }
        }

        st.num_delta_pocs = u32::from(st.num_negative_pics + st.num_positive_pics);

        Ok(())
    }

    fn parse_sublayer_hrd_parameters<T: AsRef<[u8]>>(
        h: &mut SublayerHrdParameters,
        cpb_cnt: u32,
        sub_pic_hrd_params_present_flag: bool,
        r: &mut NaluReader<T>,
    ) -> anyhow::Result<()> {
        for i in 0..cpb_cnt as usize {
            h.bit_rate_value_minus1[i] = r.read_ue_max(2u32.pow(32) - 2)?;
            h.cpb_size_value_minus1[i] = r.read_ue_max(2u32.pow(32) - 2)?;
            if sub_pic_hrd_params_present_flag {
                h.cpb_size_du_value_minus1[i] = r.read_ue_max(2u32.pow(32) - 2)?;
                h.bit_rate_du_value_minus1[i] = r.read_ue_max(2u32.pow(32) - 2)?;
            }

            h.cbr_flag[i] = r.read_bit()?;
        }

        Ok(())
    }

    fn parse_hrd_parameters<T: AsRef<[u8]>>(
        common_inf_present_flag: bool,
        max_num_sublayers_minus1: u8,
        hrd: &mut HrdParams,
        r: &mut NaluReader<T>,
    ) -> anyhow::Result<()> {
        if common_inf_present_flag {
            hrd.nal_hrd_parameters_present_flag = r.read_bit()?;
            hrd.vcl_hrd_parameters_present_flag = r.read_bit()?;
            if hrd.nal_hrd_parameters_present_flag || hrd.vcl_hrd_parameters_present_flag {
                hrd.sub_pic_hrd_params_present_flag = r.read_bit()?;
                if hrd.sub_pic_hrd_params_present_flag {
                    hrd.tick_divisor_minus2 = r.read_bits(8)?;
                    hrd.du_cpb_removal_delay_increment_length_minus1 = r.read_bits(5)?;
                    hrd.sub_pic_cpb_params_in_pic_timing_sei_flag = r.read_bit()?;
                    hrd.dpb_output_delay_du_length_minus1 = r.read_bits(5)?;
                }
                hrd.bit_rate_scale = r.read_bits(4)?;
                hrd.cpb_size_scale = r.read_bits(4)?;
                if hrd.sub_pic_hrd_params_present_flag {
                    hrd.cpb_size_du_scale = r.read_bits(4)?;
                }
                hrd.initial_cpb_removal_delay_length_minus1 = r.read_bits(5)?;
                hrd.au_cpb_removal_delay_length_minus1 = r.read_bits(5)?;
                hrd.dpb_output_delay_length_minus1 = r.read_bits(5)?;
            }
        }

        for i in 0..max_num_sublayers_minus1 as usize {
            hrd.fixed_pic_rate_general_flag[i] = r.read_bit()?;
            if !hrd.fixed_pic_rate_general_flag[i] {
                hrd.fixed_pic_rate_within_cvs_flag[i] = r.read_bit()?;
            }
            if hrd.fixed_pic_rate_within_cvs_flag[i] {
                hrd.elemental_duration_in_tc_minus1[i] = r.read_ue_max(2047)?;
            } else {
                hrd.low_delay_hrd_flag[i] = r.read_bit()?;
            }

            if !hrd.low_delay_hrd_flag[i] {
                hrd.cpb_cnt_minus1[i] = r.read_ue_max(31)?;
            }

            if hrd.nal_hrd_parameters_present_flag {
                Self::parse_sublayer_hrd_parameters(
                    &mut hrd.nal_hrd[i],
                    hrd.cpb_cnt_minus1[i] + 1,
                    hrd.sub_pic_hrd_params_present_flag,
                    r,
                )?;
            }

            if hrd.vcl_hrd_parameters_present_flag {
                Self::parse_sublayer_hrd_parameters(
                    &mut hrd.vcl_hrd[i],
                    hrd.cpb_cnt_minus1[i] + 1,
                    hrd.sub_pic_hrd_params_present_flag,
                    r,
                )?;
            }
        }

        Ok(())
    }

    fn parse_vui_parameters<T: AsRef<[u8]>>(
        sps: &mut Sps,
        r: &mut NaluReader<T>,
    ) -> anyhow::Result<()> {
        let vui = &mut sps.vui_parameters;

        vui.aspect_ratio_info_present_flag = r.read_bit()?;
        if vui.aspect_ratio_info_present_flag {
            vui.aspect_ratio_idc = r.read_bits(8)?;
            const EXTENDED_SAR: u32 = 255;
            if vui.aspect_ratio_idc == EXTENDED_SAR {
                vui.sar_width = r.read_bits(16)?;
                vui.sar_height = r.read_bits(16)?;
            }
        }

        vui.overscan_info_present_flag = r.read_bit()?;
        if vui.overscan_info_present_flag {
            vui.overscan_appropriate_flag = r.read_bit()?;
        }

        vui.video_signal_type_present_flag = r.read_bit()?;
        if vui.video_signal_type_present_flag {
            vui.video_format = r.read_bits(3)?;
            vui.video_full_range_flag = r.read_bit()?;
            vui.colour_description_present_flag = r.read_bit()?;
            if vui.colour_description_present_flag {
                vui.colour_primaries = r.read_bits(8)?;
                vui.transfer_characteristics = r.read_bits(8)?;
                vui.matrix_coeffs = r.read_bits(8)?;
            }
        }

        vui.chroma_loc_info_present_flag = r.read_bit()?;
        if vui.chroma_loc_info_present_flag {
            vui.chroma_sample_loc_type_top_field = r.read_ue_max(5)?;
            vui.chroma_sample_loc_type_bottom_field = r.read_ue_max(5)?;
        }

        vui.neutral_chroma_indication_flag = r.read_bit()?;
        vui.field_seq_flag = r.read_bit()?;
        vui.frame_field_info_present_flag = r.read_bit()?;
        vui.default_display_window_flag = r.read_bit()?;

        if vui.default_display_window_flag {
            vui.def_disp_win_left_offset = r.read_ue()?;
            vui.def_disp_win_right_offset = r.read_ue()?;
            vui.def_disp_win_top_offset = r.read_ue()?;
            vui.def_disp_win_bottom_offset = r.read_ue()?;
        }

        vui.timing_info_present_flag = r.read_bit()?;
        if vui.timing_info_present_flag {
            vui.num_units_in_tick = r.read_bits::<u32>(31)? << 1;
            vui.num_units_in_tick |= r.read_bits::<u32>(1)?;

            vui.time_scale = r.read_bits::<u32>(31)? << 1;
            vui.time_scale |= r.read_bits::<u32>(1)?;

            vui.poc_proportional_to_timing_flag = r.read_bit()?;
            if vui.poc_proportional_to_timing_flag {
                vui.num_ticks_poc_diff_one_minus1 = r.read_ue_max(2u32.pow(32) - 2)?;
            }

            vui.hrd_parameters_present_flag = r.read_bit()?;
            if vui.hrd_parameters_present_flag {
                let sps_max_sub_layers_minus1 = sps.max_sub_layers_minus1;
                Self::parse_hrd_parameters(true, sps_max_sub_layers_minus1, &mut vui.hrd, r)?;
            }
        }

        vui.bitstream_restriction_flag = r.read_bit()?;
        if vui.bitstream_restriction_flag {
            vui.tiles_fixed_structure_flag = r.read_bit()?;
            vui.motion_vectors_over_pic_boundaries_flag = r.read_bit()?;
            vui.restricted_ref_pic_lists_flag = r.read_bit()?;

            vui.min_spatial_segmentation_idc = r.read_ue_max(4095)?;
            vui.max_bytes_per_pic_denom = r.read_ue()?;
            vui.max_bits_per_min_cu_denom = r.read_ue()?;
            vui.log2_max_mv_length_horizontal = r.read_ue_max(15)?;
            vui.log2_max_mv_length_vertical = r.read_ue_max(15)?;
        }

        Ok(())
    }

    fn parse_sps_scc_extension<T: AsRef<[u8]>>(
        sps: &mut Sps,
        r: &mut NaluReader<T>,
    ) -> anyhow::Result<()> {
        let scc = &mut sps.scc_extension;

        scc.curr_pic_ref_enabled_flag = r.read_bit()?;
        scc.palette_mode_enabled_flag = r.read_bit()?;
        if scc.palette_mode_enabled_flag {
            scc.palette_max_size = r.read_ue_max(64)?;
            scc.delta_palette_max_predictor_size =
                r.read_ue_max(128 - u32::from(scc.palette_max_size))?;
            scc.palette_predictor_initializers_present_flag = r.read_bit()?;
            if scc.palette_predictor_initializers_present_flag {
                let max =
                    u32::from(scc.palette_max_size + scc.delta_palette_max_predictor_size - 1);
                scc.num_palette_predictor_initializer_minus1 = r.read_ue_max(max)?;

                let num_comps = if sps.chroma_format_idc == 0 { 1 } else { 3 };
                for comp in 0..num_comps {
                    for i in 0..=usize::from(scc.num_palette_predictor_initializer_minus1) {
                        let num_bits = if comp == 0 {
                            sps.bit_depth_luma_minus8 + 8
                        } else {
                            sps.bit_depth_chroma_minus8 + 8
                        };
                        scc.palette_predictor_initializer[comp][i] =
                            r.read_bits(usize::from(num_bits))?;
                    }
                }
            }
        }

        scc.motion_vector_resolution_control_idc = r.read_bits(2)?;
        scc.intra_boundary_filtering_disabled_flag = r.read_bit()?;

        Ok(())
    }

    fn parse_sps_range_extension<T: AsRef<[u8]>>(
        sps: &mut Sps,
        r: &mut NaluReader<T>,
    ) -> anyhow::Result<()> {
        let ext = &mut sps.range_extension;

        ext.transform_skip_rotation_enabled_flag = r.read_bit()?;
        ext.transform_skip_context_enabled_flag = r.read_bit()?;
        ext.implicit_rdpcm_enabled_flag = r.read_bit()?;
        ext.explicit_rdpcm_enabled_flag = r.read_bit()?;
        ext.extended_precision_processing_flag = r.read_bit()?;
        ext.intra_smoothing_disabled_flag = r.read_bit()?;
        ext.high_precision_offsets_enabled_flag = r.read_bit()?;
        ext.persistent_rice_adaptation_enabled_flag = r.read_bit()?;
        ext.cabac_bypass_alignment_enabled_flag = r.read_bit()?;

        Ok(())
    }

    /// Parse a SPS NALU.
    pub fn parse_sps<T: AsRef<[u8]>>(&mut self, nalu: &Nalu<T>) -> anyhow::Result<&Sps> {
        if !matches!(nalu.header().type_, NaluType::SpsNut) {
            return Err(anyhow!(
                "Invalid NALU type, expected {:?}, got {:?}",
                NaluType::SpsNut,
                nalu.header().type_
            ));
        }

        let data = nalu.as_ref();
        let header = nalu.header();
        let hdr_len = header.len();
        // Skip the header
        let mut r = NaluReader::new(&data[hdr_len..]);

        let mut sps = Sps {
            video_parameter_set_id: r.read_bits(4)?,
            max_sub_layers_minus1: r.read_bits(3)?,
            temporal_id_nesting_flag: r.read_bit()?,
            ..Default::default()
        };

        Self::parse_profile_tier_level(
            &mut sps.profile_tier_level,
            &mut r,
            true,
            sps.max_sub_layers_minus1,
        )?;

        sps.seq_parameter_set_id = r.read_ue_max(MAX_SPS_COUNT as u32 - 1)?;
        sps.chroma_format_idc = r.read_ue_max(3)?;

        if sps.chroma_format_idc == 3 {
            sps.separate_colour_plane_flag = r.read_bit()?;
        }

        sps.chroma_array_type = if sps.separate_colour_plane_flag {
            0
        } else {
            sps.chroma_format_idc
        };

        sps.pic_width_in_luma_samples = r.read_ue_bounded(1, 16888)?;
        sps.pic_height_in_luma_samples = r.read_ue_bounded(1, 16888)?;

        sps.conformance_window_flag = r.read_bit()?;
        if sps.conformance_window_flag {
            sps.conf_win_left_offset = r.read_ue()?;
            sps.conf_win_right_offset = r.read_ue()?;
            sps.conf_win_top_offset = r.read_ue()?;
            sps.conf_win_bottom_offset = r.read_ue()?;
        }

        sps.bit_depth_luma_minus8 = r.read_ue_max(6)?;
        sps.bit_depth_chroma_minus8 = r.read_ue_max(6)?;
        sps.log2_max_pic_order_cnt_lsb_minus4 = r.read_ue_max(6)?;
        sps.sub_layer_ordering_info_present_flag = r.read_bit()?;

        {
            let i = if sps.sub_layer_ordering_info_present_flag {
                0
            } else {
                sps.max_sub_layers_minus1
            };

            for j in i..=sps.max_sub_layers_minus1 {
                sps.max_dec_pic_buffering_minus1[j as usize] = r.read_ue_max(16)?;
                sps.max_num_reorder_pics[j as usize] =
                    r.read_ue_max(sps.max_dec_pic_buffering_minus1[j as usize] as _)?;
                sps.max_latency_increase_plus1[j as usize] = r.read_ue_max(u32::MAX - 1)?;
            }
        }

        sps.log2_min_luma_coding_block_size_minus3 = r.read_ue()?;
        sps.log2_diff_max_min_luma_coding_block_size = r.read_ue()?;
        sps.log2_min_luma_transform_block_size_minus2 = r.read_ue()?;
        sps.log2_diff_max_min_luma_transform_block_size = r.read_ue()?;

        // (7-10)
        sps.min_cb_log2_size_y = u32::from(sps.log2_min_luma_coding_block_size_minus3 + 3);
        // (7-11)
        sps.ctb_log2_size_y =
            sps.min_cb_log2_size_y + u32::from(sps.log2_diff_max_min_luma_coding_block_size);
        // (7-12)
        sps.ctb_size_y = 1 << sps.ctb_log2_size_y;
        // (7-17)
        sps.pic_height_in_ctbs_y =
            (sps.pic_height_in_luma_samples as f64 / sps.ctb_size_y as f64).ceil() as u32;
        // (7-15)
        sps.pic_width_in_ctbs_y =
            (sps.pic_width_in_luma_samples as f64 / sps.ctb_size_y as f64).ceil() as u32;

        sps.max_tb_log2_size_y = u32::from(
            sps.log2_min_luma_transform_block_size_minus2
                + 2
                + sps.log2_diff_max_min_luma_transform_block_size,
        );

        if sps.max_tb_log2_size_y > std::cmp::min(sps.ctb_log2_size_y, 5) {
            return Err(anyhow!(
                "Invalid value for MaxTbLog2SizeY: {}",
                sps.max_tb_log2_size_y
            ));
        }

        sps.pic_size_in_ctbs_y = sps.pic_width_in_ctbs_y * sps.pic_height_in_ctbs_y;

        sps.max_transform_hierarchy_depth_inter = r.read_ue()?;
        sps.max_transform_hierarchy_depth_intra = r.read_ue()?;

        sps.scaling_list_enabled_flag = r.read_bit()?;
        if sps.scaling_list_data_present_flag {
            sps.scaling_list_data_present_flag = r.read_bit()?;
            if sps.scaling_list_data_present_flag {
                Self::parse_scaling_list_data(&mut sps.scaling_list, &mut r)?;
            }
        }

        sps.amp_enabled_flag = r.read_bit()?;
        sps.sample_adaptive_offset_enabled_flag = r.read_bit()?;

        sps.pcm_enabled_flag = r.read_bit()?;
        if sps.pcm_enabled_flag {
            sps.pcm_sample_bit_depth_luma_minus1 = r.read_bits(4)?;
            sps.pcm_sample_bit_depth_chroma_minus1 = r.read_bits(4)?;
            sps.log2_min_pcm_luma_coding_block_size_minus3 = r.read_ue_max(2)?;
            sps.log2_diff_max_min_pcm_luma_coding_block_size = r.read_ue_max(2)?;
            sps.pcm_loop_filter_disabled_flag = r.read_bit()?;
        }

        sps.num_short_term_ref_pic_sets = r.read_ue_max(64)?;

        for i in 0..sps.num_short_term_ref_pic_sets {
            let mut st = ShortTermRefPicSet::default();
            Self::parse_short_term_ref_pic_set(&sps, &mut st, &mut r, i)?;
            sps.short_term_ref_pic_set.push(st);
        }

        sps.long_term_ref_pics_present_flag = r.read_bit()?;
        if sps.long_term_ref_pics_present_flag {
            sps.num_long_term_ref_pics_sps = r.read_ue_max(32)?;
            for i in 0..usize::from(sps.num_long_term_ref_pics_sps) {
                sps.lt_ref_pic_poc_lsb_sps[i] =
                    r.read_bits(usize::from(sps.log2_max_pic_order_cnt_lsb_minus4) + 4)?;
                sps.used_by_curr_pic_lt_sps_flag[i] = r.read_bit()?;
            }
        }

        sps.temporal_mvp_enabled_flag = r.read_bit()?;
        sps.strong_intra_smoothing_enabled_flag = r.read_bit()?;

        sps.vui_parameters_present_flag = r.read_bit()?;
        if sps.vui_parameters_present_flag {
            Self::parse_vui_parameters(&mut sps, &mut r)?;
        }

        sps.extension_present_flag = r.read_bit()?;
        if sps.extension_present_flag {
            sps.range_extension_flag = r.read_bit()?;
            if sps.range_extension_flag {
                Self::parse_sps_range_extension(&mut sps, &mut r)?;
            }

            let multilayer_extension_flag = r.read_bit()?;
            if multilayer_extension_flag {
                return Err(anyhow!("Multilayer extension not supported."));
            }

            let three_d_extension_flag = r.read_bit()?;
            if three_d_extension_flag {
                return Err(anyhow!("3D extension not supported."));
            }

            sps.scc_extension_flag = r.read_bit()?;
            if sps.scc_extension_flag {
                Self::parse_sps_scc_extension(&mut sps, &mut r)?;
            }
        }

        let shift = if sps.range_extension.high_precision_offsets_enabled_flag {
            sps.bit_depth_luma_minus8 + 7
        } else {
            7
        };

        sps.wp_offset_half_range_y = 1 << shift;

        let shift = if sps.range_extension.high_precision_offsets_enabled_flag {
            sps.bit_depth_chroma_minus8 + 7
        } else {
            7
        };

        sps.wp_offset_half_range_c = 1 << shift;

        let key = sps.seq_parameter_set_id;
        self.active_spses.insert(key, sps);

        if self.active_spses.keys().len() > MAX_SPS_COUNT {
            return Err(anyhow!(
                "Broken data: Number of active SPSs > MAX_SPS_COUNT"
            ));
        }

        Ok(self.get_sps(key).unwrap())
    }

    fn parse_pps_scc_extension<T: AsRef<[u8]>>(
        pps: &mut Pps,
        sps: &Sps,
        r: &mut NaluReader<T>,
    ) -> anyhow::Result<()> {
        let scc = &mut pps.scc_extension;
        scc.curr_pic_ref_enabled_flag = r.read_bit()?;
        scc.residual_adaptive_colour_transform_enabled_flag = r.read_bit()?;
        if scc.residual_adaptive_colour_transform_enabled_flag {
            scc.slice_act_qp_offsets_present_flag = r.read_bit()?;
            scc.act_y_qp_offset_plus5 = r.read_se_bounded(-7, 17)?;
            scc.act_cb_qp_offset_plus5 = r.read_se_bounded(-7, 17)?;
            scc.act_cr_qp_offset_plus3 = r.read_se_bounded(-9, 15)?;
        }

        scc.palette_predictor_initializers_present_flag = r.read_bit()?;
        if scc.palette_predictor_initializers_present_flag {
            let max = sps.scc_extension.palette_max_size
                + sps.scc_extension.delta_palette_max_predictor_size;
            scc.num_palette_predictor_initializers = r.read_ue_max(max.into())?;
            if scc.num_palette_predictor_initializers > 0 {
                scc.monochrome_palette_flag = r.read_bit()?;
                scc.luma_bit_depth_entry_minus8 = r.read_ue_bounded(
                    sps.bit_depth_luma_minus8.into(),
                    sps.bit_depth_luma_minus8.into(),
                )?;
                if !scc.monochrome_palette_flag {
                    scc.chroma_bit_depth_entry_minus8 = r.read_ue_bounded(
                        sps.bit_depth_chroma_minus8.into(),
                        sps.bit_depth_chroma_minus8.into(),
                    )?;
                }

                let num_comps = if scc.monochrome_palette_flag { 1 } else { 3 };
                for comp in 0..num_comps {
                    let num_bits = if comp == 0 {
                        scc.luma_bit_depth_entry_minus8 + 8
                    } else {
                        scc.chroma_bit_depth_entry_minus8 + 8
                    };
                    for i in 0..usize::from(scc.num_palette_predictor_initializers) {
                        scc.palette_predictor_initializer[comp][i] =
                            r.read_bits(num_bits.into())?;
                    }
                }
            }
        }
        Ok(())
    }

    fn parse_pps_range_extension<T: AsRef<[u8]>>(
        pps: &mut Pps,
        sps: &Sps,
        r: &mut NaluReader<T>,
    ) -> anyhow::Result<()> {
        let rext = &mut pps.range_extension;

        if pps.transform_skip_enabled_flag {
            rext.log2_max_transform_skip_block_size_minus2 =
                r.read_ue_max(sps.max_tb_log2_size_y - 2)?;
        }

        rext.cross_component_prediction_enabled_flag = r.read_bit()?;
        rext.chroma_qp_offset_list_enabled_flag = r.read_bit()?;
        if rext.chroma_qp_offset_list_enabled_flag {
            rext.diff_cu_chroma_qp_offset_depth = r.read_ue()?;
            rext.chroma_qp_offset_list_len_minus1 = r.read_ue_max(5)?;
            for i in 0..=rext.chroma_qp_offset_list_len_minus1 as usize {
                rext.cb_qp_offset_list[i] = r.read_se_bounded(-12, 12)?;
                rext.cr_qp_offset_list[i] = r.read_se_bounded(-12, 12)?;
            }
        }

        let bit_depth_y = sps.bit_depth_luma_minus8 + 8;
        let max = u32::from(std::cmp::max(0, bit_depth_y - 10));

        rext.log2_sao_offset_scale_luma = r.read_ue_max(max)?;
        rext.log2_sao_offset_scale_chroma = r.read_ue_max(max)?;

        Ok(())
    }

    /// Parse a PPS NALU.
    pub fn parse_pps<T: AsRef<[u8]>>(&mut self, nalu: &Nalu<T>) -> anyhow::Result<&Pps> {
        if !matches!(nalu.header().type_, NaluType::PpsNut) {
            return Err(anyhow!(
                "Invalid NALU type, expected {:?}, got {:?}",
                NaluType::PpsNut,
                nalu.header().type_
            ));
        }

        let data = nalu.as_ref();
        let header = nalu.header();
        let hdr_len = header.len();
        // Skip the header
        let mut r = NaluReader::new(&data[hdr_len..]);

        let mut pps = Pps {
            loop_filter_across_tiles_enabled_flag: true,
            ..Default::default()
        };

        pps.pic_parameter_set_id = r.read_ue_max(MAX_PPS_COUNT as u32 - 1)?;
        pps.seq_parameter_set_id = r.read_ue_max(MAX_SPS_COUNT as u32 - 1)?;

        let sps = self.get_sps(pps.seq_parameter_set_id).context(
            "Broken stream: stream references a SPS that has not been successfully parsed",
        )?;

        pps.dependent_slice_segments_enabled_flag = r.read_bit()?;
        pps.output_flag_present_flag = r.read_bit()?;
        pps.num_extra_slice_header_bits = r.read_bits(3)?;
        pps.sign_data_hiding_enabled_flag = r.read_bit()?;
        pps.cabac_init_present_flag = r.read_bit()?;

        // 7.4.7.1
        pps.num_ref_idx_l0_default_active_minus1 = r.read_ue_max(14)?;
        pps.num_ref_idx_l0_default_active_minus1 = r.read_ue_max(14)?;

        // (7-5)
        let qp_bd_offset_y = 6 * i32::from(sps.bit_depth_luma_minus8);

        pps.init_qp_minus26 = r.read_se_bounded(-(26 + qp_bd_offset_y), 25)?;
        pps.qp_bd_offset_y = qp_bd_offset_y as u32;
        pps.constrained_intra_pred_flag = r.read_bit()?;
        pps.transform_skip_enabled_flag = r.read_bit()?;
        pps.cu_qp_delta_enabled_flag = r.read_bit()?;

        if pps.cu_qp_delta_enabled_flag {
            pps.diff_cu_qp_delta_depth =
                r.read_ue_max(u32::from(sps.log2_diff_max_min_luma_coding_block_size))?;
        }

        pps.cb_qp_offset = r.read_se_bounded(-12, 12)?;
        pps.cr_qp_offset = r.read_se_bounded(-12, 12)?;

        pps.slice_chroma_qp_offsets_present_flag = r.read_bit()?;
        pps.weighted_pred_flag = r.read_bit()?;
        pps.weighted_bipred_flag = r.read_bit()?;
        pps.transquant_bypass_enabled_flag = r.read_bit()?;
        pps.tiles_enabled_flag = r.read_bit()?;
        pps.entropy_coding_sync_enabled_flag = r.read_bit()?;

        // A mix of the rbsp data and the algorithm in 6.5.1
        if pps.tiles_enabled_flag {
            pps.num_tile_columns_minus1 = r.read_ue_max(sps.pic_width_in_ctbs_y - 1)?;
            pps.num_tile_rows_minus1 = r.read_ue_max(sps.pic_height_in_ctbs_y - 1)?;
            pps.uniform_spacing_flag = r.read_bit()?;
            if !pps.uniform_spacing_flag {
                pps.column_width_minus1[usize::from(pps.num_tile_columns_minus1)] =
                    sps.pic_width_in_ctbs_y - 1;

                for i in 0..usize::from(pps.num_tile_columns_minus1) {
                    pps.column_width_minus1[i] = r.read_ue_max(pps.column_width_minus1[i] - 1)?;
                    pps.column_width_minus1[usize::from(pps.num_tile_columns_minus1)] -=
                        pps.column_width_minus1[i] + 1;
                }

                pps.row_height_minus1[usize::from(pps.num_tile_rows_minus1)] =
                    sps.pic_height_in_ctbs_y - 1;

                for i in 0..usize::from(pps.num_tile_rows_minus1) {
                    pps.row_height_minus1[i] = r.read_ue_max(pps.row_height_minus1[i] - 1)?;
                    pps.row_height_minus1[usize::from(pps.num_tile_rows_minus1)] -=
                        pps.row_height_minus1[i] + 1;
                }
            } else {
                let nrows = u32::from(pps.num_tile_rows_minus1) + 1;
                let ncols = u32::from(pps.num_tile_columns_minus1) + 1;

                for j in 0..ncols {
                    pps.column_width_minus1[j as usize] = ((j + 1) * sps.pic_width_in_ctbs_y)
                        / ncols
                        - j * sps.pic_width_in_ctbs_y / ncols
                        - 1;
                }

                for j in 0..nrows {
                    pps.column_width_minus1[j as usize] = ((j + 1) * sps.pic_height_in_ctbs_y)
                        / nrows
                        - j * sps.pic_height_in_ctbs_y / nrows
                        - 1;
                }
            }

            pps.loop_filter_across_tiles_enabled_flag = r.read_bit()?;
        }

        pps.loop_filter_across_slices_enabled_flag = r.read_bit()?;
        pps.deblocking_filter_control_present_flag = r.read_bit()?;

        if pps.deblocking_filter_control_present_flag {
            pps.deblocking_filter_override_enabled_flag = r.read_bit()?;
            pps.deblocking_filter_disabled_flag = r.read_bit()?;
            if !pps.deblocking_filter_disabled_flag {
                pps.beta_offset_div2 = r.read_se_bounded(-6, 6)?;
                pps.tc_offset_div2 = r.read_se_bounded(-6, 6)?;
            }
        }

        pps.scaling_list_data_present_flag = r.read_bit()?;

        if pps.scaling_list_data_present_flag {
            Self::parse_scaling_list_data(&mut pps.scaling_list, &mut r)?;
        }

        pps.lists_modification_present_flag = r.read_bit()?;
        pps.log2_parallel_merge_level_minus2 = r.read_ue_max(sps.ctb_log2_size_y - 2)?;
        pps.slice_segment_header_extension_present_flag = r.read_bit()?;

        pps.extension_present_flag = r.read_bit()?;
        if pps.extension_present_flag {
            pps.range_extension_flag = r.read_bit()?;

            if pps.range_extension_flag {
                Self::parse_pps_range_extension(&mut pps, sps, &mut r)?;
            }

            let multilayer_extension_flag = r.read_bit()?;
            if multilayer_extension_flag {
                return Err(anyhow!("Multilayer extension is not supported"));
            }

            let three_d_extension_flag = r.read_bit()?;
            if three_d_extension_flag {
                return Err(anyhow!("3D extension is not supported"));
            }

            pps.scc_extension_flag = r.read_bit()?;
            if pps.scc_extension_flag {
                Self::parse_pps_scc_extension(&mut pps, sps, &mut r)?;
            }

            r.skip_bits(4)?; // pps_extension_4bits
        }

        pps.temporal_id = nalu.header().temporal_id_plus1() - 1;

        let key = pps.pic_parameter_set_id;
        self.active_ppses.insert(key, pps);

        if self.active_ppses.keys().len() > MAX_PPS_COUNT {
            return Err(anyhow!(
                "Broken Data: number of active PPSs > MAX_PPS_COUNT"
            ));
        }

        Ok(self.get_pps(key).unwrap())
    }

    pub fn parse_pred_weight_table<T: AsRef<[u8]>>(
        hdr: &mut SliceHeader,
        r: &mut NaluReader<T>,
        sps: &Sps,
    ) -> anyhow::Result<()> {
        let pwt = &mut hdr.pred_weight_table;

        pwt.luma_log2_weight_denom = r.read_ue_max(7)?;
        if sps.chroma_array_type != 0 {
            pwt.delta_chroma_log2_weight_denom = r.read_se()?;
            pwt.chroma_log2_weight_denom = (pwt.luma_log2_weight_denom as i32
                + pwt.delta_chroma_log2_weight_denom as i32)
                .try_into()?;
        }

        for i in 0..=usize::from(hdr.num_ref_idx_l0_active_minus1) {
            pwt.luma_weight_l0_flag[i] = r.read_bit()?;
        }

        if sps.chroma_array_type != 0 {
            for i in 0..=usize::from(hdr.num_ref_idx_l0_active_minus1) {
                pwt.chroma_weight_l0_flag[i] = r.read_bit()?;
            }
        }

        for i in 0..=usize::from(hdr.num_ref_idx_l0_active_minus1) {
            if pwt.luma_weight_l0_flag[i] {
                pwt.delta_luma_weight_l0[i] = r.read_se_bounded(-128, 127)?;
                pwt.luma_offset_l0[i] = r.read_se_bounded(-128, 127)?;
            }

            if pwt.chroma_weight_l0_flag[i] {
                for j in 0..2 {
                    pwt.delta_chroma_weight_l0[i][j] = r.read_se_bounded(-128, 127)?;
                    pwt.delta_chroma_offset_l0[i][j] = r.read_se_bounded(
                        -4 * sps.wp_offset_half_range_c as i32,
                        4 * sps.wp_offset_half_range_c as i32 - 1,
                    )?;
                }
            }
        }

        if hdr.type_.is_b() {
            for i in 0..=usize::from(hdr.num_ref_idx_l1_active_minus1) {
                pwt.luma_weight_l1_flag[i] = r.read_bit()?;
            }

            if sps.chroma_format_idc != 0 {
                for i in 0..=usize::from(hdr.num_ref_idx_l1_active_minus1) {
                    pwt.chroma_weight_l1_flag[i] = r.read_bit()?;
                }
            }

            for i in 0..=usize::from(hdr.num_ref_idx_l1_active_minus1) {
                if pwt.luma_weight_l1_flag[i] {
                    pwt.delta_luma_weight_l1[i] = r.read_se_bounded(-128, 127)?;
                    pwt.luma_offset_l1[i] = r.read_se_bounded(-128, 127)?;
                }

                if pwt.chroma_weight_l1_flag[i] {
                    for j in 0..2 {
                        pwt.delta_chroma_weight_l1[i][j] = r.read_se_bounded(-128, 127)?;
                        pwt.delta_chroma_offset_l1[i][j] = r.read_se_bounded(
                            -4 * sps.wp_offset_half_range_c as i32,
                            4 * sps.wp_offset_half_range_c as i32 - 1,
                        )?;
                    }
                }
            }
        }

        Ok(())
    }

    fn parse_ref_pic_lists_modification<T: AsRef<[u8]>>(
        hdr: &mut SliceHeader,
        r: &mut NaluReader<T>,
    ) -> anyhow::Result<()> {
        let rplm = &mut hdr.ref_pic_list_modification;

        rplm.ref_pic_list_modification_flag_l0 = r.read_bit()?;
        if rplm.ref_pic_list_modification_flag_l0 {
            for _ in 0..=hdr.num_ref_idx_l0_active_minus1 {
                let num_bits = (hdr.num_pic_total_curr as f64).log2().ceil() as _;

                let entry = r.read_bits(num_bits)?;

                if entry > hdr.num_pic_total_curr - 1 {
                    return Err(anyhow!(
                        "Invalid list_entry_l0 {}, expected at max NumPicTotalCurr - 1: {}",
                        entry,
                        hdr.num_pic_total_curr - 1
                    ));
                }

                rplm.list_entry_l0.push(entry);
            }
        }

        if hdr.type_.is_b() {
            rplm.ref_pic_list_modification_flag_l1 = r.read_bit()?;
            if rplm.ref_pic_list_modification_flag_l1 {
                for _ in 0..=hdr.num_ref_idx_l1_active_minus1 {
                    let num_bits = (hdr.num_pic_total_curr as f64).log2().ceil() as _;

                    let entry = r.read_bits(num_bits)?;

                    if entry > hdr.num_pic_total_curr - 1 {
                        return Err(anyhow!(
                            "Invalid list_entry_l1 {}, expected at max NumPicTotalCurr - 1: {}",
                            entry,
                            hdr.num_pic_total_curr - 1
                        ));
                    }

                    rplm.list_entry_l1.push(entry);
                }
            }
        }

        Ok(())
    }

    /// Further sets default values given `sps` and `pps`.
    pub fn slice_header_set_defaults(hdr: &mut SliceHeader, sps: &Sps, pps: &Pps) {
        // Set some defaults that can't be defined in Default::default().
        hdr.deblocking_filter_disabled_flag = pps.deblocking_filter_disabled_flag;
        hdr.beta_offset_div2 = pps.beta_offset_div2;
        hdr.tc_offset_div2 = pps.tc_offset_div2;
        hdr.loop_filter_across_slices_enabled_flag = pps.loop_filter_across_slices_enabled_flag;
        hdr.curr_rps_idx = sps.num_short_term_ref_pic_sets;
        hdr.use_integer_mv_flag = sps.scc_extension.motion_vector_resolution_control_idc != 0;
    }

    /// Parses a slice header from a slice NALU.
    pub fn parse_slice_header<T: AsRef<[u8]>>(
        &mut self,
        nalu: Nalu<T>,
    ) -> anyhow::Result<Slice<T>> {
        if !matches!(
            nalu.header().type_,
            NaluType::TrailN
                | NaluType::TrailR
                | NaluType::TsaN
                | NaluType::TsaR
                | NaluType::StsaN
                | NaluType::StsaR
                | NaluType::RadlN
                | NaluType::RadlR
                | NaluType::RaslN
                | NaluType::RaslR
                | NaluType::BlaWLp
                | NaluType::BlaWRadl
                | NaluType::BlaNLp
                | NaluType::IdrWRadl
                | NaluType::IdrNLp
                | NaluType::CraNut,
        ) {
            return Err(anyhow!(
                "Invalid NALU type: {:?} is not a slice NALU",
                nalu.header().type_
            ));
        }

        let data = nalu.as_ref();
        let nalu_header = nalu.header();
        let hdr_len = nalu_header.len();
        // Skip the header
        let mut r = NaluReader::new(&data[hdr_len..]);

        let mut hdr = SliceHeader {
            first_slice_segment_in_pic_flag: r.read_bit()?,
            ..Default::default()
        };

        if nalu_header.type_ as u32 >= (NaluType::BlaWLp as u32)
            && nalu_header.type_ as u32 <= (NaluType::RsvIrapVcl23 as u32)
        {
            hdr.no_output_of_prior_pics_flag = r.read_bit()?;
        }

        hdr.pic_parameter_set_id = r.read_ue_max(63)?;

        let pps = self.get_pps(hdr.pic_parameter_set_id).context(
            "Broken stream: slice references PPS that has not been successfully parsed.",
        )?;

        let sps = self.get_sps(pps.seq_parameter_set_id).context(
            "Broken stream: slice's PPS references SPS that has not been successfully parsed.",
        )?;

        Self::slice_header_set_defaults(&mut hdr, sps, pps);

        if !hdr.first_slice_segment_in_pic_flag {
            if pps.dependent_slice_segments_enabled_flag {
                hdr.dependent_slice_segment_flag = r.read_bit()?;
            }

            let num_bits = (sps.pic_size_in_ctbs_y as f64).log2().ceil() as _;
            hdr.segment_address = r.read_bits(num_bits)?;

            if hdr.segment_address > sps.pic_size_in_ctbs_y - 1 {
                return Err(anyhow!(
                    "Invalid slice_segment_address {}",
                    hdr.segment_address
                ));
            }
        }

        if !hdr.dependent_slice_segment_flag {
            r.skip_bits(usize::from(pps.num_extra_slice_header_bits))?;

            let slice_type: u32 = r.read_ue()?;
            hdr.type_ = SliceType::n(slice_type).ok_or(anyhow!("Invalid slice type"))?;

            if pps.output_flag_present_flag {
                hdr.pic_output_flag = r.read_bit()?;
            }

            if sps.separate_colour_plane_flag {
                hdr.colour_plane_id = r.read_bits(2)?;
            }

            if !matches!(nalu_header.type_, NaluType::IdrWRadl | NaluType::IdrNLp) {
                let num_bits = usize::from(sps.log2_max_pic_order_cnt_lsb_minus4 + 4);
                hdr.pic_order_cnt_lsb = r.read_bits(num_bits)?;

                if hdr.pic_order_cnt_lsb
                    > 2u16.pow(u32::from(sps.log2_max_pic_order_cnt_lsb_minus4 + 4))
                {
                    return Err(anyhow!(
                        "Invalid pic_order_cnt_lsb {}",
                        hdr.pic_order_cnt_lsb
                    ));
                }

                hdr.short_term_ref_pic_set_sps_flag = r.read_bit()?;

                if !hdr.short_term_ref_pic_set_sps_flag {
                    let st_rps_idx = sps.num_short_term_ref_pic_sets;
                    Self::parse_short_term_ref_pic_set(
                        sps,
                        &mut hdr.short_term_ref_pic_set,
                        &mut r,
                        st_rps_idx,
                    )?;
                } else if sps.num_short_term_ref_pic_sets > 1 {
                    let num_bits = (sps.num_short_term_ref_pic_sets as f64).log2().ceil() as _;
                    hdr.short_term_ref_pic_set_idx = r.read_bits(num_bits)?;

                    if hdr.short_term_ref_pic_set_idx > sps.num_short_term_ref_pic_sets - 1 {
                        return Err(anyhow!(
                            "Invalid short_term_ref_pic_set_idx {}",
                            hdr.short_term_ref_pic_set_idx
                        ));
                    }
                }

                if hdr.short_term_ref_pic_set_sps_flag {
                    hdr.curr_rps_idx = hdr.short_term_ref_pic_set_idx;
                }

                if sps.long_term_ref_pics_present_flag {
                    if sps.num_long_term_ref_pics_sps > 0 {
                        hdr.num_long_term_sps =
                            r.read_ue_max(u32::from(sps.num_long_term_ref_pics_sps))?;
                    }

                    hdr.num_long_term_pics = r.read_ue()?;

                    let num_lt = hdr.num_long_term_sps + hdr.num_long_term_pics;
                    for i in 0..usize::from(num_lt) {
                        // The variables PocLsbLt[ i ] and UsedByCurrPicLt[ i ] are derived as follows:
                        //
                        // – If i is less than num_long_term_sps, PocLsbLt[ i ] is set equal to
                        // lt_ref_pic_poc_lsb_sps[ lt_idx_sps[ i ] ] and UsedByCurrPicLt[ i ] is set equal
                        // to used_by_curr_pic_lt_sps_flag[ lt_idx_sps[ i ] ].
                        //
                        // – Otherwise, PocLsbLt[ i ]
                        // is set equal to poc_lsb_lt[ i ] and UsedByCurrPicLt[ i ] is set equal to
                        // used_by_curr_pic_lt_flag[ i ].
                        if i < usize::from(hdr.num_long_term_sps) {
                            if sps.num_long_term_ref_pics_sps > 1 {
                                let num_bits =
                                    (sps.num_long_term_ref_pics_sps as f64).log2().ceil() as _;

                                hdr.lt_idx_sps[i] = r.read_bits(num_bits)?;

                                if hdr.lt_idx_sps[i] > sps.num_long_term_ref_pics_sps - 1 {
                                    return Err(anyhow!(
                                        "Invalid lt_idx_sps[{}] {}",
                                        i,
                                        hdr.lt_idx_sps[i]
                                    ));
                                }
                            }

                            hdr.poc_lsb_lt[i] = sps.lt_ref_pic_poc_lsb_sps[i];
                            hdr.used_by_curr_pic_lt[i] = sps.used_by_curr_pic_lt_sps_flag[i];
                        } else {
                            let num_bits = usize::from(sps.log2_max_pic_order_cnt_lsb_minus4) + 4;
                            hdr.poc_lsb_lt[i] = r.read_bits(num_bits)?;
                            hdr.used_by_curr_pic_lt[i] = r.read_bit()?;
                        }

                        hdr.delta_poc_msb_present_flag[i] = r.read_bit()?;
                        if hdr.delta_poc_msb_present_flag[i] {
                            // The value of delta_poc_msb_cycle_lt[ i ] shall be
                            // in the range of 0 to 2(32 −
                            // log2_max_pic_order_cnt_lsb_minus4 − 4 ),
                            // inclusive. When delta_poc_msb_cycle_lt[ i ] is
                            // not present, it is inferred to be equal to 0.
                            let max =
                                2u32.pow(32 - u32::from(sps.log2_max_pic_order_cnt_lsb_minus4) - 4);
                            hdr.delta_poc_msb_cycle_lt[i] = r.read_ue_max(max)?;

                            // Equation 7-52 (simplified)
                            if i != 0 && i != usize::from(hdr.num_long_term_sps) {
                                hdr.delta_poc_msb_cycle_lt[i] += hdr.delta_poc_msb_cycle_lt[i - 1];
                            }
                        }
                    }
                }

                if sps.temporal_mvp_enabled_flag {
                    hdr.temporal_mvp_enabled_flag = r.read_bit()?;
                }
            }

            if sps.sample_adaptive_offset_enabled_flag {
                hdr.sao_luma_flag = r.read_bit()?;
                if sps.chroma_array_type != 0 {
                    hdr.sao_chroma_flag = r.read_bit()?;
                }
            }

            if hdr.type_.is_p() || hdr.type_.is_b() {
                hdr.num_ref_idx_active_override_flag = r.read_bit()?;
                if hdr.num_ref_idx_active_override_flag {
                    hdr.num_ref_idx_l0_active_minus1 = r.read_ue_max(MAX_REF_IDX_ACTIVE - 1)?;
                    if hdr.type_.is_b() {
                        hdr.num_ref_idx_l1_active_minus1 = r.read_ue_max(MAX_REF_IDX_ACTIVE - 1)?;
                    }
                } else {
                    hdr.num_ref_idx_l0_active_minus1 = pps.num_ref_idx_l0_default_active_minus1;
                    hdr.num_ref_idx_l1_active_minus1 = pps.num_ref_idx_l1_default_active_minus1;
                }

                // 7-57
                let mut num_pic_total_curr = 0;
                let rps = if hdr.short_term_ref_pic_set_sps_flag {
                    sps.short_term_ref_pic_set
                        .get(usize::from(hdr.curr_rps_idx))
                        .ok_or(anyhow!("Invalid RPS"))?
                } else {
                    &hdr.short_term_ref_pic_set
                };

                for i in 0..usize::from(rps.num_negative_pics) {
                    if rps.used_by_curr_pic_s0[i] {
                        num_pic_total_curr += 1;
                    }
                }

                for i in 0..usize::from(rps.num_positive_pics) {
                    if rps.used_by_curr_pic_s1[i] {
                        num_pic_total_curr += 1;
                    }
                }

                for i in 0..usize::from(hdr.num_long_term_sps + hdr.num_long_term_pics) {
                    if hdr.used_by_curr_pic_lt[i] {
                        num_pic_total_curr += 1;
                    }
                }

                if pps.scc_extension.curr_pic_ref_enabled_flag {
                    num_pic_total_curr += 1;
                }

                hdr.num_pic_total_curr = num_pic_total_curr;

                if pps.lists_modification_present_flag && hdr.num_pic_total_curr > 1 {
                    Self::parse_ref_pic_lists_modification(&mut hdr, &mut r)?;
                }

                if hdr.type_.is_b() {
                    hdr.mvd_l1_zero_flag = r.read_bit()?;
                }

                if pps.cabac_init_present_flag {
                    hdr.cabac_init_flag = r.read_bit()?;
                }

                if hdr.temporal_mvp_enabled_flag {
                    if hdr.type_.is_b() {
                        hdr.collocated_from_l0_flag = r.read_bit()?;
                    }

                    if (hdr.collocated_from_l0_flag && hdr.num_ref_idx_l0_active_minus1 > 0)
                        || (!hdr.collocated_from_l0_flag && hdr.num_ref_idx_l1_active_minus1 > 0)
                    {
                        let max = if (hdr.type_.is_p() || hdr.type_.is_b())
                            && hdr.collocated_from_l0_flag
                        {
                            hdr.num_ref_idx_l0_active_minus1
                        } else if hdr.type_.is_b() && !hdr.collocated_from_l0_flag {
                            hdr.num_ref_idx_l1_active_minus1
                        } else {
                            return Err(anyhow!("Invalid value for collocated_ref_idx"));
                        };

                        {
                            hdr.collocated_ref_idx = r.read_ue_max(u32::from(max))?;
                        }
                    }
                }

                if (pps.weighted_pred_flag && hdr.type_.is_p())
                    || (pps.weighted_bipred_flag && hdr.type_.is_b())
                {
                    Self::parse_pred_weight_table(&mut hdr, &mut r, sps)?;
                }

                hdr.five_minus_max_num_merge_cand = r.read_ue()?;

                if sps.scc_extension.motion_vector_resolution_control_idc == 2 {
                    hdr.use_integer_mv_flag = r.read_bit()?;
                }
            }

            hdr.qp_delta = r.read_se()?;

            let slice_qp_y = (26 + pps.init_qp_minus26 + hdr.qp_delta) as i32;
            if slice_qp_y < pps.qp_bd_offset_y as i32 || slice_qp_y > 51 {
                return Err(anyhow!("Invalid slice_qp_delta: {}", hdr.qp_delta));
            }

            if pps.slice_chroma_qp_offsets_present_flag {
                hdr.cb_qp_offset = r.read_se_bounded(-12, 12)?;

                let qp_offset = pps.cb_qp_offset + hdr.cb_qp_offset;
                if !(-12..=12).contains(&qp_offset) {
                    return Err(anyhow!(
                        "Invalid value for slice_cb_qp_offset: {}",
                        hdr.cb_qp_offset
                    ));
                }

                hdr.cr_qp_offset = r.read_se_bounded(-12, 12)?;

                let qp_offset = pps.cr_qp_offset + hdr.cr_qp_offset;
                if !(-12..=12).contains(&qp_offset) {
                    return Err(anyhow!(
                        "Invalid value for slice_cr_qp_offset: {}",
                        hdr.cr_qp_offset
                    ));
                }
            }

            if pps.scc_extension.slice_act_qp_offsets_present_flag {
                hdr.slice_act_y_qp_offset = r.read_se_bounded(-12, 12)?;
                hdr.slice_act_cb_qp_offset = r.read_se_bounded(-12, 12)?;
                hdr.slice_act_cr_qp_offset = r.read_se_bounded(-12, 12)?;
            }

            if pps.range_extension.chroma_qp_offset_list_enabled_flag {
                hdr.cu_chroma_qp_offset_enabled_flag = r.read_bit()?;
            }

            if pps.deblocking_filter_override_enabled_flag {
                hdr.deblocking_filter_override_flag = r.read_bit()?;
                if !hdr.deblocking_filter_disabled_flag {
                    hdr.beta_offset_div2 = r.read_se_bounded(-6, 6)?;
                    hdr.tc_offset_div2 = r.read_se_bounded(-6, 6)?;
                }
            }

            if pps.loop_filter_across_slices_enabled_flag
                && (hdr.sao_luma_flag
                    || hdr.sao_chroma_flag
                    || !hdr.deblocking_filter_disabled_flag)
            {
                hdr.loop_filter_across_slices_enabled_flag = r.read_bit()?;
            }
        }

        if pps.tiles_enabled_flag || pps.entropy_coding_sync_enabled_flag {
            let max = if !pps.tiles_enabled_flag && pps.entropy_coding_sync_enabled_flag {
                sps.pic_height_in_ctbs_y - 1
            } else if pps.tiles_enabled_flag && !pps.entropy_coding_sync_enabled_flag {
                u32::from((pps.num_tile_columns_minus1 + 1) * (pps.num_tile_rows_minus1 + 1) - 1)
            } else {
                (u32::from(pps.num_tile_columns_minus1) + 1) * sps.pic_height_in_ctbs_y - 1
            };

            hdr.num_entry_point_offsets = r.read_ue_max(max)?;
            if hdr.num_entry_point_offsets > 0 {
                hdr.offset_len_minus1 = r.read_ue_max(31)?;
                for i in 0..hdr.num_entry_point_offsets as usize {
                    let num_bits = usize::from(hdr.offset_len_minus1 + 1);
                    hdr.entry_point_offset_minus1[i] = r.read_bits(num_bits)?;
                }
            }
        }

        if pps.slice_segment_header_extension_present_flag {
            let segment_header_extension_length = r.read_ue_max(256)?;
            for _ in 0..segment_header_extension_length {
                r.skip_bits(8)?; // slice_segment_header_extension_data_byte[i]
            }
        }

        // byte_alignment()
        r.skip_bits(1)?; // Alignment bit
        let num_bits = r.num_bits_left() % 8;
        r.skip_bits(num_bits)?;

        let epb = r.num_epb();
        hdr.header_bit_size = ((nalu.size() - epb) * 8 - r.num_bits_left()) as u32;

        hdr.n_emulation_prevention_bytes = epb as u32;

        Ok(Slice { header: hdr, nalu })
    }

    /// Returns a previously parsed vps given `vps_id`, if any.
    pub fn get_vps(&self, vps_id: u8) -> Option<&Vps> {
        self.active_vpses.get(&vps_id)
    }

    /// Returns a previously parsed sps given `sps_id`, if any.
    pub fn get_sps(&self, sps_id: u8) -> Option<&Sps> {
        self.active_spses.get(&sps_id)
    }

    /// Returns a previously parsed pps given `pps_id`, if any.
    pub fn get_pps(&self, pps_id: u8) -> Option<&Pps> {
        self.active_ppses.get(&pps_id)
    }
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use crate::decoder::stateless::h265::parser::NaluHeader;
    use crate::decoder::stateless::h265::parser::NaluType;
    use crate::decoder::stateless::h265::parser::Parser;
    use crate::decoder::stateless::h265::parser::SliceType;
    use crate::utils::nalu::Nalu;

    const STREAM_BEAR: &[u8] = include_bytes!("test_data/bear.hevc");
    const STREAM_BEAR_NUM_NALUS: usize = 35;

    const STREAM_BBB: &[u8] = include_bytes!("test_data/bbb.hevc");
    const STREAM_BBB_NUM_NALUS: usize = 64;

    const STREAM_TEST25FPS: &[u8] = include_bytes!("test_data/test-25fps.hevc");
    const STREAM_TEST25FPS_NUM_NALUS: usize = 254;

    const STREAM_TEST_25_FPS_SLICE_0: &[u8] =
        include_bytes!("test_data/test-25fps-h265-slice-data-0.bin");
    const STREAM_TEST_25_FPS_SLICE_1: &[u8] =
        include_bytes!("test_data/test-25fps-h265-slice-data-1.bin");

    fn dispatch_parse_call(
        parser: &mut Parser,
        nalu: Nalu<&[u8], NaluHeader>,
    ) -> anyhow::Result<()> {
        match nalu.header().type_ {
            NaluType::TrailN
            | NaluType::TrailR
            | NaluType::TsaN
            | NaluType::TsaR
            | NaluType::StsaN
            | NaluType::StsaR
            | NaluType::RadlN
            | NaluType::RadlR
            | NaluType::RaslN
            | NaluType::RaslR
            | NaluType::BlaWLp
            | NaluType::BlaWRadl
            | NaluType::BlaNLp
            | NaluType::IdrWRadl
            | NaluType::IdrNLp
            | NaluType::CraNut => {
                parser.parse_slice_header(nalu).unwrap();
            }
            NaluType::VpsNut => {
                parser.parse_vps(&nalu).unwrap();
            }
            NaluType::SpsNut => {
                parser.parse_sps(&nalu).unwrap();
            }
            NaluType::PpsNut => {
                parser.parse_pps(&nalu).unwrap();
            }
            _ => { /* ignore */ }
        }
        Ok(())
    }

    fn find_nalu_by_type(
        bitstream: &[u8],
        nalu_type: NaluType,
        mut nskip: i32,
    ) -> Option<Nalu<&[u8], NaluHeader>> {
        let mut cursor = Cursor::new(bitstream);
        while let Ok(Some(nalu)) = Nalu::<_, NaluHeader>::next(&mut cursor) {
            if nalu.header().type_ == nalu_type {
                if nskip == 0 {
                    return Some(nalu);
                } else {
                    nskip -= 1;
                }
            }
        }

        None
    }

    /// This test is adapted from chromium, available at media/video/h265_parser_unittest.cc
    #[test]
    fn parse_nalus_from_stream_file() {
        let mut cursor = Cursor::new(STREAM_BEAR);
        let mut num_nalus = 0;
        while let Ok(Some(_)) = Nalu::<_, NaluHeader>::next(&mut cursor) {
            num_nalus += 1;
        }

        assert_eq!(num_nalus, STREAM_BEAR_NUM_NALUS);

        let mut cursor = Cursor::new(STREAM_BBB);
        let mut num_nalus = 0;
        while let Ok(Some(_)) = Nalu::<_, NaluHeader>::next(&mut cursor) {
            num_nalus += 1;
        }

        assert_eq!(num_nalus, STREAM_BBB_NUM_NALUS);

        let mut cursor = Cursor::new(STREAM_TEST25FPS);
        let mut num_nalus = 0;
        while let Ok(Some(_)) = Nalu::<_, NaluHeader>::next(&mut cursor) {
            num_nalus += 1;
        }

        assert_eq!(num_nalus, STREAM_TEST25FPS_NUM_NALUS);
    }

    /// Parse the syntax, making sure we can parse the files without crashing.
    /// Does not check whether the parsed values are correct.
    #[test]
    fn parse_syntax_from_nals() {
        let mut cursor = Cursor::new(STREAM_BBB);
        let mut parser = Parser::default();

        while let Ok(Some(nalu)) = Nalu::<_, NaluHeader>::next(&mut cursor) {
            dispatch_parse_call(&mut parser, nalu).unwrap();
        }

        let mut cursor = Cursor::new(STREAM_BEAR);
        let mut parser = Parser::default();

        while let Ok(Some(nalu)) = Nalu::<_, NaluHeader>::next(&mut cursor) {
            dispatch_parse_call(&mut parser, nalu).unwrap();
        }

        let mut cursor = Cursor::new(STREAM_TEST25FPS);
        let mut parser = Parser::default();

        while let Ok(Some(nalu)) = Nalu::<_, NaluHeader>::next(&mut cursor) {
            dispatch_parse_call(&mut parser, nalu).unwrap();
        }
    }

    /// Adapted from Chromium (media/video/h265_parser_unittest.cc::VpsParsing())
    #[test]
    fn chromium_vps_parsing() {
        let mut cursor = Cursor::new(STREAM_BEAR);
        let mut parser = Parser::default();

        let vps_nalu = Nalu::<_, NaluHeader>::next(&mut cursor).unwrap().unwrap();
        let vps = parser.parse_vps(&vps_nalu).unwrap();

        assert!(vps.base_layer_internal_flag);
        assert!(vps.base_layer_available_flag);
        assert_eq!(vps.max_layers_minus1, 0);
        assert_eq!(vps.max_sub_layers_minus1, 0);
        assert!(vps.temporal_id_nesting_flag);
        assert_eq!(vps.profile_tier_level.general_profile_idc, 1);
        assert_eq!(vps.profile_tier_level.general_level_idc, 60);
        assert_eq!(vps.max_dec_pic_buffering_minus1[0], 4);
        assert_eq!(vps.max_num_reorder_pics[0], 2);
        assert_eq!(vps.max_latency_increase_plus1[0], 0);
        for i in 1..7 {
            assert_eq!(vps.max_dec_pic_buffering_minus1[i], 0);
            assert_eq!(vps.max_num_reorder_pics[i], 0);
            assert_eq!(vps.max_latency_increase_plus1[i], 0);
        }
        assert_eq!(vps.max_layer_id, 0);
        assert_eq!(vps.num_layer_sets_minus1, 0);
        assert!(!vps.timing_info_present_flag);
    }

    /// Adapted from Chromium (media/video/h265_parser_unittest.cc::SpsParsing())
    #[test]
    fn chromium_sps_parsing() {
        let mut parser = Parser::default();
        let sps_nalu = find_nalu_by_type(STREAM_BEAR, NaluType::SpsNut, 0).unwrap();
        let sps = parser.parse_sps(&sps_nalu).unwrap();

        assert_eq!(sps.max_sub_layers_minus1, 0);
        assert_eq!(sps.profile_tier_level.general_profile_idc, 1);
        assert_eq!(sps.profile_tier_level.general_level_idc, 60);
        assert_eq!(sps.seq_parameter_set_id, 0);
        assert_eq!(sps.chroma_format_idc, 1);
        assert!(!sps.separate_colour_plane_flag);
        assert_eq!(sps.pic_width_in_luma_samples, 320);
        assert_eq!(sps.pic_height_in_luma_samples, 184);
        assert_eq!(sps.conf_win_left_offset, 0);
        assert_eq!(sps.conf_win_right_offset, 0);
        assert_eq!(sps.conf_win_top_offset, 0);
        assert_eq!(sps.conf_win_bottom_offset, 2);
        assert_eq!(sps.bit_depth_luma_minus8, 0);
        assert_eq!(sps.bit_depth_chroma_minus8, 0);
        assert_eq!(sps.log2_max_pic_order_cnt_lsb_minus4, 4);
        assert_eq!(sps.max_dec_pic_buffering_minus1[0], 4);
        assert_eq!(sps.max_num_reorder_pics[0], 2);
        assert_eq!(sps.max_latency_increase_plus1[0], 0);
        for i in 1..7 {
            assert_eq!(sps.max_dec_pic_buffering_minus1[i], 0);
            assert_eq!(sps.max_num_reorder_pics[i], 0);
            assert_eq!(sps.max_latency_increase_plus1[i], 0);
        }
        assert_eq!(sps.log2_min_luma_coding_block_size_minus3, 0);
        assert_eq!(sps.log2_diff_max_min_luma_coding_block_size, 3);
        assert_eq!(sps.log2_min_luma_transform_block_size_minus2, 0);
        assert_eq!(sps.log2_diff_max_min_luma_transform_block_size, 3);
        assert_eq!(sps.max_transform_hierarchy_depth_inter, 0);
        assert_eq!(sps.max_transform_hierarchy_depth_intra, 0);
        assert!(!sps.scaling_list_enabled_flag);
        assert!(!sps.scaling_list_data_present_flag);
        assert!(!sps.amp_enabled_flag);
        assert!(sps.sample_adaptive_offset_enabled_flag);
        assert!(!sps.pcm_enabled_flag);
        assert_eq!(sps.pcm_sample_bit_depth_luma_minus1, 0);
        assert_eq!(sps.pcm_sample_bit_depth_chroma_minus1, 0);
        assert_eq!(sps.log2_min_pcm_luma_coding_block_size_minus3, 0);
        assert_eq!(sps.log2_diff_max_min_pcm_luma_coding_block_size, 0);
        assert!(!sps.pcm_loop_filter_disabled_flag);
        assert_eq!(sps.num_short_term_ref_pic_sets, 0);
        assert_eq!(sps.num_long_term_ref_pics_sps, 0);
        assert!(sps.temporal_mvp_enabled_flag);
        assert!(sps.strong_intra_smoothing_enabled_flag);
        assert_eq!(sps.vui_parameters.sar_width, 0);
        assert_eq!(sps.vui_parameters.sar_height, 0);
        assert!(!sps.vui_parameters.video_full_range_flag);
        assert!(!sps.vui_parameters.colour_description_present_flag);

        // Note: the original test has 0 for the three variables below, but they
        // have valid defaults in the spec (i.e.: 2).
        assert_eq!(sps.vui_parameters.colour_primaries, 2);
        assert_eq!(sps.vui_parameters.transfer_characteristics, 2);
        assert_eq!(sps.vui_parameters.matrix_coeffs, 2);

        assert_eq!(sps.vui_parameters.def_disp_win_left_offset, 0);
        assert_eq!(sps.vui_parameters.def_disp_win_right_offset, 0);
        assert_eq!(sps.vui_parameters.def_disp_win_top_offset, 0);
        assert_eq!(sps.vui_parameters.def_disp_win_bottom_offset, 0);
    }

    /// Adapted from Chromium (media/video/h265_parser_unittest.cc::PpsParsing())
    #[test]
    fn chromium_pps_parsing() {
        let mut parser = Parser::default();

        // Have to parse the SPS to set up the parser's internal state.
        let sps_nalu = find_nalu_by_type(STREAM_BEAR, NaluType::SpsNut, 0).unwrap();
        parser.parse_sps(&sps_nalu).unwrap();

        let pps_nalu = find_nalu_by_type(STREAM_BEAR, NaluType::PpsNut, 0).unwrap();
        let pps = parser.parse_pps(&pps_nalu).unwrap();

        assert_eq!(pps.pic_parameter_set_id, 0);
        assert_eq!(pps.seq_parameter_set_id, 0);
        assert!(!pps.dependent_slice_segments_enabled_flag);
        assert!(!pps.output_flag_present_flag);
        assert_eq!(pps.num_extra_slice_header_bits, 0);
        assert!(pps.sign_data_hiding_enabled_flag);
        assert!(!pps.cabac_init_present_flag);
        assert_eq!(pps.num_ref_idx_l0_default_active_minus1, 0);
        assert_eq!(pps.num_ref_idx_l1_default_active_minus1, 0);
        assert_eq!(pps.init_qp_minus26, 0);
        assert!(!pps.constrained_intra_pred_flag);
        assert!(!pps.transform_skip_enabled_flag);
        assert!(pps.cu_qp_delta_enabled_flag);
        assert_eq!(pps.diff_cu_qp_delta_depth, 0);
        assert_eq!(pps.cb_qp_offset, 0);
        assert_eq!(pps.cr_qp_offset, 0);
        assert!(!pps.slice_chroma_qp_offsets_present_flag);
        assert!(pps.weighted_pred_flag);
        assert!(!pps.weighted_bipred_flag);
        assert!(!pps.transquant_bypass_enabled_flag);
        assert!(!pps.tiles_enabled_flag);
        assert!(pps.entropy_coding_sync_enabled_flag);
        assert!(pps.loop_filter_across_tiles_enabled_flag);
        assert!(!pps.scaling_list_data_present_flag);
        assert!(!pps.lists_modification_present_flag);
        assert_eq!(pps.log2_parallel_merge_level_minus2, 0);
        assert!(!pps.slice_segment_header_extension_present_flag);
    }

    /// Adapted from Chromium (media/video/h265_parser_unittest.cc::SliceHeaderParsing())
    #[test]
    fn chromium_slice_header_parsing() {
        let mut parser = Parser::default();

        // Have to parse the SPS/VPS/PPS to set up the parser's internal state.
        let vps_nalu = find_nalu_by_type(STREAM_BEAR, NaluType::VpsNut, 0).unwrap();
        parser.parse_vps(&vps_nalu).unwrap();

        let sps_nalu = find_nalu_by_type(STREAM_BEAR, NaluType::SpsNut, 0).unwrap();
        parser.parse_sps(&sps_nalu).unwrap();

        let pps_nalu = find_nalu_by_type(STREAM_BEAR, NaluType::PpsNut, 0).unwrap();
        parser.parse_pps(&pps_nalu).unwrap();

        // Just like the Chromium test, do an IDR slice, then a non IDR slice.
        let slice_nalu = find_nalu_by_type(STREAM_BEAR, NaluType::IdrWRadl, 0).unwrap();
        let slice = parser.parse_slice_header(slice_nalu).unwrap();
        let hdr = slice.header();
        assert!(hdr.first_slice_segment_in_pic_flag);
        assert!(!hdr.no_output_of_prior_pics_flag);
        assert_eq!(hdr.pic_parameter_set_id, 0);
        assert!(!hdr.dependent_slice_segment_flag);
        assert_eq!(hdr.type_, SliceType::I);
        assert!(hdr.sao_luma_flag);
        assert!(hdr.sao_chroma_flag);
        assert_eq!(hdr.qp_delta, 8);
        assert!(hdr.loop_filter_across_slices_enabled_flag);

        let slice_nalu = find_nalu_by_type(STREAM_BEAR, NaluType::TrailR, 0).unwrap();
        let slice = parser.parse_slice_header(slice_nalu).unwrap();
        let hdr = slice.header();
        assert!(hdr.first_slice_segment_in_pic_flag);
        assert_eq!(hdr.pic_parameter_set_id, 0);
        assert!(!hdr.dependent_slice_segment_flag);
        assert_eq!(hdr.type_, SliceType::P);
        assert_eq!(hdr.pic_order_cnt_lsb, 4);
        assert!(!hdr.short_term_ref_pic_set_sps_flag);
        assert_eq!(hdr.short_term_ref_pic_set.num_negative_pics, 1);
        assert_eq!(hdr.short_term_ref_pic_set.num_positive_pics, 0);
        assert_eq!(hdr.short_term_ref_pic_set.delta_poc_s0[0], -4);
        assert!(hdr.short_term_ref_pic_set.used_by_curr_pic_s0[0]);
        assert!(hdr.temporal_mvp_enabled_flag);
        assert!(hdr.sao_luma_flag);
        assert!(hdr.sao_chroma_flag);
        assert!(!hdr.num_ref_idx_active_override_flag);
        assert_eq!(hdr.pred_weight_table.luma_log2_weight_denom, 0);
        assert_eq!(hdr.pred_weight_table.delta_chroma_log2_weight_denom, 7);
        assert_eq!(hdr.pred_weight_table.delta_luma_weight_l0[0], 0);
        assert_eq!(hdr.pred_weight_table.luma_offset_l0[0], -2);
        assert_eq!(hdr.pred_weight_table.delta_chroma_weight_l0[0][0], -9);
        assert_eq!(hdr.pred_weight_table.delta_chroma_weight_l0[0][1], -9);
        assert_eq!(hdr.pred_weight_table.delta_chroma_offset_l0[0][0], 0);
        assert_eq!(hdr.pred_weight_table.delta_chroma_offset_l0[0][1], 0);
        assert_eq!(hdr.five_minus_max_num_merge_cand, 3);
        assert_eq!(hdr.qp_delta, 8);
        assert!(hdr.loop_filter_across_slices_enabled_flag);
    }

    /// A custom test for VPS parsing with data manually extracted from
    /// GStreamer using GDB.
    #[test]
    fn test25fps_vps_header_parsing() {
        let mut cursor = Cursor::new(STREAM_TEST25FPS);
        let mut parser = Parser::default();

        let vps_nalu = Nalu::<_, NaluHeader>::next(&mut cursor).unwrap().unwrap();
        let vps = parser.parse_vps(&vps_nalu).unwrap();
        assert!(vps.base_layer_internal_flag);
        assert!(vps.base_layer_available_flag);
        assert_eq!(vps.max_layers_minus1, 0);
        assert_eq!(vps.max_sub_layers_minus1, 0);
        assert!(vps.temporal_id_nesting_flag);
        assert_eq!(vps.profile_tier_level.general_profile_space, 0);
        assert!(!vps.profile_tier_level.general_tier_flag);
        assert_eq!(vps.profile_tier_level.general_profile_idc, 1);
        for i in 0..32 {
            let val = i == 1 || i == 2;
            assert_eq!(
                vps.profile_tier_level.general_profile_compatibility_flag[i],
                val
            );
        }
        assert!(vps.profile_tier_level.general_progressive_source_flag);
        assert!(!vps.profile_tier_level.general_interlaced_source_flag);
        assert!(!vps.profile_tier_level.general_non_packed_constraint_flag,);
        assert!(vps.profile_tier_level.general_frame_only_constraint_flag,);
        assert!(!vps.profile_tier_level.general_max_12bit_constraint_flag,);
        assert!(!vps.profile_tier_level.general_max_10bit_constraint_flag,);
        assert!(!vps.profile_tier_level.general_max_8bit_constraint_flag,);
        assert!(!vps.profile_tier_level.general_max_422chroma_constraint_flag,);
        assert!(!vps.profile_tier_level.general_max_420chroma_constraint_flag,);
        assert!(
            !vps.profile_tier_level
                .general_max_monochrome_constraint_flag,
        );
        assert!(!vps.profile_tier_level.general_intra_constraint_flag);
        assert!(
            !vps.profile_tier_level
                .general_one_picture_only_constraint_flag,
        );
        assert!(
            !vps.profile_tier_level
                .general_lower_bit_rate_constraint_flag,
        );
        assert!(!vps.profile_tier_level.general_max_14bit_constraint_flag,);
        assert_eq!(vps.profile_tier_level.general_level_idc, 60);

        assert!(vps.sub_layer_ordering_info_present_flag);
        assert_eq!(vps.max_dec_pic_buffering_minus1[0], 4);
        assert_eq!(vps.max_num_reorder_pics[0], 2);
        assert_eq!(vps.max_latency_increase_plus1[0], 5);
        for i in 1..7 {
            assert_eq!(vps.max_dec_pic_buffering_minus1[i], 0);
            assert_eq!(vps.max_num_reorder_pics[i], 0);
            assert_eq!(vps.max_latency_increase_plus1[i], 0);
        }

        assert_eq!(vps.max_layer_id, 0);
        assert_eq!(vps.num_layer_sets_minus1, 0);
        assert!(!vps.timing_info_present_flag);
        assert_eq!(vps.num_units_in_tick, 0);
        assert_eq!(vps.time_scale, 0);
        assert!(!vps.poc_proportional_to_timing_flag);
        assert_eq!(vps.num_ticks_poc_diff_one_minus1, 0);
        assert_eq!(vps.num_hrd_parameters, 0);
    }

    /// A custom test for SPS parsing with data manually extracted from
    /// GStreamer using GDB.
    #[test]
    fn test25fps_sps_header_parsing() {
        let mut parser = Parser::default();

        let sps_nalu = find_nalu_by_type(STREAM_TEST25FPS, NaluType::SpsNut, 0).unwrap();
        let sps = parser.parse_sps(&sps_nalu).unwrap();

        assert_eq!(sps.max_sub_layers_minus1, 0);

        assert_eq!(sps.profile_tier_level.general_profile_space, 0);
        assert!(!sps.profile_tier_level.general_tier_flag);
        assert_eq!(sps.profile_tier_level.general_profile_idc, 1);
        for i in 0..32 {
            let val = i == 1 || i == 2;
            assert_eq!(
                sps.profile_tier_level.general_profile_compatibility_flag[i],
                val
            );
        }
        assert!(sps.profile_tier_level.general_progressive_source_flag);
        assert!(!sps.profile_tier_level.general_interlaced_source_flag);
        assert!(!sps.profile_tier_level.general_non_packed_constraint_flag,);
        assert!(sps.profile_tier_level.general_frame_only_constraint_flag,);
        assert!(!sps.profile_tier_level.general_max_12bit_constraint_flag,);
        assert!(!sps.profile_tier_level.general_max_10bit_constraint_flag,);
        assert!(!sps.profile_tier_level.general_max_8bit_constraint_flag,);
        assert!(!sps.profile_tier_level.general_max_422chroma_constraint_flag,);
        assert!(!sps.profile_tier_level.general_max_420chroma_constraint_flag,);
        assert!(
            !sps.profile_tier_level
                .general_max_monochrome_constraint_flag,
        );
        assert!(!sps.profile_tier_level.general_intra_constraint_flag);
        assert!(
            !sps.profile_tier_level
                .general_one_picture_only_constraint_flag,
        );
        assert!(
            !sps.profile_tier_level
                .general_lower_bit_rate_constraint_flag,
        );
        assert!(!sps.profile_tier_level.general_max_14bit_constraint_flag,);
        assert_eq!(sps.profile_tier_level.general_level_idc, 60);

        assert_eq!(sps.seq_parameter_set_id, 0);
        assert_eq!(sps.chroma_format_idc, 1);
        assert!(!sps.separate_colour_plane_flag);
        assert_eq!(sps.pic_width_in_luma_samples, 320);
        assert_eq!(sps.pic_height_in_luma_samples, 240);
        assert_eq!(sps.conf_win_left_offset, 0);
        assert_eq!(sps.conf_win_right_offset, 0);
        assert_eq!(sps.conf_win_top_offset, 0);
        assert_eq!(sps.conf_win_bottom_offset, 0);
        assert_eq!(sps.bit_depth_luma_minus8, 0);
        assert_eq!(sps.bit_depth_chroma_minus8, 0);
        assert_eq!(sps.log2_max_pic_order_cnt_lsb_minus4, 4);
        assert!(sps.sub_layer_ordering_info_present_flag);
        assert_eq!(sps.max_dec_pic_buffering_minus1[0], 4);
        assert_eq!(sps.max_num_reorder_pics[0], 2);
        assert_eq!(sps.max_latency_increase_plus1[0], 5);
        for i in 1..7 {
            assert_eq!(sps.max_dec_pic_buffering_minus1[i], 0);
            assert_eq!(sps.max_num_reorder_pics[i], 0);
            assert_eq!(sps.max_latency_increase_plus1[i], 0);
        }
        assert_eq!(sps.log2_min_luma_coding_block_size_minus3, 0);
        assert_eq!(sps.log2_diff_max_min_luma_coding_block_size, 3);
        assert_eq!(sps.log2_min_luma_transform_block_size_minus2, 0);
        assert_eq!(sps.log2_diff_max_min_luma_transform_block_size, 3);
        assert_eq!(sps.max_transform_hierarchy_depth_inter, 0);
        assert_eq!(sps.max_transform_hierarchy_depth_intra, 0);
        assert!(!sps.scaling_list_enabled_flag);
        assert!(!sps.scaling_list_data_present_flag);
        assert!(!sps.amp_enabled_flag);
        assert!(sps.sample_adaptive_offset_enabled_flag);
        assert!(!sps.pcm_enabled_flag);
        assert_eq!(sps.pcm_sample_bit_depth_luma_minus1, 0);
        assert_eq!(sps.pcm_sample_bit_depth_chroma_minus1, 0);
        assert_eq!(sps.log2_min_pcm_luma_coding_block_size_minus3, 0);
        assert_eq!(sps.log2_diff_max_min_pcm_luma_coding_block_size, 0);
        assert!(!sps.pcm_loop_filter_disabled_flag);
        assert_eq!(sps.num_short_term_ref_pic_sets, 0);
        assert_eq!(sps.num_long_term_ref_pics_sps, 0);
        assert!(sps.temporal_mvp_enabled_flag);
        assert!(sps.strong_intra_smoothing_enabled_flag);
        assert_eq!(sps.vui_parameters.sar_width, 0);
        assert_eq!(sps.vui_parameters.sar_height, 0);
        assert!(!sps.vui_parameters.video_full_range_flag);
        assert!(!sps.vui_parameters.colour_description_present_flag);
        assert!(sps.vui_parameters.video_signal_type_present_flag);
        assert!(sps.vui_parameters.timing_info_present_flag);
        assert_eq!(sps.vui_parameters.num_units_in_tick, 1);
        assert_eq!(sps.vui_parameters.time_scale, 25);
        assert!(!sps.vui_parameters.poc_proportional_to_timing_flag);
        assert_eq!(sps.vui_parameters.num_ticks_poc_diff_one_minus1, 0);
        assert!(!sps.vui_parameters.hrd_parameters_present_flag);
        assert_eq!(sps.vui_parameters.colour_primaries, 2);
        assert_eq!(sps.vui_parameters.transfer_characteristics, 2);
        assert_eq!(sps.vui_parameters.matrix_coeffs, 2);
        assert_eq!(sps.vui_parameters.def_disp_win_left_offset, 0);
        assert_eq!(sps.vui_parameters.def_disp_win_right_offset, 0);
        assert_eq!(sps.vui_parameters.def_disp_win_top_offset, 0);
        assert_eq!(sps.vui_parameters.def_disp_win_bottom_offset, 0);
    }

    /// A custom test for PPS parsing with data manually extracted from
    /// GStreamer using GDB.
    #[test]
    fn test25fps_pps_header_parsing() {
        let mut parser = Parser::default();

        let sps_nalu = find_nalu_by_type(STREAM_TEST25FPS, NaluType::SpsNut, 0).unwrap();
        parser.parse_sps(&sps_nalu).unwrap();

        let pps_nalu = find_nalu_by_type(STREAM_TEST25FPS, NaluType::PpsNut, 0).unwrap();
        let pps = parser.parse_pps(&pps_nalu).unwrap();

        assert!(!pps.dependent_slice_segments_enabled_flag);
        assert!(!pps.output_flag_present_flag);
        assert_eq!(pps.num_extra_slice_header_bits, 0);
        assert!(pps.sign_data_hiding_enabled_flag);
        assert!(!pps.cabac_init_present_flag);
        assert_eq!(pps.num_ref_idx_l0_default_active_minus1, 0);
        assert_eq!(pps.num_ref_idx_l1_default_active_minus1, 0);
        assert_eq!(pps.init_qp_minus26, 0);
        assert!(!pps.constrained_intra_pred_flag);
        assert!(!pps.transform_skip_enabled_flag);
        assert!(pps.cu_qp_delta_enabled_flag);
        assert_eq!(pps.diff_cu_qp_delta_depth, 1);
        assert_eq!(pps.cb_qp_offset, 0);
        assert_eq!(pps.cr_qp_offset, 0);
        assert!(!pps.slice_chroma_qp_offsets_present_flag);
        assert!(pps.weighted_pred_flag);
        assert!(!pps.weighted_bipred_flag);
        assert!(!pps.transquant_bypass_enabled_flag);
        assert!(!pps.tiles_enabled_flag);
        assert!(pps.entropy_coding_sync_enabled_flag);
        assert_eq!(pps.num_tile_rows_minus1, 0);
        assert_eq!(pps.num_tile_columns_minus1, 0);
        assert!(pps.uniform_spacing_flag);
        assert_eq!(pps.column_width_minus1, [0; 19]);
        assert_eq!(pps.row_height_minus1, [0; 21]);
        assert!(pps.loop_filter_across_slices_enabled_flag);
        assert!(pps.loop_filter_across_tiles_enabled_flag);
        assert!(!pps.deblocking_filter_control_present_flag);
        assert!(!pps.deblocking_filter_override_enabled_flag);
        assert!(!pps.deblocking_filter_disabled_flag);
        assert_eq!(pps.beta_offset_div2, 0);
        assert_eq!(pps.tc_offset_div2, 0);
        assert!(!pps.lists_modification_present_flag);
        assert_eq!(pps.log2_parallel_merge_level_minus2, 0);
        assert!(!pps.slice_segment_header_extension_present_flag);
        assert!(!pps.extension_present_flag);
    }

    /// A custom test for slice header parsing with data manually extracted from
    /// GStreamer using GDB.
    #[test]
    fn test25fps_slice_header_parsing() {
        let mut parser = Parser::default();

        // Have to parse the SPS/VPS/PPS to set up the parser's internal state.
        let vps_nalu = find_nalu_by_type(STREAM_TEST25FPS, NaluType::VpsNut, 0).unwrap();
        parser.parse_vps(&vps_nalu).unwrap();

        let sps_nalu = find_nalu_by_type(STREAM_TEST25FPS, NaluType::SpsNut, 0).unwrap();
        parser.parse_sps(&sps_nalu).unwrap();

        let pps_nalu = find_nalu_by_type(STREAM_TEST25FPS, NaluType::PpsNut, 0).unwrap();
        parser.parse_pps(&pps_nalu).unwrap();

        let slice_nalu = find_nalu_by_type(STREAM_TEST25FPS, NaluType::IdrNLp, 0).unwrap();
        let slice = parser.parse_slice_header(slice_nalu).unwrap();
        let hdr = slice.header();

        assert!(hdr.first_slice_segment_in_pic_flag);
        assert!(!hdr.no_output_of_prior_pics_flag);
        assert!(!hdr.dependent_slice_segment_flag);
        assert_eq!(hdr.type_, SliceType::I);
        assert!(hdr.pic_output_flag);
        assert_eq!(hdr.colour_plane_id, 0);
        assert_eq!(hdr.pic_order_cnt_lsb, 0);
        assert!(!hdr.short_term_ref_pic_set_sps_flag);
        assert_eq!(hdr.lt_idx_sps, [0; 16]);
        assert_eq!(hdr.poc_lsb_lt, [0; 16]);
        assert_eq!(hdr.used_by_curr_pic_lt, [false; 16]);
        assert_eq!(hdr.delta_poc_msb_cycle_lt, [0; 16]);
        assert_eq!(hdr.delta_poc_msb_present_flag, [false; 16]);
        assert!(!hdr.temporal_mvp_enabled_flag);
        assert!(hdr.sao_luma_flag);
        assert!(hdr.sao_chroma_flag);
        assert!(!hdr.num_ref_idx_active_override_flag);
        assert_eq!(hdr.num_ref_idx_l0_active_minus1, 0);
        assert_eq!(hdr.num_ref_idx_l1_active_minus1, 0);
        assert!(!hdr.cabac_init_flag);
        assert!(hdr.collocated_from_l0_flag);
        assert_eq!(hdr.five_minus_max_num_merge_cand, 0);
        assert!(!hdr.use_integer_mv_flag);
        assert_eq!(hdr.qp_delta, 7);
        assert_eq!(hdr.cb_qp_offset, 0);
        assert_eq!(hdr.cr_qp_offset, 0);
        assert!(!hdr.cu_chroma_qp_offset_enabled_flag);
        assert!(!hdr.deblocking_filter_override_flag);
        assert!(!hdr.deblocking_filter_override_flag);
        assert_eq!(hdr.beta_offset_div2, 0);
        assert_eq!(hdr.tc_offset_div2, 0);
        assert!(hdr.loop_filter_across_slices_enabled_flag);
        assert_eq!(hdr.num_entry_point_offsets, 3);
        assert_eq!(hdr.offset_len_minus1, 11);
        assert_eq!(hdr.num_pic_total_curr, 0);

        // Remove the 2 bytes from the NALU header.
        assert_eq!(hdr.header_bit_size - 16, 72);

        assert_eq!(hdr.n_emulation_prevention_bytes, 0);

        assert_eq!(slice.nalu.as_ref(), STREAM_TEST_25_FPS_SLICE_0);

        // Next slice
        let slice_nalu = find_nalu_by_type(STREAM_TEST25FPS, NaluType::TrailR, 0).unwrap();
        let slice = parser.parse_slice_header(slice_nalu).unwrap();
        let hdr = slice.header();

        assert!(hdr.first_slice_segment_in_pic_flag);
        assert!(!hdr.no_output_of_prior_pics_flag);
        assert!(!hdr.dependent_slice_segment_flag);
        assert_eq!(hdr.type_, SliceType::P);
        assert!(hdr.pic_output_flag);
        assert_eq!(hdr.colour_plane_id, 0);
        assert_eq!(hdr.pic_order_cnt_lsb, 3);
        assert!(!hdr.short_term_ref_pic_set_sps_flag);
        assert_eq!(hdr.short_term_ref_pic_set.num_delta_pocs, 1);
        assert_eq!(hdr.short_term_ref_pic_set.num_negative_pics, 1);
        assert_eq!(hdr.short_term_ref_pic_set.num_positive_pics, 0);
        assert!(hdr.short_term_ref_pic_set.used_by_curr_pic_s0[0]);
        assert_eq!(hdr.short_term_ref_pic_set.delta_poc_s0[0], -3);
        assert_eq!(hdr.lt_idx_sps, [0; 16]);
        assert_eq!(hdr.poc_lsb_lt, [0; 16]);
        assert_eq!(hdr.used_by_curr_pic_lt, [false; 16]);
        assert_eq!(hdr.delta_poc_msb_cycle_lt, [0; 16]);
        assert_eq!(hdr.delta_poc_msb_present_flag, [false; 16]);
        assert!(hdr.temporal_mvp_enabled_flag);
        assert!(hdr.sao_luma_flag);
        assert!(hdr.sao_chroma_flag);
        assert!(!hdr.num_ref_idx_active_override_flag);
        assert_eq!(hdr.num_ref_idx_l0_active_minus1, 0);
        assert_eq!(hdr.num_ref_idx_l1_active_minus1, 0);
        assert!(!hdr.cabac_init_flag);
        assert!(hdr.collocated_from_l0_flag);
        assert_eq!(hdr.pred_weight_table.luma_log2_weight_denom, 7);
        assert_eq!(hdr.five_minus_max_num_merge_cand, 2);
        assert!(!hdr.use_integer_mv_flag);
        assert_eq!(hdr.num_entry_point_offsets, 3);
        assert_eq!(hdr.qp_delta, 7);
        assert_eq!(hdr.cb_qp_offset, 0);
        assert_eq!(hdr.cr_qp_offset, 0);
        assert!(!hdr.cu_chroma_qp_offset_enabled_flag);
        assert!(!hdr.deblocking_filter_override_flag);
        assert!(!hdr.deblocking_filter_override_flag);
        assert_eq!(hdr.beta_offset_div2, 0);
        assert_eq!(hdr.tc_offset_div2, 0);
        assert!(!hdr.loop_filter_across_slices_enabled_flag);
        assert_eq!(hdr.num_entry_point_offsets, 3);
        assert_eq!(hdr.offset_len_minus1, 10);
        assert_eq!(hdr.num_pic_total_curr, 1);

        assert_eq!(slice.nalu.size(), 2983);
        // Subtract 2 bytes to account for the header size.
        assert_eq!(hdr.header_bit_size() - 16, 96);
        assert_eq!(slice.nalu.as_ref(), STREAM_TEST_25_FPS_SLICE_1);

        // Next slice
        let slice_nalu = find_nalu_by_type(STREAM_TEST25FPS, NaluType::TrailR, 1).unwrap();
        let slice = parser.parse_slice_header(slice_nalu).unwrap();
        let hdr = slice.header();

        assert_eq!(slice.nalu.size(), 290);
        // Subtract 2 bytes to account for the header size.
        assert_eq!(hdr.header_bit_size() - 16, 80);
    }
}
