// Copyright 2023 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::borrow::Cow;
use std::rc::Rc;

use anyhow::anyhow;
use anyhow::Context;
use enumn::N;

use crate::codec::av1::helpers;
use crate::codec::av1::reader::Reader;

pub const TOTAL_REFS_PER_FRAME: usize = 8;
pub const NUM_REF_FRAMES: usize = 8;
pub const REFS_PER_FRAME: usize = 7;
pub const MAX_SEGMENTS: usize = 8;
pub const SEG_LVL_ALT_Q: usize = 0;
pub const SEG_LVL_ALT_LF_Y_V: usize = 1;
pub const SEG_LVL_REF_FRAME: usize = 5;
pub const SEG_LVL_SKIP: usize = 6;
pub const SEG_LVL_GLOBAL_MV: usize = 7;
pub const SEG_LVL_MAX: usize = 8;
pub const MAX_TILE_COLS: usize = 64;
pub const MAX_TILE_ROWS: usize = 64;
pub const CDEF_MAX: usize = 1 << 3;
pub const MAX_NUM_PLANES: usize = 3;
pub const MAX_NUM_Y_POINTS: usize = 16;
pub const MAX_NUM_CB_POINTS: usize = 16;
pub const MAX_NUM_CR_POINTS: usize = 16;
pub const MAX_NUM_POS_LUMA: usize = 25;
pub const MAX_NUM_SPATIAL_LAYERS: usize = 4;
pub const MAX_NUM_TEMPORAL_LAYERS: usize = 8;
pub const MAX_NUM_OPERATING_POINTS: usize = MAX_NUM_SPATIAL_LAYERS * MAX_NUM_TEMPORAL_LAYERS;
pub const SELECT_SCREEN_CONTENT_TOOLS: usize = 2;
pub const SELECT_INTEGER_MV: usize = 2;
pub const PRIMARY_REF_NONE: u32 = 7;
pub const SUPERRES_DENOM_BITS: usize = 3;
pub const SUPERRES_DENOM_MIN: usize = 9;
pub const SUPERRES_NUM: usize = 8;
pub const MAX_TILE_WIDTH: u32 = 4096;
pub const MAX_TILE_HEIGHT: u32 = 2304;
pub const MAX_TILE_AREA: u32 = MAX_TILE_WIDTH * MAX_TILE_HEIGHT;
pub const RESTORATION_TILESIZE_MAX: u16 = 256;
pub const WARPEDMODEL_PREC_BITS: u32 = 16;
pub const WARP_PARAM_REDUCE_BITS: u32 = 6;
pub const GM_ABS_ALPHA_BITS: u32 = 12;
pub const GM_ALPHA_PREC_BITS: u32 = 15;
pub const GM_ABS_TRANS_ONLY_BITS: u32 = 9;
pub const GM_TRANS_ONLY_PREC_BITS: u32 = 3;
pub const GM_ABS_TRANS_BITS: u32 = 12;
pub const GM_TRANS_PREC_BITS: u32 = 6;

// Same as Segmentation_Feature_Bits in the specification. See 5.9.14
pub const FEATURE_BITS: [u8; SEG_LVL_MAX] = [8, 6, 6, 6, 6, 3, 0, 0];
// Same as Segmentation_Feature_Signed in the specification. See 5.9.14
pub const FEATURE_SIGNED: [bool; SEG_LVL_MAX] = [true, true, true, true, true, false, false, false];
// Same as Segmentation_Feature_Max in the specification. See 5.9.14
pub const FEATURE_MAX: [i32; SEG_LVL_MAX] = [255, 63, 63, 63, 63, 7, 0, 0];

pub enum ParsedObu<'a> {
    /// We should process the OBU normally.
    Process(Obu<'a>),
    /// We should drop this OBU and advance to the next one. The u32 is how much
    /// we should advance.
    Drop(u32),
}

#[derive(N, Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub enum ObuType {
    #[default]
    Reserved = 0,
    SequenceHeader = 1,
    TemporalDelimiter = 2,
    FrameHeader = 3,
    TileGroup = 4,
    Metadata = 5,
    Frame = 6,
    RedundantFrameHeader = 7,
    TileList = 8,
    Reserved2 = 9,
    Reserved3 = 10,
    Reserved4 = 11,
    Reserved5 = 12,
    Reserved6 = 13,
    Reserved7 = 14,
    Padding = 15,
}

#[derive(N, Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub enum Profile {
    #[default]
    Profile0 = 0,
    Profile1 = 1,
    Profile2 = 2,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct ObuHeader {
    pub obu_type: ObuType,
    pub extension_flag: bool,
    pub has_size_field: bool,
    pub temporal_id: u32,
    pub spatial_id: u32,
}

impl ObuHeader {
    /// Length in bytes
    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        if self.extension_flag {
            2
        } else {
            1
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Obu<'a> {
    /// The OBU header.
    pub header: ObuHeader,
    /// The data backing the OBU.
    pub data: Cow<'a, [u8]>,
    /// Where the OBU data starts, after the size has been read.
    pub start_offset: usize,
    /// The OBU size as per the specification after `start_offset`.
    pub size: usize,
}

impl<'a> AsRef<[u8]> for Obu<'a> {
    fn as_ref(&self) -> &[u8] {
        &self.data[self.start_offset..self.start_offset + self.size]
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Tile {
    /// Same as TileOffset in the specification.
    pub tile_offset: u32,
    /// Same as TileSize in the specification.
    pub tile_size: u32,
    /// Same as TileRow in the specification.
    pub tile_row: u32,
    /// Same as TileCol in the specification.
    pub tile_col: u32,
    // Same as MiRowStart in the specification.
    pub mi_row_start: u32,
    // Same as MiRowEnd in the specification.
    pub mi_row_end: u32,
    // Same as MiColStart in the specification.
    pub mi_col_start: u32,
    // Same as MiColEnd in the specification.
    pub mi_col_end: u32,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct TileGroupObu<'a> {
    /// The OBU backing this tile group.
    pub obu: Obu<'a>,
    /// Specifies whether tg_start and tg_end are present. If tg_start and
    /// tg_end are not present, this tile group covers the entire frame.
    pub tile_start_and_end_present_flag: bool,
    /// Specifies the zero-based index of the first tile in the current tile
    /// group.
    pub tg_start: u32,
    /// Specifies the zero-based index of the last tile in the current tile
    /// group.
    pub tg_end: u32,
    /// Contains the tiles in this tile group. Use `tile_offset`to index into
    /// the OBU data.
    ///
    /// The tiles in the Vec span from tg_start to tg_end.
    pub tiles: Vec<Tile>,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct OperatingPoint {
    /// Specifies the level that the coded video sequence conforms to when
    /// operating point i is selected.
    pub seq_level_idx: u32,
    /// Specifies the tier that the coded video sequence conforms to when
    /// operating point i is selected.
    pub seq_tier: u32,
    /// Specifies the value of operating_point_idc for the selected operating
    /// point.
    pub idc: u32,
    /// If set, indicates that there is a decoder model associated with
    /// operating point i. If not set, indicates that there is not a decoder
    /// model associated with operating point i.
    pub decoder_model_present_for_this_op: bool,
    /// Specifies the time interval between the arrival of the first bit in the
    /// smoothing buffer and the subsequent removal of the data that belongs to
    /// the first coded frame for operating point op, measured in units of
    /// 1/90000 seconds. The length of decoder_buffer_delay is specified by
    /// buffer_delay_length_minus_1 + 1, in bits.
    pub decoder_buffer_delay: u32,
    /// Specifies, in combination with decoder_buffer_delay\[ op \] syntax
    /// element, the first bit arrival time of frames to be decoded to the
    /// smoothing buffer. encoder_buffer_delay is measured in units of 1/90000
    /// seconds.
    pub encoder_buffer_delay: u32,
    /// If set, indicates that the smoothing buffer operates in low-delay mode
    /// for operating point op. In low-delay mode late decode times and buffer
    /// underflow are both permitted. If not set, indicates that the smoothing
    /// buffer operates in strict mode, where buffer underflow is not allowed.
    pub low_delay_mode_flag: bool,
    /// If set, indicates that initial_display_delay_minus_1 is specified for
    /// operating point i. If not set, indicates that
    /// initial_display_delay_minus_1 is not specified for operating point i.
    pub initial_display_delay_present_for_this_op: bool,
    /// Plus 1 specifies, for operating point i, the number of decoded frames
    /// that should be present in the buffer pool before the first presentable
    /// frame is displayed. This will ensure that all presentable frames in the
    /// sequence can be decoded at or before the time that they are scheduled
    /// for display. If not signaled then initial_display_delay_minus_1\[ i \] =
    /// BUFFER_POOL_MAX_SIZE - 1.
    pub initial_display_delay_minus_1: u32,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct TimingInfo {
    /// The number of time units of a clock operating at the frequency
    /// time_scale Hz that corresponds to one increment of a clock tick counter.
    /// A display clock tick, in seconds, is equal to num_units_in_display_tick
    /// divided by time_scale:
    pub num_units_in_display_tick: u32,
    /// The number of time units that pass in one second.
    pub time_scale: u32,
    /// If set, indicates that pictures should be displayed according to their
    /// output order with the number of ticks between two consecutive pictures
    /// (without dropping frames) specified by num_ticks_per_picture_minus_1 +
    /// 1. If not set, indicates that the interval between two consecutive
    /// pictures is not specified.
    pub equal_picture_interval: bool,
    /// Plus 1 specifies the number of clock ticks corresponding to output time
    /// between two consecutive pictures in the output order.
    pub num_ticks_per_picture_minus_1: u32,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct DecoderModelInfo {
    /// Plus 1 specifies the length of the decoder_buffer_delay and the
    /// encoder_buffer_delay syntax elements, in bits.
    pub buffer_delay_length_minus_1: u32,
    /// The number of time units of a decoding clock operating at the frequency
    /// time_scale Hz that corresponds to one increment of a clock tick counter:
    pub num_units_in_decoding_tick: u32,
    /// Plus 1 specifies the length of the buffer_removal_time syntax element,
    /// in bits.
    pub buffer_removal_time_length_minus_1: u32,
    /// Plus 1 specifies the length of the frame_presentation_time syntax
    /// element, in bits.
    pub frame_presentation_time_length_minus_1: u32,
}

/// Defined by the “Color primaries” section of ISO/IEC 23091-4/ITU-T H.273
/// See 6.4.2
#[derive(N, Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub enum ColorPrimaries {
    Bt709 = 1,
    #[default]
    Unspecified = 2,
    Bt470M = 4,
    Bt470bg = 5,
    Bt601 = 6,
    Smpte240 = 7,
    GenericFilm = 8,
    Bt2020 = 9,
    Xyz = 10,
    Smpte431 = 11,
    Smpte432 = 12,
    Ebu3213 = 22,
}

#[derive(N, Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub enum TransferCharacteristics {
    Reserved0 = 0,
    Bt709 = 1,
    #[default]
    Unspecified = 2,
    Reserved3 = 3,
    Bt470m = 4,
    Bt470bg = 5,
    Bt601 = 6,
    Smpte240 = 7,
    Linear = 8,
    Log100 = 9,
    Log100Sqrt10 = 10,
    Iec61966 = 11,
    Bt1361 = 12,
    Srgb = 13,
    Bt202010Bit = 14,
    Bt202012Bit = 15,
    Smpte2084 = 16,
    Smpte428 = 17,
    Hlg = 18,
}

#[derive(N, Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub enum BitDepth {
    #[default]
    Depth8,
    Depth10,
    Depth12,
}

#[derive(N, Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub enum MatrixCoefficients {
    Identity = 0,
    Bt709 = 1,
    #[default]
    Unspecified = 2,
    Reserved3 = 3,
    Fcc = 4,
    Bt470bg = 5,
    Bt601 = 6,
    Smpte240 = 7,
    Ycgco = 8,
    Bt2020Ncl = 9,
    Bt2020Cl = 10,
    Smpte2085 = 11,
    ChromaDerivedNcl = 12,
    ChromaDerivedCl = 13,
    Ictcp = 14,
}

#[derive(N, Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub enum ChromaSamplePosition {
    #[default]
    Unknown = 0,
    Vertical = 1,
    Colocated = 2,
    Reserved = 3,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct ColorConfig {
    /// Syntax elements which, together with seq_profile, determine the bit
    /// depth.
    pub high_bitdepth: bool,
    /// Syntax elements which, together with seq_profile, determine the bit
    /// depth.
    pub twelve_bit: bool,
    /// If set, indicates that the video does not contain U and V color planes.
    /// If not set, indicates that the video contains Y, U, and V color planes.
    pub mono_chrome: bool,
    /// If set, specifies that color_primaries, transfer_characteristics, and
    /// matrix_coefficients are present. If not set, specifies that
    /// color_primaries, transfer_characteristics and matrix_coefficients are
    /// not present.
    pub color_description_present_flag: bool,
    /// Defined by the “Color primaries” section of ISO/IEC 23091-4/ITU-T H.273.
    pub color_primaries: ColorPrimaries,
    /// Defined by the “Transfer characteristics” section of ISO/IEC
    /// 23091-4/ITU-T H.273.
    pub transfer_characteristics: TransferCharacteristics,
    /// Defined by the “Matrix coefficients” section of ISO/IEC 23091-4/ITU-T
    /// H.273.
    pub matrix_coefficients: MatrixCoefficients,
    /// Binary value that is associated with the VideoFullRangeFlag variable
    /// specified in ISO/IEC 23091-4/ITU- T H.273. color range equal to 0 shall
    /// be referred to as the studio swing representation and color range equal
    /// to 1 shall be referred to as the full swing representation for all
    /// intents relating to this specification.
    pub color_range: bool,
    /// Specify the chroma subsampling format
    pub subsampling_x: bool,
    /// Specify the chroma subsampling format
    pub subsampling_y: bool,
    /// Specifies the sample position for subsampled streams
    pub chroma_sample_position: ChromaSamplePosition,
    /// If set, indicates that the U and V planes may have separate delta
    /// quantizer values. If not set, indicates that the U and V planes will
    /// share the same delta quantizer value.
    pub separate_uv_delta_q: bool,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct SequenceHeaderObu {
    /// The OBU header from the OBU that generated this sequence.
    pub obu_header: ObuHeader,
    /// Specifies the features that can be used in the coded video sequence.
    pub seq_profile: Profile,
    /// If set, specifies that the coded video sequence contains only one coded
    /// frame. If not set, specifies that the coded video sequence contains one
    /// or more coded frames.
    pub still_picture: bool,
    /// Specifies that the syntax elements not needed by a still picture are
    /// omitted.
    pub reduced_still_picture_header: bool,
    /// Specifies the number of bits minus 1 used for transmitting the frame
    /// width syntax elements.
    pub frame_width_bits_minus_1: u32,
    /// Specifies the number of bits minus 1 used for transmitting the frame
    /// height syntax elements.
    pub frame_height_bits_minus_1: u32,
    /// Specifies the maximum frame width minus 1 for the frames represented by
    /// this sequence header.
    pub max_frame_width_minus_1: u32,
    /// Specifies the maximum frame height minus 1 for the frames represented by
    /// this sequence header.
    pub max_frame_height_minus_1: u32,
    /// Specifies whether frame id numbers are present in the coded video
    /// sequence.
    pub frame_id_numbers_present_flag: bool,
    /// Specifies the number of bits minus 2 used to encode delta_frame_id
    /// syntax elements.
    pub delta_frame_id_length_minus_2: u32,
    /// Used to calculate the number of bits used to encode the frame_id syntax
    /// element.
    pub additional_frame_id_length_minus_1: u32,
    /// When set, indicates that superblocks contain 128x128 luma samples. When
    /// not set, it indicates that superblocks contain 64x64 luma samples. (The
    /// number of contained chroma samples depends on subsampling_x and
    /// subsampling_y.)
    pub use_128x128_superblock: bool,
    /// When set, specifies that the use_filter_intra syntax element may be
    /// present. When not set, specifies that the use_filter_intra syntax
    /// element will not be present.
    pub enable_filter_intra: bool,
    /// Specifies whether the intra edge filtering process should be enabled.
    pub enable_intra_edge_filter: bool,
    /// When set, specifies that the mode info for inter blocks may contain the
    /// syntax element interintra. If not set, specifies that the syntax element
    /// interintra will not be present.
    pub enable_interintra_compound: bool,
    /// When set, specifies that the mode info for inter blocks may contain the
    /// syntax element compound_type. When not set, specifies that the syntax
    /// element compound_type will not be present.
    pub enable_masked_compound: bool,
    /// When set, indicates that the allow_warped_motion syntax element may be
    /// present. When not set, indicates that the allow_warped_motion syntax
    /// element will not be present.
    pub enable_warped_motion: bool,
    /// When set, indicates that tools based on the values of order hints may be
    /// used. When not set, indicates that tools based on order hints are
    /// disabled.
    pub enable_order_hint: bool,
    /// When set, indicates that the inter prediction filter type may be
    /// specified independently in the horizontal and vertical directions. If
    /// the flag is not set, only one filter type may be specified, which is
    /// then used in both directions.
    pub enable_dual_filter: bool,
    /// If set, indicates that the distance weights process may be used for
    /// inter prediction.
    pub enable_jnt_comp: bool,
    /// If set, indicates that the use_ref_frame_mvs syntax element may be
    /// present. If not set, indicates that the use_ref_frame_mvs syntax element
    /// will not be present.
    pub enable_ref_frame_mvs: bool,
    /// If not set, indicates that the seq_force_screen_content_tools syntax
    /// element will be present. If set, indicates that
    /// seq_force_screen_content_tools should be set equal to
    /// SELECT_SCREEN_CONTENT_TOOLS.
    pub seq_choose_screen_content_tools: bool,
    /// Equal to SELECT_SCREEN_CONTENT_TOOLS indicates that the
    /// allow_screen_content_tools syntax element will be present in the frame
    /// header. Otherwise, seq_force_screen_content_tools contains the value for
    /// allow_screen_content_tools.
    pub seq_force_screen_content_tools: u32,
    /// If not set, indicates that the seq_force_integer_mv syntax element will
    /// be present. If set, indicates that seq_force_integer_mv should be set
    /// equal to SELECT_INTEGER_MV.
    pub seq_choose_integer_mv: bool,
    /// Equal to SELECT_INTEGER_MV indicates that the force_integer_mv syntax
    /// element will be present in the frame header (providing
    /// allow_screen_content_tools is equal to 1). Otherwise,
    /// seq_force_integer_mv contains the value for force_integer_mv.
    pub seq_force_integer_mv: u32,
    /// Used to compute OrderHintBits.
    pub order_hint_bits_minus_1: i32,
    /// Specifies the number of bits used for the order_hint syntax element.
    pub order_hint_bits: i32,
    /// If set, specifies that the use_superres syntax element will be present
    /// in the uncompressed header. If not set, specifies that the use_superres
    /// syntax element will not be present (instead use_superres will be set to
    /// 0 in the uncompressed header without being read).
    pub enable_superres: bool,
    /// If set, specifies that cdef filtering may be enabled. If not set,
    /// specifies that cdef filtering is disabled.
    pub enable_cdef: bool,
    /// If set, specifies that loop restoration filtering may be enabled. If
    /// not set, specifies that loop restoration filtering is disabled.
    pub enable_restoration: bool,
    /// Specifies whether film grain parameters are present in the coded video
    /// sequence.
    pub film_grain_params_present: bool,
    /// Indicates the number of operating points minus 1 present in the coded
    /// video sequence. An operating point specifies which spatial and temporal
    /// layers should be decoded.
    pub operating_points_cnt_minus_1: u32,
    /// The set of operating points.
    pub operating_points: [OperatingPoint; MAX_NUM_OPERATING_POINTS],
    /// Specifies whether decoder model information is present in the coded
    /// video sequence.
    pub decoder_model_info_present_flag: bool,
    /// The decoder model info.
    pub decoder_model_info: DecoderModelInfo,
    /// Specifies whether initial display delay information is present in the
    /// coded video sequence.
    pub initial_display_delay_present_flag: bool,
    /// Specifies whether timing info is present in the coded video sequence.
    pub timing_info_present_flag: bool,
    /// The timing info.
    pub timing_info: TimingInfo,
    /// The color config.
    pub color_config: ColorConfig,

    /* CamelCase variables in the specification */
    pub bit_depth: BitDepth,
    pub num_planes: u32,
}

/// A TemporalDelimiterOBU
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct TemporalDelimiterObu {
    pub obu_header: ObuHeader,
}

#[derive(N, Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub enum InterpolationFilter {
    #[default]
    EightTap = 0,
    EightTapSmooth = 1,
    EightTapSharp = 2,
    Bilinear = 3,
    Switchable = 4,
}

#[derive(N, Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub enum TxModes {
    #[default]
    Only4x4 = 0,
    Largest = 1,
    Select = 2,
}

#[derive(N, Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub enum FrameRestorationType {
    #[default]
    None = 0,
    Wiener = 1,
    Sgrproj = 2,
    Switchable = 3,
}

#[derive(N, Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub enum ReferenceFrameType {
    #[default]
    Intra = 0,
    Last = 1,
    Last2 = 2,
    Last3 = 3,
    Golden = 4,
    BwdRef = 5,
    AltRef2 = 6,
    AltRef = 7,
}

#[derive(N, Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub enum WarpModelType {
    #[default]
    Identity = 0,
    Translation = 1,
    RotZoom = 2,
    Affine = 3,
}

#[derive(N, Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub enum FrameType {
    #[default]
    KeyFrame = 0,
    InterFrame = 1,
    IntraOnlyFrame = 2,
    SwitchFrame = 3,
}

#[derive(N, Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub enum TxMode {
    #[default]
    Only4x4 = 0,
    Largest = 1,
    Select = 2,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct FrameObu<'a> {
    pub header: FrameHeaderObu,
    pub tile_group: TileGroupObu<'a>,
}

/// A FrameHeaderOBU
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct FrameHeaderObu {
    /// The original OBU header. This may be from a FrameOBU or a FrameHeaderOBU
    /// directly.
    pub obu_header: ObuHeader,
    /// If set, indicates the frame indexed by frame_to_show_map_idx is to be
    /// output; If not set, indicates that further processing is required.
    pub show_existing_frame: bool,
    /// Specifies the frame to be output. It is only available if
    /// show_existing_frame is set.
    pub frame_to_show_map_idx: u32,
    /// Specifies the length of the frame_presentation_time syntax element, in
    /// bits.
    pub frame_presentation_time: u32,
    /// Provides the frame id number for the frame to output.
    pub display_frame_id: u32,
    /// Specifies the type of the frame
    pub frame_type: FrameType,
    /// If set, specifies that this frame should be immediately output once
    /// decoded. If not set specifies that this frame should not be
    /// immediately output. (It may be output later if a later uncompressed
    /// header uses show_existing_frame is set).
    pub show_frame: bool,
    /// When set, specifies that the frame may be output using the
    /// show_existing_frame mechanism. When not set, specifies that this frame
    /// will not be output using the show_existing_frame mechanism.
    pub showable_frame: bool,
    /// If set, indicates that error resilient mode is enabled;
    /// error_resilient_mode equal to 0 indicates that error resilient mode is
    /// disabled.
    pub error_resilient_mode: bool,
    /// Specifies whether the CDF update in the symbol decoding process should
    /// be disabled.
    pub disable_cdf_update: bool,
    /// When set, indicates that intra blocks may use palette encoding; When not
    /// set, indicates that palette encoding is never used.
    pub allow_screen_content_tools: u32,
    /// If set, specifies that motion vectors will always be integers. If not
    /// set, specifies that motion vectors can contain fractional bits.
    pub force_integer_mv: u32,
    /// Specifies the frame id number for the current frame. Frame id numbers
    /// are additional information that do not affect the decoding process, but
    /// provide decoders with a way of detecting missing reference frames so
    /// that appropriate action can be taken.
    pub current_frame_id: u32,
    /// If not set, specifies that the frame size is equal to the size in the
    /// sequence header. If set, specifies that the frame size will either be
    /// specified as the size of one of the reference frames, or computed from
    /// the frame_width_minus_1 and frame_height_minus_1 syntax elements.
    pub frame_size_override_flag: bool,
    /// Specifies OrderHintBits least significant bits of the expected output
    /// order for this frame.
    pub order_hint: u32,
    /// Specifies which reference frame contains the CDF values and other state
    /// that should be loaded at the start of the frame.
    pub primary_ref_frame: u32,
    /// If set, specifies that buffer_removal_time is present.  If not set,
    /// specifies that buffer_removal_time is not present.
    pub buffer_removal_time_present_flag: bool,
    /// Specifies the frame removal time in units of DecCT clock ticks counted
    /// from the removal time of the last random access point for operating
    /// point opNum. buffer_removal_time is signaled as a fixed length unsigned
    /// integer with a length in bits given by
    /// buffer_removal_time_length_minus_1 + 1.
    pub buffer_removal_time: Vec<u32>,
    /// Contains a bitmask that specifies which reference frame slots will be
    /// updated with the current frame after it is decoded.
    pub refresh_frame_flags: u32,
    /// Specifies the expected output order hint for each reference frame.
    pub ref_order_hint: [u32; NUM_REF_FRAMES],
    /// If set, indicates that intra block copy may be used in this frame. If
    /// not set indicates that intra block copy is not allowed in this frame.
    pub allow_intrabc: bool,
    /// If set, indicates that only two reference frames are explicitly
    /// signaled. If not set, indicates that all reference frames are explicitly
    /// signaled.
    pub frame_refs_short_signaling: bool,
    /// Specifies the reference frame to use for LAST_FRAME.
    pub last_frame_idx: u8,
    /// Specifies the reference frame to use for GOLDEN_FRAME.
    pub gold_frame_idx: u8,
    /// Specifies which reference frames are used by inter frames
    pub ref_frame_idx: [u8; REFS_PER_FRAME],
    /// If not set, specifies that motion vectors are specified to quarter pel
    /// precision; If set, specifies that motion vectors are specified to eighth
    /// pel precision.
    pub allow_high_precision_mv: bool,
    /// If not set, specifies that only the SIMPLE motion mode will be used.
    pub is_motion_mode_switchable: bool,
    /// If set, specifies that motion vector information from a previous frame
    /// can be used when decoding the current frame. If not set, specifies that
    /// this information will not be used.
    pub use_ref_frame_mvs: bool,
    /// If set, indicates that the end of frame CDF update is disabled; If not
    /// set, indicates that the end of frame CDF update is enabled.
    pub disable_frame_end_update_cdf: bool,
    /// If set, indicates that the syntax element motion_mode may be present.
    /// If not set, indicates that the syntax element motion_mode will not be
    /// present
    pub allow_warped_motion: bool,
    /// If set, specifies that the frame is restricted to a reduced subset of
    /// the full set of transform types.
    pub reduced_tx_set: bool,
    /// If not set, means that the render width and height are inferred from the
    /// frame width and height. If set, means that the render width and height
    /// are explicitly coded.
    pub render_and_frame_size_different: bool,
    /// If not set, indicates that no upscaling is needed. If set, indicates
    /// that upscaling is needed.
    pub use_superres: bool,
    /// If set indicates that the filter selection is signaled at the block
    /// level; If not set, indicates that the filter selection is signaled at
    /// the frame level.
    pub is_filter_switchable: bool,
    /// The interpolation filter parameters.
    pub interpolation_filter: InterpolationFilter,
    /// The loop filter parameters.
    pub loop_filter_params: LoopFilterParams,
    /// The quantization parameters.
    pub quantization_params: QuantizationParams,
    /// The segmentation parameters.
    pub segmentation_params: SegmentationParams,
    /// The tile info.
    pub tile_info: TileInfo,
    /// The CDEF parameters.
    pub cdef_params: CdefParams,
    /// The loop restoration parameters.
    pub loop_restoration_params: LoopRestorationParams,
    /// Used to compute TxMode.
    pub tx_mode_select: u32,
    /// If set specifies that the syntax element skip_mode will be present.  If
    /// not set, specifies that skip_mode will not be used for this frame.
    pub skip_mode_present: bool,
    /// If set, specifies that the mode info for inter blocks contains the
    /// syntax element comp_mode that indicates whether to use single or
    /// compound reference prediction. If not set, specifies that all inter
    /// blocks will use single prediction.
    pub reference_select: bool,
    /// The global motion parameters.
    pub global_motion_params: GlobalMotionParams,
    /// The film grain parameters.
    pub film_grain_params: FilmGrainParams,

    /* CamelCase variables */
    pub superres_denom: u32,
    pub frame_is_intra: bool,
    pub order_hints: [u32; NUM_REF_FRAMES],
    pub ref_frame_sign_bias: [bool; NUM_REF_FRAMES],
    pub coded_lossless: bool,
    pub all_lossless: bool,
    pub lossless_array: [bool; MAX_SEGMENTS],
    pub seg_qm_level: [[u32; MAX_SEGMENTS]; 3],
    pub upscaled_width: u32,
    pub frame_width: u32,
    pub frame_height: u32,
    pub render_width: u32,
    pub render_height: u32,
    pub tx_mode: TxMode,
    pub skip_mode_frame: [u32; 2],
    pub mi_cols: u32,
    pub mi_rows: u32,
    pub header_bytes: usize,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct LoopFilterParams {
    /// An array containing loop filter strength values. Different loop filter
    /// strength values from the array are used depending on the image plane
    /// being filtered, and the edge direction (vertical or horizontal) being
    /// filtered.
    pub loop_filter_level: [u8; 4],
    /// Indicates the sharpness level. The loop_filter_level and
    /// loop_filter_sharpness together determine when a block edge is filtered,
    /// and by how much the filtering can change the sample values.
    pub loop_filter_sharpness: u8,
    /// If set, means that the filter level depends on the mode and reference
    /// frame used to predict a block. If not set, means that the filter level
    /// does not depend on the mode and reference frame.
    pub loop_filter_delta_enabled: bool,
    /// If set, means that additional syntax elements are present that specify
    /// which mode and reference frame deltas are to be updated.
    /// loop_filter_delta_update equal to 0 means that these syntax elements are
    /// not present.
    pub loop_filter_delta_update: bool,
    /// Contains the adjustment needed for the filter level based on the chosen
    /// reference frame. If this syntax element is not present, it maintains
    /// its previous value.
    pub loop_filter_ref_deltas: [i8; TOTAL_REFS_PER_FRAME],
    /// Contains the adjustment needed for the filter level based on the chosen
    /// mode. If this syntax element is not present in the, it maintains its
    /// previous value.
    pub loop_filter_mode_deltas: [i8; 2],
    /// Specifies whether loop filter delta values are present.
    pub delta_lf_present: bool,
    /// Specifies the left shift which should be applied to decoded loop filter
    /// delta values.
    pub delta_lf_res: u32,
    /// If set, specifies that separate loop filter deltas are sent for
    /// horizontal luma edges, vertical luma edges, the U edges, and the V
    /// edges. If not set, specifies that the same loop filter delta is used for
    /// all edges.
    pub delta_lf_multi: u32,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct QuantizationParams {
    /// Indicates the base frame qindex. This is used for Y AC coefficients and
    /// as the base value for the other quantizers.
    pub base_q_idx: u32,
    /// Indicates the base frame qindex. This is used for Y AC coefficients and
    /// as the base value for the other quantizers.
    pub diff_uv_delta: bool,
    /// Specifies that the quantizer matrix will be used to compute quantizers.
    pub using_qmatrix: bool,
    /// Specifies the level in the quantizer matrix that should be used for luma
    /// plane decoding.
    pub qm_y: u32,
    /// Specifies the level in the quantizer matrix that should be used for
    /// chroma U plane decoding.
    pub qm_u: u32,
    /// Specifies the level in the quantizer matrix that should be used for
    /// chroma V plane decoding.
    pub qm_v: u32,
    /// Specifies whether quantizer index delta values are present.
    pub delta_q_present: bool,
    /// Specifies the left shift which should be applied to decoded quantizer
    /// index delta values.
    pub delta_q_res: u32,
    /// Same as DeltaQYDc
    pub delta_q_y_dc: i32,
    /// Same as DeltaQUDc
    pub delta_q_u_dc: i32,
    /// Same as DeltaQUAc
    pub delta_q_u_ac: i32,
    /// Same as DeltaQVDc
    pub delta_q_v_dc: i32,
    /// Same as DeltaQVAc
    pub delta_q_v_ac: i32,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct SegmentationParams {
    /// If set, indicates that this frame makes use of the segmentation tool; If
    /// not set, indicates that the frame does not use segmentation.
    pub segmentation_enabled: bool,
    /// If set, indicates that the segmentation map are updated during the
    /// decoding of this frame. If not set, means that the segmentation map from
    /// the previous frame is used.
    pub segmentation_update_map: bool,
    /// If set, indicates that the updates to the segmentation map are coded
    /// relative to the existing segmentation map. If not set, indicates that
    /// the new segmentation map is coded without reference to the existing
    /// segmentation map.
    pub segmentation_temporal_update: bool,
    /// If set, indicates that new parameters are about to be specified for each
    /// segment. If not set, indicates that the segmentation parameters should
    /// keep their existing values.
    pub segmentation_update_data: bool,
    /// If not set, indicates that the corresponding feature is unused and has
    /// value equal to 0. If set, indicates that the feature value is coded.
    pub feature_enabled: [[bool; SEG_LVL_MAX]; MAX_SEGMENTS],
    /// Specifies the feature data for a segment feature.
    pub feature_data: [[i16; SEG_LVL_MAX]; MAX_SEGMENTS],
    /// Same as SegIdPreSkip
    pub seg_id_pre_skip: u32,
    /// Same as LastActiveSegId
    pub last_active_seg_id: u32,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TileInfo {
    /// If set, means that the tiles are uniformly spaced across the frame. (In
    /// other words, all tiles are the same size except for the ones at the
    /// right and bottom edge which can be smaller.) If not set, means that the
    /// tile sizes are coded.
    pub uniform_tile_spacing_flag: bool,
    /// Used to compute TileColsLog2.
    pub increment_tile_rows_log2: u32,
    /// Specifies the width of a tile minus 1 in units of superblocks.
    pub width_in_sbs_minus_1: [u32; MAX_TILE_COLS],
    /// Specifies the height of a tile minus 1 in units of superblocks.
    pub height_in_sbs_minus_1: [u32; MAX_TILE_ROWS],
    /// Specifies which tile to use for the CDF update
    pub context_update_tile_id: u32,
    /// An array specifying the start column (in units of 4x4 luma samples) for
    /// each tile across the image.
    pub mi_col_starts: [u32; MAX_TILE_COLS + 1],
    /// An array specifying the start row (in units of 4x4 luma samples) for
    /// each tile down the image.
    pub mi_row_starts: [u32; MAX_TILE_ROWS + 1],
    /// Specifies the base 2 logarithm of the desired number of tiles down the
    /// frame.
    pub tile_cols_log2: u32,
    /// Specifies the number of tiles across the frame.
    pub tile_cols: u32,
    /// Specifies the base 2 logarithm of the desired number of tiles down the
    /// frame.
    pub tile_rows_log2: u32,
    /// Secifies the number of tiles down the frame
    pub tile_rows: u32,
    /// Specifies the number of bytes needed to code each tile size.
    pub tile_size_bytes: u32,
}

impl Default for TileInfo {
    fn default() -> Self {
        Self {
            uniform_tile_spacing_flag: Default::default(),
            increment_tile_rows_log2: Default::default(),
            width_in_sbs_minus_1: [0; MAX_TILE_COLS],
            height_in_sbs_minus_1: [0; MAX_TILE_ROWS],
            context_update_tile_id: Default::default(),
            mi_col_starts: [0; MAX_TILE_COLS + 1],
            mi_row_starts: [0; MAX_TILE_ROWS + 1],
            tile_cols_log2: Default::default(),
            tile_cols: Default::default(),
            tile_rows_log2: Default::default(),
            tile_rows: Default::default(),
            tile_size_bytes: Default::default(),
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct CdefParams {
    /// Controls the amount of damping in the deringing filter.
    pub cdef_damping: u32,
    /// Specifies the number of bits needed to specify which CDEF filter to
    /// apply.
    pub cdef_bits: u32,
    /// Specify the strength of the primary filter.
    pub cdef_y_pri_strength: [u32; CDEF_MAX],
    /// Specify the strength of the secondary filter.
    pub cdef_y_sec_strength: [u32; CDEF_MAX],
    /// Specify the strength of the primary filter.
    pub cdef_uv_pri_strength: [u32; CDEF_MAX],
    /// Specify the strength of the secondary filter.
    pub cdef_uv_sec_strength: [u32; CDEF_MAX],
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct LoopRestorationParams {
    /// Specifies if the luma restoration size should be halved.
    pub lr_unit_shift: u8,
    /// Only present for 4:2:0 formats and specifies if the chroma size should
    /// be half the luma size.
    pub lr_uv_shift: u8,
    /// Same as FrameRestorationType in the specification.
    pub frame_restoration_type: [FrameRestorationType; MAX_NUM_PLANES],
    /// Same as LoopRestorationSize in the specification.
    pub loop_restoration_size: [u16; MAX_NUM_PLANES],
    /// Same as UsesLr in the specification.
    pub uses_lr: bool,
    /// Same as UsesChromaLr in the specification.
    pub uses_chroma_lr: bool,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct GlobalMotionParams {
    /// Specifies whether global motion parameters are present for a particular
    /// reference frame.
    pub is_global: [bool; NUM_REF_FRAMES],
    /// Specifies whether a particular reference frame uses rotation and zoom
    /// global motion.
    pub is_rot_zoom: [bool; NUM_REF_FRAMES],
    /// Specifies whether a particular reference frame uses translation global
    /// motion.
    pub is_translation: [bool; NUM_REF_FRAMES],
    /// gm_params\[ ref \]\[ j \] is set equal to SavedGmParams\[
    /// frame_to_show_map_idx \]\[ ref \]\[ j \] for ref = LAST_FRAME..ALTREF_FRAME,
    /// for j = 0..5.
    pub gm_params: [[i32; 6]; NUM_REF_FRAMES],
    /// Whether the parameters are valid (see warpValid and section 7.11.3.6)
    pub warp_valid: [bool; NUM_REF_FRAMES],
    /// Same as GmType.
    pub gm_type: [WarpModelType; NUM_REF_FRAMES],
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct FilmGrainParams {
    /// If set, specifies that film grain should be added to this frame. If not
    /// set, specifies that film grain should not be added.
    pub apply_grain: bool,
    /// Specifies the starting value for the pseudo-random numbers used during
    /// film grain synthesis.
    pub grain_seed: u16,
    /// If set means that a new set of parameters should be sent.  If not set,
    /// means that the previous set of parameters should be used.
    pub update_grain: bool,
    /// Indicates which reference frame contains the film grain parameters to be
    /// used for this frame.
    pub film_grain_params_ref_idx: u8,
    /// Specifies the number of points for the piece-wise linear scaling
    /// function of the luma component.
    pub num_y_points: u8,
    /// Represents the x (luma value) coordinate for the i-th point of the
    /// piecewise linear scaling function for luma component. The values are
    /// signaled on the scale of 0..255. (In case of 10 bit video, these values
    /// correspond to luma values divided by 4. In case of 12 bit video, these
    /// values correspond to luma values divided by 16.)
    pub point_y_value: [u8; MAX_NUM_Y_POINTS],
    /// Pepresents the scaling (output) value for the i-th point of the
    /// piecewise linear scaling function for luma component.
    pub point_y_scaling: [u8; MAX_NUM_Y_POINTS],
    /// Specifies that the chroma scaling is inferred from the luma scaling.
    pub chroma_scaling_from_luma: bool,
    /// Specifies the number of points for the piece-wise linear scaling
    /// function of the cb component.
    pub num_cb_points: u8,
    /// Represents the x coordinate for the i-th point of the piece-wise linear
    /// scaling function for cb component. The values are signaled on the scale
    /// of 0..255.
    pub point_cb_value: [u8; MAX_NUM_CB_POINTS],
    /// Represents the scaling (output) value for the i-th point of the
    /// piecewise linear scaling function for cb component.
    pub point_cb_scaling: [u8; MAX_NUM_CB_POINTS],
    /// Specifies represents the number of points for the piece-wise linear
    /// scaling function of the cr component.
    pub num_cr_points: u8,
    /// Represents the x coordinate for the i-th point of the piece-wise linear
    /// scaling function for cr component. The values are signaled on the scale
    /// of 0..255.
    pub point_cr_value: [u8; MAX_NUM_CR_POINTS],
    /// Represents the scaling (output) value for the i-th point of the
    /// piecewise linear scaling function for cr component.
    pub point_cr_scaling: [u8; MAX_NUM_CR_POINTS],
    /// Represents the shift – 8 applied to the values of the chroma component.
    /// The grain_scaling_minus_8 can take values of 0..3 and determines the
    /// range and quantization step of the standard deviation of film grain.
    pub grain_scaling_minus_8: u8,
    /// Specifies the number of auto-regressive coefficients for luma and chroma.
    pub ar_coeff_lag: u32,
    /// Specifies auto-regressive coefficients used for the Y plane.
    pub ar_coeffs_y_plus_128: [u8; MAX_NUM_POS_LUMA],
    /// Specifies auto-regressive coefficients used for the U plane.
    pub ar_coeffs_cb_plus_128: [u8; MAX_NUM_POS_LUMA],
    /// Specifies auto-regressive coefficients used for the V plane.
    pub ar_coeffs_cr_plus_128: [u8; MAX_NUM_POS_LUMA],
    /// Specifies the range of the auto-regressive coefficients. Values of 0, 1,
    /// 2, and 3 correspond to the ranges for auto-regressive coefficients of
    /// [-2, 2), [-1, 1), [-0.5, 0.5) and [-0.25, 0.25) respectively.
    pub ar_coeff_shift_minus_6: u8,
    /// Specifies how much the Gaussian random numbers should be scaled down
    /// during the grain synthesis process.
    pub grain_scale_shift: u8,
    /// Represents a multiplier for the cb component used in derivation of the
    /// input index to the cb component scaling function.
    pub cb_mult: u8,
    /// Represents a multiplier for the average luma component used in
    /// derivation of the input index to the cb component scaling function.
    pub cb_luma_mult: u8,
    /// Represents an offset used in derivation of the input index to the cb
    /// component scaling function.
    pub cb_offset: u16,
    /// Represents a multiplier for the cr component used in derivation of the
    /// input index to the cr component scaling function.
    pub cr_mult: u8,
    /// Represents a multiplier for the average luma component used in
    /// derivation of the input index to the cr component scaling function.
    pub cr_luma_mult: u8,
    /// Represents an offset used in derivation of the input index to the cr
    /// component scaling function.
    pub cr_offset: u16,
    /// If set, indicates that the overlap between film grain blocks shall be
    /// applied. If not set, indicates that the overlap between film grain
    /// blocks shall not be applied.
    pub overlap_flag: bool,
    /// If set, indicates that clipping to the restricted (studio) range shall
    /// be applied to the sample values after adding the film grain (see the
    /// semantics for color_range for an explanation of studio swing).  If not
    /// set, indicates that clipping to the full range shall be applied to the
    /// sample values after adding the film grain.
    pub clip_to_restricted_range: bool,
}

/// Keeps track of the state of the reference frames in the parser. All
/// variables are CamelCase.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
struct ReferenceFrameInfo {
    /// An array which is indexed by a reference picture slot number. A value of
    /// true in the array signifies that the corresponding reference picture
    /// slot is valid for use as a reference picture, while a value of false
    /// signifies that the corresponding reference picture slot is not valid for
    /// use as a reference picture.
    ref_valid: bool,
    /// Specifies the frame id for each reference frame.
    ref_frame_id: u32,
    /// See 7.20 Reference Frame Update Process.
    ref_upscaled_width: u32,
    /// See 7.20 Reference Frame Update Process.
    ref_frame_width: u32,
    /// See 7.20 Reference Frame Update Process.
    ref_frame_height: u32,
    /// See 7.20 Reference Frame Update Process.
    ref_render_width: u32,
    /// See 7.20 Reference Frame Update Process.
    ref_render_height: u32,
    /// See 7.20 Reference Frame Update Process.
    ref_mi_cols: u32,
    /// See 7.20 Reference Frame Update Process.
    ref_mi_rows: u32,
    /// See 7.20 Reference Frame Update Process.
    ref_frame_type: FrameType,
    /// See 7.20 Reference Frame Update Process.
    ref_subsampling_x: bool,
    /// See 7.20 Reference Frame Update Process.
    ref_subsampling_y: bool,
    /// See 7.20 Reference Frame Update Process.
    ref_bit_depth: BitDepth,
    /// See 7.20 Reference Frame Update Process.
    ref_order_hint: u32,
    /// The saved segmentation parameters.
    segmentation_params: SegmentationParams,
    /// The saved global motion parameters.
    global_motion_params: GlobalMotionParams,
    /// The saved loop filter parameters.
    loop_filter_params: LoopFilterParams,
    /// The saved film grain parameters.
    film_grain_params: FilmGrainParams,
    /// The saved tile info parameters.
    tile_info: TileInfo,
    display_frame_id: u32,
    showable_frame: bool,
}

#[derive(Clone, Debug, Default)]
pub struct AnnexBState {
    pub temporal_unit_size: u32,
    pub frame_unit_size: u32,
    pub temporal_unit_consumed: u32,
    pub frame_unit_consumed: u32,
}

#[derive(Clone, Debug)]
enum StreamFormat {
    LowOverhead,
    AnnexB(AnnexBState),
}

#[derive(Debug)]
pub struct Parser {
    stream_format: StreamFormat,
    operating_point: u32,
    /// Same as SeenFrameHeader in the specification
    seen_frame_header: bool,
    /// We keep this to implement frame_header_copy() in the specification.
    last_frame_header: Option<FrameHeaderObu>,
    operating_point_idc: u32,
    should_probe_for_annexb: bool,
    is_first_frame: bool,
    ref_info: [ReferenceFrameInfo; NUM_REF_FRAMES],

    /* CamelCase variables */
    mi_cols: u32,
    mi_rows: u32,
    prev_frame_id: u32,
    current_frame_id: u32,
    mi_col_starts: [u32; MAX_TILE_COLS + 1],
    mi_row_starts: [u32; MAX_TILE_ROWS + 1],
    tile_cols_log2: u32,
    tile_cols: u32,
    tile_rows_log2: u32,
    tile_rows: u32,
    tile_size_bytes: u32,

    /// The last SequenceHeaderObu parsed.
    pub sequence_header: Option<Rc<SequenceHeaderObu>>,
}

impl Parser {
    /// Probes the input data for the Annex B format. Anything other than
    /// Ok(true) refers to data in "low-overhead" format instead, as we are trying to parse
    fn annexb_probe(data: &[u8]) -> anyhow::Result<bool> {
        let mut r = Reader::new(data);
        let mut seen_sequence = false;
        let mut seen_frame = false;

        // Try reading the first TU and frame unit size
        let temporal_unit_size = r.read_leb128()?;
        if temporal_unit_size == 0 {
            return Ok(false);
        }

        let frame_unit_size = r.read_leb128()?;
        if frame_unit_size == 0 || frame_unit_size > temporal_unit_size {
            return Ok(false);
        }

        let obu_length = r.read_leb128()?;
        if obu_length == 0 || obu_length > frame_unit_size {
            return Ok(false);
        }

        // The first OBU in the first frame_unit of each temporal_unit must
        // be a temporal delimiter OBU (and this is the only place temporal
        // delimiter OBUs can appear)
        let header = Self::parse_obu_header(&mut r.clone())?;
        if !matches!(header.obu_type, ObuType::TemporalDelimiter) {
            return Ok(false);
        }

        // Try identifying a sequence and a frame.
        r.skip(u64::from(obu_length) * 8)?;
        let mut num_bytes_read = 0;

        loop {
            let obu_length = r.read_leb128()?;
            let mut obu_reader = r.clone();

            r.skip(u64::from(obu_length) * 8)?;
            num_bytes_read += obu_length;

            if !seen_sequence {
                let header = Self::parse_obu_header(&mut obu_reader)?;
                seen_sequence = matches!(header.obu_type, ObuType::SequenceHeader);
            }

            if !seen_frame {
                let header = Self::parse_obu_header(&mut obu_reader)?;
                seen_frame = matches!(header.obu_type, ObuType::Frame | ObuType::FrameHeader);
            }

            if seen_sequence && seen_frame {
                // OK, enough evidence of Annex B format.
                return Ok(true);
            }

            if num_bytes_read >= frame_unit_size {
                // We read what we've identified as the first frame and yet no
                // sequence and no actual frames were found.
                return Ok(false);
            }
        }
    }

    fn compute_image_size(&mut self, fh: &mut FrameHeaderObu) {
        fh.mi_cols = 2 * ((fh.frame_width + 7) >> 3);
        fh.mi_rows = 2 * ((fh.frame_height + 7) >> 3);
        self.mi_cols = fh.mi_cols;
        self.mi_rows = fh.mi_rows;
    }

    // 5.9.8
    fn parse_superres_params(
        fh: &mut FrameHeaderObu,
        r: &mut Reader,
        seq: &SequenceHeaderObu,
    ) -> anyhow::Result<()> {
        if seq.enable_superres {
            fh.use_superres = r.read_bit()?;
        } else {
            fh.use_superres = false;
        }

        if fh.use_superres {
            fh.superres_denom = r.read_bits(SUPERRES_DENOM_BITS as u8)? + SUPERRES_DENOM_MIN as u32;
        } else {
            fh.superres_denom = SUPERRES_NUM as u32;
        }

        fh.upscaled_width = fh.frame_width;
        fh.frame_width =
            (fh.upscaled_width * SUPERRES_NUM as u32 + (fh.superres_denom / 2)) / fh.superres_denom;

        Ok(())
    }

    // 7.8 verbatim.
    fn set_frame_refs(
        &self,
        fh: &mut FrameHeaderObu,
        ref_order_hint: &[u32; NUM_REF_FRAMES],
    ) -> anyhow::Result<()> {
        let seq = self.sequence()?;
        let mut ref_frame_idx = [-1i32; REFS_PER_FRAME];

        ref_frame_idx[0] = fh.last_frame_idx.into();
        ref_frame_idx[ReferenceFrameType::Golden as usize - ReferenceFrameType::Last as usize] =
            fh.gold_frame_idx.into();

        let mut used_frame = [false; NUM_REF_FRAMES];
        used_frame[fh.last_frame_idx as usize] = true;
        used_frame[fh.gold_frame_idx as usize] = true;

        let cur_frame_hint = 1 << (seq.order_hint_bits - 1);
        let mut shifted_order_hints = [0; NUM_REF_FRAMES];
        for i in 0..NUM_REF_FRAMES {
            shifted_order_hints[i] = cur_frame_hint
                + helpers::get_relative_dist(
                    seq.enable_order_hint,
                    seq.order_hint_bits,
                    ref_order_hint[i].try_into().unwrap(),
                    fh.order_hint.try_into().unwrap(),
                );
        }

        let mut latest_order_hint = shifted_order_hints[fh.last_frame_idx as usize];
        if latest_order_hint >= cur_frame_hint {
            return Err(anyhow!("It is a requirement of bitstream conformance that last_order_hint < cur_frame_hint"));
        }

        let mut earliest_order_hint = shifted_order_hints[fh.gold_frame_idx as usize];
        if earliest_order_hint >= cur_frame_hint {
            return Err(anyhow!("It is a requirement of bitstream conformance that gold_order_hint < cur_frame_hint"));
        }

        let ref_ = helpers::find_latest_backward(
            &shifted_order_hints,
            &used_frame,
            cur_frame_hint,
            &mut latest_order_hint,
        );

        if ref_ >= 0 {
            ref_frame_idx
                [ReferenceFrameType::AltRef as usize - ReferenceFrameType::Last as usize] = ref_;
            used_frame[ref_ as usize] = true;
        }

        let ref_ = helpers::find_earliest_backward(
            &shifted_order_hints,
            &used_frame,
            cur_frame_hint,
            &mut earliest_order_hint,
        );

        if ref_ >= 0 {
            ref_frame_idx
                [ReferenceFrameType::BwdRef as usize - ReferenceFrameType::Last as usize] = ref_;
            used_frame[ref_ as usize] = true;
        }

        let ref_ = helpers::find_earliest_backward(
            &shifted_order_hints,
            &used_frame,
            cur_frame_hint,
            &mut earliest_order_hint,
        );

        if ref_ >= 0 {
            ref_frame_idx
                [ReferenceFrameType::AltRef2 as usize - ReferenceFrameType::Last as usize] = ref_;
            used_frame[ref_ as usize] = true;
        }

        const REF_FRAME_LIST: [usize; 5] = [
            ReferenceFrameType::Last2 as usize - ReferenceFrameType::Last as usize,
            ReferenceFrameType::Last3 as usize - ReferenceFrameType::Last as usize,
            ReferenceFrameType::BwdRef as usize - ReferenceFrameType::Last as usize,
            ReferenceFrameType::AltRef2 as usize - ReferenceFrameType::Last as usize,
            ReferenceFrameType::AltRef as usize - ReferenceFrameType::Last as usize,
        ];

        #[allow(clippy::needless_range_loop)]
        for i in 0..REFS_PER_FRAME - 2 {
            let ref_frame = REF_FRAME_LIST[i];

            if ref_frame_idx[ref_frame] < 0 {
                let ref_ = helpers::find_latest_forward(
                    &shifted_order_hints,
                    &used_frame,
                    cur_frame_hint,
                    &mut latest_order_hint,
                );

                if ref_ >= 0 {
                    ref_frame_idx[ref_frame] = ref_;
                    used_frame[ref_ as usize] = true;
                }
            }
        }

        let mut ref_ = 0;
        earliest_order_hint = shifted_order_hints[0];
        #[allow(clippy::needless_range_loop)]
        for i in 1..NUM_REF_FRAMES {
            let hint = shifted_order_hints[i];
            if hint < earliest_order_hint {
                ref_ = i as u8;
                earliest_order_hint = hint;
            }
        }

        fh.ref_frame_idx
            .iter_mut()
            .zip(ref_frame_idx.iter().copied())
            .for_each(|(dest, src)| *dest = if src < 0 { ref_ } else { src as u8 });

        Ok(())
    }

    // 5.9.5.
    fn parse_frame_size(&mut self, fh: &mut FrameHeaderObu, r: &mut Reader) -> anyhow::Result<()> {
        let seq = self.sequence()?;
        if fh.frame_size_override_flag {
            let n = seq.frame_width_bits_minus_1 + 1;
            fh.frame_width = r.read_bits(n.try_into().unwrap())? + 1;

            let n = seq.frame_height_bits_minus_1 + 1;
            fh.frame_height = r.read_bits(n.try_into().unwrap())? + 1;
        } else {
            fh.frame_width = seq.max_frame_width_minus_1 + 1;
            fh.frame_height = seq.max_frame_height_minus_1 + 1;
        }

        Self::parse_superres_params(fh, r, seq)?;
        self.compute_image_size(fh);

        Ok(())
    }

    fn parse_render_size(fh: &mut FrameHeaderObu, r: &mut Reader) -> anyhow::Result<()> {
        fh.render_and_frame_size_different = r.read_bit()?;
        if fh.render_and_frame_size_different {
            fh.render_width = r.read_bits(16)? + 1;
            fh.render_height = r.read_bits(16)? + 1;
        } else {
            fh.render_width = fh.upscaled_width;
            fh.render_height = fh.frame_height;
        }
        Ok(())
    }

    fn frame_size_with_refs(
        &mut self,
        fh: &mut FrameHeaderObu,
        r: &mut Reader,
    ) -> anyhow::Result<()> {
        let mut found_ref = false;
        let seq = self.sequence()?;

        for i in 0..REFS_PER_FRAME {
            found_ref = r.read_bit()?;

            if found_ref {
                let rf = &self.ref_info[i];
                fh.upscaled_width = rf.ref_upscaled_width;
                fh.frame_width = fh.upscaled_width;
                fh.frame_height = rf.ref_frame_height;
                fh.render_width = rf.ref_render_width;
                fh.render_height = rf.ref_render_height;
                break;
            }
        }

        if !found_ref {
            self.parse_frame_size(fh, r)?;
            Self::parse_render_size(fh, r)?;
        } else {
            Self::parse_superres_params(fh, r, seq)?;
            self.compute_image_size(fh);
        }

        Ok(())
    }

    /// Skip the padding bits, ensuring that they actually make sense.
    fn skip_and_check_trailing_bits(r: &mut Reader, obu: &Obu) -> anyhow::Result<()> {
        // We can't have that in parse_obu as per the spec, because the reader
        // is not initialized on our design at that point, so move the check to
        // inside this function.
        if obu.size == 0
            || matches!(
                obu.header.obu_type,
                ObuType::TileList | ObuType::TileGroup | ObuType::Frame
            )
        {
            return Ok(());
        }
        let num_trailing = obu.as_ref().len() as u64 * 8 - r.position();
        r.read_trailing_bits(num_trailing)?;
        Ok(())
    }

    fn parse_obu_header(r: &mut Reader) -> anyhow::Result<ObuHeader> {
        let _obu_forbidden_bit = r.read_bit()?;

        let mut header = ObuHeader {
            obu_type: ObuType::n(r.read_bits(4)?).ok_or(anyhow!("Invalid OBU type"))?,
            extension_flag: r.read_bit()?,
            has_size_field: r.read_bit()?,
            temporal_id: Default::default(),
            spatial_id: Default::default(),
        };

        let obu_reserved_1bit = r.read_bit()?;
        assert!(!obu_reserved_1bit); // Must be set to zero as per spec.

        if header.extension_flag {
            header.temporal_id = r.read_bits(3)?;
            header.spatial_id = r.read_bits(2)?;
            let _ = r.read_bits(3)?;
        }

        Ok(header)
    }

    /// Parses one OBU from `data`, which can be in Annex B or low-overhead
    /// format.
    ///
    /// `None` may eventually be returned if the OBU is to be dropped.
    pub fn parse_obu<'a>(&mut self, data: &'a [u8]) -> anyhow::Result<ParsedObu<'a>> {
        if data.is_empty() {
            return Err(anyhow!("Empty data"));
        }

        let mut reader = Reader::new(data);

        if self.should_probe_for_annexb {
            // Try probing for Annex B data.
            self.stream_format = if matches!(Self::annexb_probe(data), Ok(true)) {
                log::debug!("Parsing an Annex B stream");
                StreamFormat::AnnexB(AnnexBState::default())
            } else {
                log::debug!("Parsing a low-overhead stream");
                StreamFormat::LowOverhead
            };

            self.should_probe_for_annexb = false;
        }

        let obu_length = if let StreamFormat::AnnexB(annexb_state) = &mut self.stream_format {
            // Read the length to skip to the start of the open_bitstream_unit()
            // syntax element.
            let obu_length = reader.current_annexb_obu_length(annexb_state)?;
            match obu_length {
                Some(length) => length,
                None => return Ok(ParsedObu::Drop(reader.consumed(0))),
            }
        } else {
            0
        };

        let start_pos = reader.consumed(0);

        // Both "low-overhead" and Annex B are now at the same point, i.e.: a
        // open_bitstream_unit() follows.
        let header = Self::parse_obu_header(&mut reader)?;
        if matches!(self.stream_format, StreamFormat::LowOverhead) {
            assert!(header.has_size_field);
        }

        let obu_size = if header.has_size_field {
            reader.read_leb128()? as usize
        } else {
            /* trap any bugs when computing the final length */
            obu_length
                .checked_sub(1)
                .unwrap()
                .checked_sub(usize::from(header.extension_flag))
                .unwrap()
        };

        let consumed = reader.consumed(start_pos);

        if let StreamFormat::AnnexB(annexb_state) = &mut self.stream_format {
            annexb_state.temporal_unit_consumed += consumed;
            annexb_state.frame_unit_consumed += consumed;

            annexb_state.temporal_unit_consumed += u32::try_from(obu_size).unwrap();
            annexb_state.frame_unit_consumed += u32::try_from(obu_size).unwrap();
        }

        assert!(reader.position() % 8 == 0);
        let start_offset: usize = (reader.position() / 8).try_into().unwrap();

        log::debug!(
            "Identified OBU type {:?}, data size: {}, obu_size: {}",
            header.obu_type,
            start_offset + obu_size,
            obu_size
        );

        if header.obu_type != ObuType::SequenceHeader
            && header.obu_type != ObuType::TemporalDelimiter
            && self.operating_point_idc != 0
            && header.extension_flag
        {
            let in_temporal_layer = ((self.operating_point_idc >> header.temporal_id) & 1) != 0;
            let in_spatial_layer = ((self.operating_point_idc >> (header.spatial_id + 8)) & 1) != 0;
            if !in_temporal_layer || !in_spatial_layer {
                log::debug!("Dropping obu as per drop_obu() in the specification",);
                return Ok(ParsedObu::Drop(reader.consumed(0)));
            }
        }

        Ok(ParsedObu::Process(Obu {
            header,
            data: Cow::from(&data[..start_offset + obu_size]),
            start_offset,
            size: obu_size,
        }))
    }

    fn parse_color_config(s: &mut SequenceHeaderObu, r: &mut Reader) -> anyhow::Result<()> {
        let cc = &mut s.color_config;

        cc.high_bitdepth = r.read_bit()?;
        if s.seq_profile as u32 == 2 && cc.high_bitdepth {
            cc.twelve_bit = r.read_bit()?;
            if cc.twelve_bit {
                s.bit_depth = BitDepth::Depth12;
            } else {
                s.bit_depth = BitDepth::Depth10;
            }
        } else if s.seq_profile as u32 <= 2 {
            s.bit_depth = if cc.high_bitdepth {
                BitDepth::Depth10
            } else {
                BitDepth::Depth8
            };
        }

        if s.seq_profile as u32 == 1 {
            cc.mono_chrome = false;
        } else {
            cc.mono_chrome = r.read_bit()?;
        }

        if cc.mono_chrome {
            s.num_planes = 1;
        } else {
            s.num_planes = 3;
        }

        cc.color_description_present_flag = r.read_bit()?;
        if cc.color_description_present_flag {
            cc.color_primaries =
                ColorPrimaries::n(r.read_bits(8)?).ok_or(anyhow!("Invalid color_primaries"))?;
            cc.transfer_characteristics = TransferCharacteristics::n(r.read_bits(8)?)
                .ok_or(anyhow!("Invalid transfer_characteristics"))?;
            cc.matrix_coefficients = MatrixCoefficients::n(r.read_bits(8)?)
                .ok_or(anyhow!("Invalid matrix_coefficients"))?;
        } else {
            cc.color_primaries = ColorPrimaries::Unspecified;
            cc.transfer_characteristics = TransferCharacteristics::Unspecified;
            cc.matrix_coefficients = MatrixCoefficients::Unspecified;
        }

        if cc.mono_chrome {
            cc.color_range = r.read_bit()?;
            cc.subsampling_x = true;
            cc.subsampling_y = true;
            cc.chroma_sample_position = ChromaSamplePosition::Unknown;
            cc.separate_uv_delta_q = false;
            return Ok(());
        } else if matches!(cc.color_primaries, ColorPrimaries::Bt709)
            && matches!(cc.transfer_characteristics, TransferCharacteristics::Srgb)
            && matches!(cc.matrix_coefficients, MatrixCoefficients::Identity)
        {
            cc.color_range = true;
            cc.subsampling_x = false;
            cc.subsampling_y = false;
        } else {
            cc.color_range = r.read_bit()?;
            if s.seq_profile as u32 == 0 {
                cc.subsampling_x = true;
                cc.subsampling_y = true;
            } else if s.seq_profile as u32 == 1 {
                cc.subsampling_x = false;
                cc.subsampling_y = false;
            } else if matches!(s.bit_depth, BitDepth::Depth12) {
                cc.subsampling_x = r.read_bit()?;
                if cc.subsampling_x {
                    cc.subsampling_y = r.read_bit()?;
                } else {
                    cc.subsampling_y = false;
                }
            } else {
                cc.subsampling_x = true;
                cc.subsampling_y = false;
            }

            if cc.subsampling_x && cc.subsampling_y {
                cc.chroma_sample_position = ChromaSamplePosition::n(r.read_bits(2)?)
                    .ok_or(anyhow!("Invalid chroma_sample_position"))?;
            }
        }

        cc.separate_uv_delta_q = r.read_bit()?;

        Ok(())
    }

    fn parse_operating_parameters_info(
        opi: &mut OperatingPoint,
        r: &mut Reader,
        buffer_delay_length_minus_1: u32,
    ) -> anyhow::Result<()> {
        let n = u8::try_from(buffer_delay_length_minus_1 + 1).unwrap();
        opi.decoder_buffer_delay = r.read_bits(n)?;
        opi.encoder_buffer_delay = r.read_bits(n)?;
        opi.low_delay_mode_flag = r.read_bit()?;
        Ok(())
    }

    fn parse_decoder_model_info(dmi: &mut DecoderModelInfo, r: &mut Reader) -> anyhow::Result<()> {
        dmi.buffer_delay_length_minus_1 = r.read_bits(5)?;
        dmi.num_units_in_decoding_tick = r.read_bits(32)?;
        dmi.buffer_removal_time_length_minus_1 = r.read_bits(5)?;
        dmi.frame_presentation_time_length_minus_1 = r.read_bits(5)?;
        Ok(())
    }

    fn parse_timing_info(ti: &mut TimingInfo, r: &mut Reader) -> anyhow::Result<()> {
        ti.num_units_in_display_tick = r.read_bits(32)?;
        ti.time_scale = r.read_bits(32)?;
        ti.equal_picture_interval = r.read_bit()?;
        if ti.equal_picture_interval {
            ti.num_ticks_per_picture_minus_1 = r.read_uvlc()?;
        }
        Ok(())
    }

    /// Selects an operating point. Only call this after the Sequence OBU for
    /// which the operating point should apply has been parsed.
    pub fn choose_operating_point(&mut self, operating_point: u32) -> anyhow::Result<()> {
        if operating_point > self.sequence()?.operating_points_cnt_minus_1 {
            return Err(anyhow!(
                "Invalid operating point {} (max {})",
                operating_point,
                self.sequence()?.operating_points_cnt_minus_1
            ));
        }
        self.operating_point = operating_point;
        self.operating_point_idc = self.sequence()?.operating_points[operating_point as usize].idc;
        Ok(())
    }

    pub fn parse_temporal_delimiter_obu(&mut self, obu: &Obu) -> anyhow::Result<()> {
        if !matches!(obu.header.obu_type, ObuType::TemporalDelimiter) {
            return Err(anyhow!(
                "Expected a TemporalDelimiterOBU, got {:?}",
                obu.header.obu_type
            ));
        }

        self.seen_frame_header = false;
        Ok(())
    }

    pub fn parse_sequence_header_obu(
        &mut self,
        obu: &Obu,
    ) -> anyhow::Result<Rc<SequenceHeaderObu>> {
        if !matches!(obu.header.obu_type, ObuType::SequenceHeader) {
            return Err(anyhow!(
                "Expected a SequenceHeaderOBU, got {:?}",
                obu.header.obu_type
            ));
        }

        let mut s = SequenceHeaderObu {
            obu_header: obu.header.clone(),
            ..Default::default()
        };

        let mut r = Reader::new(obu.as_ref());
        let profile = r.read_bits(3)?;

        s.seq_profile = Profile::n(profile).ok_or(anyhow!("Invalid profile {}", profile))?;
        s.still_picture = r.read_bit()?;
        s.reduced_still_picture_header = r.read_bit()?;

        if s.reduced_still_picture_header {
            /* Default::default() already ensures a lot of this, but lets go verbatim */
            s.timing_info_present_flag = false;
            s.decoder_model_info_present_flag = false;
            s.initial_display_delay_present_flag = false;
            s.operating_points_cnt_minus_1 = 0;
            s.operating_points[0].idc = 0;
            s.operating_points[0].seq_level_idx = r.read_bits(5)?;
            s.operating_points[0].seq_tier = 0;
            s.operating_points[0].decoder_model_present_for_this_op = false;
            s.operating_points[0].initial_display_delay_present_for_this_op = false;
        } else {
            s.timing_info_present_flag = r.read_bit()?;
            if s.timing_info_present_flag {
                Self::parse_timing_info(&mut s.timing_info, &mut r)?;
                s.decoder_model_info_present_flag = r.read_bit()?;
                if s.decoder_model_info_present_flag {
                    Self::parse_decoder_model_info(&mut s.decoder_model_info, &mut r)?;
                }
            } else {
                s.decoder_model_info_present_flag = false;
            }

            s.initial_display_delay_present_flag = r.read_bit()?;
            s.operating_points_cnt_minus_1 = r.read_bits(5)?;
            if s.operating_points_cnt_minus_1 > MAX_NUM_OPERATING_POINTS as u32 {
                return Err(anyhow!(
                    "Invalid operating_points_cnt_minus_1 {}",
                    s.operating_points_cnt_minus_1
                ));
            }

            for i in 0..=s.operating_points_cnt_minus_1 as usize {
                s.operating_points[i].idc = r.read_bits(12)?;
                s.operating_points[i].seq_level_idx = r.read_bits(5)?;
                if s.operating_points[i].seq_level_idx > 7 {
                    s.operating_points[i].seq_tier = u32::from(r.read_bit()?);
                } else {
                    s.operating_points[i].seq_tier = 0;
                }
                if s.decoder_model_info_present_flag {
                    s.operating_points[i].decoder_model_present_for_this_op = r.read_bit()?;
                    if s.operating_points[i].decoder_model_present_for_this_op {
                        let buffer_delay_length_minus_1 =
                            s.decoder_model_info.buffer_delay_length_minus_1;
                        Self::parse_operating_parameters_info(
                            &mut s.operating_points[i],
                            &mut r,
                            buffer_delay_length_minus_1,
                        )?;
                    }
                } else {
                    s.operating_points[i].decoder_model_present_for_this_op = false;
                }

                if s.initial_display_delay_present_flag {
                    s.operating_points[i].initial_display_delay_present_for_this_op =
                        r.read_bit()?;
                    if s.operating_points[i].initial_display_delay_present_for_this_op {
                        s.operating_points[i].initial_display_delay_minus_1 = r.read_bits(4)?;
                    }
                }
            }
        }

        s.frame_width_bits_minus_1 = r.read_bits(4)?;
        s.frame_height_bits_minus_1 = r.read_bits(4)?;
        s.max_frame_width_minus_1 =
            r.read_bits(u8::try_from(s.frame_width_bits_minus_1).unwrap() + 1)?;
        s.max_frame_height_minus_1 =
            r.read_bits(u8::try_from(s.frame_height_bits_minus_1).unwrap() + 1)?;
        if s.reduced_still_picture_header {
            s.frame_id_numbers_present_flag = false;
        } else {
            s.frame_id_numbers_present_flag = r.read_bit()?;
        }
        if s.frame_id_numbers_present_flag {
            s.delta_frame_id_length_minus_2 = r.read_bits(4)?;
            s.additional_frame_id_length_minus_1 = r.read_bits(3)?;
            let frame_id_length =
                s.additional_frame_id_length_minus_1 + s.delta_frame_id_length_minus_2 + 3;
            if frame_id_length > 16 {
                return Err(anyhow!("Invalid frame_id_length {}", frame_id_length));
            }
        }

        s.use_128x128_superblock = r.read_bit()?;
        s.enable_filter_intra = r.read_bit()?;
        s.enable_intra_edge_filter = r.read_bit()?;
        if s.reduced_still_picture_header {
            s.enable_interintra_compound = false;
            s.enable_masked_compound = false;
            s.enable_warped_motion = false;
            s.enable_dual_filter = false;
            s.enable_order_hint = false;
            s.enable_jnt_comp = false;
            s.enable_ref_frame_mvs = false;
            s.seq_force_screen_content_tools = SELECT_SCREEN_CONTENT_TOOLS as _;
            s.seq_force_integer_mv = SELECT_INTEGER_MV as _;
            s.order_hint_bits = 0;
            s.order_hint_bits_minus_1 = -1;
        } else {
            s.enable_interintra_compound = r.read_bit()?;
            s.enable_masked_compound = r.read_bit()?;
            s.enable_warped_motion = r.read_bit()?;
            s.enable_dual_filter = r.read_bit()?;
            s.enable_order_hint = r.read_bit()?;
            if s.enable_order_hint {
                s.enable_jnt_comp = r.read_bit()?;
                s.enable_ref_frame_mvs = r.read_bit()?;
            } else {
                s.enable_jnt_comp = false;
                s.enable_ref_frame_mvs = false;
            }
            s.seq_choose_screen_content_tools = r.read_bit()?;
            if s.seq_choose_screen_content_tools {
                s.seq_force_screen_content_tools = SELECT_SCREEN_CONTENT_TOOLS as _;
            } else {
                s.seq_force_screen_content_tools = r.read_bit()? as _;
            }
            if s.seq_force_screen_content_tools > 0 {
                s.seq_choose_integer_mv = r.read_bit()?;
                if s.seq_choose_integer_mv {
                    s.seq_force_integer_mv = SELECT_INTEGER_MV as _;
                } else {
                    s.seq_force_integer_mv = r.read_bit()? as _;
                }
            } else {
                s.seq_force_integer_mv = SELECT_INTEGER_MV as _;
            }

            if s.enable_order_hint {
                s.order_hint_bits_minus_1 = r.read_bits(3)?.try_into().unwrap();
                s.order_hint_bits = s.order_hint_bits_minus_1 + 1;
            } else {
                s.order_hint_bits_minus_1 = -1;
                s.order_hint_bits = 0;
            }
        }

        s.enable_superres = r.read_bit()?;
        s.enable_cdef = r.read_bit()?;
        s.enable_restoration = r.read_bit()?;

        Self::parse_color_config(&mut s, &mut r)?;

        s.film_grain_params_present = r.read_bit()?;

        Self::skip_and_check_trailing_bits(&mut r, obu)?;
        let rc = Rc::new(s);
        self.sequence_header = Some(rc.clone());

        /* Client is supposed to set the operating point through external means,
         * here we just set 0 as default. */
        self.choose_operating_point(0)?;

        Ok(rc)
    }

    /// Implements 7.21. Note that 7.20 will use the information from the
    /// header, so we must save them now, as they will not be parsed from the
    /// bitstream. We also save some internal parser state which will be useful
    /// later.
    fn load_reference_frame(&mut self, fh: &mut FrameHeaderObu) -> anyhow::Result<()> {
        let idx = usize::try_from(fh.frame_to_show_map_idx).unwrap();
        let rf = &self.ref_info[idx];

        // Section 6.8.1: It is a requirement of bitstream conformance that a
        // sequence header OBU has been received before a frame header OBU.
        let seq = self.sequence()?;

        /* at least save the sizes and for both kf and non-kf */
        fh.frame_type = rf.ref_frame_type;
        fh.upscaled_width = rf.ref_upscaled_width;
        fh.frame_width = rf.ref_frame_width;
        fh.frame_height = rf.ref_frame_height;
        fh.render_width = rf.ref_render_width;
        fh.render_height = rf.ref_render_height;

        /* Save into the frame header */
        if fh.frame_type == FrameType::KeyFrame {
            fh.current_frame_id = rf.ref_frame_id;
            /* We don't keep track of sequence information at the frame level */
            fh.mi_cols = rf.ref_mi_cols;
            fh.mi_rows = rf.ref_mi_rows;
            /* The accelerator is keeping track of CDF values, so that is skipped too */
            fh.global_motion_params = rf.global_motion_params.clone();

            if seq.film_grain_params_present {
                fh.film_grain_params = rf.film_grain_params.clone();
            }
            fh.loop_filter_params = rf.loop_filter_params.clone();
            fh.segmentation_params = rf.segmentation_params.clone();
        }

        Ok(())
    }

    fn setup_past_independence(fh: &mut FrameHeaderObu) {
        fh.segmentation_params.feature_enabled = Default::default();
        fh.segmentation_params.feature_data = Default::default();

        for i in ReferenceFrameType::Last as usize..ReferenceFrameType::AltRef as usize {
            fh.global_motion_params.gm_type[i] = WarpModelType::Identity;
        }

        fh.loop_filter_params.loop_filter_delta_enabled = true;
        fh.loop_filter_params.loop_filter_ref_deltas = [1, 0, 0, 0, -1, 0, -1, -1];
        fh.loop_filter_params.loop_filter_mode_deltas = Default::default();
    }

    fn parse_tile_info(&mut self, r: &mut Reader, ti: &mut TileInfo) -> anyhow::Result<()> {
        let seq = self.sequence()?;

        let sb_cols = if seq.use_128x128_superblock {
            (self.mi_cols + 31) >> 5
        } else {
            (self.mi_cols + 15) >> 4
        };

        let sb_rows = if seq.use_128x128_superblock {
            (self.mi_rows + 31) >> 5
        } else {
            (self.mi_rows + 15) >> 4
        };

        let sb_shift = if seq.use_128x128_superblock { 5 } else { 4 };
        let sb_size = sb_shift + 2;

        let max_tile_width_sb = MAX_TILE_WIDTH >> sb_size;
        let mut max_tile_area_sb = MAX_TILE_AREA >> (2 * sb_size);

        let min_log2_tile_cols = helpers::tile_log2(max_tile_width_sb, sb_cols);

        let max_log2_tile_cols =
            helpers::tile_log2(1, std::cmp::min(sb_cols, MAX_TILE_COLS as u32));

        let max_log2_tile_rows =
            helpers::tile_log2(1, std::cmp::min(sb_rows, MAX_TILE_ROWS as u32));

        let min_log2_tiles = std::cmp::max(
            min_log2_tile_cols,
            helpers::tile_log2(max_tile_area_sb, sb_rows * sb_cols),
        );

        ti.uniform_tile_spacing_flag = r.read_bit()?;

        if ti.uniform_tile_spacing_flag {
            self.tile_cols_log2 = min_log2_tile_cols;
            while self.tile_cols_log2 < max_log2_tile_cols {
                let increment_tile_cols_log_2 = r.read_bit()?;
                if increment_tile_cols_log_2 {
                    self.tile_cols_log2 += 1;
                } else {
                    break;
                }
            }

            let tile_width_sb = (sb_cols + (1 << self.tile_cols_log2) - 1) >> self.tile_cols_log2;

            let mut i = 0;
            let mut start_sb = 0;

            while start_sb < sb_cols {
                self.mi_col_starts[i] = start_sb << sb_shift;
                i += 1;
                start_sb += tile_width_sb;
            }

            self.mi_col_starts[i] = self.mi_cols;
            self.tile_cols = i as _;

            if self.tile_cols > MAX_TILE_COLS as u32 {
                return Err(anyhow!("Invalid tile_cols {}", self.tile_cols));
            }

            /* compute this anyways */
            while i >= 1 {
                ti.width_in_sbs_minus_1[i - 1] =
                    ((self.mi_col_starts[i] - self.mi_col_starts[i - 1] + ((1 << sb_shift) - 1))
                        >> sb_shift)
                        - 1;
                i -= 1;
            }

            let min_log2_tile_rows =
                std::cmp::max(min_log2_tiles.saturating_sub(self.tile_cols_log2), 0);
            self.tile_rows_log2 = min_log2_tile_rows;

            while self.tile_rows_log2 < max_log2_tile_rows {
                let increment_tile_rows_log_2 = r.read_bit()?;

                if increment_tile_rows_log_2 {
                    self.tile_rows_log2 += 1;
                } else {
                    break;
                }
            }

            let tile_height_sb = (sb_rows + (1 << self.tile_rows_log2) - 1) >> self.tile_rows_log2;

            let mut i = 0;
            let mut start_sb = 0;

            while start_sb < sb_rows {
                self.mi_row_starts[i] = start_sb << sb_shift;
                i += 1;
                start_sb += tile_height_sb;
            }

            self.mi_row_starts[i] = self.mi_rows;
            self.tile_rows = i as _;

            if self.tile_rows > MAX_TILE_ROWS as u32 {
                return Err(anyhow!("Invalid tile_rows {}", self.tile_cols));
            }

            /* compute this anyways */
            while i >= 1 {
                ti.height_in_sbs_minus_1[i - 1] =
                    ((self.mi_row_starts[i] - self.mi_row_starts[i - 1] + ((1 << sb_shift) - 1))
                        >> sb_shift)
                        - 1;
                i -= 1;
            }
        } else {
            let mut widest_tile_sb = 0;
            let mut start_sb = 0;
            let mut i = 0;

            while start_sb < sb_cols {
                self.mi_col_starts[i] = start_sb << sb_shift;

                let max_width = std::cmp::min(sb_cols - start_sb, max_tile_width_sb);
                ti.width_in_sbs_minus_1[i] = r.read_ns(max_width.try_into().unwrap())?;

                let size_sb = ti.width_in_sbs_minus_1[i] + 1;
                widest_tile_sb = std::cmp::max(size_sb, widest_tile_sb);

                start_sb += size_sb;
                i += 1;
            }

            self.mi_col_starts[i] = self.mi_cols;
            self.tile_cols = i as _;
            self.tile_cols_log2 = helpers::tile_log2(1, self.tile_cols);

            if min_log2_tiles > 0 {
                max_tile_area_sb = (sb_rows * sb_cols) >> (min_log2_tiles + 1);
            } else {
                max_tile_area_sb = sb_rows * sb_cols;
            }

            let max_tile_height_sb = std::cmp::max(max_tile_area_sb / widest_tile_sb, 1);
            let mut start_sb = 0;
            let mut i = 0;
            while start_sb < sb_rows {
                self.mi_row_starts[i] = start_sb << sb_shift;
                let max_height = std::cmp::min(sb_rows - start_sb, max_tile_height_sb);
                ti.height_in_sbs_minus_1[i] = r.read_ns(max_height.try_into().unwrap())?;

                let size_sb = ti.height_in_sbs_minus_1[i] + 1;
                start_sb += size_sb;
                i += 1;
            }

            self.mi_row_starts[i] = self.mi_rows;
            self.tile_rows = i as _;
            self.tile_rows_log2 = helpers::tile_log2(1, self.tile_rows);
        }

        if self.tile_cols_log2 > 0 || self.tile_rows_log2 > 0 {
            let num_bits = (self.tile_rows_log2 + self.tile_cols_log2)
                .try_into()
                .unwrap();
            ti.context_update_tile_id = r.read_bits(num_bits)?;

            if ti.context_update_tile_id >= self.tile_rows * self.tile_cols {
                return Err(anyhow!(
                    "Invalid context_update_tile_id {}",
                    ti.context_update_tile_id
                ));
            }
            self.tile_size_bytes = r.read_bits(2)? + 1;
        } else {
            ti.context_update_tile_id = 0;
        }

        ti.mi_col_starts = self.mi_col_starts;
        ti.mi_row_starts = self.mi_row_starts;
        ti.tile_cols_log2 = self.tile_cols_log2;
        ti.tile_cols = self.tile_cols;
        ti.tile_rows_log2 = self.tile_rows_log2;
        ti.tile_rows = self.tile_rows;
        ti.tile_size_bytes = self.tile_size_bytes;

        Ok(())
    }

    fn parse_quantization_params(
        r: &mut Reader,
        q: &mut QuantizationParams,
        num_planes: u32,
        separate_uv_delta_q: bool,
    ) -> anyhow::Result<()> {
        q.base_q_idx = r.read_bits(8)?;
        q.delta_q_y_dc = r.read_delta_q()?;
        if num_planes > 1 {
            if separate_uv_delta_q {
                q.diff_uv_delta = r.read_bit()?;
            } else {
                q.diff_uv_delta = false;
            }

            q.delta_q_u_dc = r.read_delta_q()?;
            q.delta_q_u_ac = r.read_delta_q()?;
            if q.diff_uv_delta {
                q.delta_q_v_dc = r.read_delta_q()?;
                q.delta_q_v_ac = r.read_delta_q()?;
            } else {
                q.delta_q_v_dc = q.delta_q_u_dc;
                q.delta_q_v_ac = q.delta_q_u_ac;
            }
        } else {
            q.delta_q_u_dc = 0;
            q.delta_q_u_ac = 0;
            q.delta_q_v_dc = 0;
            q.delta_q_v_ac = 0;
        }

        q.using_qmatrix = r.read_bit()?;
        if q.using_qmatrix {
            q.qm_y = r.read_bits(4)?;
            q.qm_u = r.read_bits(4)?;
            if !separate_uv_delta_q {
                q.qm_v = q.qm_u;
            } else {
                q.qm_v = r.read_bits(4)?;
            }
        }
        Ok(())
    }

    fn parse_delta_q_params(r: &mut Reader, q: &mut QuantizationParams) -> anyhow::Result<()> {
        q.delta_q_res = 0;
        q.delta_q_present = false;
        if q.base_q_idx > 0 {
            q.delta_q_present = r.read_bit()?;
        }
        if q.delta_q_present {
            q.delta_q_res = r.read_bits(2)?;
        }

        Ok(())
    }

    fn parse_delta_lf_params(
        r: &mut Reader,
        lf: &mut LoopFilterParams,
        delta_q_present: bool,
        allow_intrabc: bool,
    ) -> anyhow::Result<()> {
        lf.delta_lf_present = false;
        lf.delta_lf_res = 0;
        lf.delta_lf_multi = 0;
        if delta_q_present {
            if !allow_intrabc {
                lf.delta_lf_present = r.read_bit()?;
            }
            if lf.delta_lf_present {
                lf.delta_lf_res = r.read_bits(2)?;
                lf.delta_lf_multi = r.read_bits(1)?;
            }
        }
        Ok(())
    }

    fn parse_segmentation_params(
        &self,
        r: &mut Reader,
        fh: &mut FrameHeaderObu,
    ) -> anyhow::Result<()> {
        let s = &mut fh.segmentation_params;
        s.segmentation_enabled = r.read_bit()?;
        if s.segmentation_enabled {
            if fh.primary_ref_frame == PRIMARY_REF_NONE {
                s.segmentation_update_map = true;
                s.segmentation_temporal_update = false;
                s.segmentation_update_data = true;
            } else {
                s.segmentation_update_map = r.read_bit()?;
                if s.segmentation_update_map {
                    s.segmentation_temporal_update = r.read_bit()?;
                }
                s.segmentation_update_data = r.read_bit()?;
            }
            if s.segmentation_update_data {
                for i in 0..MAX_SEGMENTS {
                    for j in 0..SEG_LVL_MAX {
                        let feature_enabled = r.read_bit()?;
                        s.feature_enabled[i][j] = feature_enabled;
                        if feature_enabled {
                            let bits_to_read = FEATURE_BITS[j];
                            let limit = FEATURE_MAX[j];
                            let signed = FEATURE_SIGNED[j];

                            if signed {
                                let feature_value = r.read_su(1 + bits_to_read)?;
                                let clipped_value = helpers::clip3(-limit, limit, feature_value);
                                s.feature_data[i][j] = clipped_value as _;
                            } else {
                                let feature_value = r.read_bits(bits_to_read)?;
                                let clipped_value = helpers::clip3(
                                    0,
                                    limit,
                                    feature_value.try_into().context("Invalid feature_value")?,
                                );
                                s.feature_data[i][j] = clipped_value as _;
                            }
                        }
                    }
                }
            } else {
                /* copy from prev_frame */
                let prev_frame =
                    &self.ref_info[fh.ref_frame_idx[fh.primary_ref_frame as usize] as usize];

                if !prev_frame.ref_valid {
                    return Err(anyhow!("Reference is invalid"));
                }

                s.feature_enabled = prev_frame.segmentation_params.feature_enabled;
                s.feature_data = prev_frame.segmentation_params.feature_data;
            }
        } else {
            for i in 0..MAX_SEGMENTS {
                for j in 0..SEG_LVL_MAX {
                    s.feature_enabled[i][j] = false;
                    s.feature_data[i][j] = 0;
                }
            }
        }

        s.seg_id_pre_skip = 0;
        s.last_active_seg_id = 0;
        for i in 0..MAX_SEGMENTS {
            for j in 0..SEG_LVL_MAX {
                if s.feature_enabled[i][j] {
                    s.last_active_seg_id = i as _;
                    if j >= SEG_LVL_REF_FRAME {
                        s.seg_id_pre_skip = 1;
                    }
                }
            }
        }

        Ok(())
    }

    fn parse_loop_filter_parameters(
        r: &mut Reader,
        fh: &mut FrameHeaderObu,
        num_planes: u32,
    ) -> anyhow::Result<()> {
        let lf = &mut fh.loop_filter_params;
        if fh.coded_lossless || fh.allow_intrabc {
            lf.loop_filter_level[0] = 0;
            lf.loop_filter_level[1] = 0;
            lf.loop_filter_ref_deltas = [1, 0, 0, 0, -1, 0, -1, -1];

            lf.loop_filter_mode_deltas = Default::default();

            return Ok(());
        }

        lf.loop_filter_level[0] = r.read_bits(6)? as u8;
        lf.loop_filter_level[1] = r.read_bits(6)? as u8;
        if num_planes > 1 && (lf.loop_filter_level[0] > 0 || lf.loop_filter_level[1] > 0) {
            lf.loop_filter_level[2] = r.read_bits(6)? as u8;
            lf.loop_filter_level[3] = r.read_bits(6)? as u8;
        }

        lf.loop_filter_sharpness = r.read_bits(3)? as u8;
        lf.loop_filter_delta_enabled = r.read_bit()?;
        if lf.loop_filter_delta_enabled {
            lf.loop_filter_delta_update = r.read_bit()?;
            if lf.loop_filter_delta_update {
                for i in 0..TOTAL_REFS_PER_FRAME {
                    let update_ref_delta = r.read_bit()?;
                    if update_ref_delta {
                        lf.loop_filter_ref_deltas[i] = r.read_su(7)? as i8;
                    }
                }

                for i in 0..2 {
                    let update_mode_delta = r.read_bit()?;
                    if update_mode_delta {
                        lf.loop_filter_mode_deltas[i] = r.read_su(7)? as i8;
                    }
                }
            }
        }

        Ok(())
    }

    fn parse_cdef_params(
        r: &mut Reader,
        fh: &mut FrameHeaderObu,
        enable_cdef: bool,
        num_planes: u32,
    ) -> anyhow::Result<()> {
        let cdef = &mut fh.cdef_params;

        if fh.coded_lossless || fh.allow_intrabc || !enable_cdef {
            cdef.cdef_bits = 0;
            cdef.cdef_y_pri_strength[0] = 0;
            cdef.cdef_y_sec_strength[0] = 0;
            cdef.cdef_uv_pri_strength[0] = 0;
            cdef.cdef_uv_sec_strength[0] = 0;
            cdef.cdef_damping = 3;
            return Ok(());
        }

        cdef.cdef_damping = r.read_bits(2)? + 3;
        cdef.cdef_bits = r.read_bits(2)?;
        for i in 0..(1 << cdef.cdef_bits) as usize {
            cdef.cdef_y_pri_strength[i] = r.read_bits(4)?;
            cdef.cdef_y_sec_strength[i] = r.read_bits(2)?;
            if cdef.cdef_y_sec_strength[i] == 3 {
                cdef.cdef_y_sec_strength[i] += 1;
            }
            if num_planes > 1 {
                cdef.cdef_uv_pri_strength[i] = r.read_bits(4)?;
                cdef.cdef_uv_sec_strength[i] = r.read_bits(2)?;
                if cdef.cdef_uv_sec_strength[i] == 3 {
                    cdef.cdef_uv_sec_strength[i] += 1;
                }
            }
        }

        Ok(())
    }

    fn parse_loop_restoration_params(
        r: &mut Reader,
        fh: &mut FrameHeaderObu,
        enable_restoration: bool,
        num_planes: u32,
        use_128x128_superblock: bool,
        subsampling_x: bool,
        subsampling_y: bool,
    ) -> anyhow::Result<()> {
        let lr = &mut fh.loop_restoration_params;

        if fh.all_lossless || fh.allow_intrabc || !enable_restoration {
            lr.frame_restoration_type[0] = FrameRestorationType::None;
            lr.frame_restoration_type[1] = FrameRestorationType::None;
            lr.frame_restoration_type[2] = FrameRestorationType::None;
            lr.uses_lr = false;
            return Ok(());
        }

        lr.uses_lr = false;
        lr.uses_chroma_lr = false;

        const REMAP_LR_TYPE: [FrameRestorationType; 4] = [
            FrameRestorationType::None,
            FrameRestorationType::Switchable,
            FrameRestorationType::Wiener,
            FrameRestorationType::Sgrproj,
        ];

        for i in 0..num_planes as usize {
            let lr_type = r.read_bits(2)?;
            lr.frame_restoration_type[i] = REMAP_LR_TYPE[lr_type as usize];
            if lr.frame_restoration_type[i] != FrameRestorationType::None {
                lr.uses_lr = true;
                if i > 0 {
                    lr.uses_chroma_lr = true;
                }
            }
        }

        if lr.uses_lr {
            if use_128x128_superblock {
                lr.lr_unit_shift = r.read_bits(1)? as u8 + 1;
            } else {
                lr.lr_unit_shift = r.read_bits(1)? as u8;
                if lr.lr_unit_shift > 0 {
                    lr.lr_unit_shift += r.read_bits(1)? as u8;
                }
            }

            lr.loop_restoration_size[0] = RESTORATION_TILESIZE_MAX >> (2 - lr.lr_unit_shift);
            if subsampling_x && subsampling_y && lr.uses_chroma_lr {
                lr.lr_uv_shift = r.read_bits(1)? as u8;
            } else {
                lr.lr_uv_shift = 0;
            }

            lr.loop_restoration_size[1] = lr.loop_restoration_size[0] >> lr.lr_uv_shift;
            lr.loop_restoration_size[2] = lr.loop_restoration_size[0] >> lr.lr_uv_shift;
        }

        Ok(())
    }

    fn read_tx_mode(r: &mut Reader, fh: &mut FrameHeaderObu) -> anyhow::Result<()> {
        if fh.coded_lossless {
            fh.tx_mode = TxMode::Only4x4;
        } else {
            let tx_mode_select = r.read_bit()?;

            if tx_mode_select {
                fh.tx_mode = TxMode::Select;
            } else {
                fh.tx_mode = TxMode::Largest;
            }
        }

        Ok(())
    }

    fn parse_skip_mode_params(
        &self,
        r: &mut Reader,
        fh: &mut FrameHeaderObu,
        enable_order_hint: bool,
        order_hint_bits: i32,
    ) -> anyhow::Result<()> {
        let skip_mode_allowed;

        if fh.frame_is_intra || !fh.reference_select || !enable_order_hint {
            skip_mode_allowed = false;
        } else {
            let mut forward_idx = -1;
            let mut backward_idx = -1;
            let mut forward_hint = 0;
            let mut backward_hint = 0;
            for i in 0..REFS_PER_FRAME {
                let ref_hint = self.ref_info[fh.ref_frame_idx[i] as usize].ref_order_hint;
                if helpers::get_relative_dist(
                    enable_order_hint,
                    order_hint_bits,
                    ref_hint.try_into().unwrap(),
                    fh.order_hint.try_into().unwrap(),
                ) < 0
                    && (forward_idx < 0
                        || helpers::get_relative_dist(
                            enable_order_hint,
                            order_hint_bits,
                            ref_hint.try_into().unwrap(),
                            forward_hint,
                        ) > 0)
                {
                    forward_idx = i32::try_from(i).unwrap();
                    forward_hint = ref_hint.try_into().unwrap();
                } else if helpers::get_relative_dist(
                    enable_order_hint,
                    order_hint_bits,
                    ref_hint.try_into().unwrap(),
                    fh.order_hint.try_into().unwrap(),
                ) > 0
                    && (backward_idx < 0 || {
                        helpers::get_relative_dist(
                            enable_order_hint,
                            order_hint_bits,
                            ref_hint.try_into().unwrap(),
                            backward_hint,
                        ) < 0
                    })
                {
                    backward_idx = i32::try_from(i).unwrap();
                    backward_hint = ref_hint.try_into().unwrap();
                }
            }

            if forward_idx < 0 {
                skip_mode_allowed = false;
            } else if backward_idx >= 0 {
                skip_mode_allowed = true;
                fh.skip_mode_frame[0] = ReferenceFrameType::Last as u32
                    + u32::try_from(std::cmp::min(forward_idx, backward_idx)).unwrap();
                fh.skip_mode_frame[1] = ReferenceFrameType::Last as u32
                    + u32::try_from(std::cmp::max(forward_idx, backward_idx)).unwrap();
            } else {
                let mut second_forward_idx = -1;
                let mut second_forward_hint = 0;
                for i in 0..REFS_PER_FRAME {
                    let ref_hint = self.ref_info[fh.ref_frame_idx[i] as usize].ref_order_hint;
                    if helpers::get_relative_dist(
                        enable_order_hint,
                        order_hint_bits,
                        ref_hint.try_into().unwrap(),
                        forward_hint,
                    ) < 0
                        && (second_forward_idx < 0
                            || helpers::get_relative_dist(
                                enable_order_hint,
                                order_hint_bits,
                                ref_hint.try_into().unwrap(),
                                second_forward_hint,
                            ) > 0)
                    {
                        second_forward_idx = i32::try_from(i).unwrap();
                        second_forward_hint = ref_hint.try_into().unwrap();
                    }
                }

                if second_forward_idx < 0 {
                    skip_mode_allowed = false;
                } else {
                    skip_mode_allowed = true;
                    fh.skip_mode_frame[0] = ReferenceFrameType::Last as u32
                        + u32::try_from(std::cmp::min(forward_idx, second_forward_idx)).unwrap();
                    fh.skip_mode_frame[1] = ReferenceFrameType::Last as u32
                        + u32::try_from(std::cmp::max(forward_idx, second_forward_idx)).unwrap();
                }
            }
        }

        if skip_mode_allowed {
            fh.skip_mode_present = r.read_bit()?;
        } else {
            fh.skip_mode_present = false;
        }

        Ok(())
    }

    fn parse_frame_reference_mode(r: &mut Reader, fh: &mut FrameHeaderObu) -> anyhow::Result<()> {
        if fh.frame_is_intra {
            fh.reference_select = false;
        } else {
            fh.reference_select = r.read_bit()?;
        }
        Ok(())
    }

    fn seg_feature_active_idx(seg: &SegmentationParams, idx: u32, feature: u32) -> bool {
        seg.segmentation_enabled && seg.feature_enabled[idx as usize][feature as usize]
    }

    fn get_qindex(fh: &FrameHeaderObu, ignore_deltaq: bool, segment_id: u32) -> i32 {
        let base_q_idx = i32::try_from(fh.quantization_params.base_q_idx).unwrap();
        if Self::seg_feature_active_idx(&fh.segmentation_params, segment_id, SEG_LVL_ALT_Q as u32) {
            let data = fh.segmentation_params.feature_data[segment_id as usize][SEG_LVL_ALT_Q];
            let mut qindex = base_q_idx + i32::from(data);
            if !ignore_deltaq && fh.quantization_params.delta_q_present {
                qindex += i32::try_from(fh.quantization_params.delta_q_res).unwrap();
            }
            helpers::clip3(0, 255, qindex)
        } else {
            base_q_idx
        }
    }

    fn setup_shear(warp_params: &[i32; 6]) -> anyhow::Result<bool> {
        let mut default = true;
        for (i, param) in warp_params.iter().enumerate() {
            let default_value = if i % 3 == 2 {
                1 << WARPEDMODEL_PREC_BITS
            } else {
                0
            };
            if *param != default_value {
                default = false;
                break;
            }
        }

        /* assume the default params to be valid */
        if default {
            return Ok(true);
        }

        let alpha0 = helpers::clip3(-32768, 32767, warp_params[2] - (1 << WARPEDMODEL_PREC_BITS));
        let beta0 = helpers::clip3(-32768, 32767, warp_params[3]);

        let (div_shift, div_factor) = helpers::resolve_divisor(warp_params[2])?;

        let v = i64::from(warp_params[4] << WARPEDMODEL_PREC_BITS);
        let v = (v * i64::from(div_factor)) as i32;
        let gamma0 = helpers::clip3(-32678, 32767, helpers::round2signed(v, div_shift)?);

        let w = warp_params[3] * warp_params[4];

        let delta0 = helpers::clip3(
            -32768,
            32767,
            warp_params[5]
                - helpers::round2signed(w * div_factor, div_shift)?
                - (1 << WARPEDMODEL_PREC_BITS),
        );

        let alpha =
            helpers::round2signed(alpha0, WARP_PARAM_REDUCE_BITS)? << WARP_PARAM_REDUCE_BITS;
        let beta = helpers::round2signed(beta0, WARP_PARAM_REDUCE_BITS)? << WARP_PARAM_REDUCE_BITS;
        let gamma =
            helpers::round2signed(gamma0, WARP_PARAM_REDUCE_BITS)? << WARP_PARAM_REDUCE_BITS;
        let delta =
            helpers::round2signed(delta0, WARP_PARAM_REDUCE_BITS)? << WARP_PARAM_REDUCE_BITS;

        #[allow(clippy::needless_bool)]
        let warp_valid = if 4 * alpha.abs() + 7 * beta.abs() >= (1 << WARPEDMODEL_PREC_BITS)
            || 4 * gamma.abs() + 4 * delta.abs() >= (1 << WARPEDMODEL_PREC_BITS)
        {
            false
        } else {
            true
        };

        Ok(warp_valid)
    }

    fn read_global_param(
        reader: &mut Reader,
        type_: WarpModelType,
        ref_frame: usize,
        idx: usize,
        allow_high_precision_mv: bool,
        prev_gm_params: &[[i32; 6]; NUM_REF_FRAMES],
        gm_params: &mut [[i32; 6]; NUM_REF_FRAMES],
    ) -> anyhow::Result<()> {
        let mut abs_bits = GM_ABS_ALPHA_BITS;
        let mut prec_bits = GM_ALPHA_PREC_BITS;
        if idx < 2 {
            if type_ == WarpModelType::Translation {
                abs_bits = GM_ABS_TRANS_ONLY_BITS - !allow_high_precision_mv as u32;
                prec_bits = GM_TRANS_ONLY_PREC_BITS - !allow_high_precision_mv as u32;
            } else {
                abs_bits = GM_ABS_TRANS_BITS;
                prec_bits = GM_TRANS_PREC_BITS;
            }
        }

        let prec_diff = WARPEDMODEL_PREC_BITS - prec_bits;

        let (round, sub) = if (idx % 3) == 2 {
            (1 << WARPEDMODEL_PREC_BITS, 1 << prec_bits)
        } else {
            (0, 0)
        };

        let mx = 1 << abs_bits;
        let r = (prev_gm_params[ref_frame][idx] >> prec_diff) - sub;
        gm_params[ref_frame][idx] =
            (reader.decode_signed_subexp_with_ref(-mx, mx + 1, r)? << prec_diff) + round;

        Ok(())
    }

    fn parse_global_motion_params(
        &mut self,
        r: &mut Reader,
        fh: &mut FrameHeaderObu,
    ) -> anyhow::Result<()> {
        let gm = &mut fh.global_motion_params;
        let mut type_;
        let mut prev_gm_params: [[i32; 6]; NUM_REF_FRAMES] = Default::default();

        for ref_frame in ReferenceFrameType::Last as usize..=ReferenceFrameType::AltRef as usize {
            gm.gm_type[ref_frame] = WarpModelType::Identity;
            for i in 0..6 {
                gm.gm_params[ref_frame][i] = if i % 3 == 2 {
                    1 << WARPEDMODEL_PREC_BITS
                } else {
                    0
                }
            }
            gm.warp_valid[ref_frame] = true;
        }

        if fh.frame_is_intra {
            return Ok(());
        }

        // Following libgav1: implement part of setup_past_independence() and
        // load_previous(), i.e.: the parts that refer to the global motion
        // parameters.
        if fh.primary_ref_frame == PRIMARY_REF_NONE {
            // setup_past_independence()
            #[allow(clippy::needless_range_loop)]
            for ref_frame in ReferenceFrameType::Last as usize..ReferenceFrameType::AltRef as usize
            {
                for i in 0..5 {
                    prev_gm_params[ref_frame][i] = if i % 3 == 2 {
                        1 << WARPEDMODEL_PREC_BITS
                    } else {
                        0
                    }
                }
            }
        } else {
            // load_previous():
            // 1. The variable prevFrame is set equal to ref_frame_idx[ primary_ref_frame ].
            // 2. PrevGmParams is set equal to SavedGmParams[ prevFrame ].
            let prev_frame = fh.ref_frame_idx[fh.primary_ref_frame as usize];
            prev_gm_params = self.ref_info[prev_frame as usize]
                .global_motion_params
                .gm_params;
        }

        for ref_frame in ReferenceFrameType::Last as usize..=ReferenceFrameType::AltRef as usize {
            gm.is_global[ref_frame] = r.read_bit()?;
            if gm.is_global[ref_frame] {
                gm.is_rot_zoom[ref_frame] = r.read_bit()?;
                if gm.is_rot_zoom[ref_frame] {
                    type_ = WarpModelType::RotZoom;
                } else {
                    gm.is_translation[ref_frame] = r.read_bit()?;
                    if gm.is_translation[ref_frame] {
                        type_ = WarpModelType::Translation;
                    } else {
                        type_ = WarpModelType::Affine;
                    }
                }
            } else {
                type_ = WarpModelType::Identity;
            }

            gm.gm_type[ref_frame] = type_;
            if gm.gm_type[ref_frame] as u32 >= WarpModelType::RotZoom as u32 {
                Self::read_global_param(
                    r,
                    type_,
                    ref_frame,
                    2,
                    fh.allow_high_precision_mv,
                    &prev_gm_params,
                    &mut gm.gm_params,
                )?;

                Self::read_global_param(
                    r,
                    type_,
                    ref_frame,
                    3,
                    fh.allow_high_precision_mv,
                    &prev_gm_params,
                    &mut gm.gm_params,
                )?;

                if type_ == WarpModelType::Affine {
                    Self::read_global_param(
                        r,
                        type_,
                        ref_frame,
                        4,
                        fh.allow_high_precision_mv,
                        &prev_gm_params,
                        &mut gm.gm_params,
                    )?;

                    Self::read_global_param(
                        r,
                        type_,
                        ref_frame,
                        5,
                        fh.allow_high_precision_mv,
                        &prev_gm_params,
                        &mut gm.gm_params,
                    )?;
                } else {
                    gm.gm_params[ref_frame][4] = -gm.gm_params[ref_frame][3];
                    gm.gm_params[ref_frame][5] = gm.gm_params[ref_frame][2];
                }
            }

            if gm.gm_type[ref_frame] as u32 >= WarpModelType::Translation as u32 {
                Self::read_global_param(
                    r,
                    type_,
                    ref_frame,
                    0,
                    fh.allow_high_precision_mv,
                    &prev_gm_params,
                    &mut gm.gm_params,
                )?;

                Self::read_global_param(
                    r,
                    type_,
                    ref_frame,
                    1,
                    fh.allow_high_precision_mv,
                    &prev_gm_params,
                    &mut gm.gm_params,
                )?;
            }

            gm.warp_valid[ref_frame] = Self::setup_shear(&gm.gm_params[ref_frame])?;
        }

        Ok(())
    }

    fn parse_film_grain_parameters(
        &self,
        r: &mut Reader,
        fh: &mut FrameHeaderObu,
        film_grain_params_present: bool,
        mono_chrome: bool,
        subsampling_x: bool,
        subsampling_y: bool,
    ) -> anyhow::Result<()> {
        let fg = &mut fh.film_grain_params;

        if !film_grain_params_present || (!fh.show_frame && !fh.showable_frame) {
            *fg = Default::default();
            return Ok(());
        }

        fg.apply_grain = r.read_bit()?;
        if !fg.apply_grain {
            *fg = Default::default();
            return Ok(());
        }

        fg.grain_seed = r.read_bits(16)? as u16;
        if fh.frame_type == FrameType::InterFrame {
            fg.update_grain = r.read_bit()?;
        } else {
            fg.update_grain = true;
        }

        if !fg.update_grain {
            fg.film_grain_params_ref_idx = r.read_bits(3)? as u8;
            let temp_grain_seed = fg.grain_seed;

            if !fh
                .ref_frame_idx
                .iter()
                .any(|&ref_frame_idx| ref_frame_idx == fg.film_grain_params_ref_idx)
            {
                return Err(anyhow!("Invalid film_grain_params_ref_idx"));
            }

            // load_grain_params()
            *fg = self.ref_info[fg.film_grain_params_ref_idx as usize]
                .film_grain_params
                .clone();

            fg.grain_seed = temp_grain_seed;

            return Ok(());
        }

        fg.num_y_points = r.read_bits(4)? as u8;
        fg.point_y_value
            .iter_mut()
            .zip(fg.point_y_scaling.iter_mut())
            .take(fg.num_y_points as usize)
            .try_for_each(|(point_y_value, point_y_scaling)| {
                *point_y_value = r.read_bits(8)? as u8;
                *point_y_scaling = r.read_bits(8)? as u8;
                Ok::<_, anyhow::Error>(())
            })?;

        if mono_chrome {
            fg.chroma_scaling_from_luma = false;
        } else {
            fg.chroma_scaling_from_luma = r.read_bit()?;
        }

        if mono_chrome
            || fg.chroma_scaling_from_luma
            || (subsampling_x && subsampling_y && fg.num_y_points == 0)
        {
            fg.num_cb_points = 0;
            fg.num_cr_points = 0;
        } else {
            fg.num_cb_points = r.read_bits(4)? as u8;
            if fg.num_cb_points > 10 {
                return Err(anyhow!("Invalid num_cb_points {}", fg.num_cb_points));
            }

            for i in 0..fg.num_cb_points as usize {
                fg.point_cb_value[i] = r.read_bits(8)? as u8;
                if i > 0 && fg.point_cb_value[i - 1] >= fg.point_cb_value[i] {
                    return Err(anyhow!(
                        "Invalid point_cb_value[{}] {}",
                        i,
                        fg.point_cb_value[i]
                    ));
                }
                fg.point_cb_scaling[i] = r.read_bits(8)? as u8;
            }

            fg.num_cr_points = r.read_bits(4)? as u8;
            for i in 0..fg.num_cr_points as usize {
                fg.point_cr_value[i] = r.read_bits(8)? as u8;
                if i > 0 && fg.point_cr_value[i - 1] >= fg.point_cr_value[i] {
                    return Err(anyhow!(
                        "Invalid point_cr_value[{}] {}",
                        i,
                        fg.point_cr_value[i]
                    ));
                }
                fg.point_cr_scaling[i] = r.read_bits(8)? as u8;
            }
        }

        fg.grain_scaling_minus_8 = r.read_bits(2)? as u8;
        fg.ar_coeff_lag = r.read_bits(2)?;

        let num_pos_luma = 2 * fg.ar_coeff_lag * (fg.ar_coeff_lag + 1);
        let num_pos_chroma = if fg.num_y_points > 0 {
            for i in 0..num_pos_luma as usize {
                fg.ar_coeffs_y_plus_128[i] = r.read_bits(8)? as u8;
            }
            num_pos_luma + 1
        } else {
            num_pos_luma
        };

        if fg.chroma_scaling_from_luma || fg.num_cb_points > 0 {
            for i in 0..num_pos_chroma as usize {
                fg.ar_coeffs_cb_plus_128[i] = r.read_bits(8)? as u8;
            }
        }

        if fg.chroma_scaling_from_luma || fg.num_cr_points > 0 {
            for i in 0..num_pos_chroma as usize {
                fg.ar_coeffs_cr_plus_128[i] = r.read_bits(8)? as u8;
            }
        }

        fg.ar_coeff_shift_minus_6 = r.read_bits(2)? as u8;
        fg.grain_scale_shift = r.read_bits(2)? as u8;

        if fg.num_cb_points > 0 {
            fg.cb_mult = r.read_bits(8)? as u8;
            fg.cb_luma_mult = r.read_bits(8)? as u8;
            fg.cb_offset = r.read_bits(9)? as u16;
        }

        if fg.num_cr_points > 0 {
            fg.cr_mult = r.read_bits(8)? as u8;
            fg.cr_luma_mult = r.read_bits(8)? as u8;
            fg.cr_offset = r.read_bits(9)? as u16;
        }

        fg.overlap_flag = r.read_bit()?;
        fg.clip_to_restricted_range = r.read_bit()?;

        Ok(())
    }

    fn sequence(&self) -> anyhow::Result<&SequenceHeaderObu> {
        let Some(seq) = self.sequence_header.as_ref() else {
            return Err(anyhow!("No sequence header parsed yet"));
        };

        Ok(seq)
    }

    fn parse_uncompressed_frame_header(&mut self, obu: &Obu) -> anyhow::Result<FrameHeaderObu> {
        let mut r = Reader::new(obu.as_ref());

        let mut fh = FrameHeaderObu {
            obu_header: obu.header.clone(),
            ..Default::default()
        };

        // Section 6.8.1: It is a requirement of bitstream conformance that a
        // sequence header OBU has been received before a frame header OBU.
        let &SequenceHeaderObu {
            operating_points_cnt_minus_1,
            seq_force_integer_mv,
            additional_frame_id_length_minus_1,
            delta_frame_id_length_minus_2,
            decoder_model_info_present_flag,
            reduced_still_picture_header,
            frame_id_numbers_present_flag,
            use_128x128_superblock,
            enable_order_hint,
            seq_force_screen_content_tools,
            order_hint_bits,
            enable_cdef,
            enable_restoration,
            enable_warped_motion,
            color_config:
                ColorConfig {
                    subsampling_x,
                    subsampling_y,
                    separate_uv_delta_q,
                    mono_chrome,
                    ..
                },
            timing_info:
                TimingInfo {
                    equal_picture_interval,
                    ..
                },
            decoder_model_info:
                DecoderModelInfo {
                    frame_presentation_time_length_minus_1,
                    buffer_removal_time_length_minus_1,
                    ..
                },
            num_planes,
            film_grain_params_present,
            ..
        } = self.sequence()?;

        let mut id_len = 0;

        if frame_id_numbers_present_flag {
            id_len = additional_frame_id_length_minus_1 + delta_frame_id_length_minus_2 + 3;
        }

        const ALL_FRAMES: u32 = (1 << NUM_REF_FRAMES) - 1;

        if reduced_still_picture_header {
            fh.show_existing_frame = false;
            fh.frame_type = FrameType::KeyFrame;
            fh.frame_is_intra = true;
            fh.show_frame = true;
            fh.showable_frame = false;
        } else {
            fh.show_existing_frame = r.read_bit()?;
            if matches!(obu.header.obu_type, ObuType::Frame) && fh.show_existing_frame {
                return Err(anyhow!("If obu_type is equal to OBU_FRAME, it is a requirement of bitstream conformance that show_existing_frame is equal to 0."));
            }
            if fh.show_existing_frame {
                fh.frame_to_show_map_idx = r.read_bits(3)?;

                if decoder_model_info_present_flag && !equal_picture_interval {
                    fh.frame_presentation_time = r.read_bits(
                        u8::try_from(frame_presentation_time_length_minus_1).unwrap() + 1,
                    )?;
                }

                let ref_frame = &self.ref_info[usize::try_from(fh.frame_to_show_map_idx).unwrap()];

                fh.refresh_frame_flags = 0;
                if frame_id_numbers_present_flag {
                    if id_len == 0 {
                        return Err(anyhow!("Invalid id_len {}", id_len));
                    }
                    fh.display_frame_id = r.read_bits(id_len.try_into().unwrap())?;
                    if ref_frame.display_frame_id != fh.display_frame_id || !ref_frame.ref_valid {
                        return Err(anyhow!("Invalid display_frame_id"));
                    }
                }

                if !ref_frame.showable_frame {
                    return Err(anyhow!("Invalid bitstream: can't show this past frame"));
                }

                // In decode_frame_wrapup():
                //
                // Otherwise (show_existing_frame is equal to 1), if frame_type
                // is equal to KEY_FRAME, the reference frame loading process as
                // specified in section 7.21 is invoked (this process loads
                // frame state from the reference frames into the current frame
                // state variables)
                //
                // The following ordered steps now apply:
                //
                // 1. The reference frame update process as specified in section
                // 7.20 is invoked (this process saves the current frame state
                // into the reference frames).
                //
                // 2. If show_frame is equal to 1 or show_existing_frame is
                // equal to 1, the output process as specified in section 7.18
                // is invoked (this will output the current frame or a saved
                // frame).
                //
                // We implement 1. here while 2. is left to the actual decoder
                self.load_reference_frame(&mut fh)?;
                if fh.frame_type == FrameType::KeyFrame {
                    fh.refresh_frame_flags = ALL_FRAMES;
                }

                if film_grain_params_present {
                    // load_grain_params()
                    fh.film_grain_params = self.ref_info[fh.frame_to_show_map_idx as usize]
                        .film_grain_params
                        .clone();
                }

                // See 5.10.
                if matches!(obu.header.obu_type, ObuType::Frame) {
                    r.byte_alignment()?;
                }

                fh.header_bytes = usize::try_from(r.position() / 8).unwrap();
                return Ok(fh);
            }

            fh.frame_type = FrameType::n(r.read_bits(2)?).ok_or(anyhow!("Invalid frame type"))?;
            fh.frame_is_intra = matches!(
                fh.frame_type,
                FrameType::IntraOnlyFrame | FrameType::KeyFrame
            );

            fh.show_frame = r.read_bit()?;

            if fh.show_frame && decoder_model_info_present_flag && equal_picture_interval {
                fh.frame_presentation_time =
                    r.read_bits(u8::try_from(frame_presentation_time_length_minus_1).unwrap() + 1)?;
            }

            if fh.show_frame {
                fh.showable_frame = !matches!(fh.frame_type, FrameType::KeyFrame);
            } else {
                fh.showable_frame = r.read_bit()?;
            }

            if fh.frame_type == FrameType::SwitchFrame
                || (fh.frame_type == FrameType::KeyFrame && fh.show_frame)
            {
                fh.error_resilient_mode = true;
            } else {
                fh.error_resilient_mode = r.read_bit()?;
            }
        }

        if fh.frame_type == FrameType::KeyFrame && fh.show_frame {
            for i in 0..NUM_REF_FRAMES {
                self.ref_info[i].ref_valid = false;
                self.ref_info[i].ref_order_hint = 0;
            }
            for i in 0..REFS_PER_FRAME {
                fh.order_hints[ReferenceFrameType::Last as usize + i] = 0;
            }
        }

        fh.disable_cdf_update = r.read_bit()?;
        if seq_force_screen_content_tools == SELECT_SCREEN_CONTENT_TOOLS as u32 {
            fh.allow_screen_content_tools = r.read_bit()? as u32;
        } else {
            fh.allow_screen_content_tools = seq_force_screen_content_tools;
        }

        if fh.allow_screen_content_tools > 0 {
            if seq_force_integer_mv == SELECT_INTEGER_MV as u32 {
                fh.force_integer_mv = r.read_bit()? as u32;
            } else {
                fh.force_integer_mv = seq_force_integer_mv;
            }
        } else {
            fh.force_integer_mv = 0;
        }

        if fh.frame_is_intra {
            fh.force_integer_mv = 1;
        }

        if frame_id_numbers_present_flag {
            self.prev_frame_id = self.current_frame_id;
            self.current_frame_id = r.read_bits(id_len.try_into().unwrap())?;
            fh.current_frame_id = self.current_frame_id;

            /* conformance checking, as per aom */
            let have_prev_frame_id =
                !(self.is_first_frame || fh.frame_type == FrameType::KeyFrame && fh.show_frame);

            if have_prev_frame_id {
                let frame_id_length =
                    additional_frame_id_length_minus_1 + delta_frame_id_length_minus_2 + 3;

                let diff_frame_id = if self.current_frame_id > self.prev_frame_id {
                    self.current_frame_id - self.prev_frame_id
                } else {
                    if frame_id_length > 16 {
                        return Err(anyhow!("Invalid frame_id_length {}", frame_id_length));
                    }
                    (1 << frame_id_length) + self.current_frame_id - self.prev_frame_id
                };

                if self.prev_frame_id == self.current_frame_id
                    || diff_frame_id >= (1 << (frame_id_length - 1))
                {
                    return Err(anyhow!(
                        "Invalid frame_id: prev_frame_id = {}, current_frame_id = {}",
                        self.prev_frame_id,
                        self.current_frame_id
                    ));
                }
            }

            /* mark_ref_frames (idLen) */
            let diff_len = delta_frame_id_length_minus_2 + 2;
            let shifted_diff_len = 1 << diff_len;
            let shifted_id_len = 1 << id_len;

            for i in 0..NUM_REF_FRAMES {
                if self.current_frame_id > shifted_diff_len {
                    if self.ref_info[i].ref_frame_id > self.current_frame_id
                        || self.ref_info[i].ref_frame_id
                            < (self.current_frame_id - shifted_diff_len)
                    {
                        self.ref_info[i].ref_valid = false;
                    }
                } else if self.ref_info[i].ref_frame_id > self.current_frame_id
                    && self.ref_info[i].ref_frame_id
                        < shifted_id_len + self.current_frame_id - shifted_diff_len
                {
                    self.ref_info[i].ref_valid = false;
                }
            }
        } else {
            self.current_frame_id = 0;
            self.prev_frame_id = self.current_frame_id;
            fh.current_frame_id = self.current_frame_id;
        }

        if fh.frame_type == FrameType::SwitchFrame {
            fh.frame_size_override_flag = true;
        } else if reduced_still_picture_header {
            fh.frame_size_override_flag = false;
        } else {
            fh.frame_size_override_flag = r.read_bit()?;
        }

        fh.order_hint = r.read_bits(order_hint_bits.try_into().unwrap())?;

        if fh.frame_is_intra || fh.error_resilient_mode {
            fh.primary_ref_frame = PRIMARY_REF_NONE;
        } else {
            fh.primary_ref_frame = r.read_bits(3)?;
        }

        let operating_points = &self.sequence()?.operating_points;
        if decoder_model_info_present_flag {
            fh.buffer_removal_time_present_flag = r.read_bit()?;
            if fh.buffer_removal_time_present_flag {
                #[allow(clippy::needless_range_loop)]
                for op_num in 0..=operating_points_cnt_minus_1 as usize {
                    if operating_points[op_num].decoder_model_present_for_this_op {
                        let op_pt_idc = operating_points[op_num].idc;
                        let in_temporal_layer = (op_pt_idc >> fh.obu_header.temporal_id) & 1 != 0;
                        let in_spatial_layer =
                            (op_pt_idc >> (fh.obu_header.spatial_id + 8)) & 1 != 0;

                        if op_pt_idc == 0 || (in_temporal_layer && in_spatial_layer) {
                            let n = buffer_removal_time_length_minus_1 + 1;
                            fh.buffer_removal_time[op_num] = r.read_bits(n.try_into().unwrap())?;
                        }
                    }
                }
            }
        }

        fh.allow_high_precision_mv = false;
        fh.use_ref_frame_mvs = false;
        fh.allow_intrabc = false;
        if fh.frame_type == FrameType::SwitchFrame
            || (fh.frame_type == FrameType::KeyFrame && fh.show_frame)
        {
            fh.refresh_frame_flags = ALL_FRAMES;
        } else {
            fh.refresh_frame_flags = r.read_bits(8)?;
        }

        /* equivalent boolean expression */
        if (!fh.frame_is_intra || fh.refresh_frame_flags != ALL_FRAMES)
            && fh.error_resilient_mode
            && enable_order_hint
        {
            for i in 0..NUM_REF_FRAMES {
                fh.ref_order_hint[i] = r.read_bits(order_hint_bits.try_into().unwrap())?;
                if fh.ref_order_hint[i] != self.ref_info[i].ref_order_hint {
                    self.ref_info[i].ref_valid = false;
                }
            }
        }

        if fh.frame_is_intra {
            self.parse_frame_size(&mut fh, &mut r)?;
            Self::parse_render_size(&mut fh, &mut r)?;
            if fh.allow_screen_content_tools > 0 && fh.upscaled_width == fh.frame_width {
                fh.allow_intrabc = r.read_bit()?;
            }
        } else {
            if !enable_order_hint {
                fh.frame_refs_short_signaling = false;
            } else {
                fh.frame_refs_short_signaling = r.read_bit()?;
                if fh.frame_refs_short_signaling {
                    fh.last_frame_idx = r.read_bits(3)? as u8;
                    fh.gold_frame_idx = r.read_bits(3)? as u8;
                    let ref_order_hints = self
                        .ref_info
                        .iter()
                        .map(|i| i.ref_order_hint)
                        .collect::<Vec<_>>()
                        .try_into()
                        .unwrap();
                    self.set_frame_refs(&mut fh, &ref_order_hints)?;
                }
            }

            let mut expected_frame_id = [0; REFS_PER_FRAME];
            #[allow(clippy::needless_range_loop)]
            for i in 0..REFS_PER_FRAME {
                if !fh.frame_refs_short_signaling {
                    fh.ref_frame_idx[i] = r.read_bits(3)?.try_into().unwrap();
                }

                if frame_id_numbers_present_flag {
                    /* DeltaFrameId */
                    let delta_frame_id =
                        r.read_bits(u8::try_from(delta_frame_id_length_minus_2).unwrap() + 2)? + 1;

                    if id_len == 0 {
                        return Err(anyhow!("Invalid id_len {}", id_len));
                    }

                    let shifted_id_len = 1 << id_len;

                    expected_frame_id[i] =
                        (self.current_frame_id + shifted_id_len - delta_frame_id) % shifted_id_len;

                    let actual_frame_id = self.ref_info[fh.ref_frame_idx[i] as usize].ref_frame_id;

                    if expected_frame_id[i] != actual_frame_id {
                        return Err(anyhow!(
                            "Invalid frame id, expected {} got {}",
                            expected_frame_id[i],
                            actual_frame_id
                        ));
                    }
                }
            }

            if fh.frame_size_override_flag && !fh.error_resilient_mode {
                self.frame_size_with_refs(&mut fh, &mut r)?;
            } else {
                self.parse_frame_size(&mut fh, &mut r)?;
                Self::parse_render_size(&mut fh, &mut r)?;
            }

            if fh.force_integer_mv > 0 {
                fh.allow_high_precision_mv = false;
            } else {
                fh.allow_high_precision_mv = r.read_bit()?;
            }

            /* read_interpolation_filter */
            fh.is_filter_switchable = r.read_bit()?;
            if fh.is_filter_switchable {
                fh.interpolation_filter = InterpolationFilter::Switchable;
            } else {
                fh.interpolation_filter = InterpolationFilter::n(r.read_bits(2)?)
                    .ok_or(anyhow!("Invalid interpolation filter"))?;
            }

            fh.is_motion_mode_switchable = r.read_bit()?;

            if fh.error_resilient_mode || !self.sequence()?.enable_ref_frame_mvs {
                fh.use_ref_frame_mvs = false;
            } else {
                fh.use_ref_frame_mvs = r.read_bit()?;
            }

            for i in 0..REFS_PER_FRAME {
                let ref_frame = ReferenceFrameType::Last as usize + i;
                let hint = self.ref_info[fh.ref_frame_idx[i] as usize].ref_order_hint;
                fh.order_hints[ref_frame] = hint;

                if !enable_order_hint {
                    fh.ref_frame_sign_bias[i] = false;
                } else {
                    fh.ref_frame_sign_bias[i] = helpers::get_relative_dist(
                        enable_order_hint,
                        order_hint_bits,
                        hint.try_into().unwrap(),
                        fh.order_hint.try_into().unwrap(),
                    ) > 0;
                }
            }
        }

        if reduced_still_picture_header || fh.disable_cdf_update {
            fh.disable_frame_end_update_cdf = true;
        } else {
            fh.disable_frame_end_update_cdf = r.read_bit()?;
        }

        if fh.primary_ref_frame == PRIMARY_REF_NONE {
            Self::setup_past_independence(&mut fh);
        } else {
            /* load from the past reference */
            let prev_frame =
                &self.ref_info[fh.ref_frame_idx[fh.primary_ref_frame as usize] as usize];

            if !prev_frame.ref_valid {
                return Err(anyhow!("Reference is invalid"));
            }

            /* load_loop_filter_params: load ref_deltas and mode_deltas */
            fh.loop_filter_params.loop_filter_ref_deltas =
                prev_frame.loop_filter_params.loop_filter_ref_deltas;
            fh.loop_filter_params.loop_filter_mode_deltas =
                prev_frame.loop_filter_params.loop_filter_mode_deltas;

            /* load_segmentation_params: load feature_enabled and feature_data */
            fh.segmentation_params.feature_enabled = prev_frame.segmentation_params.feature_enabled;
            fh.segmentation_params.feature_data = prev_frame.segmentation_params.feature_data;
        }

        // TODO: we can live without this for now.
        // if fh.use_ref_frame_mvs {
        //     // motion_field_estimators()
        // }

        self.parse_tile_info(&mut r, &mut fh.tile_info)?;
        Self::parse_quantization_params(
            &mut r,
            &mut fh.quantization_params,
            num_planes,
            separate_uv_delta_q,
        )?;
        self.parse_segmentation_params(&mut r, &mut fh)?;
        Self::parse_delta_q_params(&mut r, &mut fh.quantization_params)?;
        Self::parse_delta_lf_params(
            &mut r,
            &mut fh.loop_filter_params,
            fh.quantization_params.delta_q_present,
            fh.allow_intrabc,
        )?;

        fh.coded_lossless = true;
        for segment_id in 0..MAX_SEGMENTS {
            let q_index = Self::get_qindex(&fh, true, segment_id as _);
            let q = &fh.quantization_params;
            fh.lossless_array[segment_id] = q_index == 0
                && q.delta_q_y_dc == 0
                && q.delta_q_u_ac == 0
                && q.delta_q_u_dc == 0
                && q.delta_q_v_ac == 0
                && q.delta_q_v_dc == 0;
            if !fh.lossless_array[segment_id] {
                fh.coded_lossless = false;
            }
            if q.using_qmatrix {
                if fh.lossless_array[segment_id] {
                    fh.seg_qm_level[0][segment_id] = 15;
                    fh.seg_qm_level[1][segment_id] = 15;
                    fh.seg_qm_level[2][segment_id] = 15;
                } else {
                    fh.seg_qm_level[0][segment_id] = q.qm_y;
                    fh.seg_qm_level[1][segment_id] = q.qm_u;
                    fh.seg_qm_level[2][segment_id] = q.qm_v;
                }
            }
        }

        fh.all_lossless = fh.coded_lossless && fh.frame_width == fh.upscaled_width;
        Self::parse_loop_filter_parameters(&mut r, &mut fh, num_planes)?;
        Self::parse_cdef_params(&mut r, &mut fh, enable_cdef, num_planes)?;
        Self::parse_loop_restoration_params(
            &mut r,
            &mut fh,
            enable_restoration,
            num_planes,
            use_128x128_superblock,
            subsampling_x,
            subsampling_y,
        )?;
        Self::read_tx_mode(&mut r, &mut fh)?;
        Self::parse_frame_reference_mode(&mut r, &mut fh)?;
        self.parse_skip_mode_params(&mut r, &mut fh, enable_order_hint, order_hint_bits)?;

        if fh.frame_is_intra || fh.error_resilient_mode || !enable_warped_motion {
            fh.allow_warped_motion = false;
        } else {
            fh.allow_warped_motion = r.read_bit()?;
        }

        fh.reduced_tx_set = r.read_bit()?;
        self.parse_global_motion_params(&mut r, &mut fh)?;
        self.parse_film_grain_parameters(
            &mut r,
            &mut fh,
            film_grain_params_present,
            mono_chrome,
            subsampling_x,
            subsampling_y,
        )?;

        Self::skip_and_check_trailing_bits(&mut r, obu)?;

        // See 5.10
        if matches!(obu.header.obu_type, ObuType::Frame) {
            r.byte_alignment()?;
        }

        fh.header_bytes = usize::try_from(r.position() / 8).unwrap();
        Ok(fh)
    }

    pub fn parse_tile_group_obu<'a>(&mut self, obu: Obu<'a>) -> anyhow::Result<TileGroupObu<'a>> {
        let mut tg = TileGroupObu {
            obu,
            ..Default::default()
        };

        let mut r = Reader::new(tg.obu.as_ref());

        if r.remaining_bits() % 8 != 0 {
            return Err(anyhow!("Bitstream is not byte aligned"));
        }

        let mut sz: u64 = r.remaining_bits() / 8;

        let num_tiles = self.tile_rows * self.tile_cols;
        let start_bit_pos = r.position();

        if num_tiles > 1 {
            tg.tile_start_and_end_present_flag = r.read_bit()?;
        }

        if num_tiles == 1 || !tg.tile_start_and_end_present_flag {
            tg.tg_start = 0;
            tg.tg_end = num_tiles - 1;
        } else {
            let tile_bits = u8::try_from(self.tile_cols_log2 + self.tile_rows_log2).unwrap();
            tg.tg_start = r.read_bits(tile_bits)?;
            tg.tg_end = r.read_bits(tile_bits)?;
        }

        r.byte_alignment()?;

        let end_bit_pos = r.position();
        let header_bytes = (end_bit_pos - start_bit_pos) / 8;
        sz -= header_bytes;

        let mut tile_num = tg.tg_start;
        while tile_num <= tg.tg_end {
            let tile_row = tile_num / self.tile_cols;
            let tile_col = tile_num % self.tile_cols;
            let last_tile = tile_num == tg.tg_end;
            let tile_size;

            if last_tile {
                tile_size = u32::try_from(sz).unwrap();
            } else {
                tile_size = r.read_le(self.tile_size_bytes.try_into().unwrap())? + 1;
                sz -= u64::from(tile_size + self.tile_size_bytes);
            }

            let tile = Tile {
                tile_offset: u32::try_from(r.position()).unwrap() / 8,
                tile_size,
                tile_row,
                tile_col,
                mi_row_start: self.mi_row_starts[tile_row as usize],
                mi_row_end: self.mi_row_starts[tile_row as usize + 1],
                mi_col_start: self.mi_row_starts[tile_col as usize],
                mi_col_end: self.mi_row_starts[tile_col as usize + 1],
            };

            tg.tiles.push(tile);

            // init_symbol, decode_tile() and exit_symbol() left to the accelerator.

            // Skip the actual tile data
            if tile_num < tg.tg_end {
                r.skip(u64::from(tile_size * 8))?;
            }

            tile_num += 1;
        }

        if tg.tg_end == num_tiles - 1 {
            // left to the accelerator:
            // if ( !disable_frame_end_update_cdf ) {
            //  frame_end_update_cdf( )
            // }
            // decode_frame_wrapup( )
            self.seen_frame_header = false;
        }

        Ok(tg)
    }

    pub fn parse_frame_obu<'a>(&mut self, obu: Obu<'a>) -> anyhow::Result<FrameObu<'a>> {
        if !matches!(obu.header.obu_type, ObuType::Frame) {
            return Err(anyhow!(
                "Expected a FrameOBU, got {:?}",
                obu.header.obu_type
            ));
        }

        let frame_header_obu = self.parse_frame_header_obu(&obu)?;
        let obu = Obu {
            header: obu.header,
            data: obu.data,
            start_offset: obu.start_offset + frame_header_obu.header_bytes,
            size: obu.size - frame_header_obu.header_bytes,
        };
        let tile_group_obu = self.parse_tile_group_obu(obu)?;

        Ok(FrameObu {
            header: frame_header_obu,
            tile_group: tile_group_obu,
        })
    }

    pub fn parse_frame_header_obu(&mut self, obu: &Obu) -> anyhow::Result<FrameHeaderObu> {
        if !matches!(obu.header.obu_type, ObuType::FrameHeader | ObuType::Frame) {
            return Err(anyhow!(
                "Expected a FrameHeaderOBU, got {:?}",
                obu.header.obu_type
            ));
        }

        if self.seen_frame_header {
            Ok(self
                .last_frame_header
                .clone()
                .take()
                .ok_or(anyhow!("Broken stream: no previous frame header to copy"))?)
        } else {
            self.seen_frame_header = true;
            let header = self.parse_uncompressed_frame_header(obu)?;
            if header.show_existing_frame {
                self.last_frame_header = None;
                self.seen_frame_header = false;
            } else {
                /* TileNum = 0 */
                self.seen_frame_header = true;
                self.last_frame_header = Some(header.clone());
            }

            Ok(header)
        }
    }

    /// Implements 7.20. This function should be called right after decoding a
    /// frame.
    pub fn ref_frame_update(&mut self, fh: &FrameHeaderObu) -> anyhow::Result<()> {
        // This was found as a bug otherwise by Nicolas Dufresne in GStreamer's
        // av1parse.
        if fh.show_existing_frame && !matches!(fh.frame_type, FrameType::KeyFrame) {
            return Ok(());
        }

        if matches!(fh.frame_type, FrameType::IntraOnlyFrame) && fh.refresh_frame_flags == 0xff {
            return Err(anyhow!(
                "Intra-only frames cannot refresh all of the DPB as per the spec."
            ));
        }

        let &SequenceHeaderObu {
            color_config:
                ColorConfig {
                    subsampling_x,
                    subsampling_y,
                    ..
                },
            film_grain_params_present,
            bit_depth,
            ..
        } = self.sequence()?;

        for (i, ref_info) in self.ref_info.iter_mut().enumerate() {
            if ((fh.refresh_frame_flags >> i) & 1) != 0 {
                ref_info.ref_valid = true;

                ref_info.ref_frame_id = fh.current_frame_id;
                ref_info.ref_frame_type = fh.frame_type;
                ref_info.ref_upscaled_width = fh.upscaled_width;
                ref_info.ref_frame_width = fh.frame_width;
                ref_info.ref_frame_height = fh.frame_height;
                ref_info.ref_render_width = fh.render_width;
                ref_info.ref_render_height = fh.render_height;
                ref_info.ref_order_hint = fh.order_hint;
                ref_info.ref_mi_cols = self.mi_cols;
                ref_info.ref_mi_rows = self.mi_rows;
                ref_info.ref_subsampling_x = subsampling_x;
                ref_info.ref_subsampling_y = subsampling_y;
                ref_info.ref_bit_depth = bit_depth;
                ref_info.segmentation_params = fh.segmentation_params.clone();
                ref_info.global_motion_params = fh.global_motion_params.clone();
                ref_info.loop_filter_params = fh.loop_filter_params.clone();
                ref_info.tile_info = fh.tile_info.clone();
                ref_info.display_frame_id = fh.display_frame_id;
                ref_info.showable_frame = fh.showable_frame;

                if film_grain_params_present {
                    ref_info.film_grain_params = fh.film_grain_params.clone();
                }
            }
        }

        Ok(())
    }

    pub fn highest_operating_point(&self) -> Option<u32> {
        if self.operating_point_idc == 0 {
            /* No scalability information, all OBUs must be decoded */
            None
        } else {
            Some(helpers::floor_log2(self.operating_point_idc >> 8))
        }
    }
}

impl Default for Parser {
    fn default() -> Self {
        Self {
            stream_format: StreamFormat::LowOverhead,
            operating_point: Default::default(),
            seen_frame_header: Default::default(),
            last_frame_header: Default::default(),
            operating_point_idc: Default::default(),
            should_probe_for_annexb: true,
            is_first_frame: Default::default(),
            mi_cols: Default::default(),
            mi_rows: Default::default(),
            prev_frame_id: Default::default(),
            current_frame_id: Default::default(),
            ref_info: Default::default(),
            mi_col_starts: [0; MAX_TILE_COLS + 1],
            mi_row_starts: [0; MAX_TILE_ROWS + 1],
            tile_cols_log2: Default::default(),
            tile_cols: Default::default(),
            tile_rows_log2: Default::default(),
            tile_rows: Default::default(),
            tile_size_bytes: Default::default(),
            sequence_header: Default::default(),
        }
    }
}

impl Clone for Parser {
    fn clone(&self) -> Self {
        let sequence_header = self
            .sequence_header
            .as_ref()
            .map(|s| Rc::new((**s).clone()));

        Self {
            stream_format: self.stream_format.clone(),
            operating_point: self.operating_point,
            seen_frame_header: self.seen_frame_header,
            last_frame_header: self.last_frame_header.clone(),
            operating_point_idc: self.operating_point_idc,
            should_probe_for_annexb: self.should_probe_for_annexb,
            is_first_frame: self.is_first_frame,
            ref_info: self.ref_info.clone(),
            mi_cols: self.mi_cols,
            mi_rows: self.mi_rows,
            prev_frame_id: self.prev_frame_id,
            current_frame_id: self.current_frame_id,
            mi_col_starts: self.mi_col_starts,
            mi_row_starts: self.mi_row_starts,
            tile_cols_log2: self.tile_cols_log2,
            tile_cols: self.tile_cols,
            tile_rows_log2: self.tile_rows_log2,
            tile_rows: self.tile_rows,
            tile_size_bytes: self.tile_size_bytes,
            sequence_header,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::codec::av1::parser::{ParsedObu, Parser, StreamFormat};
    use crate::utils::IvfIterator;

    use super::ObuType;

    /// Same as test-25fps.av1.ivf from Chromium
    const STREAM_TEST_25_FPS: &[u8] = include_bytes!("test_data/test-25fps.ivf.av1");

    /// Encoded with
    ///
    /// gst-launch-1.0 videotestsrc num-buffers=1 !
    /// video/x-raw,format=I420,width=64,height=64 ! filesink
    /// location=aom_input.yuv
    ///
    /// And:
    ///
    /// aomenc -p 1 --ivf -w 64 -h 64 -o av1-annexb.ivf.av1 aom_input.yuv --annexb=1
    const STREAM_ANNEXB: &[u8] = include_bytes!("test_data/av1-annexb.ivf.av1");

    #[test]
    fn parse_test25fps() {
        let mut parser = Parser::default();
        let ivf_iter = IvfIterator::new(STREAM_TEST_25_FPS);
        let mut num_obus = 0;

        for packet in ivf_iter {
            let mut consumed = 0;

            while let Ok(obu) = parser.parse_obu(&packet[consumed..]) {
                let obu = match obu {
                    ParsedObu::Process(obu) => obu,
                    // This OBU should be dropped.
                    ParsedObu::Drop(length) => {
                        consumed += usize::try_from(length).unwrap();
                        continue;
                    }
                };
                consumed += obu.data.len();
                num_obus += 1;
            }
        }

        // Manually checked with GStreamer under GDB by using a hitcount on
        // "gst_av1_parse_identify_one_obu" *after* the stream format has been
        // detected.
        assert_eq!(num_obus, 525);
    }

    #[test]
    /// Test that we can correctly identify streams in both "low-overhead" and
    /// Annex B formats.
    fn parse_annexb() {
        let mut parser = Parser::default();
        let mut ivf_iter = IvfIterator::new(STREAM_TEST_25_FPS);
        let packet = ivf_iter.next().unwrap();

        parser.parse_obu(packet).unwrap();
        assert!(matches!(parser.stream_format, StreamFormat::LowOverhead));

        let mut parser = Parser::default();
        let mut ivf_iter = IvfIterator::new(STREAM_ANNEXB);
        let packet = ivf_iter.next().unwrap();

        parser.parse_obu(packet).unwrap();
        assert!(matches!(parser.stream_format, StreamFormat::AnnexB { .. }));
    }

    #[test]
    /// Test that we can correctly identify streams in both "low-overhead" and
    /// Annex B formats and identify all the OBUs in the stream until the end.
    fn parse_annexb_full() {
        let mut parser = Parser::default();
        let ivf_iter = IvfIterator::new(STREAM_TEST_25_FPS);

        for packet in ivf_iter {
            let mut consumed = 0;

            while let Ok(obu) = parser.parse_obu(&packet[consumed..]) {
                let obu = match obu {
                    ParsedObu::Process(obu) => obu,
                    // This OBU should be dropped.
                    ParsedObu::Drop(length) => {
                        consumed += usize::try_from(length).unwrap();
                        continue;
                    }
                };
                assert!(matches!(parser.stream_format, StreamFormat::LowOverhead));
                consumed += obu.data.len();
            }
        }

        let mut parser = Parser::default();
        let ivf_iter = IvfIterator::new(STREAM_ANNEXB);
        let mut num_obus = 0;

        for packet in ivf_iter {
            let mut consumed = 0;

            while let Ok(obu) = parser.parse_obu(&packet[consumed..]) {
                let obu = match obu {
                    ParsedObu::Process(obu) => obu,
                    // This OBU should be dropped.
                    ParsedObu::Drop(length) => {
                        consumed += usize::try_from(length).unwrap();
                        continue;
                    }
                };
                assert!(matches!(parser.stream_format, StreamFormat::AnnexB { .. }));
                consumed += obu.data.len();
                num_obus += 1;
            }
        }

        assert_eq!(num_obus, 3);
        let annexb_state = match parser.stream_format {
            StreamFormat::AnnexB(annexb_state) => annexb_state,
            _ => panic!("Wrong StreamFormat, expected AnnexB"),
        };
        assert_eq!(
            annexb_state.temporal_unit_consumed,
            annexb_state.temporal_unit_size
        );
        assert_eq!(
            annexb_state.frame_unit_consumed,
            annexb_state.frame_unit_size
        );
    }

    #[test]
    fn parse_test25fps_obus() {
        let mut parser = Parser::default();
        let ivf_iter = IvfIterator::new(STREAM_TEST_25_FPS);

        for packet in ivf_iter {
            let mut consumed = 0;

            while let Ok(obu) = parser.parse_obu(&packet[consumed..]) {
                let obu = match obu {
                    ParsedObu::Process(obu) => obu,
                    // This OBU should be dropped.
                    ParsedObu::Drop(length) => {
                        consumed += usize::try_from(length).unwrap();
                        continue;
                    }
                };

                let data_len = obu.data.len();

                match obu.header.obu_type {
                    ObuType::SequenceHeader => {
                        parser.parse_sequence_header_obu(&obu).unwrap();
                    }
                    ObuType::FrameHeader | ObuType::RedundantFrameHeader => {
                        let fh = parser.parse_frame_header_obu(&obu).unwrap();
                        parser.ref_frame_update(&fh).unwrap();
                    }
                    ObuType::TileGroup => {
                        parser.parse_tile_group_obu(obu).unwrap();
                    }
                    ObuType::Frame => {
                        let frame = parser.parse_frame_obu(obu).unwrap();
                        parser.ref_frame_update(&frame.header).unwrap();
                    }
                    _ => {}
                };

                consumed += data_len;
            }
        }
    }
}
