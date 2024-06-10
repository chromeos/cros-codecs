// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::io::Write;
use std::num::TryFromIntError;

use thiserror::Error;

use crate::codec::av1::helpers::clip3;
use crate::codec::av1::parser::BitDepth;
use crate::codec::av1::parser::ChromaSamplePosition;
use crate::codec::av1::parser::ColorPrimaries;
use crate::codec::av1::parser::FrameHeaderObu;
use crate::codec::av1::parser::FrameRestorationType;
use crate::codec::av1::parser::FrameType;
use crate::codec::av1::parser::InterpolationFilter;
use crate::codec::av1::parser::MatrixCoefficients;
use crate::codec::av1::parser::ObuHeader;
use crate::codec::av1::parser::ObuType;
use crate::codec::av1::parser::Profile;
use crate::codec::av1::parser::ReferenceFrameType;
use crate::codec::av1::parser::SequenceHeaderObu;
use crate::codec::av1::parser::TemporalDelimiterObu;
use crate::codec::av1::parser::TransferCharacteristics;
use crate::codec::av1::parser::TxMode;
use crate::codec::av1::parser::WarpModelType;
use crate::codec::av1::parser::FEATURE_BITS;
use crate::codec::av1::parser::FEATURE_MAX;
use crate::codec::av1::parser::FEATURE_SIGNED;
use crate::codec::av1::parser::MAX_NUM_OPERATING_POINTS;
use crate::codec::av1::parser::MAX_NUM_PLANES;
use crate::codec::av1::parser::MAX_SEGMENTS;
use crate::codec::av1::parser::NUM_REF_FRAMES;
use crate::codec::av1::parser::PRIMARY_REF_NONE;
use crate::codec::av1::parser::REFS_PER_FRAME;
use crate::codec::av1::parser::SEG_LVL_MAX;
use crate::codec::av1::parser::SELECT_INTEGER_MV;
use crate::codec::av1::parser::SELECT_SCREEN_CONTENT_TOOLS;
use crate::codec::av1::parser::SUPERRES_DENOM_BITS;
use crate::codec::av1::parser::SUPERRES_DENOM_MIN;
use crate::codec::av1::parser::SUPERRES_NUM;
use crate::codec::av1::parser::TOTAL_REFS_PER_FRAME;
use crate::codec::av1::writer::ObuWriter;
use crate::codec::av1::writer::ObuWriterError;

mod private {
    pub trait ObuStruct {}
}

impl private::ObuStruct for SequenceHeaderObu {}

impl private::ObuStruct for TemporalDelimiterObu {}

impl private::ObuStruct for FrameHeaderObu {}

#[derive(Error, Debug)]
pub enum SynthesizerError {
    #[error("tried to synthesize unsupported settings")]
    Unsupported,
    #[error("invalid syntax element value {0}")]
    InvalidSyntaxElementValue(&'static str),
    #[error(transparent)]
    ConversionError(#[from] TryFromIntError),
    #[error(transparent)]
    ObuWriter(#[from] ObuWriterError),
    #[error(transparent)]
    Io(#[from] std::io::Error),
}

pub type SynthesizerResult<T> = Result<T, SynthesizerError>;

pub struct Synthesizer<'o, O: private::ObuStruct, W: Write> {
    writer: ObuWriter<W>,
    obu: &'o O,
}

impl<'o, O, W> Synthesizer<'o, O, W>
where
    O: private::ObuStruct,
    W: Write,
{
    fn new(writer: W, obu: &'o O) -> Self {
        Self {
            writer: ObuWriter::new(writer),
            obu,
        }
    }

    fn f<T: Into<u32>>(&mut self, bits: usize, value: T) -> SynthesizerResult<()> {
        let value: u32 = value.into();
        self.writer.write_f(bits, value)?;
        Ok(())
    }

    fn leb128<T: Into<u32>>(&mut self, value: T) -> SynthesizerResult<()> {
        let value: u32 = value.into();
        self.writer.write_leb128(value, 0)?;
        Ok(())
    }

    fn uvlc<T: Into<u32>>(&mut self, value: T) -> SynthesizerResult<()> {
        self.writer.write_uvlc(value)?;
        Ok(())
    }

    fn su<T: Into<i32>>(&mut self, bits: usize, value: T) -> SynthesizerResult<()> {
        self.writer.write_su(bits, value)?;
        Ok(())
    }

    #[cfg(any(test, debug_assertions))]
    fn invalid_element_value(&mut self, element: &'static str) -> SynthesizerResult<()> {
        Err(SynthesizerError::InvalidSyntaxElementValue(element))
    }

    #[cfg(not(any(test, debug_assertions)))]
    fn invalid_element_value(&mut self, element: &'static str) -> SynthesizerResult<()> {
        log::error!("Invalid syntax element value: '{element}', expect corrupted bitstream");
        Ok(())
    }

    /// Writes 5.3.2. OBU header syntax
    fn obu_header(&mut self, obu: &ObuHeader) -> SynthesizerResult<()> {
        self.f(1, /* obu_forbidden_bit */ 0u32)?;
        self.f(4, obu.obu_type as u32)?;
        self.f(1, obu.extension_flag)?;
        self.f(1, obu.has_size_field)?;
        self.f(1, /* obu_reserved_1bit */ 0u32)?;

        if obu.extension_flag {
            self.obu_extension_header(obu)?;
        }

        Ok(())
    }

    /// Writes AV1 5.3.3. OBU extension header syntax
    fn obu_extension_header(&mut self, obu: &ObuHeader) -> SynthesizerResult<()> {
        // AV1 5.3.3
        self.f(3, obu.temporal_id)?;
        self.f(2, obu.spatial_id)?;
        self.f(3, /* extension_header_reserved_3bits */ 0u32)?;

        Ok(())
    }

    fn obu_size(&mut self, size: u32) -> SynthesizerResult<()> {
        self.leb128(size)
    }

    /// Writes AV1 5.3.4. Trailing bits syntax
    fn trailing_bits(&mut self) -> SynthesizerResult<()> {
        self.f(1, /* trailing_one_bit */ 1u32)?;
        while !self.writer.aligned() {
            self.f(1, /* trailing_zero_bit */ 0u32)?;
        }

        Ok(())
    }
}

impl<'o, W> Synthesizer<'o, TemporalDelimiterObu, W>
where
    W: Write,
{
    pub fn synthesize(obu: &'o TemporalDelimiterObu, writer: W) -> SynthesizerResult<()> {
        let mut s = Self::new(writer, obu);

        if obu.obu_header.obu_type != ObuType::TemporalDelimiter {
            s.invalid_element_value("obu_type")?;
        }

        s.obu_header(&obu.obu_header)?;

        if obu.obu_header.has_size_field {
            s.obu_size(0u32)?;
        }

        Ok(())
    }
}

impl<'o, W> Synthesizer<'o, SequenceHeaderObu, W>
where
    W: Write,
{
    pub fn synthesize(obu: &'o SequenceHeaderObu, mut writer: W) -> SynthesizerResult<()> {
        let mut s = Synthesizer::new(&mut writer, obu);

        if obu.obu_header.obu_type != ObuType::SequenceHeader {
            s.invalid_element_value("obu_type")?;
        }

        s.obu_header(&obu.obu_header)?;

        if !obu.obu_header.has_size_field {
            s.sequence_header_obu()?;
            s.trailing_bits()?;
            return Ok(());
        }

        let mut buf = Vec::<u8>::new();
        let mut buffered = Synthesizer::new(&mut buf, obu);
        buffered.sequence_header_obu()?;
        buffered.trailing_bits()?;
        drop(buffered);

        s.obu_size(buf.len() as u32)?;
        drop(s);

        writer.write_all(&buf)?;

        Ok(())
    }

    /// Writes AV1 5.5.1. General sequence header OBU syntax
    fn sequence_header_obu(&mut self) -> SynthesizerResult<()> {
        self.f(3, self.obu.seq_profile as u32)?;
        self.f(1, self.obu.still_picture)?;
        self.f(1, self.obu.reduced_still_picture_header)?;

        if self.obu.reduced_still_picture_header {
            if self.obu.timing_info_present_flag {
                self.invalid_element_value("reduced_still_picture_header")?
            }
            if self.obu.timing_info_present_flag {
                self.invalid_element_value("timing_info_present_flag")?
            }
            if self.obu.initial_display_delay_present_flag {
                self.invalid_element_value("initial_display_delay_present_flag")?
            }
            if self.obu.operating_points_cnt_minus_1 != 0 {
                self.invalid_element_value("operating_points_cnt_minus_1")?
            }
            if self.obu.operating_points[0].idc != 0 {
                self.invalid_element_value("operating_point_idc")?
            }

            self.f(5, self.obu.operating_points[0].seq_level_idx)?;

            if self.obu.operating_points[0].decoder_model_present_for_this_op {
                self.invalid_element_value("decoder_model_present_for_this_op")?
            }
            if self.obu.operating_points[0].initial_display_delay_present_for_this_op {
                self.invalid_element_value("initial_display_delay_present_for_this_op")?
            }
        } else {
            self.f(1, self.obu.timing_info_present_flag)?;
            if self.obu.timing_info_present_flag {
                self.timing_info()?;
                self.f(1, self.obu.decoder_model_info_present_flag)?;
                if self.obu.decoder_model_info_present_flag {
                    self.decoder_model_info()?;
                }
            } else if self.obu.decoder_model_info_present_flag {
                self.invalid_element_value("decoder_model_info_present_flag")?;
            }

            self.f(1, self.obu.initial_display_delay_present_flag)?;

            if self.obu.operating_points_cnt_minus_1 > MAX_NUM_OPERATING_POINTS as u32 {
                self.invalid_element_value("operating_points_cnt_minus_1")?;
            }
            self.f(5, self.obu.operating_points_cnt_minus_1)?;
            for i in 0..=self.obu.operating_points_cnt_minus_1 {
                let op = &self.obu.operating_points[i as usize];

                self.f(12, op.idc)?;
                self.f(5, op.seq_level_idx)?;
                if op.seq_level_idx > 7 {
                    self.f(1, op.seq_tier)?;
                } else if op.seq_tier != 0 {
                    self.invalid_element_value("seq_tier")?;
                }

                if self.obu.decoder_model_info_present_flag {
                    self.f(1, op.decoder_model_present_for_this_op)?;
                    if op.decoder_model_present_for_this_op {
                        self.operating_parameters_info(i as usize)?;
                    }
                } else if op.decoder_model_present_for_this_op {
                    self.invalid_element_value("decoder_model_present_for_this_op")?;
                }

                if self.obu.initial_display_delay_present_flag {
                    self.f(1, op.initial_display_delay_present_for_this_op)?;
                    if op.initial_display_delay_present_for_this_op {
                        self.f(4, op.initial_display_delay_minus_1)?;
                    }
                }
            }
        }

        let bits = u32::BITS - self.obu.max_frame_width_minus_1.leading_zeros();
        if self.obu.frame_width_bits_minus_1 + 1 < bits {
            self.invalid_element_value("frame_width_bits_minus_1")?;
        }

        let bits = u32::BITS - self.obu.max_frame_height_minus_1.leading_zeros();
        if self.obu.frame_height_bits_minus_1 + 1 < bits {
            self.invalid_element_value("frame_height_bits_minus_1")?;
        }

        self.f(4, self.obu.frame_width_bits_minus_1)?;
        self.f(4, self.obu.frame_height_bits_minus_1)?;

        let n = self.obu.frame_width_bits_minus_1 as usize + 1;
        if (n as u32) < 32 - self.obu.max_frame_width_minus_1.leading_zeros() {
            self.invalid_element_value("max_frame_width_minus_1")?;
        }
        self.f(n, self.obu.max_frame_width_minus_1)?;

        let n = self.obu.frame_height_bits_minus_1 as usize + 1;
        if (n as u32) < 32 - self.obu.max_frame_height_minus_1.leading_zeros() {
            self.invalid_element_value("max_frame_height_minus_1")?;
        }
        self.f(n, self.obu.max_frame_height_minus_1)?;

        if self.obu.reduced_still_picture_header {
            if self.obu.frame_id_numbers_present_flag {
                self.invalid_element_value("frame_id_numbers_present_flag")?;
            }
        } else {
            self.f(1, self.obu.frame_id_numbers_present_flag)?;
        }

        if self.obu.frame_id_numbers_present_flag {
            self.f(4, self.obu.delta_frame_id_length_minus_2)?;
            self.f(3, self.obu.additional_frame_id_length_minus_1)?;
        }

        self.f(1, self.obu.use_128x128_superblock)?;
        self.f(1, self.obu.enable_filter_intra)?;
        self.f(1, self.obu.enable_intra_edge_filter)?;

        if self.obu.reduced_still_picture_header {
            if self.obu.enable_interintra_compound {
                self.invalid_element_value("enable_interintra_compound")?;
            }
            if self.obu.enable_masked_compound {
                self.invalid_element_value("enable_masked_compound")?;
            }
            if self.obu.enable_warped_motion {
                self.invalid_element_value("enable_warped_motion")?;
            }
            if self.obu.enable_dual_filter {
                self.invalid_element_value("enable_dual_filter")?;
            }
            if self.obu.enable_order_hint {
                self.invalid_element_value("enable_order_hint")?;
            }
            if self.obu.enable_jnt_comp {
                self.invalid_element_value("enable_jnt_comp")?;
            }
            if self.obu.enable_ref_frame_mvs {
                self.invalid_element_value("enable_ref_frame_mvs")?;
            }
            if self.obu.seq_force_screen_content_tools != SELECT_SCREEN_CONTENT_TOOLS as u32 {
                self.invalid_element_value("seq_force_screen_content_tools")?;
            }
            if self.obu.seq_force_integer_mv != SELECT_INTEGER_MV as u32 {
                self.invalid_element_value("seq_force_integer_mv")?;
            }
            if self.obu.order_hint_bits != 0 {
                self.invalid_element_value("OrderHintBits")?;
            }
        } else {
            self.f(1, self.obu.enable_interintra_compound)?;
            self.f(1, self.obu.enable_masked_compound)?;
            self.f(1, self.obu.enable_warped_motion)?;
            self.f(1, self.obu.enable_dual_filter)?;
            self.f(1, self.obu.enable_order_hint)?;

            if self.obu.enable_order_hint {
                self.f(1, self.obu.enable_jnt_comp)?;
                self.f(1, self.obu.enable_ref_frame_mvs)?;
            } else {
                if self.obu.enable_jnt_comp {
                    self.invalid_element_value("enable_jnt_comp")?;
                }
                if self.obu.enable_ref_frame_mvs {
                    self.invalid_element_value("enable_ref_frame_mvs")?;
                }
            }

            self.f(1, self.obu.seq_choose_screen_content_tools)?;
            if self.obu.seq_choose_screen_content_tools {
                if self.obu.seq_force_screen_content_tools != SELECT_SCREEN_CONTENT_TOOLS as u32 {
                    self.invalid_element_value("seq_force_screen_content_tools")?;
                }
            } else {
                self.f(1, self.obu.seq_force_screen_content_tools)?;
            }

            if self.obu.seq_force_screen_content_tools > 0 {
                self.f(1, self.obu.seq_choose_integer_mv)?;
                if self.obu.seq_choose_integer_mv {
                    if self.obu.seq_force_integer_mv != SELECT_INTEGER_MV as u32 {
                        self.invalid_element_value("seq_force_integer_mv")?;
                    }
                } else {
                    self.f(1, self.obu.seq_force_integer_mv)?;
                }
            } else if self.obu.seq_force_integer_mv != SELECT_INTEGER_MV as u32 {
                self.invalid_element_value("seq_force_integer_mv")?;
            }

            if self.obu.enable_order_hint {
                self.f(3, self.obu.order_hint_bits_minus_1 as u32)?;
                if self.obu.order_hint_bits != self.obu.order_hint_bits_minus_1 + 1 {
                    self.invalid_element_value("OrderHintBits")?;
                }
            } else if self.obu.order_hint_bits != 0 {
                self.invalid_element_value("OrderHintBits")?;
            }
        }

        self.f(1, self.obu.enable_superres)?;
        self.f(1, self.obu.enable_cdef)?;
        self.f(1, self.obu.enable_restoration)?;
        self.color_config()?;
        self.f(1, self.obu.film_grain_params_present)?;

        Ok(())
    }

    /// Writes AV1 5.5.2. Color config syntax
    fn color_config(&mut self) -> SynthesizerResult<()> {
        let cc = &self.obu.color_config;

        self.f(1, cc.high_bitdepth)?;
        if matches!(self.obu.seq_profile, Profile::Profile2) && cc.high_bitdepth {
            self.f(1, cc.twelve_bit)?;

            if (cc.twelve_bit && self.obu.bit_depth != BitDepth::Depth12)
                || (!cc.twelve_bit && self.obu.bit_depth != BitDepth::Depth10)
            {
                self.invalid_element_value("BitDepth")?;
            }
        } else if self.obu.seq_profile <= Profile::Profile2
            && ((cc.high_bitdepth && self.obu.bit_depth != BitDepth::Depth10)
                || (!cc.high_bitdepth && self.obu.bit_depth != BitDepth::Depth8))
        {
            self.invalid_element_value("BitDepth")?;
        }

        if matches!(self.obu.seq_profile, Profile::Profile1) {
            if cc.mono_chrome {
                self.invalid_element_value("mono_chrome")?;
            }
        } else {
            self.f(1, cc.mono_chrome)?;
        }

        if (cc.mono_chrome && self.obu.num_planes != 1)
            || (!cc.mono_chrome && self.obu.num_planes != 3)
        {
            self.invalid_element_value("NumPlanes")?;
        }

        self.f(1, cc.color_description_present_flag)?;
        if cc.color_description_present_flag {
            self.f(8, cc.color_primaries as u32)?;
            self.f(8, cc.transfer_characteristics as u32)?;
            self.f(8, cc.matrix_coefficients as u32)?;
        } else {
            if !matches!(cc.color_primaries, ColorPrimaries::Unspecified) {
                self.invalid_element_value("color_primaries")?;
            }
            if !matches!(
                cc.transfer_characteristics,
                TransferCharacteristics::Unspecified
            ) {
                self.invalid_element_value("transfer_characteristics")?;
            }
            if !matches!(cc.matrix_coefficients, MatrixCoefficients::Unspecified) {
                self.invalid_element_value("matrix_coefficients")?;
            }
        }

        if cc.mono_chrome {
            self.f(1, cc.color_range)?;

            if !cc.subsampling_x {
                self.invalid_element_value("subsampling_x")?;
            }
            if !cc.subsampling_y {
                self.invalid_element_value("subsampling_y")?;
            }
            if !matches!(cc.chroma_sample_position, ChromaSamplePosition::Unknown) {
                self.invalid_element_value("chroma_sample_position")?;
            }
            if !matches!(cc.chroma_sample_position, ChromaSamplePosition::Unknown) {
                self.invalid_element_value("chroma_sample_position")?;
            }
            if cc.separate_uv_delta_q {
                self.invalid_element_value("separate_uv_delta_q")?;
            }

            return Ok(());
        } else if matches!(cc.color_primaries, ColorPrimaries::Bt709)
            && matches!(cc.transfer_characteristics, TransferCharacteristics::Srgb)
            && matches!(cc.matrix_coefficients, MatrixCoefficients::Identity)
        {
            if !cc.color_range {
                self.invalid_element_value("color_range")?;
            }
            if cc.subsampling_x {
                self.invalid_element_value("subsampling_x")?;
            }
            if cc.subsampling_y {
                self.invalid_element_value("subsampling_y")?;
            }
        } else {
            self.f(1, cc.color_range)?;

            match self.obu.seq_profile {
                Profile::Profile0 => {
                    if !cc.subsampling_x {
                        self.invalid_element_value("subsampling_x")?;
                    }
                    if !cc.subsampling_y {
                        self.invalid_element_value("subsampling_y")?;
                    }
                }
                Profile::Profile1 => {
                    if cc.subsampling_x {
                        self.invalid_element_value("subsampling_x")?;
                    }
                    if cc.subsampling_y {
                        self.invalid_element_value("subsampling_y")?;
                    }
                }
                _ => {
                    if matches!(self.obu.bit_depth, BitDepth::Depth12) {
                        self.f(1, cc.subsampling_x)?;
                        if cc.subsampling_x {
                            self.f(1, cc.subsampling_y)?;
                        } else if cc.subsampling_y {
                            self.invalid_element_value("subsampling_y")?;
                        }
                    } else {
                        if !cc.subsampling_x {
                            self.invalid_element_value("subsampling_x")?;
                        }
                        if cc.subsampling_y {
                            self.invalid_element_value("subsampling_y")?;
                        }
                    }
                }
            }

            if cc.subsampling_x && cc.subsampling_y {
                self.f(2, cc.chroma_sample_position as u32)?;
            }
        }

        self.f(1, cc.separate_uv_delta_q)?;

        Ok(())
    }

    /// Writes AV1 5.5.3. Timing info syntax
    fn timing_info(&mut self) -> SynthesizerResult<()> {
        // AV1 5.5.3
        let ti = &self.obu.timing_info;

        self.f(32, ti.num_units_in_display_tick)?;
        self.f(32, ti.time_scale)?;
        self.f(1, ti.equal_picture_interval)?;
        if ti.equal_picture_interval {
            self.uvlc(ti.num_ticks_per_picture_minus_1)?;
        }

        Ok(())
    }

    /// Writes AV1 5.5.4. Decoder model info syntax
    fn decoder_model_info(&mut self) -> SynthesizerResult<()> {
        let dm = &self.obu.decoder_model_info;

        self.f(5, dm.buffer_delay_length_minus_1)?;
        self.f(32, dm.num_units_in_decoding_tick)?;
        self.f(32, dm.buffer_removal_time_length_minus_1)?;
        self.f(32, dm.frame_presentation_time_length_minus_1)?;

        Ok(())
    }

    /// Writes AV1 5.5.5. Operating parameters info syntax
    fn operating_parameters_info(&mut self, i: usize) -> SynthesizerResult<()> {
        let op = &self.obu.operating_points[i];

        let n = usize::try_from(self.obu.decoder_model_info.buffer_delay_length_minus_1)? + 1;
        self.f(n, op.decoder_buffer_delay)?;
        self.f(n, op.encoder_buffer_delay)?;
        self.f(1, op.low_delay_mode_flag)?;

        Ok(())
    }
}

impl<'o, W> Synthesizer<'o, FrameHeaderObu, W>
where
    W: Write,
{
    pub fn synthesize(
        obu: &'o FrameHeaderObu,
        sequence: &'o SequenceHeaderObu,
        mut writer: W,
    ) -> SynthesizerResult<()> {
        let mut s = Synthesizer::new(&mut writer, obu);

        if obu.obu_header.obu_type != ObuType::FrameHeader {
            s.invalid_element_value("obu_type")?;
        }

        s.obu_header(&obu.obu_header)?;

        if !obu.obu_header.has_size_field {
            s.frame_header_obu(sequence)?;
            s.trailing_bits()?;
            return Ok(());
        }

        let mut buf = Vec::<u8>::new();
        let mut buffered = Synthesizer::new(&mut buf, obu);
        buffered.frame_header_obu(sequence)?;
        buffered.trailing_bits()?;
        drop(buffered);

        s.obu_size(buf.len() as u32)?;
        drop(s);

        writer.write_all(&buf)?;

        Ok(())
    }

    /// Writes AV1 5.9.1. General frame header OBU syntax
    fn frame_header_obu(&mut self, sequence: &'o SequenceHeaderObu) -> SynthesizerResult<()> {
        self.uncompressed_header(sequence)
    }

    /// Writes AV1 5.9.2. Uncompressed header syntax
    fn uncompressed_header(&mut self, sequence: &'o SequenceHeaderObu) -> SynthesizerResult<()> {
        const ALL_FRAMES: u32 = (1 << NUM_REF_FRAMES) - 1;

        // idLen
        let id_len = usize::try_from(
            sequence.additional_frame_id_length_minus_1
                + sequence.delta_frame_id_length_minus_2
                + 3,
        )?;

        if sequence.reduced_still_picture_header {
            if !self.obu.show_existing_frame {
                self.invalid_element_value("show_existing_frame")?;
            }
            if !matches!(self.obu.frame_type, FrameType::KeyFrame) {
                self.invalid_element_value("frame_type")?;
            }
            if !self.obu.show_frame {
                self.invalid_element_value("show_frame")?;
            }
            if !self.obu.showable_frame {
                self.invalid_element_value("showable_frame")?;
            }
            if !self.obu.frame_is_intra {
                self.invalid_element_value("FrameIsIntra")?;
            }
        } else {
            self.f(1, self.obu.show_existing_frame)?;
            if self.obu.show_existing_frame {
                self.f(3, self.obu.frame_to_show_map_idx)?;

                if sequence.decoder_model_info_present_flag
                    && !sequence.timing_info.equal_picture_interval
                {
                    self.temporal_point_info(sequence)?;
                }

                if sequence.frame_id_numbers_present_flag {
                    self.f(id_len, self.obu.display_frame_id)?;
                }
                return Ok(());
            }

            self.f(2, self.obu.frame_type as u32)?;
            if self.obu.frame_is_intra
                ^ matches!(
                    self.obu.frame_type,
                    FrameType::IntraOnlyFrame | FrameType::KeyFrame
                )
            {
                self.invalid_element_value("FrameIsIntra")?;
            }

            self.f(1, self.obu.show_frame)?;
            if self.obu.show_frame
                && sequence.decoder_model_info_present_flag
                && !sequence.timing_info.equal_picture_interval
            {
                self.temporal_point_info(sequence)?;
            }

            if self.obu.show_frame {
                if self.obu.showable_frame ^ !matches!(self.obu.frame_type, FrameType::KeyFrame) {
                    self.invalid_element_value("showable_frame")?;
                }
            } else {
                self.f(1, self.obu.showable_frame)?;
            }

            if matches!(self.obu.frame_type, FrameType::SwitchFrame)
                || (matches!(self.obu.frame_type, FrameType::KeyFrame) && self.obu.show_frame)
            {
                if !self.obu.error_resilient_mode {
                    self.invalid_element_value("error_resilient_mode")?;
                }
            } else {
                self.f(1, self.obu.error_resilient_mode)?;
            }
        }

        self.f(1, self.obu.disable_cdf_update)?;
        if sequence.seq_force_screen_content_tools == SELECT_SCREEN_CONTENT_TOOLS as u32 {
            self.f(1, self.obu.allow_screen_content_tools)?;
        } else if self.obu.allow_screen_content_tools != sequence.seq_force_screen_content_tools {
            self.invalid_element_value("allow_screen_content_tools")?;
        }

        if self.obu.allow_screen_content_tools != 0 {
            if sequence.seq_force_integer_mv == SELECT_INTEGER_MV as u32 {
                self.f(1, self.obu.force_integer_mv)?;
            } else if self.obu.force_integer_mv != sequence.seq_force_integer_mv {
                self.invalid_element_value("force_integer_mv")?;
            }
        } else if self.obu.force_integer_mv != 0
            && !(self.obu.frame_is_intra && self.obu.force_integer_mv != 1)
        {
            self.invalid_element_value("force_integer_mv")?;
        }

        if sequence.frame_id_numbers_present_flag {
            self.f(id_len, self.obu.current_frame_id)?;
        } else if self.obu.current_frame_id != 0 {
            self.invalid_element_value("current_frame_id")?;
        }

        if matches!(self.obu.frame_type, FrameType::SwitchFrame) {
            if !self.obu.frame_size_override_flag {
                self.invalid_element_value("frame_size_override_flag")?;
            }
        } else if sequence.reduced_still_picture_header {
            if self.obu.frame_size_override_flag {
                self.invalid_element_value("frame_size_override_flag")?;
            }
        } else {
            self.f(1, self.obu.frame_size_override_flag)?;
        }

        if sequence.order_hint_bits != 0 {
            if sequence.order_hint_bits != sequence.order_hint_bits_minus_1 + 1 {
                self.invalid_element_value("order_hint_bits_minus_1")?;
            }

            self.f(sequence.order_hint_bits as usize, self.obu.order_hint)?;
        }

        if self.obu.frame_is_intra || self.obu.error_resilient_mode {
            if self.obu.primary_ref_frame != PRIMARY_REF_NONE {
                self.invalid_element_value("primary_ref_frame")?;
            }
        } else {
            self.f(3, self.obu.primary_ref_frame)?;
        }

        if sequence.decoder_model_info_present_flag {
            self.f(1, self.obu.buffer_removal_time_present_flag)?;

            for op_num in 0..=sequence.operating_points_cnt_minus_1 {
                let op = &sequence.operating_points[op_num as usize];
                if op.decoder_model_present_for_this_op {
                    let in_temporal_layer = (op.idc >> self.obu.obu_header.temporal_id) & 1 != 0;
                    let in_spatial_layer =
                        (op.idc >> (self.obu.obu_header.spatial_id + 8)) & 1 != 0;

                    if op.idc == 0 || (in_temporal_layer && in_spatial_layer) {
                        let n = usize::try_from(
                            sequence
                                .decoder_model_info
                                .buffer_removal_time_length_minus_1
                                + 1,
                        )?;

                        self.f(n, op.decoder_buffer_delay)?;
                    }
                }
            }
        }

        if matches!(self.obu.frame_type, FrameType::SwitchFrame)
            || (matches!(self.obu.frame_type, FrameType::KeyFrame) && self.obu.show_frame)
        {
            if self.obu.refresh_frame_flags != ALL_FRAMES {
                self.invalid_element_value("refresh_frame_flags")?;
            }
        } else {
            self.f(8, self.obu.refresh_frame_flags)?;
        }

        if (!self.obu.frame_is_intra || self.obu.refresh_frame_flags != ALL_FRAMES)
            && self.obu.error_resilient_mode
            && sequence.enable_order_hint
        {
            for i in 0..NUM_REF_FRAMES {
                self.f(
                    sequence.order_hint_bits as usize,
                    self.obu.ref_order_hint[i],
                )?;
            }
        }

        if self.obu.frame_is_intra {
            self.frame_size(sequence)?;
            self.render_size()?;

            if self.obu.allow_screen_content_tools != 0
                && self.obu.upscaled_width == self.obu.frame_width
            {
                self.f(1, self.obu.allow_intrabc)?;
            }
        } else {
            if !sequence.enable_order_hint {
                if self.obu.frame_refs_short_signaling {
                    self.invalid_element_value("frame_refs_short_signaling")?;
                }
            } else {
                self.f(1, self.obu.frame_refs_short_signaling)?;
                if self.obu.frame_refs_short_signaling {
                    self.f(3, self.obu.last_frame_idx)?;
                    self.f(3, self.obu.gold_frame_idx)?;
                }
            }

            for i in 0..REFS_PER_FRAME {
                let ref_frame_idx = u32::try_from(self.obu.ref_frame_idx[i])?;
                if !self.obu.frame_refs_short_signaling {
                    self.f(3, ref_frame_idx)?;
                }

                if sequence.frame_id_numbers_present_flag {
                    let n = usize::try_from(sequence.delta_frame_id_length_minus_2 + 2)?;

                    let delta_frame_id_minus_1 = ((self.obu.current_frame_id - ref_frame_idx
                        + (1 << id_len))
                        % (1 << id_len))
                        - 1;

                    self.f(n, delta_frame_id_minus_1)?;
                }
            }

            if self.obu.frame_size_override_flag && !self.obu.error_resilient_mode {
                self.frame_size_with_refs()?;
            } else {
                self.frame_size(sequence)?;
                self.render_size()?;
            }

            if self.obu.force_integer_mv != 0 {
                if !self.obu.allow_high_precision_mv {
                    self.invalid_element_value("allow_high_precision_mv")?;
                }
            } else {
                self.f(1, self.obu.allow_high_precision_mv)?;
            }

            self.read_interpolation_filter()?;
            self.f(1, self.obu.is_motion_mode_switchable)?;

            if self.obu.error_resilient_mode || !sequence.enable_ref_frame_mvs {
                if self.obu.use_ref_frame_mvs {
                    self.invalid_element_value("use_ref_frame_mvs")?;
                }
            } else {
                self.f(1, self.obu.use_ref_frame_mvs)?;
            }
        }

        if sequence.reduced_still_picture_header || self.obu.disable_cdf_update {
            if !self.obu.disable_frame_end_update_cdf {
                self.invalid_element_value("disable_frame_end_update_cdf")?
            }
        } else {
            self.f(1, self.obu.disable_frame_end_update_cdf)?;
        }

        self.tile_info()?;
        self.quantization_params(sequence)?;
        self.segmentation_params()?;

        self.delta_q_params()?;
        self.delta_lf_params()?;

        if self.obu.coded_lossless && self.obu.lossless_array != [true; MAX_SEGMENTS] {
            self.invalid_element_value("CodedLossless")?;
        }

        if self.obu.all_lossless
            ^ (self.obu.coded_lossless && self.obu.frame_width == self.obu.upscaled_width)
        {
            self.invalid_element_value("AllLossless")?;
        }

        self.loop_filter_params(sequence)?;
        self.cdef_params(sequence)?;
        self.lr_params(sequence)?;
        self.read_tx_mode()?;
        self.frame_reference_mode()?;
        self.skip_mode_params()?;

        if self.obu.frame_is_intra
            || self.obu.error_resilient_mode
            || !sequence.enable_warped_motion
        {
            if self.obu.allow_warped_motion {
                self.invalid_element_value("allow_warped_motion")?;
            }
        } else {
            self.f(1, self.obu.allow_warped_motion)?;
        }

        self.f(1, self.obu.reduced_tx_set)?;

        self.global_motion_params()?;
        self.film_grain_params(sequence)?;

        Ok(())
    }

    /// Writes AV1 5.9.31. Temporal point info syntax
    fn temporal_point_info(&mut self, sequence: &'o SequenceHeaderObu) -> SynthesizerResult<()> {
        let n = usize::try_from(
            sequence
                .decoder_model_info
                .frame_presentation_time_length_minus_1
                + 1,
        )?;

        self.f(n, self.obu.frame_presentation_time)?;

        Ok(())
    }

    /// Writes AV1 5.9.5. Frame size syntax
    fn frame_size(&mut self, sequence: &'o SequenceHeaderObu) -> SynthesizerResult<()> {
        if self.obu.frame_size_override_flag {
            let n = usize::try_from(sequence.frame_width_bits_minus_1 + 1)?;
            self.f(n, self.obu.frame_width - 1)?;
            let n = usize::try_from(sequence.frame_height_bits_minus_1 + 1)?;
            self.f(n, self.obu.frame_height - 1)?;
        } else {
            if self.obu.frame_width != sequence.max_frame_width_minus_1 + 1 {
                self.invalid_element_value("FrameWidth")?;
            }
            if self.obu.frame_height != sequence.max_frame_height_minus_1 + 1 {
                self.invalid_element_value("FrameHeight")?;
            }

            self.superres_params(sequence)?;
        }

        Ok(())
    }

    /// Writes AV1 5.9.6. Render size syntax
    fn render_size(&mut self) -> SynthesizerResult<()> {
        self.f(1, self.obu.render_and_frame_size_different)?;

        if self.obu.render_and_frame_size_different {
            self.f(16, self.obu.render_width - 1)?;
            self.f(16, self.obu.render_height - 1)?;
        } else {
            if self.obu.render_width != self.obu.upscaled_width {
                self.invalid_element_value("RenderWidth")?;
            }
            if self.obu.render_height != self.obu.frame_height {
                self.invalid_element_value("RenderHeight")?;
            }
        }

        Ok(())
    }

    /// Writes AV1 5.9.7. Frame size with refs syntax
    fn frame_size_with_refs(&mut self) -> SynthesizerResult<()> {
        log::error!("Syntax element frame_size_with_refs is unsupported");
        Err(SynthesizerError::Unsupported)
    }

    /// Writes AV1 5.9.8. Superres params syntax
    fn superres_params(&mut self, sequence: &'o SequenceHeaderObu) -> SynthesizerResult<()> {
        if sequence.enable_superres {
            self.f(1, self.obu.use_superres)?;
        } else if self.obu.use_superres {
            self.invalid_element_value("use_superres")?;
        }

        if self.obu.use_superres {
            let coded_denom = self.obu.superres_denom - SUPERRES_DENOM_MIN as u32;

            self.f(SUPERRES_DENOM_BITS, coded_denom)?;
        } else if self.obu.superres_denom != SUPERRES_NUM as u32 {
            self.invalid_element_value("superres_denom")?;
        }

        Ok(())
    }

    /// Writes AV1 5.9.10. Interpolation filter syntax
    fn read_interpolation_filter(&mut self) -> SynthesizerResult<()> {
        self.f(1, self.obu.is_filter_switchable)?;
        if self.obu.is_filter_switchable {
            if !matches!(
                self.obu.interpolation_filter,
                InterpolationFilter::Switchable
            ) {
                self.invalid_element_value("interpolation_filter")?;
            }
        } else {
            self.f(2, self.obu.interpolation_filter as u32)?;
        }

        Ok(())
    }

    /// Writes AV1 5.9.11. Loop filter params syntax
    fn loop_filter_params(&mut self, sequence: &'o SequenceHeaderObu) -> SynthesizerResult<()> {
        if self.obu.coded_lossless || self.obu.allow_intrabc {
            if !matches!(self.obu.loop_filter_params.loop_filter_level, [0, 0, _, _]) {
                self.invalid_element_value("loop_filter_level")?;
            }
            if self.obu.loop_filter_params.loop_filter_ref_deltas != [1, 0, 0, 0, 0, -1, -1, -1] {
                self.invalid_element_value("loop_filter_ref_deltas")?;
            }
            if self.obu.loop_filter_params.loop_filter_mode_deltas != [0, 0] {
                self.invalid_element_value("loop_filter_mode_deltas")?;
            }
            return Ok(());
        }

        self.f(6, self.obu.loop_filter_params.loop_filter_level[0])?;
        self.f(6, self.obu.loop_filter_params.loop_filter_level[1])?;
        if sequence.num_planes > 1
            && self.obu.loop_filter_params.loop_filter_level[0] != 0
            && self.obu.loop_filter_params.loop_filter_level[1] != 0
        {
            self.f(6, self.obu.loop_filter_params.loop_filter_level[2])?;
            self.f(6, self.obu.loop_filter_params.loop_filter_level[3])?;
        }

        self.f(3, self.obu.loop_filter_params.loop_filter_sharpness)?;
        self.f(1, self.obu.loop_filter_params.loop_filter_delta_enabled)?;
        if self.obu.loop_filter_params.loop_filter_delta_enabled {
            self.f(1, self.obu.loop_filter_params.loop_filter_delta_update)?;
            if self.obu.loop_filter_params.loop_filter_delta_update {
                for i in 0..TOTAL_REFS_PER_FRAME {
                    // NOTE: Currently we have no way of checking if the value changed between
                    // frames, always update the value to make sure the decoder will recreate
                    // the same state.
                    const UPDATE_REF_DELTA: bool = true;
                    if UPDATE_REF_DELTA {
                        self.su(1 + 6, self.obu.loop_filter_params.loop_filter_ref_deltas[i])?;
                    }
                }

                for i in 0..2 {
                    // NOTE: Same as above
                    const UPDATE_MODE_DELTA: bool = true;
                    if UPDATE_MODE_DELTA {
                        self.su(
                            1 + 6,
                            self.obu.loop_filter_params.loop_filter_mode_deltas[i],
                        )?;
                    }
                }
            }
        }

        Ok(())
    }

    /// Writes AV1 5.9.12. Quantization params syntax
    fn quantization_params(&mut self, sequence: &'o SequenceHeaderObu) -> SynthesizerResult<()> {
        self.f(8, self.obu.quantization_params.base_q_idx)?;
        self.read_delta_q(self.obu.quantization_params.delta_q_y_dc)?;
        if sequence.num_planes > 1 {
            if sequence.color_config.separate_uv_delta_q {
                self.f(1, self.obu.quantization_params.diff_uv_delta)?;
            } else if self.obu.quantization_params.diff_uv_delta {
                self.invalid_element_value("diff_uv_delta")?;
            };

            self.read_delta_q(self.obu.quantization_params.delta_q_u_dc)?;
            self.read_delta_q(self.obu.quantization_params.delta_q_u_ac)?;

            if self.obu.quantization_params.diff_uv_delta {
                self.read_delta_q(self.obu.quantization_params.delta_q_v_dc)?;
                self.read_delta_q(self.obu.quantization_params.delta_q_v_ac)?;
            } else {
                if self.obu.quantization_params.delta_q_v_dc != 0 {
                    self.invalid_element_value("delta_q_v_dc")?;
                }
                if self.obu.quantization_params.delta_q_v_ac != 0 {
                    self.invalid_element_value("delta_q_v_ac")?;
                }
            }
        } else {
            if self.obu.quantization_params.delta_q_u_dc != 0 {
                self.invalid_element_value("delta_q_u_dc")?;
            }
            if self.obu.quantization_params.delta_q_u_ac != 0 {
                self.invalid_element_value("delta_q_u_ac")?;
            }
            if self.obu.quantization_params.delta_q_v_dc != 0 {
                self.invalid_element_value("delta_q_v_dc")?;
            }
            if self.obu.quantization_params.delta_q_v_ac != 0 {
                self.invalid_element_value("delta_q_v_ac")?;
            }
        }

        self.f(1, self.obu.quantization_params.using_qmatrix)?;
        if self.obu.quantization_params.using_qmatrix {
            self.f(4, self.obu.quantization_params.qm_y)?;
            self.f(4, self.obu.quantization_params.qm_u)?;

            if !sequence.color_config.separate_uv_delta_q {
                if self.obu.quantization_params.qm_v != self.obu.quantization_params.qm_u {
                    self.invalid_element_value("qm_v")?;
                }
            } else {
                self.f(4, self.obu.quantization_params.qm_v)?;
            }
        }

        Ok(())
    }

    /// Writes AV1 5.9.13. Delta quantizer syntax
    fn read_delta_q(&mut self, delta_q: i32) -> SynthesizerResult<()> {
        self.f(1, delta_q != 0)?;
        if delta_q != 0 {
            self.su(1 + 6, delta_q)?;
        }
        Ok(())
    }

    /// Writes AV1 5.9.14. Segmentation params syntax
    fn segmentation_params(&mut self) -> SynthesizerResult<()> {
        self.f(1, self.obu.segmentation_params.segmentation_enabled)?;
        if self.obu.segmentation_params.segmentation_enabled {
            if self.obu.primary_ref_frame == PRIMARY_REF_NONE {
                if !self.obu.segmentation_params.segmentation_update_map {
                    self.invalid_element_value("segmentation_update_map")?;
                }

                if self.obu.segmentation_params.segmentation_temporal_update {
                    self.invalid_element_value("segmentation_temporal_update")?;
                }

                if !self.obu.segmentation_params.segmentation_update_data {
                    self.invalid_element_value("segmentation_update_data")?;
                }
            } else {
                self.f(1, self.obu.segmentation_params.segmentation_update_map)?;
                if self.obu.segmentation_params.segmentation_update_map {
                    self.f(1, self.obu.segmentation_params.segmentation_temporal_update)?;
                }
                self.f(1, self.obu.segmentation_params.segmentation_temporal_update)?;
            }

            if self.obu.segmentation_params.segmentation_update_data {
                for i in 0..MAX_SEGMENTS {
                    for j in 0..SEG_LVL_MAX {
                        let feature_enabled = self.obu.segmentation_params.feature_enabled[i][j];
                        self.f(1, feature_enabled)?;

                        if feature_enabled {
                            let bits_to_read = FEATURE_BITS[j] as usize;
                            let limit = FEATURE_MAX[j];
                            let signed = FEATURE_SIGNED[j];

                            let value = i32::from(self.obu.segmentation_params.feature_data[i][j]);
                            if signed {
                                let clipped_value = clip3(-limit, limit, value);
                                self.su(bits_to_read + 1, clipped_value)?;
                            } else {
                                let clipped_value = clip3(0, limit, value);
                                self.f(bits_to_read, u32::try_from(clipped_value)?)?;
                            }
                        }
                    }
                }
            } else {
                if self.obu.segmentation_params.feature_enabled
                    != [[false; SEG_LVL_MAX]; MAX_SEGMENTS]
                {
                    self.invalid_element_value("feature_enabled")?;
                }
                if self.obu.segmentation_params.feature_data != [[0; SEG_LVL_MAX]; MAX_SEGMENTS] {
                    self.invalid_element_value("feature_data")?;
                }
            }
        }

        Ok(())
    }

    /// Writes AV1 5.9.15. Tile info syntax
    fn tile_info(&mut self) -> SynthesizerResult<()> {
        // From AV1 5.9.9. Compute image size function
        self.f(1, self.obu.tile_info.uniform_tile_spacing_flag)?;
        if self.obu.tile_info.uniform_tile_spacing_flag {
            if self.obu.tile_info.tile_cols != 1
                && self.obu.tile_info.tile_cols_log2 != 0
                && self.obu.tile_info.tile_rows != 1
                && self.obu.tile_info.tile_rows_log2 != 0
            {
                // TODO: Allow more then single tile
                log::error!("Only 1x1 tiles frame is currently supported");
                return Err(SynthesizerError::Unsupported);
            }

            const INCREMENT_TILE_COLS_LOG2: u32 = 0;
            const INCREMENT_TILE_ROWS_LOG2: u32 = 0;

            self.f(1, INCREMENT_TILE_COLS_LOG2)?;
            self.f(1, INCREMENT_TILE_ROWS_LOG2)?;
        } else {
            // TODO
            log::error!("Only uniformly sized tiles are currently supported");
            return Err(SynthesizerError::Unsupported);
        }

        Ok(())
    }

    /// Writes AV1 5.9.17. Quantizer index delta parameters syntax
    fn delta_q_params(&mut self) -> SynthesizerResult<()> {
        if self.obu.quantization_params.base_q_idx > 0 {
            self.f(1, self.obu.quantization_params.delta_q_present)?;

            if self.obu.quantization_params.delta_q_present {
                self.f(2, self.obu.quantization_params.delta_q_res)?;
            }
        } else if self.obu.quantization_params.delta_q_present {
            self.invalid_element_value("delta_q_present")?;
        }

        Ok(())
    }

    /// Writes AV1 5.9.18. Loop filter delta parameters syntax
    fn delta_lf_params(&mut self) -> SynthesizerResult<()> {
        if self.obu.quantization_params.delta_q_present {
            if self.obu.allow_intrabc {
                self.f(1, self.obu.loop_filter_params.delta_lf_present)?;

                if self.obu.loop_filter_params.delta_lf_present {
                    self.f(2, self.obu.loop_filter_params.delta_lf_res)?;
                    self.f(1, self.obu.loop_filter_params.delta_lf_multi)?;
                }
            } else if self.obu.loop_filter_params.delta_lf_present {
                self.invalid_element_value("delta_lf_present")?;
            }
        }

        Ok(())
    }

    /// Writes AV1 5.9.19. CDEF params syntax
    fn cdef_params(&mut self, sequence: &'o SequenceHeaderObu) -> SynthesizerResult<()> {
        if self.obu.coded_lossless || self.obu.allow_intrabc || !sequence.enable_cdef {
            if self.obu.cdef_params.cdef_bits != 0 {
                self.invalid_element_value("cdef_bits")?;
            }

            if self.obu.cdef_params.cdef_y_pri_strength[0] != 0 {
                self.invalid_element_value("cdef_y_pri_strength")?;
            }

            if self.obu.cdef_params.cdef_y_sec_strength[0] != 0 {
                self.invalid_element_value("cdef_y_sec_strength")?;
            }

            if self.obu.cdef_params.cdef_uv_pri_strength[0] != 0 {
                self.invalid_element_value("cdef_uv_pri_strength")?;
            }

            if self.obu.cdef_params.cdef_uv_sec_strength[0] != 0 {
                self.invalid_element_value("cdef_uv_sec_strength")?;
            }

            if self.obu.cdef_params.cdef_damping != 3 {
                self.invalid_element_value("cdef_damping")?;
            }

            return Ok(());
        }

        self.f(2, self.obu.cdef_params.cdef_damping - 3)?;
        self.f(2, self.obu.cdef_params.cdef_bits)?;

        for i in 0..(1 << self.obu.cdef_params.cdef_bits) {
            self.f(4, self.obu.cdef_params.cdef_y_pri_strength[i])?;

            let mut cdef_y_sec_strength = self.obu.cdef_params.cdef_y_sec_strength[i];
            if cdef_y_sec_strength == 4 {
                cdef_y_sec_strength -= 1;
            }

            self.f(2, cdef_y_sec_strength)?;

            if sequence.num_planes > 1 {
                self.f(4, self.obu.cdef_params.cdef_uv_pri_strength[i])?;

                let mut cdef_uv_sec_strength = self.obu.cdef_params.cdef_uv_sec_strength[i];
                if cdef_uv_sec_strength == 4 {
                    cdef_uv_sec_strength -= 1;
                }

                self.f(2, cdef_uv_sec_strength)?;
            }
        }

        Ok(())
    }

    /// Writes AV1 5.9.20. Loop restoration params syntax
    fn lr_params(&mut self, sequence: &'o SequenceHeaderObu) -> SynthesizerResult<()> {
        if self.obu.all_lossless || self.obu.allow_intrabc || !sequence.enable_restoration {
            if self.obu.loop_restoration_params.frame_restoration_type
                != [FrameRestorationType::None; MAX_NUM_PLANES]
            {
                self.invalid_element_value("frame_restoration_type")?;
            }

            if self.obu.loop_restoration_params.uses_lr {
                self.invalid_element_value("uses_lr")?;
            }

            return Ok(());
        }

        let mut uses_lr = false;
        let mut uses_chroma_lr = false;
        for i in 0..MAX_NUM_PLANES {
            let lr_type = self.obu.loop_restoration_params.frame_restoration_type[i];
            self.f(2, lr_type as u32)?;

            if lr_type != FrameRestorationType::None {
                uses_lr = true;

                if i > 0 {
                    uses_chroma_lr = true;
                }
            }
        }

        if uses_lr ^ self.obu.loop_restoration_params.uses_lr {
            self.invalid_element_value("uses_lr")?;
        }
        if uses_chroma_lr ^ self.obu.loop_restoration_params.uses_chroma_lr {
            self.invalid_element_value("uses_chroma_lr")?;
        }

        if uses_lr {
            if sequence.use_128x128_superblock {
                if self.obu.loop_restoration_params.lr_unit_shift == 0 {
                    self.invalid_element_value("lr_unit_shift")?;
                }

                self.f(1, self.obu.loop_restoration_params.lr_unit_shift - 1)?;
            } else {
                self.f(1, self.obu.loop_restoration_params.lr_unit_shift)?;
                if self.obu.loop_restoration_params.lr_unit_shift != 0 {
                    self.f(1, self.obu.loop_restoration_params.lr_unit_shift > 1)?;
                }
            }

            if sequence.color_config.subsampling_x
                && sequence.color_config.subsampling_y
                && uses_chroma_lr
            {
                self.f(1, self.obu.loop_restoration_params.lr_uv_shift)?;
            } else if self.obu.loop_restoration_params.lr_uv_shift != 0 {
                self.invalid_element_value("lr_uv_shift")?;
            }
        }

        Ok(())
    }

    /// Writes AV1 5.9.21. TX mode syntax
    fn read_tx_mode(&mut self) -> SynthesizerResult<()> {
        if self.obu.coded_lossless {
            if self.obu.tx_mode != TxMode::Only4x4 {
                self.invalid_element_value("TxMode")?;
            }
        } else {
            self.f(1, self.obu.tx_mode_select)?;

            if (self.obu.tx_mode_select != 0 && self.obu.tx_mode != TxMode::Select)
                || (self.obu.tx_mode_select == 0 && self.obu.tx_mode != TxMode::Largest)
            {
                self.invalid_element_value("TxMode")?;
            }
        }

        Ok(())
    }

    /// Writes AV1 5.9.22. Skip mode params syntax
    fn skip_mode_params(&mut self) -> SynthesizerResult<()> {
        // TODO: Implement if needed
        Ok(())
    }

    /// Writes AV1 5.9.23. Frame reference mode syntax
    fn frame_reference_mode(&mut self) -> SynthesizerResult<()> {
        if self.obu.frame_is_intra {
            if self.obu.reference_select {
                self.invalid_element_value("reference_select")?;
            }
        } else {
            self.f(1, self.obu.reference_select)?;
        }

        Ok(())
    }

    /// Writes AV1 5.9.23. Frame reference mode syntax
    fn global_motion_params(&mut self) -> SynthesizerResult<()> {
        if self.obu.frame_is_intra {
            return Ok(());
        }

        for ref_ in ReferenceFrameType::Last as usize..=ReferenceFrameType::AltRef as usize {
            let is_global = self.obu.global_motion_params.is_global[ref_];
            let is_rot_zoom = self.obu.global_motion_params.is_rot_zoom[ref_];
            let is_translation = self.obu.global_motion_params.is_translation[ref_];
            let gm_type = self.obu.global_motion_params.gm_type[ref_];

            let expected_type = match (is_global, is_rot_zoom, is_translation) {
                (false, _, _) => WarpModelType::Identity,
                (true, true, _) => WarpModelType::RotZoom,
                (true, false, true) => WarpModelType::Translation,
                (true, false, false) => WarpModelType::Affine,
            };

            if expected_type != gm_type {
                self.invalid_element_value("GmType")?;
            }

            self.f(1, is_global)?;
            if is_global {
                self.f(1, is_rot_zoom)?;
                if is_rot_zoom {
                } else {
                    self.f(1, is_translation)?;
                }
            }

            if gm_type >= WarpModelType::RotZoom {
                self.read_global_param(gm_type, ref_, 2)?;
                self.read_global_param(gm_type, ref_, 3)?;
                if gm_type == WarpModelType::Affine {
                    self.read_global_param(gm_type, ref_, 4)?;
                    self.read_global_param(gm_type, ref_, 5)?;
                }
            }

            if gm_type >= WarpModelType::Translation {
                self.read_global_param(gm_type, ref_, 0)?;
                self.read_global_param(gm_type, ref_, 1)?;
            }
        }

        Ok(())
    }

    /// Writes AV1 5.9.25. Global param syntax
    fn read_global_param(
        &mut self,
        _gm_type: WarpModelType,
        _ref_: usize,
        _idx: u32,
    ) -> SynthesizerResult<()> {
        // TODO
        log::warn!(
            "Syntax element read_global_param() is not currently supported. Use GmType=IDENTITY"
        );
        Err(SynthesizerError::Unsupported)
    }

    /// Writes AV1 5.9.30. Film grain params syntax
    fn film_grain_params(&mut self, sequence: &'o SequenceHeaderObu) -> SynthesizerResult<()> {
        if !sequence.film_grain_params_present || (!self.obu.show_frame && !self.obu.showable_frame)
        {
            return Ok(());
        }

        self.f(1, self.obu.film_grain_params.apply_grain)?;
        if !self.obu.film_grain_params.apply_grain {
            return Ok(());
        }

        Err(SynthesizerError::Unsupported)
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::codec::av1::parser::CdefParams;
    use crate::codec::av1::parser::ChromaSamplePosition;
    use crate::codec::av1::parser::ColorConfig;
    use crate::codec::av1::parser::TileInfo;
    use crate::codec::av1::parser::MAX_TILE_COLS;
    use crate::codec::av1::parser::MAX_TILE_ROWS;

    #[test]
    fn sequence_header_obu_test25fps() {
        // Extraced from ./src/codec/av1/test_data/test-25fps.ivf.av1
        const SEQ_HDR_RAW: [u8; 13] = [
            0x0a, 0x0b, 0x00, 0x00, 0x00, 0x04, 0x3c, 0xff, 0xbd, 0xff, 0xf9, 0x80, 0x40,
        ];

        let seq_hdr = SequenceHeaderObu {
            obu_header: ObuHeader {
                obu_type: ObuType::SequenceHeader,
                extension_flag: false,
                has_size_field: true,
                temporal_id: 0,
                spatial_id: 0,
            },

            seq_profile: Profile::Profile0,
            num_planes: 3,
            still_picture: false,
            reduced_still_picture_header: false,
            timing_info_present_flag: false,
            initial_display_delay_present_flag: false,
            operating_points_cnt_minus_1: 0,
            frame_width_bits_minus_1: 8,
            frame_height_bits_minus_1: 7,
            max_frame_width_minus_1: 319,
            max_frame_height_minus_1: 239,
            frame_id_numbers_present_flag: false,
            use_128x128_superblock: true,
            enable_filter_intra: true,
            enable_intra_edge_filter: true,
            enable_interintra_compound: true,
            enable_masked_compound: true,
            enable_warped_motion: true,
            enable_dual_filter: true,
            enable_order_hint: true,
            enable_jnt_comp: true,
            enable_ref_frame_mvs: true,
            seq_choose_screen_content_tools: true,
            seq_force_screen_content_tools: SELECT_SCREEN_CONTENT_TOOLS as u32,
            seq_choose_integer_mv: true,
            seq_force_integer_mv: SELECT_INTEGER_MV as u32,
            order_hint_bits_minus_1: 6,
            order_hint_bits: 7,
            enable_superres: false,
            enable_cdef: true,
            enable_restoration: true,
            color_config: ColorConfig {
                high_bitdepth: false,
                mono_chrome: false,
                color_description_present_flag: false,
                color_range: false,
                subsampling_x: true,
                subsampling_y: true,
                chroma_sample_position: ChromaSamplePosition::Unknown,
                separate_uv_delta_q: false,
                ..Default::default()
            },
            film_grain_params_present: false,

            ..Default::default()
        };

        let mut buf = Vec::<u8>::new();
        Synthesizer::<'_, SequenceHeaderObu, _>::synthesize(&seq_hdr, &mut buf).unwrap();
        assert_eq!(buf, SEQ_HDR_RAW);
    }

    #[test]
    fn sequence_header_obu_av1_annexb() {
        // Extraced from: ./src/codec/av1/test_data/av1-annexb.ivf.av1
        const SEQ_HDR_RAW: [u8; 12] = [
            0x0a, 0x0a, 0x00, 0x00, 0x00, 0x02, 0xaf, 0xff, 0xbf, 0xff, 0x30, 0x08,
        ];

        let seq_hdr = SequenceHeaderObu {
            obu_header: ObuHeader {
                obu_type: ObuType::SequenceHeader,
                extension_flag: false,
                has_size_field: true,
                temporal_id: 0,
                spatial_id: 0,
            },

            seq_profile: Profile::Profile0,
            num_planes: 3,
            still_picture: false,
            reduced_still_picture_header: false,
            timing_info_present_flag: false,
            initial_display_delay_present_flag: false,
            operating_points_cnt_minus_1: 0,
            frame_width_bits_minus_1: 5,
            frame_height_bits_minus_1: 5,
            max_frame_width_minus_1: 63,
            max_frame_height_minus_1: 63,
            frame_id_numbers_present_flag: false,
            use_128x128_superblock: true,
            enable_filter_intra: true,
            enable_intra_edge_filter: true,
            enable_interintra_compound: true,
            enable_masked_compound: true,
            enable_warped_motion: true,
            enable_dual_filter: true,
            enable_order_hint: true,
            enable_jnt_comp: true,
            enable_ref_frame_mvs: true,
            seq_choose_screen_content_tools: true,
            seq_force_screen_content_tools: SELECT_SCREEN_CONTENT_TOOLS as u32,
            seq_choose_integer_mv: true,
            seq_force_integer_mv: SELECT_INTEGER_MV as u32,
            order_hint_bits_minus_1: 6,
            order_hint_bits: 7,
            enable_superres: false,
            enable_cdef: true,
            enable_restoration: true,
            color_config: ColorConfig {
                high_bitdepth: false,
                mono_chrome: false,
                color_description_present_flag: false,
                color_range: false,
                subsampling_x: true,
                subsampling_y: true,
                chroma_sample_position: ChromaSamplePosition::Unknown,
                separate_uv_delta_q: false,
                ..Default::default()
            },
            film_grain_params_present: false,

            ..Default::default()
        };

        let mut buf = Vec::<u8>::new();
        Synthesizer::<'_, SequenceHeaderObu, _>::synthesize(&seq_hdr, &mut buf).unwrap();
        assert_eq!(buf, SEQ_HDR_RAW);
    }

    #[test]
    fn temporal_delim_obu() {
        const TD_RAW: [u8; 2] = [0x12, 0x00];

        let td = TemporalDelimiterObu {
            obu_header: ObuHeader {
                obu_type: ObuType::TemporalDelimiter,
                extension_flag: false,
                has_size_field: true,
                temporal_id: 0,
                spatial_id: 0,
            },
        };

        let mut buf = Vec::<u8>::new();
        Synthesizer::<'_, TemporalDelimiterObu, _>::synthesize(&td, &mut buf).unwrap();

        assert_eq!(buf, TD_RAW);
    }

    #[test]
    fn frame_header_obu() {
        let _ = env_logger::try_init();

        const WIDTH: u32 = 512;
        const HEIGHT: u32 = 512;

        let seq = SequenceHeaderObu {
            obu_header: ObuHeader {
                obu_type: ObuType::SequenceHeader,
                extension_flag: false,
                has_size_field: true,
                temporal_id: 0,
                spatial_id: 0,
            },

            seq_profile: Profile::Profile0,

            frame_width_bits_minus_1: 16 - 1,
            frame_height_bits_minus_1: 16 - 1,
            max_frame_width_minus_1: WIDTH - 1,
            max_frame_height_minus_1: HEIGHT - 1,

            seq_force_integer_mv: SELECT_INTEGER_MV as u32,

            enable_order_hint: true,
            order_hint_bits: 8,
            order_hint_bits_minus_1: 7,
            num_planes: 3,

            color_config: ColorConfig {
                subsampling_x: true,
                subsampling_y: true,
                ..Default::default()
            },

            ..Default::default()
        };

        let frame = FrameHeaderObu {
            obu_header: ObuHeader {
                obu_type: ObuType::FrameHeader,
                extension_flag: false,
                has_size_field: true,
                temporal_id: 0,
                spatial_id: 0,
            },

            frame_type: FrameType::KeyFrame,
            frame_is_intra: true,
            primary_ref_frame: PRIMARY_REF_NONE,
            refresh_frame_flags: 0xff,
            error_resilient_mode: true,

            reduced_tx_set: true,
            tx_mode_select: 1,
            tx_mode: TxMode::Select,

            tile_info: TileInfo {
                uniform_tile_spacing_flag: true,
                tile_cols: 1,
                tile_rows: 1,
                tile_cols_log2: 0,
                tile_rows_log2: 0,
                width_in_sbs_minus_1: {
                    let mut value = [0u32; MAX_TILE_COLS];
                    value[0] = WIDTH / 64 - 1;
                    value
                },
                height_in_sbs_minus_1: {
                    let mut value = [0u32; MAX_TILE_ROWS];
                    value[0] = HEIGHT / 64 - 1;
                    value
                },
                ..Default::default()
            },

            cdef_params: CdefParams {
                cdef_damping: 3,
                ..Default::default()
            },

            superres_denom: SUPERRES_NUM as u32,
            upscaled_width: WIDTH,
            frame_width: WIDTH,
            frame_height: HEIGHT,
            render_width: WIDTH,
            render_height: HEIGHT,

            ..Default::default()
        };

        let mut buf = Vec::<u8>::new();
        Synthesizer::<'_, SequenceHeaderObu, _>::synthesize(&seq, &mut buf).unwrap();

        Synthesizer::<'_, FrameHeaderObu, _>::synthesize(&frame, &seq, &mut buf).unwrap();

        // TODO actual test

        let write_to_file = std::option_env!("CROS_CODECS_TEST_WRITE_TO_FILE") == Some("true");
        if write_to_file {
            let mut out = std::fs::File::create("frame_header_obu.av1").unwrap();
            out.write_all(&buf).unwrap();
            out.flush().unwrap();
        }
    }
}
