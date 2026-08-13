// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::rc::Rc;

use log::trace;

use crate::codec::h265::parser::Pps;
use crate::codec::h265::parser::PpsBuilder;
use crate::codec::h265::parser::ShortTermRefPicSet;
use crate::codec::h265::parser::SliceType;
use crate::codec::h265::parser::Sps;
use crate::codec::h265::parser::SpsBuilder;
use crate::codec::h265::parser::Vps;
use crate::codec::h265::parser::VpsBuilder;
use crate::codec::h265::synthesizer::Synthesizer;
use crate::encoder::stateless::h265::BackendRequest;
use crate::encoder::stateless::h265::DpbEntry;
use crate::encoder::stateless::h265::DpbEntryMeta;
use crate::encoder::stateless::h265::EncoderConfig;
use crate::encoder::stateless::h265::IsReference;
use crate::encoder::stateless::h265::NUH_TEMPORAL_ID_PLUS1;
use crate::encoder::stateless::predictor::LowDelay;
use crate::encoder::stateless::predictor::LowDelayDelegate;
use crate::encoder::stateless::FrameMetadata;
use crate::encoder::EncodeError;
use crate::encoder::EncodeResult;
use crate::encoder::RateControl;
use crate::encoder::Tunings;

pub(crate) const MIN_QP: u8 = 1;
pub(crate) const MAX_QP: u8 = 51;

/// The encoder never reorders and keeps at most one reference picture, therefore the DPB holds
/// no more than one picture besides the currently decoded one. Note that this value must not be
/// lower than `NumNegativePics` of any short term reference picture set, see H.265 7.4.3.2.1.
const MAX_DEC_PIC_BUFFERING_MINUS1: u8 = 1;

pub(crate) struct LowDelayH265Delegate {
    /// Current sequence VPS
    vps: Option<Rc<Vps>>,
    /// Current sequence SPS
    sps: Option<Rc<Sps>>,
    /// Current sequence PPS
    pps: Option<Rc<Pps>>,

    // True if VPS, SPS or PPS changed and should reappear in the bitstream
    update_params_sets: bool,

    /// Encoder config
    config: EncoderConfig,
}

pub(crate) type LowDelayH265<Picture, Reference> = LowDelay<
    Picture,
    DpbEntry<Reference>,
    LowDelayH265Delegate,
    BackendRequest<Picture, Reference>,
>;

impl<Picture, Reference> LowDelayH265<Picture, Reference> {
    pub(super) fn new(config: EncoderConfig, limit: u16) -> Self {
        Self {
            queue: Default::default(),
            references: Default::default(),
            counter: 0,
            limit,
            tunings: config.initial_tunings.clone(),
            delegate: LowDelayH265Delegate {
                config,
                update_params_sets: false,
                vps: None,
                sps: None,
                pps: None,
            },
            tunings_queue: Default::default(),
            _phantom: Default::default(),
        }
    }

    fn new_sequence(&mut self) {
        trace!("beginning new sequence");
        let config = &self.delegate.config;

        let vps = VpsBuilder::new()
            .video_parameter_set_id(0)
            .max_layers_minus1(0)
            // A single temporal sub-layer. The synthesizer does not support more.
            .max_sub_layers_minus1(0)
            .general_profile(config.profile)
            .general_level_idc(config.level)
            .max_dec_pic_buffering_minus1(MAX_DEC_PIC_BUFFERING_MINUS1 as u32)
            // Output order is the coding order, there is no reordering nor latency.
            .max_num_reorder_pics(0)
            .max_latency_increase_plus1(0)
            .timing_info(1, self.tunings.framerate)
            .build();

        // The only reference an inter frame ever uses is the picture that immediately precedes
        // it, ie. `DeltaPocS0[0]` is -1 and the set is used by the current picture. Backends
        // write the slice segment headers themselves and refer to this set by index, therefore
        // it has to be signalled in the SPS. See H.265 7.4.8.
        let mut short_term_ref_pic_set =
            ShortTermRefPicSet { num_negative_pics: 1, num_delta_pocs: 1, ..Default::default() };
        short_term_ref_pic_set.delta_poc_s0[0] = -1;
        short_term_ref_pic_set.used_by_curr_pic_s0[0] = true;

        // H.265 Table 6-1, the encoder only supports 4:2:0 subsampling. Must be set before
        // `resolution()`, which derives the conformance window from it.
        let sps = SpsBuilder::new(Rc::clone(&vps))
            .seq_parameter_set_id(0)
            .chroma_format_idc(1)
            .resolution(config.resolution.width, config.resolution.height)
            .bit_depth_luma(8)
            .bit_depth_chroma(8)
            .max_dec_pic_buffering_minus1(MAX_DEC_PIC_BUFFERING_MINUS1)
            .max_num_reorder_pics(0)
            .max_latency_increase_plus1(0)
            .short_term_ref_pic_set(short_term_ref_pic_set)
            .long_term_ref_pics_present_flag(false)
            .timing_info(1, self.tunings.framerate, false)
            .build();

        let min_qp = self.tunings.min_quality.max(MIN_QP as u32);
        let max_qp = self.tunings.max_quality.min(MAX_QP as u32);

        let init_qp = if let RateControl::ConstantQuality(init_qp) = self.tunings.rate_control {
            // Limit QP to valid values
            init_qp.clamp(min_qp, max_qp) as u8
        } else {
            // Pick middle QP for default qp
            ((min_qp + max_qp) / 2) as u8
        };

        let pps = PpsBuilder::new(Rc::clone(&sps))
            .pic_parameter_set_id(0)
            .init_qp(init_qp as i8)
            .deblocking_filter_control_present_flag(true)
            .num_ref_idx_l0_default_active_minus1(0)
            // Unused, P slices rely only on list0
            .num_ref_idx_l1_default_active_minus1(0)
            .build();

        self.delegate.vps = Some(vps);
        self.delegate.sps = Some(sps);
        self.delegate.pps = Some(pps);
        self.delegate.update_params_sets = true;
    }

    /// Synthesizes the active parameter sets into `headers` if they have not been written since
    /// they were last changed.
    fn synthesize_parameter_sets(
        &mut self,
        force: bool,
        headers: &mut Vec<u8>,
    ) -> EncodeResult<()> {
        if !force && !self.delegate.update_params_sets {
            return Ok(());
        }

        let vps = self.delegate.vps.clone().ok_or(EncodeError::InvalidInternalState)?;
        let sps = self.delegate.sps.clone().ok_or(EncodeError::InvalidInternalState)?;
        let pps = self.delegate.pps.clone().ok_or(EncodeError::InvalidInternalState)?;

        Synthesizer::<Vps, &mut Vec<u8>>::synthesize(NUH_TEMPORAL_ID_PLUS1, &vps, headers, true)?;
        Synthesizer::<Sps, &mut Vec<u8>>::synthesize(NUH_TEMPORAL_ID_PLUS1, &sps, headers, true)?;
        Synthesizer::<Pps, &mut Vec<u8>>::synthesize(NUH_TEMPORAL_ID_PLUS1, &pps, headers, true)?;

        self.delegate.update_params_sets = false;

        Ok(())
    }
}

impl<Picture, Reference>
    LowDelayDelegate<Picture, DpbEntry<Reference>, BackendRequest<Picture, Reference>>
    for LowDelayH265<Picture, Reference>
{
    fn request_keyframe(
        &mut self,
        input: Picture,
        input_meta: FrameMetadata,
        idr: bool,
    ) -> EncodeResult<BackendRequest<Picture, Reference>> {
        if idr {
            // Begin new sequence and start with I frame and no references.
            self.new_sequence();
        }

        let mut headers = vec![];
        self.synthesize_parameter_sets(idr, &mut headers)?;

        let sps = self.delegate.sps.clone().ok_or(EncodeError::InvalidInternalState)?;
        let pps = self.delegate.pps.clone().ok_or(EncodeError::InvalidInternalState)?;

        let dpb_meta =
            DpbEntryMeta { poc: self.counter as i32, is_reference: IsReference::ShortTerm };

        let num_ctus = sps.pic_size_in_ctbs_y as usize;

        let request = BackendRequest {
            sps,
            pps,
            slice_type: SliceType::I,
            input,
            input_meta,
            dpb_meta,
            // This frame is a random access point, therefore it has no references
            ref_list_0: vec![],
            ref_list_1: vec![],

            // I frame is every `self.limit` is requested
            intra_period: self.limit as u32,
            // There is no B frames between I and P frames
            ip_period: 1,

            num_ctus,

            is_idr: idr,
            tunings: self.tunings.clone(),

            coded_output: headers,
        };

        Ok(request)
    }

    fn request_interframe(
        &mut self,
        input: Picture,
        input_meta: FrameMetadata,
    ) -> EncodeResult<BackendRequest<Picture, Reference>> {
        let mut ref_list_0 = vec![];

        // Use all avaiable reference frames in DPB. Their number is limited by the parameter
        for reference in self.references.iter().rev() {
            ref_list_0.push(Rc::clone(reference));
        }

        let mut headers = Vec::new();
        self.synthesize_parameter_sets(false, &mut headers)?;

        let sps = self.delegate.sps.clone().ok_or(EncodeError::InvalidInternalState)?;
        let pps = self.delegate.pps.clone().ok_or(EncodeError::InvalidInternalState)?;

        let dpb_meta =
            DpbEntryMeta { poc: self.counter as i32, is_reference: IsReference::ShortTerm };

        let num_ctus = sps.pic_size_in_ctbs_y as usize;

        let request = BackendRequest {
            sps,
            pps,
            slice_type: SliceType::P,
            input,
            input_meta,
            dpb_meta,
            ref_list_0,
            ref_list_1: vec![], // No future references

            // I frame is every `self.limit` is requested
            intra_period: self.limit as u32,
            // There is no B frames between I and P frames
            ip_period: 1,

            num_ctus,

            is_idr: false,
            tunings: self.tunings.clone(),

            coded_output: headers,
        };

        self.references.clear();

        Ok(request)
    }

    fn try_tunings(&self, _tunings: &Tunings) -> EncodeResult<()> {
        Ok(())
    }

    fn apply_tunings(&mut self, _tunings: &Tunings) -> EncodeResult<()> {
        self.new_sequence();
        Ok(())
    }
}
