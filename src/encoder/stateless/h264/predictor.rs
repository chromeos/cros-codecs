// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::rc::Rc;

use log::trace;

use crate::codec::h264::parser::Level;
use crate::codec::h264::parser::Pps;
use crate::codec::h264::parser::PpsBuilder;
use crate::codec::h264::parser::Profile;
use crate::codec::h264::parser::SliceHeaderBuilder;
use crate::codec::h264::parser::SliceType;
use crate::codec::h264::parser::Sps;
use crate::codec::h264::parser::SpsBuilder;
use crate::codec::h264::synthesizer::Synthesizer;
use crate::encoder::stateless::h264::BackendRequest;
use crate::encoder::stateless::h264::DpbEntry;
use crate::encoder::stateless::h264::DpbEntryMeta;
use crate::encoder::stateless::h264::EncoderConfig;
use crate::encoder::stateless::h264::IsReference;
use crate::encoder::stateless::predictor::LowDelay;
use crate::encoder::stateless::predictor::LowDelayDelegate;
use crate::encoder::stateless::EncodeError;
use crate::encoder::stateless::EncodeResult;
use crate::encoder::stateless::FrameMetadata;
use crate::encoder::RateControl;
use crate::encoder::Tunings;

pub(crate) const MIN_QP: u8 = 1;
pub(crate) const MAX_QP: u8 = 51;

pub(crate) struct LowDelayH264Delegate {
    /// Current sequence SPS
    sps: Option<Rc<Sps>>,
    /// Current sequence PPS
    pps: Option<Rc<Pps>>,

    // True if SPS or PPS changed and should reappear in the bitstream
    update_params_sets: bool,

    /// Encoder config
    config: EncoderConfig,
}

pub(crate) type LowDelayH264<Picture, Reference> = LowDelay<
    Picture,
    DpbEntry<Reference>,
    LowDelayH264Delegate,
    BackendRequest<Picture, Reference>,
>;

impl<Picture, Reference> LowDelayH264<Picture, Reference> {
    pub(super) fn new(config: EncoderConfig, limit: u16) -> Self {
        Self {
            queue: Default::default(),
            references: Default::default(),
            counter: 0,
            limit,
            tunings: config.initial_tunings.clone(),
            delegate: LowDelayH264Delegate {
                config,
                update_params_sets: false,
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

        let mut sps = SpsBuilder::new()
            .seq_parameter_set_id(0)
            .profile_idc(config.profile);

        // H.264 Table 6-1
        sps = match config.profile {
            // 4:2:2 subsampling
            Profile::High422P => sps.chroma_format_idc(2),
            // 4:2:0 subsampling
            _ => sps.chroma_format_idc(1),
        };

        let sps = sps
            .level_idc(config.level)
            .max_frame_num(self.limit as u32)
            .pic_order_cnt_type(0)
            .max_pic_order_cnt_lsb(self.limit as u32 * 2)
            .max_num_ref_frames(1)
            .frame_mbs_only_flag(true)
            // H264 spec Table A-4
            .direct_8x8_inference_flag(config.level >= Level::L3)
            .resolution(config.resolution.width, config.resolution.height)
            .bit_depth_luma(8)
            .bit_depth_chroma(8)
            .aspect_ratio(1, 1)
            .timing_info(1, self.tunings.framerate * 2, false)
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
            .pic_init_qp(init_qp)
            .deblocking_filter_control_present_flag(true)
            .num_ref_idx_l0_default_active(1)
            // Unused, P frame relies only on list0
            .num_ref_idx_l1_default_active_minus1(0)
            .build();

        self.delegate.sps = Some(sps);
        self.delegate.pps = Some(pps);
        self.delegate.update_params_sets = true;
    }
}

impl<Picture, Reference>
    LowDelayDelegate<Picture, DpbEntry<Reference>, BackendRequest<Picture, Reference>>
    for LowDelayH264<Picture, Reference>
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

        let sps = self
            .delegate
            .sps
            .clone()
            .ok_or(EncodeError::InvalidInternalState)?;
        let pps = self
            .delegate
            .pps
            .clone()
            .ok_or(EncodeError::InvalidInternalState)?;

        let dpb_meta = DpbEntryMeta {
            poc: ((self.counter * 2) & 0xffff) as u16,
            frame_num: self.counter as u32,
            is_reference: IsReference::ShortTerm,
        };

        let header = SliceHeaderBuilder::new(&pps)
            .slice_type(SliceType::I)
            .first_mb_in_slice(0)
            .pic_order_cnt_lsb(dpb_meta.poc)
            .build();

        let mut headers = vec![];
        if idr || self.delegate.update_params_sets {
            Synthesizer::<Sps, &mut Vec<u8>>::synthesize(3, &sps, &mut headers, true)?;
            Synthesizer::<Pps, &mut Vec<u8>>::synthesize(3, &pps, &mut headers, true)?;
            self.delegate.update_params_sets = false;
        }

        let num_macroblocks =
            ((sps.pic_width_in_mbs_minus1 + 1) * (sps.pic_height_in_map_units_minus1 + 1)) as usize;

        let request = BackendRequest {
            sps,
            pps,
            header,
            input,
            input_meta,
            dpb_meta,
            // This frame is IDR, therefore it has no references
            ref_list_0: vec![],
            ref_list_1: vec![],

            // I frame is every `self.limit` is requested
            intra_period: self.limit as u32,
            // There is no B frames between I and P frames
            ip_period: 0,

            num_macroblocks,

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

        let sps = self
            .delegate
            .sps
            .clone()
            .ok_or(EncodeError::InvalidInternalState)?;
        let pps = self
            .delegate
            .pps
            .clone()
            .ok_or(EncodeError::InvalidInternalState)?;

        let dpb_meta = DpbEntryMeta {
            poc: ((self.counter * 2) & 0xffff) as u16,
            frame_num: self.counter as u32,
            is_reference: IsReference::ShortTerm,
        };

        let header = SliceHeaderBuilder::new(&pps)
            .slice_type(SliceType::P)
            .first_mb_in_slice(0)
            .pic_order_cnt_lsb(dpb_meta.poc)
            .build();

        let mut headers = Vec::new();
        if self.delegate.update_params_sets {
            Synthesizer::<Sps, &mut Vec<u8>>::synthesize(3, &sps, &mut headers, true)?;
            Synthesizer::<Pps, &mut Vec<u8>>::synthesize(3, &pps, &mut headers, true)?;
            self.delegate.update_params_sets = false;
        }

        let num_macroblocks =
            ((sps.pic_width_in_mbs_minus1 + 1) * (sps.pic_height_in_map_units_minus1 + 1)) as usize;

        let request = BackendRequest {
            sps,
            pps,
            header,
            input,
            input_meta,
            dpb_meta,
            ref_list_0,
            ref_list_1: vec![], // No future references

            // I frame is every `self.limit` is requested
            intra_period: self.limit as u32,
            // There is no B frames between I and P frames
            ip_period: 0,

            num_macroblocks,

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
