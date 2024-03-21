// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::collections::VecDeque;
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
use crate::encoder::stateless::EncodeError;
use crate::encoder::stateless::EncodeResult;
use crate::encoder::stateless::FrameMetadata;
use crate::encoder::stateless::Predictor;

/// Available predictors and initialization parameters
#[derive(Clone)]
pub enum PredictionStructure {
    /// Simplest prediction structure, suitable eg. for RTC. IDR is produced at the start of
    /// the stream and every time when [`limit`] frames are reached. IDR is built with SPS, PPS
    /// and frame with single I slice. Following IDR frames are single P slice frames referencing
    /// maximum [`tail`] previous frames.
    LowDelay { tail: u16, limit: u16 },
}

/// Implementation of [`LowDelay`] prediction structure. See [`LowDelay`] for details.
///
/// [`LowDelay`]: PredictionStructure::LowDelay
pub(super) struct LowDelay<P, R> {
    /// Current frame in the sequence counter
    counter: u16,
    /// Limit of frames in the sequence
    limit: u16,
    /// Target number of reference frames that an interframe should have
    tail: u16,

    /// Queue of pending frames to be encoded
    queue: VecDeque<(P, FrameMetadata)>,

    /// The currently held frames in POC increasing order.
    dpb: VecDeque<Rc<DpbEntry<R>>>,

    /// Current sequence SPS
    sps: Option<Rc<Sps>>,
    /// Current sequence PPS
    pps: Option<Rc<Pps>>,

    /// Encoder config
    config: EncoderConfig,
}

impl<P, R> LowDelay<P, R> {
    pub(super) fn new(config: EncoderConfig) -> Self {
        let (tail, limit) = match config.pred_structure {
            PredictionStructure::LowDelay { tail, limit } => (tail, limit),
        };

        Self {
            counter: 0,
            limit,
            tail,
            queue: Default::default(),
            dpb: Default::default(),
            sps: None,
            pps: None,
            config,
        }
    }
}

impl<Picture, Reference> LowDelay<Picture, Reference> {
    fn new_sequence(&mut self) {
        trace!("beginning new sequence");
        let mut sps = SpsBuilder::new()
            .seq_parameter_set_id(0)
            .profile_idc(self.config.profile);

        // H.264 Table 6-1
        sps = match self.config.profile {
            // 4:2:2 subsampling
            Profile::High422P => sps.chroma_format_idc(2),
            // 4:2:0 subsampling
            _ => sps.chroma_format_idc(1),
        };

        let sps = sps
            .level_idc(self.config.level)
            .max_frame_num(self.limit as u32)
            .pic_order_cnt_type(0)
            .max_pic_order_cnt_lsb(self.limit as u32 * 2)
            .max_num_ref_frames(self.tail as u32 + 1)
            .frame_mbs_only_flag(true)
            // H264 spec Table A-4
            .direct_8x8_inference_flag(self.config.level >= Level::L3)
            .resolution(self.config.resolution.width, self.config.resolution.height)
            .bit_depth_luma(8)
            .bit_depth_chroma(8)
            .aspect_ratio(1, 1)
            .timing_info(1, self.config.framerate * 2, false)
            .build();

        let pps = PpsBuilder::new(Rc::clone(&sps))
            .pic_parameter_set_id(0)
            .pic_init_qp(self.config.default_qp)
            .deblocking_filter_control_present_flag(true)
            .num_ref_idx_l0_default_active(self.tail as u8)
            // Unused, P frame relies only on list0
            .num_ref_idx_l1_default_active_minus1(0)
            .build();

        self.dpb.clear();
        self.sps = Some(sps);
        self.pps = Some(pps);
    }

    fn request_idr(
        &mut self,
        input: Picture,
        input_meta: FrameMetadata,
    ) -> EncodeResult<Vec<BackendRequest<Picture, Reference>>> {
        // Begin new sequence and start with I frame and no references.
        self.counter = 0;
        self.new_sequence();

        // SAFETY: SPS and PPS were initialized by [`Self::new_sequence()`]
        let sps = self.sps.clone().unwrap();
        let pps = self.pps.clone().unwrap();

        let dpb_meta = DpbEntryMeta {
            poc: self.counter * 2,
            frame_num: self.counter as u32,
            is_reference: IsReference::ShortTerm,
        };

        let header = SliceHeaderBuilder::new(&pps)
            .slice_type(SliceType::I)
            .first_mb_in_slice(0)
            .pic_order_cnt_lsb(dpb_meta.poc)
            .build();

        self.counter += 1;

        let mut headers = vec![];
        Synthesizer::<Sps, &mut Vec<u8>>::synthesize(3, &sps, &mut headers, true)?;
        Synthesizer::<Pps, &mut Vec<u8>>::synthesize(3, &pps, &mut headers, true)?;

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

            is_idr: true,
            bitrate: self.config.bitrate.clone(),

            coded_output: headers,
        };

        Ok(vec![request])
    }

    fn request_interframe(
        &mut self,
        input: Picture,
        input_meta: FrameMetadata,
    ) -> Vec<BackendRequest<Picture, Reference>> {
        let mut ref_list_0 = vec![];

        // Use all avaiable reference frames in DPB. Their number is limited by the parameter
        for reference in self.dpb.iter().rev() {
            ref_list_0.push(Rc::clone(reference));
        }

        // SAFETY: SPS and PPS were initialized during IDR request
        let sps = self.sps.clone().unwrap();
        let pps = self.pps.clone().unwrap();

        let dpb_meta = DpbEntryMeta {
            poc: self.counter * 2,
            frame_num: self.counter as u32,
            is_reference: IsReference::ShortTerm,
        };

        let header = SliceHeaderBuilder::new(&pps)
            .slice_type(SliceType::P)
            .first_mb_in_slice(0)
            .pic_order_cnt_lsb(dpb_meta.poc)
            .build();

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
            bitrate: self.config.bitrate.clone(),

            coded_output: vec![],
        };

        self.counter += 1;

        // Remove obselete reference frames
        while self.dpb.len() > self.tail as usize - 1 {
            self.dpb.pop_front();
        }

        vec![request]
    }

    fn next_request(&mut self) -> EncodeResult<Vec<BackendRequest<Picture, Reference>>> {
        self.counter %= self.limit;

        match self.queue.pop_front() {
            // Nothing to do. Quit.
            None => Ok(Vec::new()),

            // If first frame in the sequence or forced IDR then create IDR request.
            Some((input, meta)) if self.counter == 0 || meta.force_keyframe => {
                Ok(self.request_idr(input, meta)?)
            }

            // There is no enough frames in the DPB
            Some((input, meta))
                if self.dpb.is_empty()
                    || self.dpb.len() < (self.counter.min(self.tail) as usize) =>
            {
                self.queue.push_front((input, meta));
                Ok(Vec::new())
            }

            Some((input, meta)) => {
                // Make sure that reference frames in DPB is consistent
                assert!(self.dpb.back().unwrap().meta.frame_num == self.counter as u32 - 1);
                Ok(self.request_interframe(input, meta))
            }
        }
    }
}

impl<Picture, Reference> Predictor<Picture, DpbEntry<Reference>, BackendRequest<Picture, Reference>>
    for LowDelay<Picture, Reference>
{
    fn new_frame(
        &mut self,
        input: Picture,
        frame_metadata: FrameMetadata,
    ) -> EncodeResult<Vec<BackendRequest<Picture, Reference>>> {
        // Add new frame in the request queue and request new encoding if possible
        self.queue.push_back((input, frame_metadata));
        self.next_request()
    }

    fn reconstructed(
        &mut self,
        recon: DpbEntry<Reference>,
    ) -> EncodeResult<Vec<BackendRequest<Picture, Reference>>> {
        // Add new reconstructed surface and request next encoding if possible
        self.dpb.push_back(Rc::new(recon));
        self.next_request()
    }

    fn drain(&mut self) -> EncodeResult<Vec<BackendRequest<Picture, Reference>>> {
        // [`LowDelay`] will not hold any frames, therefore the drain function shall never be called.
        Err(EncodeError::InvalidInternalState)
    }
}
