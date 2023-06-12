// Copyright 2023 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::cell::RefCell;
use std::io::Cursor;
use std::rc::Rc;

use anyhow::anyhow;
use anyhow::Context;

use crate::codec::h265::dpb::Dpb;
use crate::codec::h265::dpb::DpbEntry;
use crate::codec::h265::parser::Nalu;
use crate::codec::h265::parser::NaluType;
use crate::codec::h265::parser::Parser;
use crate::codec::h265::parser::Pps;
use crate::codec::h265::parser::ShortTermRefPicSet;
use crate::codec::h265::parser::Slice;
use crate::codec::h265::parser::SliceHeader;
use crate::codec::h265::parser::Sps;
use crate::codec::h265::picture::PictureData;
use crate::codec::h265::picture::Reference;
use crate::decoder::stateless::private;
use crate::decoder::stateless::DecodeError;
use crate::decoder::stateless::DecodingState;
use crate::decoder::stateless::StatelessBackendResult;
use crate::decoder::stateless::StatelessDecoderBackend;
use crate::decoder::stateless::StatelessDecoderFormatNegotiator;
use crate::decoder::stateless::StatelessVideoDecoder;
use crate::decoder::BlockingMode;
use crate::decoder::DecodedHandle;
use crate::decoder::DecoderEvent;
use crate::decoder::ReadyFramesQueue;
use crate::DecodedFormat;
use crate::Resolution;

const MAX_DPB_SIZE: usize = 16;

/// Stateless backend methods specific to H.265.
pub(crate) trait StatelessH265DecoderBackend: StatelessDecoderBackend<Sps> {
    /// Type used by the backend to represent a picture in the process of being decoded.
    type Picture;

    /// Called when a new SPS is parsed.
    fn new_sequence(&mut self, sps: &Sps) -> StatelessBackendResult<()>;

    /// Called when the decoder determines that a frame or field was found.
    fn new_picture(
        &mut self,
        picture: &PictureData,
        timestamp: u64,
    ) -> StatelessBackendResult<Self::Picture>;

    /// Called by the decoder for every frame or field found.
    fn handle_picture(
        &mut self,
        picture: &mut Self::Picture,
        picture_data: &PictureData,
        sps: &Sps,
        pps: &Pps,
        dpb: &Dpb<Self::Handle>,
        slice: &Slice<&[u8]>,
    ) -> StatelessBackendResult<()>;

    /// Called to dispatch a decode operation to the backend.
    #[allow(clippy::too_many_arguments)]
    fn decode_slice(
        &mut self,
        picture: &mut Self::Picture,
        slice: &Slice<&[u8]>,
        sps: &Sps,
        pps: &Pps,
        dpb: &Dpb<Self::Handle>,
        ref_pic_list0: &[Option<RefPicListEntry<Self::Handle>>; 16],
        ref_pic_list1: &[Option<RefPicListEntry<Self::Handle>>; 16],
    ) -> StatelessBackendResult<()>;

    /// Called when the decoder wants the backend to finish the decoding
    /// operations for `picture`. At this point, `decode_slice` has been called
    /// for all slices.
    ///
    /// This call will assign the ownership of the BackendHandle to the Picture
    /// and then assign the ownership of the Picture to the Handle.
    fn submit_picture(&mut self, picture: Self::Picture) -> StatelessBackendResult<Self::Handle>;
}

/// An entry in the Reference Picture Lists. Unlike H.264, H.265 can use the
/// current picture itself as a reference.
#[derive(Clone)]
pub enum RefPicListEntry<T: crate::decoder::DecodedHandle + Clone> {
    CurrentPicture(PictureData),
    DpbEntry(DpbEntry<T>),
}

enum BumpingType {
    BeforeDecoding,
    AfterDecoding,
}

pub struct Decoder<T, P>
where
    T: DecodedHandle + Clone,
{
    /// A parser to extract bitstream metadata
    parser: Parser,

    /// Whether the decoder should block on decode operations.
    blocking_mode: BlockingMode,

    /// The backend used for hardware acceleration.
    backend: Box<dyn StatelessH265DecoderBackend<Handle = T, Picture = P>>,

    /// The backend used for hardware acceleration.
    // backend: Box<dyn StatelessDecoderBackend<Handle = T, Picture = P>>,
    decoding_state: DecodingState<Sps>,

    /// The current coded resolution
    coded_resolution: Resolution,

    ready_queue: ReadyFramesQueue<T>,

    /// The decoded picture buffer
    dpb: Dpb<T>,

    /// The current active SPS id.
    cur_sps_id: u8,
    /// The current active PPS id.
    cur_pps_id: u8,

    /// The current picture being worked on. It is set during `handle_picture()`
    /// and passed to the accelerator during `submit_picture()`.
    cur_pic: Option<PictureData>,
    /// The current picture representation on the backend side.
    cur_backend_pic: Option<P>,

    // Used to identify first picture in decoding order or first picture that
    // follows an EOS NALU.
    is_first_picture_in_au: bool,

    // Same as PrevTid0Pic in the specification.
    prev_tid_0_pic: Option<PictureData>,

    // Internal variables needed during the decoding process.
    max_pic_order_cnt_lsb: i32,
    curr_delta_poc_msb_present_flag: [bool; MAX_DPB_SIZE],
    foll_delta_poc_msb_present_flag: [bool; MAX_DPB_SIZE],
    num_poc_st_curr_before: usize,
    num_poc_st_curr_after: usize,
    num_poc_st_foll: usize,
    num_poc_lt_curr: usize,
    num_poc_lt_foll: usize,
    poc_st_curr_before: [i32; MAX_DPB_SIZE],
    poc_st_curr_after: [i32; MAX_DPB_SIZE],
    poc_st_foll: [i32; MAX_DPB_SIZE],
    poc_lt_curr: [i32; MAX_DPB_SIZE],
    poc_lt_foll: [i32; MAX_DPB_SIZE],

    // The reference picture set. Derived once per picture.
    ref_pic_set_lt_curr: [Option<DpbEntry<T>>; MAX_DPB_SIZE],
    ref_pic_set_st_curr_after: [Option<DpbEntry<T>>; MAX_DPB_SIZE],
    ref_pic_set_st_curr_before: [Option<DpbEntry<T>>; MAX_DPB_SIZE],
    ref_pic_set_st_foll: [Option<DpbEntry<T>>; MAX_DPB_SIZE],
    ref_pic_set_lt_foll: [Option<DpbEntry<T>>; MAX_DPB_SIZE],

    /// Reference picture list 0 for P and B slices. Retains the same meaning as
    /// in the specification. Points into the pictures stored in the DPB.
    /// Derived once per slice.
    ref_pic_list0: [Option<RefPicListEntry<T>>; MAX_DPB_SIZE],
    /// Reference picture list 1 for B slices. Retains the same meaning as in
    /// the specification. Points into the pictures stored in the DPB. Derived
    /// once per slice.
    ref_pic_list1: [Option<RefPicListEntry<T>>; MAX_DPB_SIZE],
}

impl<T, P> Decoder<T, P>
where
    T: DecodedHandle + Clone + 'static,
{
    // Creates a new instance of the decoder.
    #[cfg(any(feature = "vaapi", test))]
    #[allow(dead_code)]
    pub(crate) fn new(
        backend: Box<dyn StatelessH265DecoderBackend<Handle = T, Picture = P>>,
        blocking_mode: BlockingMode,
    ) -> anyhow::Result<Self> {
        Ok(Self {
            backend,
            blocking_mode,
            is_first_picture_in_au: true,
            parser: Default::default(),
            decoding_state: Default::default(),
            coded_resolution: Default::default(),
            ready_queue: Default::default(),
            cur_sps_id: Default::default(),
            cur_pps_id: Default::default(),
            cur_pic: Default::default(),
            prev_tid_0_pic: Default::default(),
            max_pic_order_cnt_lsb: Default::default(),
            curr_delta_poc_msb_present_flag: Default::default(),
            foll_delta_poc_msb_present_flag: Default::default(),
            num_poc_st_curr_before: Default::default(),
            num_poc_st_curr_after: Default::default(),
            num_poc_st_foll: Default::default(),
            num_poc_lt_curr: Default::default(),
            num_poc_lt_foll: Default::default(),
            poc_st_curr_before: Default::default(),
            poc_st_curr_after: Default::default(),
            poc_st_foll: Default::default(),
            poc_lt_curr: Default::default(),
            poc_lt_foll: Default::default(),
            ref_pic_list0: Default::default(),
            ref_pic_list1: Default::default(),
            cur_backend_pic: Default::default(),
            dpb: Default::default(),
            ref_pic_set_lt_curr: Default::default(),
            ref_pic_set_st_curr_after: Default::default(),
            ref_pic_set_st_curr_before: Default::default(),
            ref_pic_set_st_foll: Default::default(),
            ref_pic_set_lt_foll: Default::default(),
        })
    }

    /// Whether the stream parameters have changed, indicating that a negotiation window has opened.
    fn negotiation_possible(_sps: &Sps, _dpb: (), _current_resolution: Resolution) -> bool {
        todo!()
    }

    fn peek_sps(parser: &mut Parser, bitstream: &[u8]) -> Option<Sps> {
        let mut cursor = Cursor::new(bitstream);

        while let Ok(Some(nalu)) = Nalu::next(&mut cursor) {
            if matches!(nalu.header().type_(), NaluType::SpsNut) {
                let sps = parser.parse_sps(&nalu).ok()?;
                return Some(sps.clone());
            }
        }

        None
    }

    // See 8.3.2, Note 2.
    fn st_ref_pic_set<'a>(hdr: &'a SliceHeader, sps: &'a Sps) -> &'a ShortTermRefPicSet {
        if hdr.curr_rps_idx() == sps.num_long_term_ref_pics_sps() {
            hdr.short_term_ref_pic_set()
        } else {
            &sps.short_term_ref_pic_set()[usize::from(hdr.curr_rps_idx())]
        }
    }

    // See 8.3.2.
    fn decode_rps(&mut self, hdr: &SliceHeader) -> anyhow::Result<()> {
        let cur_pic = self.cur_pic.as_ref().unwrap();

        if cur_pic.nal_unit_type.is_irap() && cur_pic.no_rasl_output_flag {
            self.dpb.mark_all_as_unused_for_ref();
        }

        if cur_pic.is_irap {
            self.poc_st_curr_before = Default::default();
            self.poc_st_curr_after = Default::default();
            self.poc_st_foll = Default::default();
            self.poc_lt_curr = Default::default();
            self.poc_lt_foll = Default::default();

            self.num_poc_st_curr_before = 0;
            self.num_poc_st_curr_after = 0;
            self.num_poc_st_foll = 0;
            self.num_poc_lt_curr = 0;
            self.num_poc_lt_foll = 0;
        } else {
            // Equation 8-5
            let pps = self
                .parser
                .get_pps(hdr.pic_parameter_set_id())
                .context("Invalid SPS in init_current_pic")?;

            let sps = self
                .parser
                .get_sps(pps.seq_parameter_set_id())
                .context("Invalid PPS in init_current_pic")?;

            let curr_st_rps = Self::st_ref_pic_set(hdr, sps);
            let mut j = 0;
            let mut k = 0;
            for i in 0..usize::from(curr_st_rps.num_negative_pics()) {
                let poc = cur_pic.pic_order_cnt_val + curr_st_rps.delta_poc_s0()[i];

                if curr_st_rps.used_by_curr_pic_s0()[i] {
                    self.poc_st_curr_before[j] = poc;
                    j += 1;
                } else {
                    self.poc_st_foll[k] = poc;
                    k += 1;
                }
            }

            self.num_poc_st_curr_before = j as _;

            let mut j = 0;
            for i in 0..usize::from(curr_st_rps.num_positive_pics()) {
                let poc = cur_pic.pic_order_cnt_val + curr_st_rps.delta_poc_s1()[i];

                if curr_st_rps.used_by_curr_pic_s1()[i] {
                    self.poc_st_curr_after[j] = poc;
                    j += 1;
                } else {
                    self.poc_st_foll[k] = poc;
                    k += 1;
                }
            }

            self.num_poc_st_curr_after = j as _;
            self.num_poc_st_foll = k as _;

            let mut j = 0;
            let mut k = 0;
            for i in 0..usize::from(hdr.num_long_term_sps() + hdr.num_long_term_pics()) {
                let mut poc_lt = hdr.poc_lsb_lt()[i] as i32;
                if hdr.delta_poc_msb_present_flag()[i] {
                    poc_lt += cur_pic.pic_order_cnt_val;
                    let delta_poc =
                        hdr.delta_poc_msb_cycle_lt()[i] as i32 * self.max_pic_order_cnt_lsb;

                    poc_lt -= delta_poc;
                    poc_lt -= cur_pic.pic_order_cnt_val & (self.max_pic_order_cnt_lsb - 1);
                }

                if hdr.used_by_curr_pic_lt()[i] {
                    self.poc_lt_curr[j] = poc_lt;
                    self.curr_delta_poc_msb_present_flag[j] = hdr.delta_poc_msb_present_flag()[i];
                    j += 1;
                } else {
                    self.poc_lt_foll[k] = poc_lt;
                    self.foll_delta_poc_msb_present_flag[k] = hdr.delta_poc_msb_present_flag()[i];
                    k += 1;
                }
            }

            self.num_poc_lt_curr = j as _;
            self.num_poc_lt_foll = k as _;
        }

        self.derive_and_mark_rps(hdr)?;
        Ok(())
    }

    // See the derivation process in the second half of 8.3.2.
    fn derive_and_mark_rps(&mut self, hdr: &SliceHeader) -> anyhow::Result<()> {
        let pps = self
            .parser
            .get_pps(hdr.pic_parameter_set_id())
            .context("Invalid SPS in init_current_pic")?;

        let sps = self
            .parser
            .get_sps(pps.seq_parameter_set_id())
            .context("Invalid PPS in init_current_pic")?;

        let max_pic_order_cnt_lsb = 2i32.pow((sps.log2_max_pic_order_cnt_lsb_minus4() + 4).into());

        // Equation 8-6
        for i in 0..self.num_poc_lt_curr {
            if !self.curr_delta_poc_msb_present_flag[i] {
                let poc = self.poc_lt_curr[i] & (max_pic_order_cnt_lsb - 1);
                self.ref_pic_set_lt_curr[i] = self.dpb.find_ref_by_poc(poc);
            } else {
                let poc = self.poc_lt_curr[i];
                self.ref_pic_set_lt_curr[i] = self.dpb.find_ref_by_poc(poc);
            }
        }

        for i in 0..self.num_poc_lt_foll {
            if !self.foll_delta_poc_msb_present_flag[i] {
                let poc = self.poc_lt_foll[i] & (max_pic_order_cnt_lsb - 1);
                self.ref_pic_set_lt_foll[i] = self.dpb.find_ref_by_poc(poc);
            } else {
                let poc = self.poc_lt_foll[i];
                self.ref_pic_set_lt_foll[i] = self.dpb.find_ref_by_poc(poc);
            }
        }

        for pic in self.ref_pic_set_lt_curr.iter().flatten() {
            pic.0.borrow_mut().reference = Reference::LongTerm;
        }

        for pic in self.ref_pic_set_lt_foll.iter().flatten() {
            pic.0.borrow_mut().reference = Reference::LongTerm;
        }

        // Equation 8-7
        for i in 0..self.num_poc_st_curr_before {
            let poc = self.poc_st_curr_before[i];
            self.ref_pic_set_st_curr_before[i] = self.dpb.find_short_term_ref_by_poc(poc);
        }

        for i in 0..self.num_poc_st_curr_after {
            let poc = self.poc_st_curr_after[i];
            self.ref_pic_set_st_curr_after[i] = self.dpb.find_short_term_ref_by_poc(poc);
        }

        for i in 0..self.num_poc_st_foll {
            let poc = self.poc_st_foll[i];
            self.ref_pic_set_st_foll[i] = self.dpb.find_short_term_ref_by_poc(poc);
        }

        // 4. All reference pictures in the DPB that are not included in
        // RefPicSetLtCurr, RefPicSetLtFoll, RefPicSetStCurrBefore,
        // RefPicSetStCurrAfter, or RefPicSetStFoll and have nuh_layer_id equal
        // to currPicLayerId are marked as "unused for reference"
        for dpb_pic in self.dpb.entries() {
            let find_predicate = |p: &Option<DpbEntry<T>>| match p {
                Some(p) => p.0.borrow().pic_order_cnt_val == dpb_pic.0.borrow().pic_order_cnt_val,
                None => false,
            };

            if !self.ref_pic_set_lt_curr.iter().any(find_predicate)
                && !self.ref_pic_set_lt_foll.iter().any(find_predicate)
                && !self.ref_pic_set_st_curr_after.iter().any(find_predicate)
                && !self.ref_pic_set_st_curr_before.iter().any(find_predicate)
                && !self.ref_pic_set_st_foll.iter().any(find_predicate)
            {
                dpb_pic.0.borrow_mut().reference = Reference::None;
            }
        }

        let total_rps_len = self.ref_pic_set_lt_curr.iter().count()
            + self.ref_pic_set_lt_foll.iter().count()
            + self.ref_pic_set_st_curr_after.iter().count()
            + self.ref_pic_set_st_curr_before.iter().count()
            + self.ref_pic_set_st_foll.iter().count();

        if self.dpb.entries().len() != total_rps_len {
            log::warn!("A reference pic is in more than one RPS list. This is against the specification. See 8.3.2. NOTE 5")
        }

        // According to Chromium, unavailable reference pictures are handled by
        // the accelerators internally.
        Ok(())
    }

    // See 8.3.4.
    // Builds the reference picture list for `hdr` for P and B slices.
    fn build_ref_pic_lists(&mut self, hdr: &SliceHeader) -> anyhow::Result<()> {
        // I slices do not use inter prediction.
        if !hdr.type_().is_p() && !hdr.type_().is_b() {
            return Ok(());
        }

        let pps = self
            .parser
            .get_pps(hdr.pic_parameter_set_id())
            .context("Invalid PPS in build_ref_pic_lists")?;

        let cur_pic = self.cur_pic.as_ref().unwrap();
        let rplm = hdr.ref_pic_list_modification();

        let num_rps_curr_temp_list0 = std::cmp::max(
            u32::from(hdr.num_ref_idx_l0_active_minus1()) + 1,
            hdr.num_pic_total_curr(),
        );

        // This could be simplified using a Vec, but lets not change the
        // algorithm from the spec too much.
        let mut ref_pic_list_temp0: [Option<RefPicListEntry<T>>; MAX_DPB_SIZE] = Default::default();

        // Equation 8-8
        let mut r_idx = 0;
        assert!(num_rps_curr_temp_list0 as usize <= MAX_DPB_SIZE);
        while r_idx < num_rps_curr_temp_list0 {
            let mut i = 0;
            while i < self.num_poc_st_curr_before && r_idx < num_rps_curr_temp_list0 {
                ref_pic_list_temp0[r_idx as usize] = self.ref_pic_set_st_curr_before[i]
                    .clone()
                    .map(|e| RefPicListEntry::DpbEntry(e));

                i += 1;
                r_idx += 1;
            }

            let mut i = 0;
            while i < self.num_poc_st_curr_after && r_idx < num_rps_curr_temp_list0 {
                ref_pic_list_temp0[r_idx as usize] = self.ref_pic_set_st_curr_after[i]
                    .clone()
                    .map(|e| RefPicListEntry::DpbEntry(e));

                i += 1;
                r_idx += 1;
            }

            let mut i = 0;
            while i < self.num_poc_lt_curr && r_idx < num_rps_curr_temp_list0 {
                ref_pic_list_temp0[r_idx as usize] = self.ref_pic_set_lt_curr[i]
                    .clone()
                    .map(|e| RefPicListEntry::DpbEntry(e));

                i += 1;
                r_idx += 1;
            }

            if pps.scc_extension().curr_pic_ref_enabled_flag() {
                ref_pic_list_temp0[r_idx as usize] =
                    Some(RefPicListEntry::CurrentPicture(cur_pic.clone()));

                r_idx += 1;
            }
        }

        // Equation 8-9
        for r_idx in 0..=usize::from(hdr.num_ref_idx_l0_active_minus1()) {
            let entry = if rplm.ref_pic_list_modification_flag_l0() {
                let idx = rplm.list_entry_l0()[r_idx];
                ref_pic_list_temp0[idx as usize].clone()
            } else {
                ref_pic_list_temp0[r_idx].clone()
            };

            self.ref_pic_list0[r_idx] = entry;
        }

        if pps.scc_extension().curr_pic_ref_enabled_flag()
            && !rplm.ref_pic_list_modification_flag_l0()
            && num_rps_curr_temp_list0 > (u32::from(hdr.num_ref_idx_l0_active_minus1()) + 1)
        {
            self.ref_pic_list0[r_idx as usize] =
                Some(RefPicListEntry::CurrentPicture(cur_pic.clone()));
        }

        if hdr.type_().is_b() {
            let mut ref_pic_list_temp1: [Option<RefPicListEntry<T>>; MAX_DPB_SIZE] =
                Default::default();

            let num_rps_curr_temp_list1 = std::cmp::max(
                u32::from(hdr.num_ref_idx_l1_active_minus1()) + 1,
                hdr.num_pic_total_curr(),
            );

            // Equation 8-10
            let mut r_idx = 0;
            assert!(num_rps_curr_temp_list1 as usize <= MAX_DPB_SIZE);
            while r_idx < num_rps_curr_temp_list1 {
                let mut i = 0;
                while i < self.num_poc_st_curr_after && r_idx < num_rps_curr_temp_list1 {
                    ref_pic_list_temp1[r_idx as usize] = self.ref_pic_set_st_curr_after[i]
                        .clone()
                        .map(|e| RefPicListEntry::DpbEntry(e));
                    i += 1;
                    r_idx += 1;
                }

                let mut i = 0;
                while i < self.num_poc_st_curr_before && r_idx < num_rps_curr_temp_list1 {
                    ref_pic_list_temp1[r_idx as usize] = self.ref_pic_set_st_curr_before[i]
                        .clone()
                        .map(|e| RefPicListEntry::DpbEntry(e));
                    i += 1;
                    r_idx += 1;
                }

                let mut i = 0;
                while i < self.num_poc_lt_curr && r_idx < num_rps_curr_temp_list1 {
                    ref_pic_list_temp1[r_idx as usize] = self.ref_pic_set_lt_curr[i]
                        .clone()
                        .map(|e| RefPicListEntry::DpbEntry(e));
                    i += 1;
                    r_idx += 1;
                }

                if pps.scc_extension().curr_pic_ref_enabled_flag() {
                    ref_pic_list_temp1[r_idx as usize] =
                        Some(RefPicListEntry::CurrentPicture(cur_pic.clone()));

                    r_idx += 1;
                }
            }

            // Equation 8-11
            for r_idx in 0..=usize::from(hdr.num_ref_idx_l1_active_minus1()) {
                let entry = if rplm.ref_pic_list_modification_flag_l1() {
                    let idx = rplm.list_entry_l1()[r_idx];
                    ref_pic_list_temp1[idx as usize].clone()
                } else {
                    ref_pic_list_temp1[r_idx].clone()
                };

                self.ref_pic_list1[r_idx] = entry;
            }
        }

        Ok(())
    }

    /// Drain the decoder, processing all pending frames.
    fn drain(&mut self) {
        let pics = self.dpb.drain();

        self.ready_queue.extend(pics.into_iter().map(|h| h.1));

        self.dpb.clear();
    }

    /// Bumps the DPB if needed.
    fn bump_as_needed(&mut self, bumping_type: BumpingType) -> anyhow::Result<Vec<DpbEntry<T>>> {
        let mut pics = vec![];

        let sps = self
            .parser
            .get_sps(self.cur_sps_id)
            .context("Invalid SPS in bump_as_needed")?;

        let needs_bumping = match bumping_type {
            BumpingType::BeforeDecoding => Dpb::<T>::needs_bumping,
            BumpingType::AfterDecoding => Dpb::<T>::needs_additional_bumping,
        };

        while needs_bumping(&mut self.dpb, sps) {
            match self.dpb.bump(false) {
                Some(pic) => pics.push(pic),
                None => return Ok(pics),
            }
        }

        Ok(pics)
    }

    // See C.5.2.2
    fn update_dpb_before_decoding(&mut self) -> anyhow::Result<()> {
        let cur_pic = self.cur_pic.as_ref().unwrap();

        if cur_pic.is_irap && cur_pic.no_rasl_output_flag && !cur_pic.is_first_picture {
            if cur_pic.no_output_of_prior_pics_flag {
                self.dpb.clear();
            } else {
                self.drain();
            }
        } else {
            self.dpb.remove_unused();
            let bumped = self
                .bump_as_needed(BumpingType::BeforeDecoding)?
                .into_iter()
                .map(|p| p.1)
                .collect::<Vec<_>>();
            self.ready_queue.extend(bumped);
        }

        Ok(())
    }

    /// Handle a picture. Called only once per frame.
    fn handle_picture(&mut self, timestamp: u64, slice: &Slice<&[u8]>) -> anyhow::Result<()> {
        let prev_tid0 = self.prev_tid_0_pic.as_ref();

        let pps = self
            .parser
            .get_pps(slice.header().pic_parameter_set_id())
            .context("Invalid PPS in handle_picture")?;

        let _sps = self
            .parser
            .get_sps(pps.seq_parameter_set_id())
            .context("Invalid SPS in handle_picture")?;

        let hdr = slice.header();
        self.cur_pps_id = pps.pic_parameter_set_id();
        self.cur_sps_id = pps.seq_parameter_set_id();
        self.cur_pic = Some(PictureData::new_from_slice(
            slice,
            pps,
            self.is_first_picture_in_au,
            prev_tid0,
            self.max_pic_order_cnt_lsb,
            timestamp,
        ));

        self.decode_rps(hdr)?;
        self.update_dpb_before_decoding()?;

        let cur_pic = self.cur_pic.as_ref().unwrap();

        log::debug!("Decode picture POC {:?}", cur_pic.pic_order_cnt_val);

        let mut cur_backend_pic = self.backend.new_picture(cur_pic, timestamp)?;

        self.backend.handle_picture(
            &mut cur_backend_pic,
            cur_pic,
            self.parser
                .get_sps(self.cur_sps_id)
                .context("Invalid SPS in handle_picture")?,
            self.parser
                .get_pps(self.cur_pps_id)
                .context("Invalid PPS in handle_picture")?,
            &self.dpb,
            slice,
        )?;

        self.cur_backend_pic = Some(cur_backend_pic);
        Ok(())
    }

    /// Handle a slice. Called once per slice NALU.
    fn handle_slice(&mut self, _timestamp: u64, slice: &Slice<&[u8]>) -> anyhow::Result<()> {
        self.build_ref_pic_lists(slice.header())?;

        self.backend.decode_slice(
            self.cur_backend_pic.as_mut().unwrap(),
            slice,
            self.parser
                .get_sps(self.cur_sps_id)
                .context("Invalid SPS in handle_slice")?,
            self.parser
                .get_pps(self.cur_pps_id)
                .context("Invalid PPS in handle_slice")?,
            &self.dpb,
            &self.ref_pic_list0,
            &self.ref_pic_list1,
        )?;

        Ok(())
    }

    fn finish_picture(&mut self, pic: PictureData, handle: T) -> anyhow::Result<()> {
        log::debug!("Finishing picture POC {:?}", pic.pic_order_cnt_val);

        // 8.3.1
        if pic.valid_for_prev_tid0_pic {
            self.prev_tid_0_pic = Some(pic.clone());
        }

        self.ref_pic_list0 = Default::default();
        self.ref_pic_list1 = Default::default();
        self.ref_pic_set_lt_curr = Default::default();
        self.ref_pic_set_st_curr_after = Default::default();
        self.ref_pic_set_st_curr_before = Default::default();

        let bumped = self
            .bump_as_needed(BumpingType::AfterDecoding)?
            .into_iter()
            .map(|p| p.1)
            .collect::<Vec<_>>();
        self.ready_queue.extend(bumped);

        self.dpb.store_picture(Rc::new(RefCell::new(pic)), handle)?;
        Ok(())
    }

    fn decode_access_unit(&mut self, timestamp: u64, bitstream: &[u8]) -> Result<(), DecodeError> {
        if self.backend.num_resources_left() == 0 {
            return Err(DecodeError::CheckEvents);
        }

        let mut cursor = Cursor::new(bitstream);

        while let Ok(Some(nalu)) = Nalu::next(&mut cursor) {
            match nalu.header().type_() {
                NaluType::SpsNut => {
                    // Clone to avoid double-borrow on `self`.
                    let _sps = self.parser.parse_sps(&nalu)?.clone();
                    // self.apply_sps(&sps);
                }

                NaluType::PpsNut => {
                    self.parser.parse_pps(&nalu)?;
                }

                NaluType::BlaWLp
                | NaluType::BlaWRadl
                | NaluType::BlaNLp
                | NaluType::IdrWRadl
                | NaluType::IdrNLp
                | NaluType::TrailN
                | NaluType::TrailR
                | NaluType::TsaN
                | NaluType::TsaR
                | NaluType::StsaN
                | NaluType::StsaR
                | NaluType::RadlN
                | NaluType::RadlR
                | NaluType::RaslN
                | NaluType::RaslR
                | NaluType::CraNut => {
                    let slice = self.parser.parse_slice_header(nalu)?;
                    if self.cur_pic.is_none() {
                        self.handle_picture(timestamp, &slice)?;
                    } else if slice.header().first_slice_segment_in_pic_flag() {
                        // We have identified a new picture and must submit the
                        // old one before proceeding.
                        let (picture, handle) = self.submit_picture()?;
                        self.finish_picture(picture, handle)?;
                        self.handle_picture(timestamp, &slice)?;
                    }

                    self.handle_slice(timestamp, &slice)?;
                }

                NaluType::EosNut => {
                    self.is_first_picture_in_au = true;
                }
                other => {
                    log::debug!("Unsupported NAL unit type {:?}", other,);
                }
            }
        }

        let (picture, handle) = self.submit_picture()?;
        self.finish_picture(picture, handle)?;

        Ok(())
    }

    /// Submits the picture to the accelerator.
    fn submit_picture(&mut self) -> Result<(PictureData, T), DecodeError> {
        let picture = self.cur_pic.take().unwrap();

        let handle = self
            .backend
            .submit_picture(self.cur_backend_pic.take().unwrap())?;

        if self.blocking_mode == BlockingMode::Blocking {
            handle.sync()?;
        }

        Ok((picture, handle))
    }
}

impl<T, P> StatelessVideoDecoder for Decoder<T, P>
where
    T: DecodedHandle + Clone + 'static,
{
    fn decode(&mut self, timestamp: u64, bitstream: &[u8]) -> Result<(), DecodeError> {
        let sps = Self::peek_sps(&mut self.parser, bitstream);

        if let Some(sps) = sps {
            if Self::negotiation_possible(&sps, (), self.coded_resolution) {
                // self.backend.new_sequence(&sps)?;
                self.decoding_state = DecodingState::AwaitingFormat(sps);
            }
        }

        match &mut self.decoding_state {
            // Skip input until we get information from the stream.
            DecodingState::AwaitingStreamInfo => Ok(()),
            // Ask the client to confirm the format before we can process this.
            DecodingState::AwaitingFormat(_) => Err(DecodeError::CheckEvents),
            DecodingState::Decoding => self.decode_access_unit(timestamp, bitstream),
        }
    }

    fn flush(&mut self) {
        // self.drain();
        todo!()
    }

    fn num_resources_left(&self) -> usize {
        // self.backend.num_resources_left()
        todo!()
    }

    fn num_resources_total(&self) -> usize {
        // self.backend.num_resources_total()
        todo!()
    }

    fn coded_resolution(&self) -> Option<Resolution> {
        todo!()
        // self.backend.coded_resolution()
    }

    fn next_event(&mut self) -> Option<DecoderEvent> {
        // The next event is either the next frame, or, if we are awaiting negotiation, the format
        // change event that will allow us to keep going.
        (&mut self.ready_queue)
            .next()
            .map(|handle| DecoderEvent::FrameReady(Box::new(handle)))
            .or_else(|| {
                if let DecodingState::AwaitingFormat(sps) = &self.decoding_state {
                    Some(DecoderEvent::FormatChanged(Box::new(
                        StatelessDecoderFormatNegotiator::new(
                            self,
                            sps.clone(),
                            |decoder, _sps| {
                                // Apply the SPS settings to the decoder so we don't enter the AwaitingFormat state
                                // on the next decode() call.
                                // decoder.apply_sps(sps);
                                decoder.decoding_state = DecodingState::Decoding;
                            },
                        ),
                    )))
                } else {
                    None
                }
            })
    }

    fn format(&self) -> Option<DecodedFormat> {
        todo!()
    }
}

impl<T, P> private::StatelessVideoDecoder for Decoder<T, P>
where
    T: DecodedHandle + Clone + 'static,
{
    fn try_format(&mut self, format: crate::DecodedFormat) -> anyhow::Result<()> {
        match &self.decoding_state {
            DecodingState::AwaitingFormat(sps) => self.backend.try_format(sps, format),
            _ => Err(anyhow!(
                "current decoder state does not allow format change"
            )),
        }
    }
}
