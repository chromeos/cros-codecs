// Copyright 2023 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#[cfg(test)]
mod dummy;
#[cfg(feature = "vaapi")]
mod vaapi;

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
use crate::decoder::stateless::DecodeError;
use crate::decoder::stateless::DecodingState;
use crate::decoder::stateless::StatelessBackendResult;
use crate::decoder::stateless::StatelessCodec;
use crate::decoder::stateless::StatelessDecoder;
use crate::decoder::stateless::StatelessDecoderBackend;
use crate::decoder::stateless::StatelessDecoderFormatNegotiator;
use crate::decoder::stateless::StatelessVideoDecoder;
use crate::decoder::BlockingMode;
use crate::decoder::DecodedHandle;
use crate::decoder::DecoderEvent;
use crate::decoder::StreamInfo;
use crate::decoder::SurfacePool;
use crate::Resolution;

const MAX_DPB_SIZE: usize = 16;

// Equation 5-8
pub(crate) fn clip3(x: i32, y: i32, z: i32) -> i32 {
    if z < x {
        x
    } else if z > y {
        y
    } else {
        z
    }
}

// See 6.5.3
const fn up_right_diagonal<const N: usize, const ROWS: usize>() -> [usize; N] {
    // Generics can't be used in const operations for now, so [0; ROWS * ROWS]
    // is rejected by the compiler
    assert!(ROWS * ROWS == N);

    let mut i = 0;
    let mut x = 0i32;
    let mut y = 0i32;
    let mut ret = [0; N];

    loop {
        while y >= 0 {
            if x < (ROWS as i32) && y < (ROWS as i32) {
                ret[i] = (x + ROWS as i32 * y) as usize;
                i += 1;
            }
            y -= 1;
            x += 1;
        }

        y = x;
        x = 0;
        if i >= N {
            break;
        }
    }

    ret
}

const UP_RIGHT_DIAGONAL_4X4: [usize; 16] = up_right_diagonal::<16, 4>();
const UP_RIGHT_DIAGONAL_8X8: [usize; 64] = up_right_diagonal::<64, 8>();

fn get_raster_from_up_right_diagonal_8x8(src: [u8; 64], dst: &mut [u8; 64]) {
    for i in 0..64 {
        dst[UP_RIGHT_DIAGONAL_8X8[i]] = src[i];
    }
}

fn get_raster_from_up_right_diagonal_4x4(src: [u8; 16], dst: &mut [u8; 16]) {
    for i in 0..16 {
        dst[UP_RIGHT_DIAGONAL_4X4[i]] = src[i];
    }
}

/// Stateless backend methods specific to H.265.
pub trait StatelessH265DecoderBackend: StatelessDecoderBackend<Sps> {
    /// Called when a new SPS is parsed.
    fn new_sequence(&mut self, sps: &Sps) -> StatelessBackendResult<()>;

    /// Called when the decoder determines that a frame or field was found.
    fn new_picture(
        &mut self,
        picture: &PictureData,
        timestamp: u64,
    ) -> StatelessBackendResult<Self::Picture>;

    /// Called by the decoder for every frame or field found.
    #[allow(clippy::too_many_arguments)]
    fn begin_picture(
        &mut self,
        picture: &mut Self::Picture,
        picture_data: &PictureData,
        sps: &Sps,
        pps: &Pps,
        dpb: &Dpb<Self::Handle>,
        rps: &RefPicSet<Self::Handle>,
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
    fn submit_picture(&mut self, picture: Self::Picture) -> StatelessBackendResult<Self::Handle>;
}

/// An entry in the Reference Picture Lists. Unlike H.264, H.265 can use the
/// current picture itself as a reference.
#[derive(Clone)]
pub enum RefPicListEntry<T> {
    CurrentPicture(PictureData),
    DpbEntry(DpbEntry<T>),
}

enum BumpingType {
    BeforeDecoding,
    AfterDecoding,
}

enum RenegotiationType<'a> {
    CurrentSps,
    NewSps(&'a Sps),
}

/// Keeps track of the last values seen for negotiation purposes.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
struct NegotiationInfo {
    /// The current coded resolution
    coded_resolution: Resolution,
    general_profile_idc: u8,
    bit_depth_luma_minus8: u8,
    bit_depth_chroma_minus8: u8,
    chroma_format_idc: u8,
}

impl From<&Sps> for NegotiationInfo {
    fn from(sps: &Sps) -> Self {
        NegotiationInfo {
            coded_resolution: Resolution {
                width: sps.width().into(),
                height: sps.height().into(),
            },
            general_profile_idc: sps.profile_tier_level().general_profile_idc(),
            bit_depth_luma_minus8: sps.bit_depth_luma_minus8(),
            bit_depth_chroma_minus8: sps.bit_depth_chroma_minus8(),
            chroma_format_idc: sps.chroma_format_idc(),
        }
    }
}

/// The RefPicSet data, derived once per picture.
#[derive(Clone, Debug)]
pub struct RefPicSet<T> {
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

    ref_pic_set_lt_curr: [Option<DpbEntry<T>>; MAX_DPB_SIZE],
    ref_pic_set_st_curr_after: [Option<DpbEntry<T>>; MAX_DPB_SIZE],
    ref_pic_set_st_curr_before: [Option<DpbEntry<T>>; MAX_DPB_SIZE],
    ref_pic_set_st_foll: [Option<DpbEntry<T>>; MAX_DPB_SIZE],
    ref_pic_set_lt_foll: [Option<DpbEntry<T>>; MAX_DPB_SIZE],
}

impl<T: Clone> Default for RefPicSet<T> {
    fn default() -> Self {
        Self {
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
            ref_pic_set_lt_curr: Default::default(),
            ref_pic_set_st_curr_after: Default::default(),
            ref_pic_set_st_curr_before: Default::default(),
            ref_pic_set_st_foll: Default::default(),
            ref_pic_set_lt_foll: Default::default(),
        }
    }
}

/// State of the picture being currently decoded.
///
/// Stored between calls to [`StatelessDecoder::handle_slice`] that belong to the same picture.
struct CurrentPicState<B: StatelessDecoderBackend<Sps>> {
    /// Data for the current picture as extracted from the stream.
    pic: PictureData,
    /// Backend-specific data for that picture.
    backend_pic: B::Picture,
    /// List of reference pictures, used once per slice.
    ref_pic_lists: ReferencePicLists<B::Handle>,
}

/// All the reference picture lists used to decode a stream.
struct ReferencePicLists<T> {
    /// Reference picture list 0 for P and B slices. Retains the same meaning as
    /// in the specification. Points into the pictures stored in the DPB.
    /// Derived once per slice.
    ref_pic_list0: [Option<RefPicListEntry<T>>; MAX_DPB_SIZE],
    /// Reference picture list 1 for B slices. Retains the same meaning as in
    /// the specification. Points into the pictures stored in the DPB. Derived
    /// once per slice.
    ref_pic_list1: [Option<RefPicListEntry<T>>; MAX_DPB_SIZE],
}

impl<T> Default for ReferencePicLists<T> {
    fn default() -> Self {
        Self {
            ref_pic_list0: Default::default(),
            ref_pic_list1: Default::default(),
        }
    }
}

pub struct H265DecoderState<B: StatelessDecoderBackend<Sps>> {
    /// A parser to extract bitstream metadata
    parser: Parser,

    /// Keeps track of the last values seen for negotiation purposes.
    negotiation_info: NegotiationInfo,
    /// The set of reference pictures.
    rps: RefPicSet<B::Handle>,

    /// The decoded picture buffer
    dpb: Dpb<B::Handle>,

    /// The current active SPS id.
    cur_sps_id: u8,
    /// The current active PPS id.
    cur_pps_id: u8,

    /// Used to identify first picture in decoding order or first picture that
    /// follows an EOS NALU.
    first_picture_after_eos: bool,

    /// Whether this is the first picture in the bitstream in decoding order.
    first_picture_in_bitstream: bool,
    // Same as PrevTid0Pic in the specification.
    prev_tid_0_pic: Option<PictureData>,

    /// A H.265 syntax element.
    max_pic_order_cnt_lsb: i32,
    /// The value of NoRaslOutputFlag for the last IRAP picture.
    irap_no_rasl_output_flag: bool,

    /// We keep track of the last independent header so we can copy that into
    /// dependent slices.
    last_independent_slice_header: Option<SliceHeader>,

    /// The picture currently being decoded. We need to preserve it between
    /// calls to `decode` because multiple slices will be processed in different
    /// calls to `decode`.
    current_pic: Option<CurrentPicState<B>>,

    pending_pps: Vec<Vec<u8>>,
}

impl<B> Default for H265DecoderState<B>
where
    B: StatelessH265DecoderBackend,
    B::Handle: Clone,
{
    fn default() -> Self {
        H265DecoderState {
            parser: Default::default(),
            negotiation_info: Default::default(),
            rps: Default::default(),
            dpb: Default::default(),
            cur_sps_id: Default::default(),
            cur_pps_id: Default::default(),
            first_picture_after_eos: true,
            first_picture_in_bitstream: true,
            prev_tid_0_pic: Default::default(),
            max_pic_order_cnt_lsb: Default::default(),
            irap_no_rasl_output_flag: Default::default(),
            last_independent_slice_header: Default::default(),
            current_pic: Default::default(),
            pending_pps: Default::default(),
        }
    }
}

/// [`StatelessCodec`] structure to use in order to create a H.265 stateless decoder.
///
/// # Accepted input
///
/// A decoder using this codec processes exactly one NAL unit of input per call to
/// [`StatelessDecoder::decode`], and returns the number of bytes until the end of this NAL unit.
/// This makes it possible to call [`Decode`](StatelessDecoder::decode) repeatedly on some unsplit
/// Annex B stream and shrinking it by the number of bytes processed after each call, until the
/// stream ends up being empty.
pub struct H265;

impl StatelessCodec for H265 {
    type FormatInfo = Sps;
    type DecoderState<B: StatelessDecoderBackend<Sps>> = H265DecoderState<B>;
}

impl<B> StatelessDecoder<H265, B>
where
    B: StatelessH265DecoderBackend,
    B::Handle: Clone,
{
    /// Whether the stream parameters have changed, indicating that a negotiation window has opened.
    fn negotiation_possible(
        sps: &Sps,
        dpb: &Dpb<B::Handle>,
        old_negotiation_info: &NegotiationInfo,
    ) -> bool {
        let negotiation_info = NegotiationInfo::from(sps);
        let max_dpb_size = std::cmp::min(sps.max_dpb_size(), 16);
        let prev_max_dpb_size = dpb.max_num_pics();

        *old_negotiation_info != negotiation_info || prev_max_dpb_size != max_dpb_size
    }

    /// Apply the parameters of `sps` to the decoder.
    fn apply_sps(&mut self, sps: &Sps) -> anyhow::Result<()> {
        self.drain()?;
        self.codec.negotiation_info = NegotiationInfo::from(sps);

        let max_dpb_size = std::cmp::min(sps.max_dpb_size(), 16);
        self.codec.dpb.set_max_num_pics(max_dpb_size);
        Ok(())
    }

    // See 8.3.2, Note 2.
    fn st_ref_pic_set<'a>(hdr: &'a SliceHeader, sps: &'a Sps) -> &'a ShortTermRefPicSet {
        if hdr.curr_rps_idx() == sps.num_short_term_ref_pic_sets() {
            hdr.short_term_ref_pic_set()
        } else {
            &sps.short_term_ref_pic_set()[usize::from(hdr.curr_rps_idx())]
        }
    }

    // See 8.3.2.
    fn decode_rps(&mut self, slice: &Slice<&[u8]>, cur_pic: &PictureData) -> anyhow::Result<()> {
        let hdr = slice.header();

        if cur_pic.nalu_type.is_irap() && cur_pic.no_rasl_output_flag {
            self.codec.dpb.mark_all_as_unused_for_ref();
        }

        if slice.nalu().header().nalu_type().is_idr() {
            self.codec.rps.poc_st_curr_before = Default::default();
            self.codec.rps.poc_st_curr_after = Default::default();
            self.codec.rps.poc_st_foll = Default::default();
            self.codec.rps.poc_lt_curr = Default::default();
            self.codec.rps.poc_lt_foll = Default::default();

            self.codec.rps.num_poc_st_curr_before = 0;
            self.codec.rps.num_poc_st_curr_after = 0;
            self.codec.rps.num_poc_st_foll = 0;
            self.codec.rps.num_poc_lt_curr = 0;
            self.codec.rps.num_poc_lt_foll = 0;
        } else {
            let sps = self
                .codec
                .parser
                .get_sps(self.codec.cur_sps_id)
                .context("Invalid SPS")?;

            let curr_st_rps = Self::st_ref_pic_set(hdr, sps);
            let mut j = 0;
            let mut k = 0;
            for i in 0..usize::from(curr_st_rps.num_negative_pics()) {
                let poc = cur_pic.pic_order_cnt_val + curr_st_rps.delta_poc_s0()[i];

                if curr_st_rps.used_by_curr_pic_s0()[i] {
                    self.codec.rps.poc_st_curr_before[j] = poc;
                    j += 1;
                } else {
                    self.codec.rps.poc_st_foll[k] = poc;
                    k += 1;
                }
            }

            self.codec.rps.num_poc_st_curr_before = j as _;

            let mut j = 0;
            for i in 0..usize::from(curr_st_rps.num_positive_pics()) {
                let poc = cur_pic.pic_order_cnt_val + curr_st_rps.delta_poc_s1()[i];

                if curr_st_rps.used_by_curr_pic_s1()[i] {
                    self.codec.rps.poc_st_curr_after[j] = poc;
                    j += 1;
                } else {
                    self.codec.rps.poc_st_foll[k] = poc;
                    k += 1;
                }
            }

            self.codec.rps.num_poc_st_curr_after = j as _;
            self.codec.rps.num_poc_st_foll = k as _;

            let mut j = 0;
            let mut k = 0;
            for i in 0..usize::from(hdr.num_long_term_sps() + hdr.num_long_term_pics()) {
                let mut poc_lt = hdr.poc_lsb_lt()[i] as i32;
                if hdr.delta_poc_msb_present_flag()[i] {
                    poc_lt += cur_pic.pic_order_cnt_val;
                    let delta_poc =
                        hdr.delta_poc_msb_cycle_lt()[i] as i32 * self.codec.max_pic_order_cnt_lsb;

                    poc_lt -= delta_poc;
                    poc_lt -= cur_pic.pic_order_cnt_val & (self.codec.max_pic_order_cnt_lsb - 1);
                }

                if hdr.used_by_curr_pic_lt()[i] {
                    self.codec.rps.poc_lt_curr[j] = poc_lt;
                    self.codec.rps.curr_delta_poc_msb_present_flag[j] =
                        hdr.delta_poc_msb_present_flag()[i];
                    j += 1;
                } else {
                    self.codec.rps.poc_lt_foll[k] = poc_lt;
                    self.codec.rps.foll_delta_poc_msb_present_flag[k] =
                        hdr.delta_poc_msb_present_flag()[i];
                    k += 1;
                }
            }

            self.codec.rps.num_poc_lt_curr = j as _;
            self.codec.rps.num_poc_lt_foll = k as _;
        }

        self.derive_and_mark_rps()?;
        Ok(())
    }

    // See the derivation process in the second half of 8.3.2.
    fn derive_and_mark_rps(&mut self) -> anyhow::Result<()> {
        let max_pic_order_cnt_lsb = self.codec.max_pic_order_cnt_lsb;

        // Equation 8-6
        for i in 0..self.codec.rps.num_poc_lt_curr {
            if !self.codec.rps.curr_delta_poc_msb_present_flag[i] {
                let poc = self.codec.rps.poc_lt_curr[i];
                let mask = max_pic_order_cnt_lsb - 1;
                let reference = self.codec.dpb.find_ref_by_poc_masked(poc, mask);

                if reference.is_none() {
                    log::warn!("No reference found for poc {} and mask {}", poc, mask);
                }

                self.codec.rps.ref_pic_set_lt_curr[i] = reference;
            } else {
                let poc = self.codec.rps.poc_lt_curr[i];
                let reference = self.codec.dpb.find_ref_by_poc(poc);

                if reference.is_none() {
                    log::warn!("No reference found for poc {}", poc);
                }

                self.codec.rps.ref_pic_set_lt_curr[i] = reference;
            }
        }

        for i in 0..self.codec.rps.num_poc_lt_foll {
            if !self.codec.rps.foll_delta_poc_msb_present_flag[i] {
                let poc = self.codec.rps.poc_lt_foll[i];
                let mask = max_pic_order_cnt_lsb - 1;
                let reference = self.codec.dpb.find_ref_by_poc_masked(poc, mask);

                if reference.is_none() {
                    log::warn!("No reference found for poc {} and mask {}", poc, mask);
                }

                self.codec.rps.ref_pic_set_lt_foll[i] = reference;
            } else {
                let poc = self.codec.rps.poc_lt_foll[i];
                let reference = self.codec.dpb.find_ref_by_poc(poc);

                if reference.is_none() {
                    log::warn!("No reference found for poc {}", poc);
                }

                self.codec.rps.ref_pic_set_lt_foll[i] = reference;
            }
        }

        for pic in self.codec.rps.ref_pic_set_lt_curr.iter().flatten() {
            pic.0.borrow_mut().set_reference(Reference::LongTerm);
        }

        for pic in self.codec.rps.ref_pic_set_lt_foll.iter().flatten() {
            pic.0.borrow_mut().set_reference(Reference::LongTerm);
        }

        // Equation 8-7
        for i in 0..self.codec.rps.num_poc_st_curr_before {
            let poc = self.codec.rps.poc_st_curr_before[i];
            let reference = self.codec.dpb.find_short_term_ref_by_poc(poc);

            if reference.is_none() {
                log::warn!("No reference found for poc {}", poc);
            }

            self.codec.rps.ref_pic_set_st_curr_before[i] = reference;
        }

        for i in 0..self.codec.rps.num_poc_st_curr_after {
            let poc = self.codec.rps.poc_st_curr_after[i];
            let reference = self.codec.dpb.find_short_term_ref_by_poc(poc);

            if reference.is_none() {
                log::warn!("No reference found for poc {}", poc);
            }

            self.codec.rps.ref_pic_set_st_curr_after[i] = reference;
        }

        for i in 0..self.codec.rps.num_poc_st_foll {
            let poc = self.codec.rps.poc_st_foll[i];
            let reference = self.codec.dpb.find_short_term_ref_by_poc(poc);

            if reference.is_none() {
                log::warn!("No reference found for poc {}", poc);
            }

            self.codec.rps.ref_pic_set_st_foll[i] = reference;
        }

        // 4. All reference pictures in the DPB that are not included in
        // RefPicSetLtCurr, RefPicSetLtFoll, RefPicSetStCurrBefore,
        // RefPicSetStCurrAfter, or RefPicSetStFoll and have nuh_layer_id equal
        // to currPicLayerId are marked as "unused for reference"
        for dpb_pic in self.codec.dpb.entries() {
            let find_predicate = |p: &Option<DpbEntry<B::Handle>>| match p {
                Some(p) => p.0.borrow().pic_order_cnt_val == dpb_pic.0.borrow().pic_order_cnt_val,
                None => false,
            };

            if !self.codec.rps.ref_pic_set_lt_curr[0..self.codec.rps.num_poc_lt_curr]
                .iter()
                .any(find_predicate)
                && !self.codec.rps.ref_pic_set_lt_foll[0..self.codec.rps.num_poc_lt_foll]
                    .iter()
                    .any(find_predicate)
                && !self.codec.rps.ref_pic_set_st_curr_after
                    [0..self.codec.rps.num_poc_st_curr_after]
                    .iter()
                    .any(find_predicate)
                && !self.codec.rps.ref_pic_set_st_curr_before
                    [0..self.codec.rps.num_poc_st_curr_before]
                    .iter()
                    .any(find_predicate)
                && !self.codec.rps.ref_pic_set_st_foll[0..self.codec.rps.num_poc_st_foll]
                    .iter()
                    .any(find_predicate)
            {
                dpb_pic.0.borrow_mut().set_reference(Reference::None);
            }
        }

        let total_rps_len = self.codec.rps.ref_pic_set_lt_curr[0..self.codec.rps.num_poc_lt_curr]
            .len()
            + self.codec.rps.ref_pic_set_lt_foll[0..self.codec.rps.num_poc_lt_foll].len()
            + self.codec.rps.ref_pic_set_st_curr_after[0..self.codec.rps.num_poc_st_curr_after]
                .len()
            + self.codec.rps.ref_pic_set_st_curr_before[0..self.codec.rps.num_poc_st_curr_before]
                .len()
            + self.codec.rps.ref_pic_set_st_foll[0..self.codec.rps.num_poc_st_foll].len();

        let dpb_len = self.codec.dpb.entries().len();
        if dpb_len != total_rps_len {
            log::warn!(
                "The total RPS length {} is not the same as the DPB length {}",
                total_rps_len,
                dpb_len
            );
            log::warn!("A reference pic may be in more than one RPS list. This is against the specification. See 8.3.2. NOTE 5")
        }

        // According to Chromium, unavailable reference pictures are handled by
        // the accelerators internally.
        Ok(())
    }

    // See 8.3.4.
    // Builds the reference picture list for `hdr` for P and B slices.
    fn build_ref_pic_lists(
        &self,
        hdr: &SliceHeader,
        cur_pic: &PictureData,
    ) -> anyhow::Result<ReferencePicLists<B::Handle>> {
        let mut ref_pic_lists = ReferencePicLists::default();

        // I slices do not use inter prediction.
        if !hdr.type_().is_p() && !hdr.type_().is_b() {
            return Ok(ref_pic_lists);
        }

        let pps = self
            .codec
            .parser
            .get_pps(hdr.pic_parameter_set_id())
            .context("Invalid PPS in build_ref_pic_lists")?;

        if self.codec.rps.num_poc_st_curr_before == 0
            && self.codec.rps.num_poc_st_curr_after == 0
            && self.codec.rps.num_poc_lt_curr == 0
            && pps.scc_extension_flag()
            && !pps.scc_extension().curr_pic_ref_enabled_flag()
        {
            // Let's try and keep going, if it is a broken stream then maybe it
            // will sort itself out as we go. In any case, we must not loop
            // infinitely here.
            log::error!("Bug or broken stream: out of pictures and can't build ref pic lists.");
            return Ok(ref_pic_lists);
        }

        let rplm = hdr.ref_pic_list_modification();

        let num_rps_curr_temp_list0 = std::cmp::max(
            u32::from(hdr.num_ref_idx_l0_active_minus1()) + 1,
            hdr.num_pic_total_curr(),
        );

        // This could be simplified using a Vec, but lets not change the
        // algorithm from the spec too much.
        let mut ref_pic_list_temp0: [Option<RefPicListEntry<B::Handle>>; MAX_DPB_SIZE] =
            Default::default();

        // Equation 8-8
        let mut r_idx = 0;
        assert!(num_rps_curr_temp_list0 as usize <= MAX_DPB_SIZE);
        while r_idx < num_rps_curr_temp_list0 {
            let mut i = 0;
            while i < self.codec.rps.num_poc_st_curr_before && r_idx < num_rps_curr_temp_list0 {
                ref_pic_list_temp0[r_idx as usize] = self.codec.rps.ref_pic_set_st_curr_before[i]
                    .clone()
                    .map(RefPicListEntry::DpbEntry);

                i += 1;
                r_idx += 1;
            }

            let mut i = 0;
            while i < self.codec.rps.num_poc_st_curr_after && r_idx < num_rps_curr_temp_list0 {
                ref_pic_list_temp0[r_idx as usize] = self.codec.rps.ref_pic_set_st_curr_after[i]
                    .clone()
                    .map(RefPicListEntry::DpbEntry);

                i += 1;
                r_idx += 1;
            }

            let mut i = 0;
            while i < self.codec.rps.num_poc_lt_curr && r_idx < num_rps_curr_temp_list0 {
                ref_pic_list_temp0[r_idx as usize] = self.codec.rps.ref_pic_set_lt_curr[i]
                    .clone()
                    .map(RefPicListEntry::DpbEntry);

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

            ref_pic_lists.ref_pic_list0[r_idx] = entry;
        }

        if pps.scc_extension().curr_pic_ref_enabled_flag()
            && !rplm.ref_pic_list_modification_flag_l0()
            && num_rps_curr_temp_list0 > (u32::from(hdr.num_ref_idx_l0_active_minus1()) + 1)
        {
            ref_pic_lists.ref_pic_list0[r_idx as usize] =
                Some(RefPicListEntry::CurrentPicture(cur_pic.clone()));
        }

        if hdr.type_().is_b() {
            let mut ref_pic_list_temp1: [Option<RefPicListEntry<B::Handle>>; MAX_DPB_SIZE] =
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
                while i < self.codec.rps.num_poc_st_curr_after && r_idx < num_rps_curr_temp_list1 {
                    ref_pic_list_temp1[r_idx as usize] = self.codec.rps.ref_pic_set_st_curr_after
                        [i]
                        .clone()
                        .map(RefPicListEntry::DpbEntry);
                    i += 1;
                    r_idx += 1;
                }

                let mut i = 0;
                while i < self.codec.rps.num_poc_st_curr_before && r_idx < num_rps_curr_temp_list1 {
                    ref_pic_list_temp1[r_idx as usize] = self.codec.rps.ref_pic_set_st_curr_before
                        [i]
                        .clone()
                        .map(RefPicListEntry::DpbEntry);
                    i += 1;
                    r_idx += 1;
                }

                let mut i = 0;
                while i < self.codec.rps.num_poc_lt_curr && r_idx < num_rps_curr_temp_list1 {
                    ref_pic_list_temp1[r_idx as usize] = self.codec.rps.ref_pic_set_lt_curr[i]
                        .clone()
                        .map(RefPicListEntry::DpbEntry);
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

                ref_pic_lists.ref_pic_list1[r_idx] = entry;
            }
        }

        Ok(ref_pic_lists)
    }

    /// Drain the decoder, processing all pending frames.
    fn drain(&mut self) -> anyhow::Result<()> {
        log::debug!("Draining the decoder");

        // Finish the current picture if there is one pending.
        if let Some(cur_pic) = self.codec.current_pic.take() {
            self.finish_picture(cur_pic)?;
        }

        let pics = self.codec.dpb.drain();

        log::debug!(
            "Adding POCs {:?} to the ready queue while draining",
            pics.iter()
                .map(|p| p.0.borrow().pic_order_cnt_val)
                .collect::<Vec<_>>()
        );

        log::trace!(
            "{:#?}",
            pics.iter().map(|p| p.0.borrow()).collect::<Vec<_>>()
        );

        self.ready_queue.extend(pics.into_iter().map(|h| h.1));
        self.codec.dpb.clear();

        Ok(())
    }

    fn clear_ref_lists(&mut self) {
        if let Some(pic) = self.codec.current_pic.as_mut() {
            pic.ref_pic_lists = Default::default();
        }

        self.codec.rps.ref_pic_set_lt_curr = Default::default();
        self.codec.rps.ref_pic_set_st_curr_after = Default::default();
        self.codec.rps.ref_pic_set_st_curr_before = Default::default();

        self.codec.rps.num_poc_lt_curr = Default::default();
        self.codec.rps.num_poc_lt_foll = Default::default();
        self.codec.rps.num_poc_st_curr_after = Default::default();
        self.codec.rps.num_poc_st_curr_before = Default::default();
        self.codec.rps.num_poc_st_foll = Default::default();
    }

    /// Bumps the DPB if needed.
    fn bump_as_needed(
        &mut self,
        bumping_type: BumpingType,
    ) -> anyhow::Result<Vec<DpbEntry<B::Handle>>> {
        let mut pics = vec![];

        let needs_bumping = match bumping_type {
            BumpingType::BeforeDecoding => Dpb::<B::Handle>::needs_bumping,
            BumpingType::AfterDecoding => Dpb::<B::Handle>::needs_additional_bumping,
        };

        let sps = self
            .codec
            .parser
            .get_sps(self.codec.cur_sps_id)
            .context("Invalid SPS id")?;

        while needs_bumping(&mut self.codec.dpb, sps) {
            match self.codec.dpb.bump(false) {
                Some(pic) => pics.push(pic),
                None => return Ok(pics),
            }
        }

        Ok(pics)
    }

    // See C.5.2.2
    fn update_dpb_before_decoding(&mut self, cur_pic: &PictureData) -> anyhow::Result<()> {
        if cur_pic.is_irap && cur_pic.no_rasl_output_flag && !self.codec.first_picture_after_eos {
            if cur_pic.no_output_of_prior_pics_flag {
                self.codec.dpb.clear();
            } else {
                self.drain()?;
            }
        } else {
            self.codec.dpb.remove_unused();
            let bumped = self.bump_as_needed(BumpingType::BeforeDecoding)?;

            log::debug!(
                "Adding POCs {:?} to the ready queue before decoding",
                bumped
                    .iter()
                    .map(|p| p.0.borrow().pic_order_cnt_val)
                    .collect::<Vec<_>>()
            );

            log::trace!(
                "{:#?}",
                bumped.iter().map(|p| p.0.borrow()).collect::<Vec<_>>()
            );

            let bumped = bumped.into_iter().map(|p| p.1).collect::<Vec<_>>();
            self.ready_queue.extend(bumped);
        }

        Ok(())
    }

    /// Called once per picture to start it.
    fn begin_picture(
        &mut self,
        timestamp: u64,
        slice: &Slice<&[u8]>,
    ) -> Result<Option<CurrentPicState<B>>, DecodeError> {
        if self.backend.surface_pool().num_free_surfaces() == 0 {
            return Err(DecodeError::NotEnoughOutputBuffers(1));
        }

        let pps = self
            .codec
            .parser
            .get_pps(slice.header().pic_parameter_set_id())
            .context("Invalid PPS in handle_picture")?;

        let pps_id = pps.pic_parameter_set_id();
        self.update_current_set_ids(pps_id)?;
        self.renegotiate_if_needed(RenegotiationType::CurrentSps)?;

        // We renegotiated and must return the NALU and wait.
        if matches!(self.decoding_state, DecodingState::AwaitingFormat(_)) {
            return Err(DecodeError::CheckEvents);
        }

        let pic = PictureData::new_from_slice(
            slice,
            self.codec
                .parser
                .get_pps(self.codec.cur_pps_id)
                .context("Invalid PPS")?,
            self.codec.first_picture_in_bitstream,
            self.codec.first_picture_after_eos,
            self.codec.prev_tid_0_pic.as_ref(),
            self.codec.max_pic_order_cnt_lsb,
            timestamp,
        );

        self.codec.first_picture_after_eos = false;
        self.codec.first_picture_in_bitstream = false;

        if pic.is_irap {
            self.codec.irap_no_rasl_output_flag = pic.no_rasl_output_flag;
        } else if pic.nalu_type.is_rasl() && self.codec.irap_no_rasl_output_flag {
            // NOTE – All RASL pictures are leading pictures of an associated
            // BLA or CRA picture. When the associated IRAP picture has
            // NoRaslOutputFlag equal to 1, the RASL picture is not output and
            // may not be correctly decodable, as the RASL picture may contain
            // references to pictures that are not present in the bitstream.
            // RASL pictures are not used as reference pictures for the decoding
            // process of non-RASL pictures.
            log::debug!(
                "Dropping POC {}, as it may not be decodable according to the specification",
                pic.pic_order_cnt_val
            );

            return Ok(None);
        }

        log::debug!("Decode picture POC {}", pic.pic_order_cnt_val);

        self.decode_rps(slice, &pic)?;
        self.update_dpb_before_decoding(&pic)?;

        let mut backend_pic = self.backend.new_picture(&pic, timestamp)?;

        self.backend.begin_picture(
            &mut backend_pic,
            &pic,
            self.codec
                .parser
                .get_sps(self.codec.cur_sps_id)
                .context("Invalid SPS")?,
            self.codec
                .parser
                .get_pps(self.codec.cur_pps_id)
                .context("Invalid PPS")?,
            &self.codec.dpb,
            &self.codec.rps,
            slice,
        )?;

        Ok(Some(CurrentPicState {
            pic,
            backend_pic,
            ref_pic_lists: Default::default(),
        }))
    }

    fn update_current_set_ids(&mut self, pps_id: u8) -> anyhow::Result<()> {
        let pps = self.codec.parser.get_pps(pps_id).context("Invalid PPS")?;

        self.codec.cur_pps_id = pps.pic_parameter_set_id();
        self.codec.cur_sps_id = pps.seq_parameter_set_id();
        Ok(())
    }

    /// Handle a slice. Called once per slice NALU.
    fn handle_slice(
        &mut self,
        pic: &mut CurrentPicState<B>,
        slice: &Slice<&[u8]>,
    ) -> anyhow::Result<()> {
        // A dependent slice may refer to a previous SPS which
        // is not the one currently in use.
        self.update_current_set_ids(slice.header().pic_parameter_set_id())?;

        let sps = self
            .codec
            .parser
            .get_sps(self.codec.cur_sps_id)
            .context("Invalid SPS")?;

        // Make sure that no negotiation is possible mid-picture. How could it?
        // We'd lose the context with the previous slices on it.
        assert!(!Self::negotiation_possible(
            sps,
            &self.codec.dpb,
            &self.codec.negotiation_info,
        ));

        pic.ref_pic_lists = self.build_ref_pic_lists(slice.header(), &pic.pic)?;

        self.backend.decode_slice(
            &mut pic.backend_pic,
            slice,
            self.codec
                .parser
                .get_sps(self.codec.cur_sps_id)
                .context("Invalid SPS id")?,
            self.codec
                .parser
                .get_pps(self.codec.cur_pps_id)
                .context("Invalid PPS id")?,
            &self.codec.dpb,
            &pic.ref_pic_lists.ref_pic_list0,
            &pic.ref_pic_lists.ref_pic_list1,
        )?;

        Ok(())
    }

    fn finish_picture(&mut self, pic: CurrentPicState<B>) -> anyhow::Result<()> {
        log::debug!("Finishing picture POC {:?}", pic.pic.pic_order_cnt_val);

        // Submit the picture to the backend.
        let handle = self.submit_picture(pic.backend_pic)?;
        let pic = pic.pic;

        // 8.3.1
        if pic.valid_for_prev_tid0_pic {
            self.codec.prev_tid_0_pic = Some(pic.clone());
        }

        self.clear_ref_lists();

        // First store the current picture in the DPB, only then we should
        // decide whether to bump.
        self.codec
            .dpb
            .store_picture(Rc::new(RefCell::new(pic)), handle)?;
        let bumped = self.bump_as_needed(BumpingType::AfterDecoding)?;

        log::debug!(
            "Adding POCs {:?} to the ready queue after decoding",
            bumped
                .iter()
                .map(|p| p.0.borrow().pic_order_cnt_val)
                .collect::<Vec<_>>()
        );

        log::trace!(
            "{:#?}",
            bumped.iter().map(|p| p.0.borrow()).collect::<Vec<_>>()
        );

        let bumped = bumped.into_iter().map(|p| p.1).collect::<Vec<_>>();
        self.ready_queue.extend(bumped);

        Ok(())
    }

    fn renegotiate_if_needed(
        &mut self,
        renegotiation_type: RenegotiationType,
    ) -> anyhow::Result<()> {
        let sps = match renegotiation_type {
            RenegotiationType::CurrentSps => self
                .codec
                .parser
                .get_sps(self.codec.cur_sps_id)
                .context("Invalid SPS")?,
            RenegotiationType::NewSps(sps) => sps,
        };

        if Self::negotiation_possible(sps, &self.codec.dpb, &self.codec.negotiation_info) {
            // Make sure all the frames we decoded so far are in the ready queue.
            self.drain()?;
            let sps = match renegotiation_type {
                RenegotiationType::CurrentSps => self
                    .codec
                    .parser
                    .get_sps(self.codec.cur_sps_id)
                    .context("Invalid SPS")?,
                RenegotiationType::NewSps(sps) => sps,
            };
            self.backend.new_sequence(sps)?;
            self.decoding_state = DecodingState::AwaitingFormat(sps.clone());
        }

        Ok(())
    }

    fn process_nalu(&mut self, timestamp: u64, nalu: Nalu<&[u8]>) -> Result<(), DecodeError> {
        log::debug!(
            "Processing NALU {:?}, length is {}",
            nalu.header().nalu_type(),
            nalu.size()
        );

        match nalu.header().nalu_type() {
            NaluType::VpsNut => {
                self.codec.parser.parse_vps(&nalu)?;
            }
            NaluType::SpsNut => {
                let sps = self.codec.parser.parse_sps(&nalu)?;
                self.codec.max_pic_order_cnt_lsb =
                    1 << (sps.log2_max_pic_order_cnt_lsb_minus4() + 4);

                // Try parsing the PPS again.
                for pending_pps in self.codec.pending_pps.clone().iter().enumerate() {
                    let mut cursor: Cursor<&[u8]> = Cursor::new(pending_pps.1.as_ref());
                    let nalu = crate::codec::h265::parser::Nalu::next(&mut cursor)?;
                    if self.codec.parser.parse_pps(&nalu).is_ok() {
                        self.codec.pending_pps.remove(pending_pps.0);
                    }
                }
            }

            NaluType::PpsNut => {
                if self.codec.parser.parse_pps(&nalu).is_err() {
                    let data = &nalu.data()[nalu.sc_offset()..nalu.offset() + nalu.size()];
                    self.codec.pending_pps.push(Vec::from(data))
                }
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
                let mut slice = self.codec.parser.parse_slice_header(nalu)?;

                let first_slice_segment_in_pic_flag =
                    slice.header().first_slice_segment_in_pic_flag();

                if slice.header().dependent_slice_segment_flag() {
                    let previous_independent_header = self.codec.last_independent_slice_header.as_ref().ok_or(anyhow!("Cannot process an dependent slice without first processing and independent one"))?.clone();
                    slice.replace_header(previous_independent_header)?;
                } else {
                    self.codec.last_independent_slice_header = Some(slice.header().clone());
                }

                let cur_pic = match self.codec.current_pic.take() {
                    // No current picture, start a new one.
                    None => self.begin_picture(timestamp, &slice)?,
                    Some(cur_pic) if first_slice_segment_in_pic_flag => {
                        self.finish_picture(cur_pic)?;
                        self.begin_picture(timestamp, &slice)?
                    }
                    Some(cur_pic) => Some(cur_pic),
                };

                // Picture may have been dropped during begin_picture()
                if let Some(mut cur_pic) = cur_pic {
                    self.handle_slice(&mut cur_pic, &slice)?;
                    self.codec.current_pic = Some(cur_pic);
                }
            }

            NaluType::EosNut => {
                self.codec.first_picture_after_eos = true;
            }

            NaluType::EobNut => {
                self.codec.first_picture_in_bitstream = true;
            }

            other => {
                log::debug!("Unsupported NAL unit type {:?}", other,);
            }
        }

        Ok(())
    }

    /// Submits the picture to the accelerator.
    fn submit_picture(&mut self, backend_pic: B::Picture) -> Result<B::Handle, DecodeError> {
        let handle = self.backend.submit_picture(backend_pic)?;

        if self.blocking_mode == BlockingMode::Blocking {
            handle.sync()?;
        }

        Ok(handle)
    }
}

impl<B> StatelessVideoDecoder<<B::Handle as DecodedHandle>::Descriptor>
    for StatelessDecoder<H265, B>
where
    B: StatelessH265DecoderBackend,
    B::Handle: Clone + 'static,
{
    fn decode(&mut self, timestamp: u64, bitstream: &[u8]) -> Result<usize, DecodeError> {
        let mut cursor = Cursor::new(bitstream);
        let nalu = Nalu::next(&mut cursor)?;

        if nalu.header().nalu_type() == NaluType::SpsNut {
            let sps = self.codec.parser.parse_sps(&nalu)?.clone();
            if matches!(self.decoding_state, DecodingState::AwaitingStreamInfo) {
                // If more SPS come along we will renegotiate in begin_picture().
                self.renegotiate_if_needed(RenegotiationType::NewSps(&sps))?;
            } else if matches!(self.decoding_state, DecodingState::Reset) {
                // We can resume decoding since the decoding parameters have not changed.
                self.decoding_state = DecodingState::Decoding;
            }
        } else if matches!(self.decoding_state, DecodingState::Reset) {
            let mut cursor = Cursor::new(bitstream);

            while let Ok(nalu) = Nalu::next(&mut cursor) {
                // In the Reset state we can resume decoding from any key frame.
                if nalu.header().nalu_type().is_idr() {
                    self.decoding_state = DecodingState::Decoding;
                    break;
                }
            }
        }

        let nalu_len = nalu.offset() + nalu.size();

        match &mut self.decoding_state {
            // Process parameter sets, but skip input until we get information
            // from the stream.
            DecodingState::AwaitingStreamInfo | DecodingState::Reset => {
                if matches!(
                    nalu.header().nalu_type(),
                    NaluType::VpsNut | NaluType::SpsNut | NaluType::PpsNut
                ) {
                    self.process_nalu(timestamp, nalu)?;
                }
            }
            // Ask the client to confirm the format before we can process this.
            DecodingState::AwaitingFormat(_) => return Err(DecodeError::CheckEvents),
            DecodingState::Decoding => {
                self.process_nalu(timestamp, nalu)?;
            }
        }

        Ok(nalu_len)
    }

    fn flush(&mut self) -> Result<(), DecodeError> {
        self.drain()?;
        self.decoding_state = DecodingState::Reset;

        Ok(())
    }

    fn next_event(&mut self) -> Option<DecoderEvent<<B::Handle as DecodedHandle>::Descriptor>> {
        // The next event is either the next frame, or, if we are awaiting negotiation, the format
        // change event that will allow us to keep going.
        (&mut self.ready_queue)
            .next()
            .map(|handle| DecoderEvent::FrameReady(Box::new(handle)))
            .or_else(|| {
                if let DecodingState::AwaitingFormat(sps) = &self.decoding_state {
                    Some(DecoderEvent::FormatChanged(Box::new(
                        StatelessDecoderFormatNegotiator::new(self, sps.clone(), |decoder, sps| {
                            // Apply the SPS settings to the decoder so we don't enter the AwaitingFormat state
                            // on the next decode() call.
                            // TODO: unwrap this for now, but ideally change this closure to return Result
                            decoder.apply_sps(sps).unwrap();
                            decoder.decoding_state = DecodingState::Decoding;
                        }),
                    )))
                } else {
                    None
                }
            })
    }

    fn surface_pool(&mut self) -> &mut dyn SurfacePool<<B::Handle as DecodedHandle>::Descriptor> {
        self.backend.surface_pool()
    }

    fn stream_info(&self) -> Option<&StreamInfo> {
        self.backend.stream_info()
    }
}

#[cfg(test)]
pub mod tests {

    use crate::codec::h265::parser::Nalu;
    use crate::decoder::stateless::h265::H265;
    use crate::decoder::stateless::tests::test_decode_stream;
    use crate::decoder::stateless::tests::TestStream;
    use crate::decoder::stateless::StatelessDecoder;
    use crate::decoder::BlockingMode;
    use crate::utils::simple_playback_loop;
    use crate::utils::simple_playback_loop_owned_surfaces;
    use crate::utils::NalIterator;
    use crate::DecodedFormat;

    /// Run `test` using the dummy decoder, in both blocking and non-blocking modes.
    fn test_decoder_dummy(test: &TestStream, blocking_mode: BlockingMode) {
        let decoder = StatelessDecoder::<H265, _>::new_dummy(blocking_mode);

        test_decode_stream(
            |d, s, f| {
                simple_playback_loop(
                    d,
                    NalIterator::<Nalu<_>>::new(s),
                    f,
                    &mut simple_playback_loop_owned_surfaces,
                    DecodedFormat::NV12,
                    blocking_mode,
                )
            },
            decoder,
            test,
            false,
            false,
        );
    }

    /// A 64x64 progressive byte-stream encoded I-frame to make it easier to
    /// spot errors on the libva trace.
    /// Encoded with the following GStreamer pipeline:
    ///
    /// gst-launch-1.0 videotestsrc num-buffers=1 ! video/x-raw,format=I420,width=64,height=64 ! x265enc ! video/x-h265,profile=main ! filesink location="64x64-I.h265"
    pub const DECODE_64X64_PROGRESSIVE_I: TestStream = TestStream {
        stream: include_bytes!("../../codec/h265/test_data/64x64-I.h265"),
        crcs: include_str!("../../codec/h265/test_data/64x64-I.h265.crc"),
    };

    #[test]
    fn test_64x64_progressive_i_block() {
        test_decoder_dummy(&DECODE_64X64_PROGRESSIVE_I, BlockingMode::Blocking);
    }

    #[test]
    fn test_64x64_progressive_i_nonblock() {
        test_decoder_dummy(&DECODE_64X64_PROGRESSIVE_I, BlockingMode::NonBlocking);
    }

    /// A 64x64 progressive byte-stream encoded I-frame and P-frame to make
    /// it easier to spot errors on the libva trace.
    /// Encoded with the following GStreamer pipeline:
    /// gst-launch-1.0 videotestsrc num-buffers=2 ! video/x-raw,format=I420,width=64,height=64 ! x265enc option-string="b-adapt=0" ! video/x-h265,profile=main ! filesink location="64x64-I-P.h265"
    pub const DECODE_64X64_PROGRESSIVE_I_P: TestStream = TestStream {
        stream: include_bytes!("../../codec/h265/test_data/64x64-I-P.h265"),
        crcs: include_str!("../../codec/h265/test_data/64x64-I-P.h265.crc"),
    };

    #[test]
    fn test_64x64_progressive_i_p_block() {
        test_decoder_dummy(&DECODE_64X64_PROGRESSIVE_I_P, BlockingMode::Blocking);
    }

    #[test]
    fn test_64x64_progressive_i_p_nonblock() {
        test_decoder_dummy(&DECODE_64X64_PROGRESSIVE_I_P, BlockingMode::NonBlocking);
    }

    /// A 64x64 progressive byte-stream encoded I-P-B-P sequence to make it
    /// easier to it easier to spot errors on the libva trace.
    /// Encoded with the following GStreamer pipeline:
    /// gst-launch-1.0 videotestsrc num-buffers=3 ! video/x-raw,format=I420,width=64,height=64 ! x265enc option-string="b-adapt=0:bframes=1" ! video/x-h265,profile=main ! filesink location="64x64-I-P-B-P.h265"
    pub const DECODE_64X64_PROGRESSIVE_I_P_B_P: TestStream = TestStream {
        stream: include_bytes!("../../codec/h265/test_data/64x64-I-P-B-P.h265"),
        crcs: include_str!("../../codec/h265/test_data/64x64-I-P-B-P.h265.crc"),
    };

    #[test]
    fn test_64x64_progressive_i_p_b_p_block() {
        test_decoder_dummy(&DECODE_64X64_PROGRESSIVE_I_P_B_P, BlockingMode::Blocking);
    }

    #[test]
    fn test_64x64_progressive_i_p_b_p_nonblock() {
        test_decoder_dummy(&DECODE_64X64_PROGRESSIVE_I_P_B_P, BlockingMode::NonBlocking);
    }

    /// Same as Chromium's test-25fps.h265
    pub const DECODE_TEST_25FPS: TestStream = TestStream {
        stream: include_bytes!("../../codec/h265/test_data/test-25fps.h265"),
        crcs: include_str!("../../codec/h265/test_data/test-25fps.h265.crc"),
    };

    #[test]
    fn test_25fps_block() {
        test_decoder_dummy(&DECODE_TEST_25FPS, BlockingMode::Blocking);
    }

    #[test]
    fn test_25fps_nonblock() {
        test_decoder_dummy(&DECODE_TEST_25FPS, BlockingMode::NonBlocking);
    }

    /// Same as Chromium's bear.h265
    pub const DECODE_BEAR: TestStream = TestStream {
        stream: include_bytes!("../../codec/h265/test_data/bear.h265"),
        crcs: include_str!("../../codec/h265/test_data/bear.h265.crc"),
    };

    #[test]
    fn test_bear_block() {
        test_decoder_dummy(&DECODE_BEAR, BlockingMode::Blocking);
    }

    #[test]
    fn test_bear_nonblock() {
        test_decoder_dummy(&DECODE_BEAR, BlockingMode::NonBlocking);
    }

    /// Same as Chromium's bbb.h265
    pub const DECODE_BBB: TestStream = TestStream {
        stream: include_bytes!("../../codec/h265/test_data/bbb.h265"),
        crcs: include_str!("../../codec/h265/test_data/bbb.h265.crc"),
    };

    #[test]
    fn test_bbb_block() {
        test_decoder_dummy(&DECODE_BBB, BlockingMode::Blocking);
    }

    #[test]
    fn test_bbb_nonblock() {
        test_decoder_dummy(&DECODE_BBB, BlockingMode::NonBlocking);
    }
}
