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
use log::debug;

use crate::codec::h264::dpb::Dpb;
use crate::codec::h264::dpb::DpbEntry;
use crate::codec::h264::parser::Nalu;
use crate::codec::h264::parser::NaluType;
use crate::codec::h264::parser::Parser;
use crate::codec::h264::parser::Pps;
use crate::codec::h264::parser::RefPicListModification;
use crate::codec::h264::parser::Slice;
use crate::codec::h264::parser::SliceHeader;
use crate::codec::h264::parser::SliceType;
use crate::codec::h264::parser::Sps;
use crate::codec::h264::picture::Field;
use crate::codec::h264::picture::IsIdr;
use crate::codec::h264::picture::PictureData;
use crate::codec::h264::picture::Reference;
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
use crate::decoder::StreamInfo;
use crate::decoder::SurfacePool;
use crate::Resolution;

fn get_raster_from_zigzag_8x8(src: [u8; 64], dst: &mut [u8; 64]) {
    const ZIGZAG_8X8: [usize; 64] = [
        0, 1, 8, 16, 9, 2, 3, 10, 17, 24, 32, 25, 18, 11, 4, 5, 12, 19, 26, 33, 40, 48, 41, 34, 27,
        20, 13, 6, 7, 14, 21, 28, 35, 42, 49, 56, 57, 50, 43, 36, 29, 22, 15, 23, 30, 37, 44, 51,
        58, 59, 52, 45, 38, 31, 39, 46, 53, 60, 61, 54, 47, 55, 62, 63,
    ];

    for i in 0..64 {
        dst[ZIGZAG_8X8[i]] = src[i];
    }
}

fn get_raster_from_zigzag_4x4(src: [u8; 16], dst: &mut [u8; 16]) {
    const ZIGZAG_4X4: [usize; 16] = [0, 1, 4, 8, 5, 2, 3, 6, 9, 12, 13, 10, 7, 11, 14, 15];

    for i in 0..16 {
        dst[ZIGZAG_4X4[i]] = src[i];
    }
}

/// Stateless backend methods specific to H.264.
trait StatelessH264DecoderBackend<M>: StatelessDecoderBackend<Sps, M> {
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

    /// Called when the decoder determines that a second field was found.
    /// Indicates that the underlying BackendHandle is to be shared between the
    /// two pictures. This is so both fields decode to the same underlying
    /// resource and can thus be presented together as a single frame.
    fn new_field_picture(
        &mut self,
        picture: &PictureData,
        timestamp: u64,
        first_field: &Self::Handle,
    ) -> StatelessBackendResult<Self::Picture>;

    /// Called by the decoder when starting a new frame or field.
    fn start_picture(
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
        ref_pic_list0: &[DpbEntry<Self::Handle>],
        ref_pic_list1: &[DpbEntry<Self::Handle>],
    ) -> StatelessBackendResult<()>;

    /// Called when the decoder wants the backend to finish the decoding
    /// operations for `picture`. At this point, `decode_slice` has been called
    /// for all slices.
    ///
    /// This call will assign the ownership of the BackendHandle to the Picture
    /// and then assign the ownership of the Picture to the Handle.
    fn submit_picture(&mut self, picture: Self::Picture) -> StatelessBackendResult<Self::Handle>;
}

#[derive(Copy, Clone, Debug)]
enum RefPicList {
    RefPicList0,
    RefPicList1,
}

pub struct PrevReferencePicInfo {
    frame_num: i32,
    has_mmco_5: bool,
    top_field_order_cnt: i32,
    pic_order_cnt_msb: i32,
    pic_order_cnt_lsb: i32,
    field: Field,
}

impl Default for PrevReferencePicInfo {
    fn default() -> Self {
        Self {
            frame_num: Default::default(),
            has_mmco_5: Default::default(),
            top_field_order_cnt: Default::default(),
            pic_order_cnt_msb: Default::default(),
            pic_order_cnt_lsb: Default::default(),
            field: Field::Frame,
        }
    }
}

#[derive(Default)]
pub struct PrevPicInfo {
    frame_num: i32,
    frame_num_offset: i32,
    has_mmco_5: bool,
}

#[derive(Default)]
pub struct CurrentPicInfo {
    max_frame_num: i32,
    max_pic_num: i32,
    max_long_term_frame_idx: i32,
}

/// All the reference picture lists used to decode a stream.
struct ReferencePicLists<T> {
    /// Reference picture list for P slices. Retains the same meaning as in the
    /// specification. Points into the pictures stored in the DPB. Derived once
    /// per picture.
    ref_pic_list_p0: Vec<DpbEntry<T>>,
    /// Reference picture list 0 for B slices. Retains the same meaning as in
    /// the specification. Points into the pictures stored in the DPB. Derived
    /// once per picture.
    ref_pic_list_b0: Vec<DpbEntry<T>>,
    /// Reference picture list 1 for B slices. Retains the same meaning as in
    /// the specification. Points into the pictures stored in the DPB. Derived
    /// once per picture.
    ref_pic_list_b1: Vec<DpbEntry<T>>,
}

/// Corresponds to RefPicList0 and RefPicList1 in the specification. Computed for every slice,
/// points to the pictures in the DPB.
struct RefPicLists<T> {
    ref_pic_list0: Vec<DpbEntry<T>>,
    ref_pic_list1: Vec<DpbEntry<T>>,
}

impl<T> Default for ReferencePicLists<T> {
    fn default() -> Self {
        Self {
            ref_pic_list_p0: Default::default(),
            ref_pic_list_b0: Default::default(),
            ref_pic_list_b1: Default::default(),
        }
    }
}

pub struct Decoder<T, P, M>
where
    T: DecodedHandle<M> + Clone,
{
    /// A parser to extract bitstream metadata
    parser: Parser,

    /// Whether the decoder should block on decode operations.
    blocking_mode: BlockingMode,

    /// The backend used for hardware acceleration.
    backend: Box<dyn StatelessH264DecoderBackend<M, Handle = T, Picture = P>>,

    decoding_state: DecodingState<Sps>,

    /// The current coded resolution
    coded_resolution: Resolution,

    ready_queue: ReadyFramesQueue<T>,

    /// The decoded picture buffer
    dpb: Dpb<T>,

    /// Indicates an upper bound for the number of frames buffers, in the
    /// decoded picture buffer (DPB), that are required for storing frames,
    /// complementary field pairs, and non-paired fields before output. It is a
    /// requirement of bitstream conformance that the maximum number of frames,
    /// complementary field pairs, or non-paired fields that precede any frame,
    /// complementary field pair, or non-paired field in the coded video
    /// sequence in decoding order and follow it in output order shall be less
    /// than or equal to max_num_reorder_frames.
    max_num_reorder_frames: u32,

    /// The current active SPS id.
    cur_sps_id: u8,
    /// The current active PPS id.
    cur_pps_id: u8,

    /// Cached variables from the previous reference picture.
    prev_ref_pic_info: PrevReferencePicInfo,
    /// Cached variables from the previous picture.
    prev_pic_info: PrevPicInfo,
    /// Cached variables from the current picture.
    curr_info: CurrentPicInfo,

    /// A cached, non-reference first field that did not make it into the DPB
    /// because it was full even after bumping the smaller POC. This field will
    /// be cached until the second field is processed so they can be output
    /// together.
    ///
    /// We are not using `DbpEntry<T>` as the type because contrary to a DPB entry,
    /// the handle of this member is always valid.
    last_field: Option<(Rc<RefCell<PictureData>>, T)>,

    /// Reference picture lists.
    ref_pic_lists: ReferencePicLists<T>,
}

impl<T, P, M> Decoder<T, P, M>
where
    T: DecodedHandle<M> + Clone + 'static,
{
    /// Create a new decoder using the given `backend`.
    #[cfg(any(feature = "vaapi", test))]
    fn new(
        backend: Box<dyn StatelessH264DecoderBackend<M, Handle = T, Picture = P>>,
        blocking_mode: BlockingMode,
    ) -> anyhow::Result<Self> {
        Ok(Self {
            backend,
            blocking_mode,
            parser: Default::default(),
            coded_resolution: Default::default(),
            decoding_state: Default::default(),
            dpb: Default::default(),
            max_num_reorder_frames: Default::default(),
            cur_sps_id: Default::default(),
            cur_pps_id: Default::default(),
            prev_ref_pic_info: Default::default(),
            prev_pic_info: Default::default(),
            curr_info: Default::default(),
            last_field: Default::default(),
            ready_queue: Default::default(),
            ref_pic_lists: Default::default(),
        })
    }

    fn negotiation_possible(sps: &Sps, dpb: &Dpb<T>, current_resolution: Resolution) -> bool {
        let max_dpb_frames = sps.max_dpb_frames();
        let interlaced = !sps.frame_mbs_only_flag();

        let prev_max_dpb_frames = dpb.max_num_pics();
        let prev_interlaced = dpb.interlaced();

        let resolution = Resolution {
            width: sps.width(),
            height: sps.height(),
        };

        current_resolution != resolution
            || prev_max_dpb_frames != max_dpb_frames
            || prev_interlaced != interlaced
    }

    fn get_max_num_order_frames(sps: &Sps, max_dpb_frames: usize) -> u32 {
        let vui = sps.vui_parameters();
        let present = sps.vui_parameters_present_flag() && vui.bitstream_restriction_flag();

        if present {
            vui.max_num_reorder_frames()
        } else {
            let profile = sps.profile_idc();
            if (profile == 44
                || profile == 86
                || profile == 100
                || profile == 110
                || profile == 122
                || profile == 244)
                && sps.constraint_set3_flag()
            {
                0
            } else {
                max_dpb_frames as u32
            }
        }
    }

    // Apply the parameters of `sps` to the decoder.
    fn apply_sps(&mut self, sps: &Sps) {
        let max_dpb_frames = sps.max_dpb_frames();
        let interlaced = !sps.frame_mbs_only_flag();
        let resolution = Resolution {
            width: sps.width(),
            height: sps.height(),
        };

        let max_num_reorder_frames = Self::get_max_num_order_frames(sps, max_dpb_frames);

        if max_num_reorder_frames > max_dpb_frames as u32 {
            self.max_num_reorder_frames = 0;
        } else {
            self.max_num_reorder_frames = max_num_reorder_frames;
        }

        self.drain();

        self.coded_resolution = resolution;

        self.dpb.set_max_num_pics(max_dpb_frames);
        self.dpb.set_interlaced(interlaced);
    }

    fn compute_pic_order_count(&mut self, pic: &mut PictureData) -> anyhow::Result<()> {
        let sps = self
            .parser
            .get_sps(self.cur_sps_id)
            .context("Invalid SPS while computing the value of POC for the current picture")?;

        match pic.pic_order_cnt_type {
            // Spec 8.2.1.1
            0 => {
                let prev_pic_order_cnt_msb;
                let prev_pic_order_cnt_lsb;

                if matches!(pic.is_idr, IsIdr::Yes { .. }) {
                    prev_pic_order_cnt_lsb = 0;
                    prev_pic_order_cnt_msb = 0;
                } else if self.prev_ref_pic_info.has_mmco_5 {
                    if !matches!(self.prev_ref_pic_info.field, Field::Bottom) {
                        prev_pic_order_cnt_msb = 0;
                        prev_pic_order_cnt_lsb = self.prev_ref_pic_info.top_field_order_cnt;
                    } else {
                        prev_pic_order_cnt_msb = 0;
                        prev_pic_order_cnt_lsb = 0;
                    }
                } else {
                    prev_pic_order_cnt_msb = self.prev_ref_pic_info.pic_order_cnt_msb;
                    prev_pic_order_cnt_lsb = self.prev_ref_pic_info.pic_order_cnt_lsb;
                }

                let max_pic_order_cnt_lsb = 1 << (sps.log2_max_pic_order_cnt_lsb_minus4() + 4);

                if (pic.pic_order_cnt_lsb < self.prev_ref_pic_info.pic_order_cnt_lsb)
                    && (prev_pic_order_cnt_lsb - pic.pic_order_cnt_lsb >= max_pic_order_cnt_lsb / 2)
                {
                    pic.pic_order_cnt_msb = prev_pic_order_cnt_msb + max_pic_order_cnt_lsb;
                } else if (pic.pic_order_cnt_lsb > prev_pic_order_cnt_lsb)
                    && (pic.pic_order_cnt_lsb - prev_pic_order_cnt_lsb > max_pic_order_cnt_lsb / 2)
                {
                    pic.pic_order_cnt_msb = prev_pic_order_cnt_msb - max_pic_order_cnt_lsb;
                } else {
                    pic.pic_order_cnt_msb = prev_pic_order_cnt_msb;
                }

                if !matches!(pic.field, Field::Bottom) {
                    pic.top_field_order_cnt = pic.pic_order_cnt_msb + pic.pic_order_cnt_lsb;
                }

                if !matches!(pic.field, Field::Top) {
                    if matches!(pic.field, Field::Frame) {
                        pic.bottom_field_order_cnt =
                            pic.top_field_order_cnt + pic.delta_pic_order_cnt_bottom;
                    } else {
                        pic.bottom_field_order_cnt = pic.pic_order_cnt_msb + pic.pic_order_cnt_lsb;
                    }
                }
            }

            1 => {
                if self.prev_pic_info.has_mmco_5 {
                    self.prev_pic_info.frame_num_offset = 0;
                }

                if matches!(pic.is_idr, IsIdr::Yes { .. }) {
                    pic.frame_num_offset = 0;
                } else if self.prev_pic_info.frame_num > pic.frame_num {
                    pic.frame_num_offset =
                        self.prev_pic_info.frame_num_offset + self.curr_info.max_frame_num;
                } else {
                    pic.frame_num_offset = self.prev_pic_info.frame_num_offset;
                }

                let mut abs_frame_num = if sps.num_ref_frames_in_pic_order_cnt_cycle() != 0 {
                    pic.frame_num_offset + pic.frame_num
                } else {
                    0
                };

                if pic.nal_ref_idc == 0 && abs_frame_num > 0 {
                    abs_frame_num -= 1;
                }

                let mut expected_pic_order_cnt = 0;

                if abs_frame_num > 0 {
                    if sps.num_ref_frames_in_pic_order_cnt_cycle() == 0 {
                        return Err(anyhow!("Invalid num_ref_frames_in_pic_order_cnt_cycle"));
                    }

                    let pic_order_cnt_cycle_cnt =
                        (abs_frame_num - 1) / sps.num_ref_frames_in_pic_order_cnt_cycle() as i32;
                    let frame_num_in_pic_order_cnt_cycle =
                        (abs_frame_num - 1) % sps.num_ref_frames_in_pic_order_cnt_cycle() as i32;
                    expected_pic_order_cnt =
                        pic_order_cnt_cycle_cnt * sps.expected_delta_per_pic_order_cnt_cycle();

                    assert!(frame_num_in_pic_order_cnt_cycle < 255);

                    for i in 0..sps.num_ref_frames_in_pic_order_cnt_cycle() {
                        expected_pic_order_cnt += sps.offset_for_ref_frame()[i as usize];
                    }
                }

                if pic.nal_ref_idc == 0 {
                    expected_pic_order_cnt += sps.offset_for_non_ref_pic();
                }

                if matches!(pic.field, Field::Frame) {
                    pic.top_field_order_cnt = expected_pic_order_cnt + pic.delta_pic_order_cnt0;

                    pic.bottom_field_order_cnt = pic.top_field_order_cnt
                        + sps.offset_for_top_to_bottom_field()
                        + pic.delta_pic_order_cnt1;
                } else if !matches!(pic.field, Field::Bottom) {
                    pic.top_field_order_cnt = expected_pic_order_cnt + pic.delta_pic_order_cnt0;
                } else {
                    pic.bottom_field_order_cnt = expected_pic_order_cnt
                        + sps.offset_for_top_to_bottom_field()
                        + pic.delta_pic_order_cnt0;
                }
            }

            2 => {
                // Spec 8.2.1.3
                if self.prev_pic_info.has_mmco_5 {
                    self.prev_pic_info.frame_num_offset = 0;
                }

                if matches!(pic.is_idr, IsIdr::Yes { .. }) {
                    pic.frame_num_offset = 0;
                } else if self.prev_pic_info.frame_num > pic.frame_num {
                    pic.frame_num_offset =
                        self.prev_pic_info.frame_num_offset + self.curr_info.max_frame_num;
                } else {
                    pic.frame_num_offset = self.prev_pic_info.frame_num_offset;
                }

                let temp_pic_order_cnt;

                if matches!(pic.is_idr, IsIdr::Yes { .. }) {
                    temp_pic_order_cnt = 0;
                } else if pic.nal_ref_idc == 0 {
                    temp_pic_order_cnt = 2 * (pic.frame_num_offset + pic.frame_num) - 1;
                } else {
                    temp_pic_order_cnt = 2 * (pic.frame_num_offset + pic.frame_num);
                }

                if matches!(pic.field, Field::Frame) {
                    pic.top_field_order_cnt = temp_pic_order_cnt;
                    pic.bottom_field_order_cnt = temp_pic_order_cnt;
                } else if matches!(pic.field, Field::Bottom) {
                    pic.bottom_field_order_cnt = temp_pic_order_cnt;
                } else {
                    pic.top_field_order_cnt = temp_pic_order_cnt;
                }
            }

            _ => {
                return Err(anyhow!(
                    "Invalid pic_order_cnt_type: {}",
                    sps.pic_order_cnt_type()
                ))
            }
        }

        match pic.field {
            Field::Frame => {
                pic.pic_order_cnt =
                    std::cmp::min(pic.top_field_order_cnt, pic.bottom_field_order_cnt);
            }
            Field::Top => {
                pic.pic_order_cnt = pic.top_field_order_cnt;
            }
            Field::Bottom => {
                pic.pic_order_cnt = pic.bottom_field_order_cnt;
            }
        }

        Ok(())
    }

    fn sort_pic_num_descending(pics: &mut [DpbEntry<T>]) {
        pics.sort_by_key(|h| std::cmp::Reverse(h.0.borrow().pic_num));
    }

    fn sort_long_term_pic_num_ascending(pics: &mut [DpbEntry<T>]) {
        pics.sort_by_key(|h| h.0.borrow().long_term_pic_num);
    }

    fn sort_frame_num_wrap_descending(pics: &mut [DpbEntry<T>]) {
        pics.sort_by_key(|h| std::cmp::Reverse(h.0.borrow().frame_num_wrap));
    }

    fn sort_long_term_frame_idx_ascending(pics: &mut [DpbEntry<T>]) {
        pics.sort_by_key(|h| h.0.borrow().long_term_frame_idx);
    }

    #[cfg(debug_assertions)]
    fn debug_ref_list_p(ref_pic_list: &[DpbEntry<T>], field_pic: bool) {
        debug!(
            "ref_list_p0: (ShortTerm|LongTerm, pic_num) {:?}",
            ref_pic_list
                .iter()
                .map(|h| {
                    let p = h.0.borrow();
                    let reference = match p.reference() {
                        Reference::None => panic!("Not a reference."),
                        Reference::ShortTerm => "ShortTerm",
                        Reference::LongTerm => "LongTerm",
                    };

                    let field = if !p.is_second_field() {
                        "First field"
                    } else {
                        "Second field"
                    };

                    let field = format!("{}, {:?}", field, p.field);

                    let inner = match (field_pic, p.reference()) {
                        (false, _) => ("pic_num", p.pic_num, field),
                        (true, Reference::ShortTerm) => ("frame_num_wrap", p.frame_num_wrap, field),
                        (true, Reference::LongTerm) => {
                            ("long_term_frame_idx", p.long_term_frame_idx, field)
                        }

                        _ => panic!("Not a reference."),
                    };
                    (reference, inner)
                })
                .collect::<Vec<_>>()
        );
    }

    #[cfg(debug_assertions)]
    fn debug_ref_list_b(ref_pic_list: &[DpbEntry<T>], ref_pic_list_name: &str) {
        debug!(
            "{:?}: (ShortTerm|LongTerm, (POC|LongTermPicNum)) {:?}",
            ref_pic_list_name,
            ref_pic_list
                .iter()
                .map(|h| {
                    let p = h.0.borrow();
                    let reference = match p.reference() {
                        Reference::None => panic!("Not a reference."),
                        Reference::ShortTerm => "ShortTerm",
                        Reference::LongTerm => "LongTerm",
                    };

                    let field = if !p.is_second_field() {
                        "First field"
                    } else {
                        "Second field"
                    };

                    let field = format!("{}, {:?}", field, p.field);

                    let inner = match p.reference() {
                        Reference::ShortTerm => ("POC", p.pic_order_cnt, field),
                        Reference::LongTerm => ("LongTermPicNum", p.long_term_pic_num, field),
                        _ => panic!("Not a reference!"),
                    };
                    (reference, inner)
                })
                .collect::<Vec<_>>()
        );
    }

    /// 8.2.4.2.1 Initialization process for the reference picture list for P
    /// and SP slices in frames
    fn init_ref_pic_list_p(dpb: &Dpb<T>) -> Vec<DpbEntry<T>> {
        let mut ref_pic_list_p0 = vec![];

        dpb.get_short_term_refs(&mut ref_pic_list_p0);
        ref_pic_list_p0.retain(|h| !h.0.borrow().is_second_field());
        Self::sort_pic_num_descending(&mut ref_pic_list_p0);

        let num_short_term_refs = ref_pic_list_p0.len();

        dpb.get_long_term_refs(&mut ref_pic_list_p0);
        ref_pic_list_p0.retain(|h| !h.0.borrow().is_second_field());
        Self::sort_long_term_pic_num_ascending(&mut ref_pic_list_p0[num_short_term_refs..]);

        #[cfg(debug_assertions)]
        Self::debug_ref_list_p(&ref_pic_list_p0, false);

        ref_pic_list_p0
    }

    /// 8.2.4.2.2 Initialization process for the reference picture list for P
    /// and SP slices in fields
    fn init_ref_field_pic_list_p(dpb: &Dpb<T>, cur_pic: &PictureData) -> Vec<DpbEntry<T>> {
        let mut ref_pic_list_p0 = vec![];
        let mut ref_frame_list_0_short_term = vec![];
        let mut ref_frame_list_long_term = vec![];

        dpb.get_short_term_refs(&mut ref_frame_list_0_short_term);
        Self::sort_frame_num_wrap_descending(&mut ref_frame_list_0_short_term);

        dpb.get_long_term_refs(&mut ref_frame_list_long_term);
        Self::sort_long_term_pic_num_ascending(&mut ref_frame_list_long_term);

        // 8.2.4.2.5
        Self::init_ref_field_pic_list(
            cur_pic.field,
            Reference::ShortTerm,
            &mut ref_frame_list_0_short_term,
            &mut ref_pic_list_p0,
        );
        Self::init_ref_field_pic_list(
            cur_pic.field,
            Reference::LongTerm,
            &mut ref_frame_list_long_term,
            &mut ref_pic_list_p0,
        );

        #[cfg(debug_assertions)]
        Self::debug_ref_list_p(&ref_pic_list_p0, true);

        ref_pic_list_p0
    }

    fn sort_poc_descending(pics: &mut [DpbEntry<T>]) {
        pics.sort_by_key(|h| std::cmp::Reverse(h.0.borrow().pic_order_cnt));
    }

    fn sort_poc_ascending(pics: &mut [DpbEntry<T>]) {
        pics.sort_by_key(|h| h.0.borrow().pic_order_cnt);
    }

    // When the reference picture list RefPicList1 has more than one entry
    // and RefPicList1 is identical to the reference picture list
    // RefPicList0, the first two entries RefPicList1[0] and RefPicList1[1]
    // are switched.
    fn swap_b1_if_needed(b0: &Vec<DpbEntry<T>>, b1: &mut Vec<DpbEntry<T>>) {
        if b1.len() > 1 && b0.len() == b1.len() {
            let mut equals = true;
            for (x1, x2) in b0.iter().zip(b1.iter()) {
                if !Rc::ptr_eq(&x1.0, &x2.0) {
                    equals = false;
                    break;
                }
            }

            if equals {
                b1.swap(0, 1);
            }
        }
    }

    // 8.2.4.2.3 Initialization process for reference picture lists for B slices
    // in frames
    fn init_ref_pic_list_b(
        dpb: &Dpb<T>,
        cur_pic: &PictureData,
    ) -> (Vec<DpbEntry<T>>, Vec<DpbEntry<T>>) {
        let mut ref_pic_list_b0 = vec![];
        let mut ref_pic_list_b1 = vec![];

        let mut short_term_refs = vec![];
        let mut remaining = vec![];

        dpb.get_short_term_refs(&mut short_term_refs);
        short_term_refs.retain(|h| !h.0.borrow().is_second_field());

        // When pic_order_cnt_type is equal to 0, reference pictures that are
        // marked as "non-existing" as specified in clause 8.2.5.2 are not
        // included in either RefPicList0 or RefPicList1.
        if cur_pic.pic_order_cnt_type == 0 {
            short_term_refs.retain(|h| !h.0.borrow().nonexisting);
        }

        // b0 contains three inner lists of pictures, i.e. [[0] [1] [2]]
        // [0]: short term pictures with POC < current, sorted by descending POC.
        // [1]: short term pictures with POC > current, sorted by ascending POC.
        // [2]: long term pictures sorted by ascending long_term_pic_num
        for handle in &short_term_refs {
            let pic = handle.0.borrow();

            if pic.pic_order_cnt < cur_pic.pic_order_cnt {
                ref_pic_list_b0.push(handle.clone());
            } else {
                remaining.push(handle.clone());
            }
        }

        Self::sort_poc_descending(&mut ref_pic_list_b0);
        Self::sort_poc_ascending(&mut remaining);
        ref_pic_list_b0.append(&mut remaining);

        let mut long_term_refs = vec![];

        dpb.get_long_term_refs(&mut long_term_refs);
        long_term_refs.retain(|h| !h.0.borrow().nonexisting);
        long_term_refs.retain(|h| !h.0.borrow().is_second_field());
        Self::sort_long_term_pic_num_ascending(&mut long_term_refs);

        ref_pic_list_b0.extend(long_term_refs.clone());

        // b1 contains three inner lists of pictures, i.e. [[0] [1] [2]]
        // [0]: short term pictures with POC > current, sorted by ascending POC.
        // [1]: short term pictures with POC < current, sorted by descending POC.
        // [2]: long term pictures sorted by ascending long_term_pic_num
        for handle in &short_term_refs {
            let pic = handle.0.borrow();

            if pic.pic_order_cnt > cur_pic.pic_order_cnt {
                ref_pic_list_b1.push(handle.clone());
            } else {
                remaining.push(handle.clone());
            }
        }

        Self::sort_poc_ascending(&mut ref_pic_list_b1);
        Self::sort_poc_descending(&mut remaining);

        ref_pic_list_b1.extend(remaining);
        ref_pic_list_b1.extend(long_term_refs);

        // When the reference picture list RefPicList1 has more than one entry
        // and RefPicList1 is identical to the reference picture list
        // RefPicList0, the first two entries RefPicList1[0] and RefPicList1[1]
        // are switched.
        Self::swap_b1_if_needed(&ref_pic_list_b0, &mut ref_pic_list_b1);

        #[cfg(debug_assertions)]
        Self::debug_ref_list_b(&ref_pic_list_b0, "ref_pic_list_b0");
        #[cfg(debug_assertions)]
        Self::debug_ref_list_b(&ref_pic_list_b1, "ref_pic_list_b1");

        (ref_pic_list_b0, ref_pic_list_b1)
    }

    /// 8.2.4.2.4 Initialization process for reference picture lists for B
    /// slices in fields
    fn init_ref_field_pic_list_b(
        dpb: &Dpb<T>,
        cur_pic: &PictureData,
    ) -> (Vec<DpbEntry<T>>, Vec<DpbEntry<T>>) {
        let mut ref_pic_list_b0 = vec![];
        let mut ref_pic_list_b1 = vec![];
        let mut ref_frame_list_0_short_term = vec![];
        let mut ref_frame_list_1_short_term = vec![];
        let mut ref_frame_list_long_term = vec![];

        let mut short_term_refs = vec![];
        let mut remaining = vec![];

        dpb.get_short_term_refs(&mut short_term_refs);

        // When pic_order_cnt_type is equal to 0, reference pictures that are
        // marked as "non-existing" as specified in clause 8.2.5.2 are not
        // included in either RefPicList0 or RefPicList1.
        if cur_pic.pic_order_cnt_type == 0 {
            short_term_refs.retain(|h| !h.0.borrow().nonexisting);
        }

        // refFrameList0ShortTerm is comprised of two inner lists, [[0] [1]]
        // [0]: short term pictures with POC <= current, sorted by descending POC
        // [1]: short term pictures with POC > current, sorted by ascending POC
        // NOTE 3 – When the current field follows in decoding order a coded
        // field fldPrev with which together it forms a complementary reference
        // field pair, fldPrev is included into the list refFrameList0ShortTerm
        // using PicOrderCnt( fldPrev ) and the ordering method described in the
        // previous sentence is applied.
        for handle in &short_term_refs {
            let pic = handle.0.borrow();

            if pic.pic_order_cnt <= cur_pic.pic_order_cnt {
                ref_frame_list_0_short_term.push(handle.clone());
            } else {
                remaining.push(handle.clone());
            }
        }

        Self::sort_poc_descending(&mut ref_frame_list_0_short_term);
        Self::sort_poc_ascending(&mut remaining);
        ref_frame_list_0_short_term.append(&mut remaining);

        // refFrameList1ShortTerm is comprised of two inner lists, [[0] [1]]
        // [0]: short term pictures with POC > current, sorted by ascending POC
        // [1]: short term pictures with POC <= current, sorted by descending POC
        // NOTE 4 – When the current field follows in decoding order a coded
        // field fldPrev with which together it forms a complementary reference
        // field pair, fldPrev is included into the list refFrameList1ShortTerm
        // using PicOrderCnt( fldPrev ) and the ordering method described in the
        // previous sentence is applied.

        for handle in &short_term_refs {
            let pic = handle.0.borrow();

            if pic.pic_order_cnt > cur_pic.pic_order_cnt {
                ref_frame_list_1_short_term.push(handle.clone());
            } else {
                remaining.push(handle.clone());
            }
        }

        Self::sort_poc_ascending(&mut ref_frame_list_1_short_term);
        Self::sort_poc_descending(&mut remaining);
        ref_frame_list_1_short_term.append(&mut remaining);

        // refFrameListLongTerm: long term pictures sorted by ascending
        // LongTermFrameIdx.
        // NOTE 5 – When the current picture is the second field of a
        // complementary field pair and the first field of the complementary
        // field pair is marked as "used for long-term reference", the first
        // field is included into the list refFrameListLongTerm. A reference
        // entry in which only one field is marked as "used for long-term
        // reference" is included into the list refFrameListLongTerm
        dpb.get_long_term_refs(&mut ref_frame_list_long_term);

        ref_frame_list_long_term.retain(|h| !h.0.borrow().nonexisting);

        Self::sort_long_term_frame_idx_ascending(&mut ref_frame_list_long_term);

        #[cfg(debug_assertions)]
        Self::debug_ref_list_b(&ref_frame_list_0_short_term, "ref_frame_list_0_short_term");
        #[cfg(debug_assertions)]
        Self::debug_ref_list_b(&ref_frame_list_1_short_term, "ref_frame_list_1_short_term");
        #[cfg(debug_assertions)]
        Self::debug_ref_list_b(&ref_frame_list_long_term, "ref_frame_list_long_term");

        // 8.2.4.2.5
        let field = cur_pic.field;
        Self::init_ref_field_pic_list(
            field,
            Reference::ShortTerm,
            &mut ref_frame_list_0_short_term,
            &mut ref_pic_list_b0,
        );
        Self::init_ref_field_pic_list(
            field,
            Reference::LongTerm,
            &mut ref_frame_list_long_term,
            &mut ref_pic_list_b0,
        );

        Self::init_ref_field_pic_list(
            field,
            Reference::ShortTerm,
            &mut ref_frame_list_1_short_term,
            &mut ref_pic_list_b1,
        );
        Self::init_ref_field_pic_list(
            field,
            Reference::LongTerm,
            &mut ref_frame_list_long_term,
            &mut ref_pic_list_b1,
        );

        // When the reference picture list RefPicList1 has more than one entry
        // and RefPicList1 is identical to the reference picture list
        // RefPicList0, the first two entries RefPicList1[0] and RefPicList1[1]
        // are switched.
        Self::swap_b1_if_needed(&ref_pic_list_b0, &mut ref_pic_list_b1);

        #[cfg(debug_assertions)]
        Self::debug_ref_list_b(&ref_pic_list_b0, "ref_pic_list_b0");
        #[cfg(debug_assertions)]
        Self::debug_ref_list_b(&ref_pic_list_b1, "ref_pic_list_b1");

        (ref_pic_list_b0, ref_pic_list_b1)
    }

    /// Copies from refFrameList(XShort|Long)Term into RefPicListX as per 8.2.4.2.5. Used when
    /// building the reference list for fields in interlaced decoding.
    fn init_ref_field_pic_list(
        mut field: Field,
        reference_type: Reference,
        ref_frame_list: &mut Vec<DpbEntry<T>>,
        ref_pic_list: &mut Vec<DpbEntry<T>>,
    ) {
        // When one field of a reference frame was not decoded or is not marked as "used for
        // (short|long)-term reference", the missing field is ignored and instead the next
        // available stored reference field of the chosen parity from the ordered list of frames
        // refFrameListX(Short|Long)Term is inserted into RefPicListX.
        ref_frame_list.retain(|h| {
            let p = h.0.borrow();
            let skip = p.nonexisting || *p.reference() != reference_type;
            !skip
        });

        while let Some(position) = ref_frame_list.iter().position(|h| {
            let p = h.0.borrow();
            let found = p.field == field;

            if found {
                field = field.opposite();
            }

            found
        }) {
            let pic = ref_frame_list.remove(position);
            ref_pic_list.push(pic);
        }

        ref_pic_list.append(ref_frame_list);
    }

    fn init_ref_pic_lists(&mut self, cur_pic: &PictureData) {
        let dpb = &self.dpb;

        let num_refs = dpb
            .pictures()
            .filter(|p| p.is_ref() && !p.nonexisting)
            .count();

        // 8.2.4.2.1 ~ 8.2.4.2.4: When this process is invoked, there shall be
        // at least one reference frame or complementary reference field pair
        // that is currently marked as "used for reference" (i.e., as "used for
        // short-term reference" or "used for long-term reference") and is not
        // marked as "non-existing".
        if num_refs == 0 {
            self.ref_pic_lists = Default::default();
            return;
        }

        if matches!(cur_pic.field, Field::Frame) {
            self.ref_pic_lists.ref_pic_list_p0 = Self::init_ref_pic_list_p(dpb);
            (
                self.ref_pic_lists.ref_pic_list_b0,
                self.ref_pic_lists.ref_pic_list_b1,
            ) = Self::init_ref_pic_list_b(dpb, cur_pic);
        } else {
            self.ref_pic_lists.ref_pic_list_p0 = Self::init_ref_field_pic_list_p(dpb, cur_pic);
            (
                self.ref_pic_lists.ref_pic_list_b0,
                self.ref_pic_lists.ref_pic_list_b1,
            ) = Self::init_ref_field_pic_list_b(dpb, cur_pic);
        }
    }

    fn sliding_window_marking(&self, pic: &mut PictureData) -> anyhow::Result<()> {
        // If the current picture is a coded field that is the second field in
        // decoding order of a complementary reference field pair, and the first
        // field has been marked as "used for short-term reference", the current
        // picture and the complementary reference field pair are also marked as
        // "used for short-term reference".
        if pic.is_second_field()
            && matches!(
                pic.other_field_unchecked().borrow().reference(),
                Reference::ShortTerm
            )
        {
            pic.set_reference(Reference::ShortTerm, false);
            return Ok(());
        }

        let sps = self
            .parser
            .get_sps(self.cur_sps_id)
            .context("Invalid SPS during the sliding window marking process")?;

        let mut num_ref_pics = self.dpb.num_ref_frames();
        let max_num_ref_frames =
            usize::try_from(std::cmp::max(1, sps.max_num_ref_frames())).unwrap();

        if num_ref_pics < max_num_ref_frames {
            return Ok(());
        }

        /* 8.2.5.3 */
        while num_ref_pics >= max_num_ref_frames {
            let to_unmark = self
                .dpb
                .find_short_term_lowest_frame_num_wrap()
                .context("Could not find a ShortTerm picture to unmark in the DPB")?;

            to_unmark.borrow_mut().set_reference(Reference::None, true);
            num_ref_pics -= 1;
        }

        Ok(())
    }

    fn handle_memory_management_ops(&mut self, pic: &mut PictureData) -> anyhow::Result<()> {
        let markings = pic.ref_pic_marking.clone();

        for (i, marking) in markings.inner().iter().enumerate() {
            match marking.memory_management_control_operation() {
                0 => break,
                1 => self.dpb.mmco_op_1(pic, i)?,
                2 => self.dpb.mmco_op_2(pic, i)?,
                3 => self.dpb.mmco_op_3(pic, i)?,
                4 => self.curr_info.max_long_term_frame_idx = self.dpb.mmco_op_4(pic, i)?,
                5 => self.curr_info.max_long_term_frame_idx = self.dpb.mmco_op_5(pic)?,
                6 => self.dpb.mmco_op_6(pic, i)?,
                other => anyhow::bail!("unknown MMCO={}", other),
            }
        }

        Ok(())
    }

    /// Store some variables related to the previous reference picture. These
    /// will be used in the decoding of future pictures.
    fn fill_prev_ref_info(&mut self, pic: &PictureData) {
        let prev = &mut self.prev_ref_pic_info;

        prev.has_mmco_5 = pic.has_mmco_5;
        prev.top_field_order_cnt = pic.top_field_order_cnt;
        prev.pic_order_cnt_msb = pic.pic_order_cnt_msb;
        prev.pic_order_cnt_lsb = pic.pic_order_cnt_lsb;
        prev.field = pic.field;
        prev.frame_num = pic.frame_num;
    }

    /// Store some variables related to the previous picture. These will be used
    /// in the decoding of future pictures.
    fn fill_prev_info(&mut self, pic: &PictureData) {
        let prev = &mut self.prev_pic_info;

        prev.frame_num = pic.frame_num;
        prev.has_mmco_5 = pic.has_mmco_5;
        prev.frame_num_offset = pic.frame_num_offset;
    }

    fn reference_pic_marking(&mut self, pic: &mut PictureData) -> anyhow::Result<()> {
        /* 8.2.5.1 */
        if matches!(pic.is_idr, IsIdr::Yes { .. }) {
            self.dpb.mark_all_as_unused_for_ref();

            if pic.ref_pic_marking.long_term_reference_flag() {
                pic.set_reference(Reference::LongTerm, false);
                pic.long_term_frame_idx = 0;
                self.curr_info.max_long_term_frame_idx = 0;
            } else {
                pic.set_reference(Reference::ShortTerm, false);
                self.curr_info.max_long_term_frame_idx = -1;
            }

            return Ok(());
        }

        if pic.ref_pic_marking.adaptive_ref_pic_marking_mode_flag() {
            self.handle_memory_management_ops(pic)?;
        } else {
            self.sliding_window_marking(pic)?;
        }

        Ok(())
    }

    fn add_to_dpb(
        &mut self,
        pic: Rc<RefCell<PictureData>>,
        handle: Option<T>,
    ) -> anyhow::Result<()> {
        if !self.dpb.interlaced() {
            assert!(self.last_field.is_none());

            self.dpb.store_picture(pic, handle)?;
        } else {
            // If we have a cached field for this picture, we must combine
            // them before insertion.
            if pic
                .borrow()
                .other_field()
                .and_then(|other_field| other_field.upgrade())
                .zip(self.last_field.as_ref().map(|f| &f.0))
                .map_or_else(
                    || false,
                    |(other_field, last_field)| Rc::ptr_eq(&other_field, last_field),
                )
            {
                if let Some((last_field, last_field_handle)) = self.last_field.take() {
                    self.dpb
                        .store_picture(last_field, Some(last_field_handle))?;
                }
            }

            self.dpb.store_picture(pic, handle)?;
        }

        Ok(())
    }

    /// Adds picture to the ready queue if it could not be added to the DPB.
    fn add_to_ready_queue(&mut self, pic_rc: Rc<RefCell<PictureData>>, handle: T) {
        let pic = pic_rc.borrow();

        if matches!(pic.field, Field::Frame) {
            assert!(self.last_field.is_none());

            self.ready_queue.push(handle);
        } else {
            match &self.last_field {
                None => {
                    assert!(!pic.is_second_field());
                    drop(pic);

                    // Cache the field, wait for its pair.
                    self.last_field = Some((pic_rc, handle));
                }
                Some(last_field)
                    if pic.is_second_field()
                        && pic.other_field().is_some()
                        && Rc::ptr_eq(&pic.other_field_unchecked(), &last_field.0) =>
                {
                    if let Some((field_pic, field_handle)) = self.last_field.take() {
                        field_pic.borrow_mut().set_second_field_to(&pic_rc);
                        self.ready_queue.push(field_handle);
                    }
                }
                _ => {
                    // Somehow, the last field is not paired with the current field.
                    self.last_field = None;
                }
            }
        }
    }

    fn bump_as_needed_into_ready_queue(&mut self, current_pic: &PictureData) {
        let bumped = self
            .dpb
            .bump_as_needed(self.max_num_reorder_frames, current_pic)
            .into_iter()
            .filter_map(|p| p.1);
        self.ready_queue.extend(bumped);
    }

    fn finish_picture(&mut self, pic: (PictureData, P)) -> anyhow::Result<()> {
        debug!("Finishing picture POC {:?}", pic.0.pic_order_cnt);

        // Submit the picture to the backend.
        let handle = self.submit_picture(pic.1)?;
        let mut pic = pic.0;

        if matches!(pic.reference(), Reference::ShortTerm | Reference::LongTerm) {
            self.reference_pic_marking(&mut pic)?;
            self.fill_prev_ref_info(&pic);
        }

        self.ref_pic_lists = Default::default();
        self.fill_prev_info(&pic);

        self.dpb.remove_unused();

        if pic.has_mmco_5 {
            // C.4.5.3 "Bumping process"
            // The bumping process is invoked in the following cases:
            // Clause 3:
            // The current picture has memory_management_control_operation equal
            // to 5, as specified in clause C.4.4.
            self.drain();
        }

        // Bump the DPB as per C.4.5.3 to cover clauses 1, 4, 5 and 6.
        self.bump_as_needed_into_ready_queue(&pic);

        let pic_rc = Rc::new(RefCell::new(pic));
        let pic = pic_rc.borrow();

        // C.4.5.1, C.4.5.2
        // If the current decoded picture is the second field of a complementary
        // reference field pair, add to DPB.
        // C.4.5.1
        // For a reference decoded picture, the "bumping" process is invoked
        // repeatedly until there is an empty frame buffer, by which point it is
        // added to the DPB. Notice that Dpb::needs_bumping already accounts for
        // this.
        // C.4.5.2
        // For a non-reference decoded picture, if there is empty frame buffer
        // after bumping the smaller POC, add to DPB. Otherwise, add it to the
        // ready queue.
        if pic.is_second_field_of_complementary_ref_pair()
            || pic.is_ref()
            || self.dpb.has_empty_frame_buffer()
        {
            if self.dpb.interlaced() && matches!(pic.field, Field::Frame) {
                drop(pic);

                // Split the Frame into two complementary fields so reference
                // marking is easier. This is inspired by the GStreamer implementation.
                let other_field = PictureData::split_frame(&pic_rc);
                let other_field_handle = handle.clone();

                self.add_to_dpb(pic_rc, Some(handle))?;
                self.add_to_dpb(other_field, Some(other_field_handle))?;
            } else {
                drop(pic);
                self.add_to_dpb(pic_rc, Some(handle))?;
            }
        } else {
            drop(pic);
            self.add_to_ready_queue(pic_rc, handle);
        }

        Ok(())
    }

    fn handle_frame_num_gap(&mut self, frame_num: i32, timestamp: u64) -> anyhow::Result<()> {
        if self.dpb.is_empty() {
            return Ok(());
        }

        debug!("frame_num gap detected.");

        let sps = self
            .parser
            .get_sps(self.cur_sps_id)
            .context("Invalid SPS while handling a frame_num gap")?;

        if !sps.gaps_in_frame_num_value_allowed_flag() {
            return Err(anyhow!(
                "Invalid frame_num: {}. Assuming unintentional loss of pictures",
                frame_num
            ));
        }

        let mut unused_short_term_frame_num =
            (self.prev_ref_pic_info.frame_num + 1) % self.curr_info.max_frame_num;
        while unused_short_term_frame_num != frame_num {
            let mut pic = PictureData::new_non_existing(unused_short_term_frame_num, timestamp);
            self.compute_pic_order_count(&mut pic)?;

            self.dpb.update_pic_nums(
                unused_short_term_frame_num,
                self.curr_info.max_frame_num,
                &pic,
            );

            self.sliding_window_marking(&mut pic)?;

            self.dpb.remove_unused();
            self.bump_as_needed_into_ready_queue(&pic);

            let pic_rc = Rc::new(RefCell::new(pic));

            if self.dpb.interlaced() {
                let other_field = PictureData::split_frame(&pic_rc);

                self.add_to_dpb(pic_rc, None)?;
                self.add_to_dpb(other_field, None)?;
            } else {
                self.add_to_dpb(pic_rc, None)?;
            }

            unused_short_term_frame_num += 1;
            unused_short_term_frame_num %= self.curr_info.max_frame_num;
        }

        Ok(())
    }

    /// Init the current picture being decoded.
    fn init_current_pic(
        &mut self,
        slice: &Slice<&[u8]>,
        first_field: Option<&Rc<RefCell<PictureData>>>,
        timestamp: u64,
    ) -> anyhow::Result<PictureData> {
        let pps = self
            .parser
            .get_pps(slice.header().pic_parameter_set_id)
            .context("Invalid SPS in init_current_pic")?;

        let sps = self
            .parser
            .get_sps(pps.seq_parameter_set_id())
            .context("Invalid PPS in init_current_pic")?;

        let mut pic = PictureData::new_from_slice(slice, sps, timestamp);

        if let Some(first_field) = first_field {
            pic.set_first_field_to(first_field);
        }

        self.compute_pic_order_count(&mut pic)?;

        if matches!(pic.is_idr, IsIdr::Yes { .. }) {
            // C.4.5.3 "Bumping process"
            // The bumping process is invoked in the following cases:
            // Clause 2:
            // The current picture is an IDR picture and
            // no_output_of_prior_pics_flag is not equal to 1 and is not
            // inferred to be equal to 1, as specified in clause C.4.4.
            if !pic.ref_pic_marking.no_output_of_prior_pics_flag() {
                self.drain();
            } else {
                // C.4.4 When no_output_of_prior_pics_flag is equal to 1 or is
                // inferred to be equal to 1, all frame buffers in the DPB are
                // emptied without output of the pictures they contain, and DPB
                // fullness is set to 0.
                self.dpb.clear();
            }
        }

        self.dpb.update_pic_nums(
            i32::from(slice.header().frame_num),
            self.curr_info.max_frame_num,
            &pic,
        );

        self.init_ref_pic_lists(&pic);

        Ok(pic)
    }

    /// Drain the decoder, processing all pending frames.
    fn drain(&mut self) {
        let pics = self.dpb.drain();

        // At this point all pictures will have been decoded, as we don't buffer
        // decode requests, but instead process them immediately, so refs will
        // not be needed.
        self.ref_pic_lists = Default::default();

        // Pics in the DPB have undergone `finish_picture` already or are
        // nonexisting frames, we can just mark them as ready.
        self.ready_queue
            .extend(pics.into_iter().filter_map(|h| h.1));

        self.dpb.clear();

        self.last_field = None;
    }

    /// Find the first field for the picture started by `slice`, if any.
    fn find_first_field(
        &self,
        slice: &Slice<impl AsRef<[u8]>>,
    ) -> anyhow::Result<Option<(Rc<RefCell<PictureData>>, T)>> {
        let mut prev_field = None;

        if self.dpb.interlaced() {
            if self.last_field.is_some() {
                prev_field = self.last_field.clone();
            } else if let Some(last_handle) = self.dpb.entries().last() {
                // Use the last entry in the DPB
                let prev_pic = last_handle.0.borrow();

                if !matches!(prev_pic.field, Field::Frame) && prev_pic.other_field().is_none() {
                    if let Some(handle) = &last_handle.1 {
                        // Still waiting for the second field
                        prev_field = Some((last_handle.0.clone(), handle.clone()));
                    }
                }
            }
        }

        if !slice.header().field_pic_flag {
            if let Some(prev_field) = prev_field {
                let field = prev_field.0.borrow().field;
                return Err(anyhow!(
                    "Expecting complementary field {:?}, got {:?}",
                    field.opposite(),
                    field
                ));
            }
        }

        match prev_field {
            None => Ok(None),
            Some(prev_field) => {
                let prev_field_pic = prev_field.0.borrow();

                if prev_field_pic.frame_num != i32::from(slice.header().frame_num) {
                    return Err(anyhow!(
                "The previous field differs in frame_num value wrt. the current field. {:?} vs {:?}",
                prev_field_pic.frame_num,
                slice.header().frame_num
            ));
                } else {
                    let cur_field = if slice.header().bottom_field_flag {
                        Field::Bottom
                    } else {
                        Field::Top
                    };

                    if cur_field == prev_field_pic.field {
                        let field = prev_field_pic.field;
                        return Err(anyhow!(
                            "Expecting complementary field {:?}, got {:?}",
                            field.opposite(),
                            field
                        ));
                    }
                }

                Ok(Some(prev_field.clone()))
            }
        }
    }

    /// Called once per picture to start it.
    fn begin_picture(
        &mut self,
        timestamp: u64,
        slice: &Slice<&[u8]>,
    ) -> anyhow::Result<(PictureData, P)> {
        let nalu_hdr = slice.nalu().header();

        if nalu_hdr.idr_pic_flag() {
            self.prev_ref_pic_info.frame_num = 0;
        }

        let hdr = slice.header();
        let frame_num = i32::from(hdr.frame_num);

        self.cur_pps_id = hdr.pic_parameter_set_id;

        let pps = self
            .parser
            .get_pps(self.cur_pps_id)
            .context("Invalid PPS in handle_picture")?;

        self.cur_sps_id = pps.seq_parameter_set_id();

        let sps = self
            .parser
            .get_sps(self.cur_sps_id)
            .context("Invalid SPS in handle_picture")?;

        self.curr_info.max_frame_num = 1 << (sps.log2_max_frame_num_minus4() + 4);

        if frame_num != self.prev_ref_pic_info.frame_num
            && frame_num != (self.prev_ref_pic_info.frame_num + 1) % self.curr_info.max_frame_num
        {
            self.handle_frame_num_gap(frame_num, timestamp)?;
        }

        let first_field = self.find_first_field(slice)?;

        let cur_pic =
            self.init_current_pic(slice, first_field.as_ref().map(|f| &f.0), timestamp)?;

        debug!("Decode picture POC {:?}", cur_pic.pic_order_cnt);

        let mut cur_backend_pic = if let Some(first_field) = first_field {
            self.backend
                .new_field_picture(&cur_pic, timestamp, &first_field.1)?
        } else {
            self.backend.new_picture(&cur_pic, timestamp)?
        };

        self.backend.start_picture(
            &mut cur_backend_pic,
            &cur_pic,
            self.parser
                .get_sps(self.cur_sps_id)
                .context("Invalid SPS in handle_picture")?,
            self.parser
                .get_pps(self.cur_pps_id)
                .context("invalid PPS in handle_picture")?,
            &self.dpb,
            slice,
        )?;

        Ok((cur_pic, cur_backend_pic))
    }

    fn pic_num_f(pic: &PictureData, max_pic_num: i32) -> i32 {
        if !matches!(pic.reference(), Reference::LongTerm) {
            pic.pic_num
        } else {
            max_pic_num
        }
    }

    fn long_term_pic_num_f(pic: &PictureData, max_long_term_frame_idx: i32) -> i32 {
        if matches!(pic.reference(), Reference::LongTerm) {
            pic.long_term_pic_num
        } else {
            2 * (max_long_term_frame_idx + 1)
        }
    }

    // 8.2.4.3.1 Modification process of reference picture lists for short-term
    // reference pictures
    #[allow(clippy::too_many_arguments)]
    fn short_term_pic_list_modification(
        cur_pic: &PictureData,
        curr_info: &CurrentPicInfo,
        dpb: &Dpb<T>,
        ref_pic_list_x: &mut Vec<DpbEntry<T>>,
        num_ref_idx_lx_active_minus1: u8,
        rplm: &RefPicListModification,
        pic_num_lx_pred: &mut i32,
        ref_idx_lx: &mut usize,
    ) -> anyhow::Result<()> {
        let pic_num_lx_no_wrap;
        let abs_diff_pic_num = rplm.abs_diff_pic_num_minus1() as i32 + 1;
        let modification_of_pic_nums_idc = rplm.modification_of_pic_nums_idc();

        if modification_of_pic_nums_idc == 0 {
            if *pic_num_lx_pred - abs_diff_pic_num < 0 {
                pic_num_lx_no_wrap = *pic_num_lx_pred - abs_diff_pic_num + curr_info.max_pic_num;
            } else {
                pic_num_lx_no_wrap = *pic_num_lx_pred - abs_diff_pic_num;
            }
        } else if modification_of_pic_nums_idc == 1 {
            if *pic_num_lx_pred + abs_diff_pic_num >= curr_info.max_pic_num {
                pic_num_lx_no_wrap = *pic_num_lx_pred + abs_diff_pic_num - curr_info.max_pic_num;
            } else {
                pic_num_lx_no_wrap = *pic_num_lx_pred + abs_diff_pic_num;
            }
        } else {
            anyhow::bail!(
                "unexpected value for modification_of_pic_nums_idc {:?}",
                rplm.modification_of_pic_nums_idc()
            );
        }

        *pic_num_lx_pred = pic_num_lx_no_wrap;

        let pic_num_lx = if pic_num_lx_no_wrap > cur_pic.pic_num {
            pic_num_lx_no_wrap - curr_info.max_pic_num
        } else {
            pic_num_lx_no_wrap
        };

        let handle = dpb
            .find_short_term_with_pic_num(pic_num_lx)
            .with_context(|| format!("No ShortTerm reference found with pic_num {}", pic_num_lx))?;

        ref_pic_list_x.insert(*ref_idx_lx, handle);
        *ref_idx_lx += 1;

        let mut nidx = *ref_idx_lx;

        for cidx in *ref_idx_lx..=usize::from(num_ref_idx_lx_active_minus1) + 1 {
            if cidx == ref_pic_list_x.len() {
                break;
            }

            let target = &ref_pic_list_x[cidx].0.clone();
            let max_pic_num = curr_info.max_pic_num;

            if Self::pic_num_f(&target.borrow(), max_pic_num) != pic_num_lx {
                ref_pic_list_x[nidx] = ref_pic_list_x[cidx].clone();
                nidx += 1;
            }
        }

        while ref_pic_list_x.len() > (usize::from(num_ref_idx_lx_active_minus1) + 1) {
            ref_pic_list_x.pop();
        }

        Ok(())
    }

    fn long_term_pic_list_modification(
        curr_info: &CurrentPicInfo,
        dpb: &Dpb<T>,
        ref_pic_list_x: &mut Vec<DpbEntry<T>>,
        num_ref_idx_lx_active_minus1: u8,
        rplm: &RefPicListModification,
        ref_idx_lx: &mut usize,
    ) -> anyhow::Result<()> {
        let long_term_pic_num = rplm.long_term_pic_num();

        let handle = dpb
            .find_long_term_with_long_term_pic_num(long_term_pic_num as i32)
            .with_context(|| {
                format!(
                    "No LongTerm reference found with long_term_pic_num {}",
                    long_term_pic_num
                )
            })?;

        ref_pic_list_x.insert(*ref_idx_lx, handle);
        *ref_idx_lx += 1;

        let mut nidx = *ref_idx_lx;

        for cidx in *ref_idx_lx..=usize::from(num_ref_idx_lx_active_minus1) + 1 {
            if cidx == ref_pic_list_x.len() {
                break;
            }

            let target = ref_pic_list_x[cidx].0.clone();
            let max_long_term_frame_idx = curr_info.max_long_term_frame_idx;

            if Self::long_term_pic_num_f(&target.borrow(), max_long_term_frame_idx)
                != long_term_pic_num as i32
            {
                ref_pic_list_x[nidx] = ref_pic_list_x[cidx].clone();
                nidx += 1;
            }
        }

        while ref_pic_list_x.len() > (usize::from(num_ref_idx_lx_active_minus1) + 1) {
            ref_pic_list_x.pop();
        }

        Ok(())
    }

    fn modify_ref_pic_list(
        &mut self,
        cur_pic: &PictureData,
        hdr: &SliceHeader,
        ref_pic_list: RefPicList,
        mut ref_pic_list_x: Vec<DpbEntry<T>>,
    ) -> anyhow::Result<Vec<DpbEntry<T>>> {
        let (ref_pic_list_modification_flag_lx, num_ref_idx_lx_active_minus1, rplm) =
            match ref_pic_list {
                RefPicList::RefPicList0 => (
                    hdr.ref_pic_list_modification_flag_l0,
                    hdr.num_ref_idx_l0_active_minus1,
                    &hdr.ref_pic_list_modification_l0,
                ),
                RefPicList::RefPicList1 => (
                    hdr.ref_pic_list_modification_flag_l1,
                    hdr.num_ref_idx_l1_active_minus1,
                    &hdr.ref_pic_list_modification_l1,
                ),
            };

        while ref_pic_list_x.len() > (usize::from(num_ref_idx_lx_active_minus1) + 1) {
            ref_pic_list_x.pop();
        }

        if !ref_pic_list_modification_flag_lx {
            return Ok(ref_pic_list_x);
        }

        let mut pic_num_lx_pred = cur_pic.pic_num;
        let mut ref_idx_lx = 0;

        for modification in rplm {
            let idc = modification.modification_of_pic_nums_idc();

            match idc {
                0 | 1 => {
                    Self::short_term_pic_list_modification(
                        cur_pic,
                        &self.curr_info,
                        &self.dpb,
                        &mut ref_pic_list_x,
                        num_ref_idx_lx_active_minus1,
                        modification,
                        &mut pic_num_lx_pred,
                        &mut ref_idx_lx,
                    )?;
                }
                2 => Self::long_term_pic_list_modification(
                    &self.curr_info,
                    &self.dpb,
                    &mut ref_pic_list_x,
                    num_ref_idx_lx_active_minus1,
                    modification,
                    &mut ref_idx_lx,
                )?,
                3 => break,
                _ => anyhow::bail!("unexpected modification_of_pic_nums_idc {:?}", idc),
            }
        }

        Ok(ref_pic_list_x)
    }

    /// Generate RefPicList0 and RefPicList1 in the specification. Computed for every slice, points
    /// to the pictures in the DPB.
    fn create_ref_pic_lists(
        &mut self,
        cur_pic: &PictureData,
        current_slice: &Slice<impl AsRef<[u8]>>,
    ) -> anyhow::Result<RefPicLists<T>> {
        let mut ref_pic_list0 = Vec::new();
        let mut ref_pic_list1 = Vec::new();

        let hdr = current_slice.header();

        if let SliceType::P | SliceType::Sp = hdr.slice_type {
            ref_pic_list0 = self.modify_ref_pic_list(
                cur_pic,
                hdr,
                RefPicList::RefPicList0,
                self.ref_pic_lists.ref_pic_list_p0.clone(),
            )?;
        } else if let SliceType::B = hdr.slice_type {
            ref_pic_list0 = self.modify_ref_pic_list(
                cur_pic,
                hdr,
                RefPicList::RefPicList0,
                self.ref_pic_lists.ref_pic_list_b0.clone(),
            )?;
            ref_pic_list1 = self.modify_ref_pic_list(
                cur_pic,
                hdr,
                RefPicList::RefPicList1,
                self.ref_pic_lists.ref_pic_list_b1.clone(),
            )?;
        }

        Ok(RefPicLists {
            ref_pic_list0,
            ref_pic_list1,
        })
    }

    /// Handle a slice. Called once per slice NALU.
    fn handle_slice(
        &mut self,
        cur_pic: &mut (PictureData, P),
        slice: &Slice<&[u8]>,
    ) -> anyhow::Result<()> {
        self.curr_info.max_pic_num = slice.header().max_pic_num as i32;
        let RefPicLists {
            ref_pic_list0,
            ref_pic_list1,
        } = self.create_ref_pic_lists(&cur_pic.0, slice)?;

        let sps = self
            .parser
            .get_sps(self.cur_sps_id)
            .context("Invalid SPS in handle_slice")?;

        let pps = self
            .parser
            .get_pps(self.cur_pps_id)
            .context("Invalid PPS in handle_slice")?;

        self.backend.decode_slice(
            &mut cur_pic.1,
            slice,
            sps,
            pps,
            &self.dpb,
            &ref_pic_list0,
            &ref_pic_list1,
        )?;

        Ok(())
    }

    /// Submits the picture to the accelerator.
    fn submit_picture(&mut self, backend_pic: P) -> Result<T, DecodeError> {
        let handle = self.backend.submit_picture(backend_pic)?;

        if self.blocking_mode == BlockingMode::Blocking {
            handle.sync()?;
        }

        Ok(handle)
    }

    fn peek_sps(parser: &mut Parser, bitstream: &[u8]) -> Option<Sps> {
        let mut cursor = Cursor::new(bitstream);

        while let Ok(Some(nalu)) = Nalu::next(&mut cursor) {
            if matches!(nalu.header().nalu_type(), NaluType::Sps) {
                let sps = parser.parse_sps(&nalu).ok()?;
                return Some(sps.clone());
            }
        }

        None
    }

    fn decode_access_unit(&mut self, timestamp: u64, bitstream: &[u8]) -> Result<(), DecodeError> {
        if self.backend.surface_pool().num_free_surfaces() == 0 {
            return Err(DecodeError::CheckEvents);
        }

        let mut cursor = Cursor::new(bitstream);

        let mut cur_pic_opt: Option<(PictureData, P)> = None;

        while let Ok(Some(nalu)) = Nalu::next(&mut cursor) {
            match nalu.header().nalu_type() {
                NaluType::Sps => {
                    self.parser.parse_sps(&nalu)?;
                }

                NaluType::Pps => {
                    self.parser.parse_pps(&nalu)?;
                }

                NaluType::Slice
                | NaluType::SliceDpa
                | NaluType::SliceDpb
                | NaluType::SliceDpc
                | NaluType::SliceIdr
                | NaluType::SliceExt => {
                    let slice = self.parser.parse_slice_header(nalu)?;
                    let mut cur_pic = match cur_pic_opt {
                        // No current picture, start a new one.
                        None => self.begin_picture(timestamp, &slice)?,
                        // We have a current picture but are starting a new field: finish it and
                        // start a new one.
                        Some(cur_pic)
                            if self.dpb.interlaced()
                                && matches!(cur_pic.0.field, Field::Frame)
                                && !cur_pic.0.is_second_field()
                                && cur_pic.0.field != slice.header().field() =>
                        {
                            self.finish_picture(cur_pic)?;
                            self.begin_picture(timestamp, &slice)?
                        }
                        // This slice is part of the current picture.
                        Some(cur_pic) => cur_pic,
                    };

                    self.handle_slice(&mut cur_pic, &slice)?;
                    cur_pic_opt = Some(cur_pic);
                }

                other => {
                    debug!("Unsupported NAL unit type {:?}", other,);
                }
            }
        }

        if let Some(cur_pic) = cur_pic_opt.take() {
            self.finish_picture(cur_pic)?;
        }

        Ok(())
    }
}

impl<T, P, M> StatelessVideoDecoder<M> for Decoder<T, P, M>
where
    T: DecodedHandle<M> + Clone + 'static,
{
    fn decode(&mut self, timestamp: u64, mut bitstream: &[u8]) -> Result<(), DecodeError> {
        let sps = Self::peek_sps(&mut self.parser, bitstream);

        if let Some(sps) = sps {
            if Self::negotiation_possible(&sps, &self.dpb, self.coded_resolution) {
                self.backend.new_sequence(&sps)?;
                self.decoding_state = DecodingState::AwaitingFormat(sps);
            } else if matches!(self.decoding_state, DecodingState::Reset) {
                // We can resume decoding since the decoding parameters have not changed.
                self.decoding_state = DecodingState::Decoding;
            }
        } else if matches!(self.decoding_state, DecodingState::Reset) {
            let mut cursor = Cursor::new(bitstream);

            while let Ok(Some(nalu)) = Nalu::next(&mut cursor) {
                // In the Reset state we can resume decoding from any key frame.
                if matches!(nalu.header().nalu_type(), NaluType::SliceIdr) {
                    bitstream = &bitstream[nalu.sc_offset()..];
                    self.decoding_state = DecodingState::Decoding;
                    break;
                }
            }
        }

        match &mut self.decoding_state {
            // Skip input until we get information from the stream.
            DecodingState::AwaitingStreamInfo | DecodingState::Reset => Ok(()),
            // Ask the client to confirm the format before we can process this.
            DecodingState::AwaitingFormat(_) => Err(DecodeError::CheckEvents),
            DecodingState::Decoding => self.decode_access_unit(timestamp, bitstream),
        }
    }

    fn flush(&mut self) {
        self.drain();
        self.decoding_state = DecodingState::Reset;
    }

    fn next_event(&mut self) -> Option<DecoderEvent<M>> {
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
                            decoder.apply_sps(sps);
                            decoder.decoding_state = DecodingState::Decoding;
                        }),
                    )))
                } else {
                    None
                }
            })
    }

    fn surface_pool(&mut self) -> &mut dyn SurfacePool<M> {
        self.backend.surface_pool()
    }

    fn stream_info(&self) -> Option<&StreamInfo> {
        self.backend.stream_info()
    }
}

impl<T, P, M> private::StatelessVideoDecoder for Decoder<T, P, M>
where
    T: DecodedHandle<M> + Clone + 'static,
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

#[cfg(test)]
pub mod tests {
    use crate::decoder::stateless::h264::Decoder;
    use crate::decoder::stateless::tests::test_decode_stream;
    use crate::decoder::stateless::tests::TestStream;
    use crate::decoder::BlockingMode;
    use crate::utils::simple_playback_loop;
    use crate::utils::simple_playback_loop_owned_surfaces;
    use crate::utils::H264FrameIterator;
    use crate::DecodedFormat;

    /// Run `test` using the dummy decoder, in both blocking and non-blocking modes.
    fn test_decoder_dummy(test: &TestStream, blocking_mode: BlockingMode) {
        let decoder = Decoder::new_dummy(blocking_mode).unwrap();

        test_decode_stream(
            |d, s, f| {
                simple_playback_loop(
                    d,
                    H264FrameIterator::new(s),
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
    /// gst-launch-1.0 videotestsrc num-buffers=1 ! video/x-raw,format=I420,width=64,height=64 ! x264enc ! video/x-h264,profile=constrained-baseline,stream-format=byte-stream ! filesink location="64x64-I.h264"
    pub const DECODE_64X64_PROGRESSIVE_I: TestStream = TestStream {
        stream: include_bytes!("../../codec/h264/test_data/64x64-I.h264"),
        crcs: include_str!("../../codec/h264/test_data/64x64-I.h264.crc"),
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
    /// gst-launch-1.0 videotestsrc num-buffers=2 ! video/x-raw,format=I420,width=64,height=64 ! x264enc b-adapt=false ! video/x-h264,profile=constrained-baseline,stream-format=byte-stream ! filesink location="64x64-I-P.h264"
    pub const DECODE_64X64_PROGRESSIVE_I_P: TestStream = TestStream {
        stream: include_bytes!("../../codec/h264/test_data/64x64-I-P.h264"),
        crcs: include_str!("../../codec/h264/test_data/64x64-I-P.h264.crc"),
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
    /// gst-launch-1.0 videotestsrc num-buffers=3 ! video/x-raw,format=I420,width=64,height=64 ! x264enc b-adapt=false bframes=1 ! video/x-h264,profile=constrained-baseline,stream-format=byte-stream ! filesink location="64x64-I-P-B-P.h264"
    pub const DECODE_64X64_PROGRESSIVE_I_P_B_P: TestStream = TestStream {
        stream: include_bytes!("../../codec/h264/test_data/64x64-I-P-B-P.h264"),
        crcs: include_str!("../../codec/h264/test_data/64x64-I-P-B-P.h264.crc"),
    };

    #[test]
    fn test_64x64_progressive_i_p_b_p_block() {
        test_decoder_dummy(&DECODE_64X64_PROGRESSIVE_I_P_B_P, BlockingMode::Blocking);
    }

    #[test]
    fn test_64x64_progressive_i_p_b_p_nonblock() {
        test_decoder_dummy(&DECODE_64X64_PROGRESSIVE_I_P_B_P, BlockingMode::NonBlocking);
    }

    /// A 64x64 progressive byte-stream encoded I-P-B-P sequence to make it
    /// easier to it easier to spot errors on the libva trace.
    /// Also tests whether the decoder supports the high profile.
    ///
    /// Encoded with the following GStreamer pipeline:
    /// gst-launch-1.0 videotestsrc num-buffers=3 ! video/x-raw,format=I420,width=64,height=64 ! x264enc b-adapt=false bframes=1 ! video/x-h264,profile=high,stream-format=byte-stream ! filesink location="64x64-I-P-B-P-high.h264"
    pub const DECODE_64X64_PROGRESSIVE_I_P_B_P_HIGH: TestStream = TestStream {
        stream: include_bytes!("../../codec/h264/test_data/64x64-I-P-B-P-high.h264"),
        crcs: include_str!("../../codec/h264/test_data/64x64-I-P-B-P-high.h264.crc"),
    };

    #[test]
    fn test_64x64_progressive_i_p_b_p_high_block() {
        test_decoder_dummy(
            &DECODE_64X64_PROGRESSIVE_I_P_B_P_HIGH,
            BlockingMode::Blocking,
        );
    }

    #[test]
    fn test_64x64_progressive_i_p_b_p_high_nonblock() {
        test_decoder_dummy(
            &DECODE_64X64_PROGRESSIVE_I_P_B_P_HIGH,
            BlockingMode::NonBlocking,
        );
    }

    /// Same as Chromium's test-25fps.h264
    pub const DECODE_TEST_25FPS: TestStream = TestStream {
        stream: include_bytes!("../../codec/h264/test_data/test-25fps.h264"),
        crcs: include_str!("../../codec/h264/test_data/test-25fps.h264.crc"),
    };

    #[test]
    fn test_25fps_block() {
        test_decoder_dummy(&DECODE_TEST_25FPS, BlockingMode::Blocking);
    }

    #[test]
    fn test_25fps_nonblock() {
        test_decoder_dummy(&DECODE_TEST_25FPS, BlockingMode::NonBlocking);
    }

    // Adapted from Chromium's test-25fps.h264. Same file, but encoded as
    // interlaced instead using the following ffmpeg command:
    // ffmpeg -i
    // src/third_party/blink/web_tests/media/content/test-25fps.mp4
    // -flags +ilme+ildct  -vbsf h264_mp4toannexb -an test-25fps.h264
    //
    // This test makes sure that the interlaced logic in the decoder
    // actually works, specially that "frame splitting" works, as the fields
    // here were encoded as frames.
    pub const DECODE_TEST_25FPS_INTERLACED: TestStream = TestStream {
        stream: include_bytes!("../../codec/h264/test_data/test-25fps-interlaced.h264"),
        crcs: include_str!("../../codec/h264/test_data/test-25fps-interlaced.h264.crc"),
    };

    #[test]
    fn test_25fps_interlaced_block() {
        test_decoder_dummy(&DECODE_TEST_25FPS_INTERLACED, BlockingMode::Blocking);
    }

    #[test]
    fn test_25fps_interlaced_nonblock() {
        test_decoder_dummy(&DECODE_TEST_25FPS_INTERLACED, BlockingMode::NonBlocking);
    }
}
