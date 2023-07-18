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

type DpbPicList<H> = Vec<DpbEntry<H>>;

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
pub trait StatelessH264DecoderBackend: StatelessDecoderBackend<Sps> {
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

/// All the reference picture lists used to decode a stream.
struct ReferencePicLists<T> {
    /// Reference picture list for P slices. Retains the same meaning as in the
    /// specification. Points into the pictures stored in the DPB. Derived once
    /// per picture.
    ref_pic_list_p0: DpbPicList<T>,
    /// Reference picture list 0 for B slices. Retains the same meaning as in
    /// the specification. Points into the pictures stored in the DPB. Derived
    /// once per picture.
    ref_pic_list_b0: DpbPicList<T>,
    /// Reference picture list 1 for B slices. Retains the same meaning as in
    /// the specification. Points into the pictures stored in the DPB. Derived
    /// once per picture.
    ref_pic_list_b1: DpbPicList<T>,
}

/// Corresponds to RefPicList0 and RefPicList1 in the specification. Computed for every slice,
/// points to the pictures in the DPB.
struct RefPicLists<T> {
    ref_pic_list0: DpbPicList<T>,
    ref_pic_list1: DpbPicList<T>,
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

/// State of the picture being currently decoded.
///
/// Stored between calls to [`StatelessDecoder::handle_slice`] that belong to the same picture.
struct CurrentPicState<B: StatelessH264DecoderBackend> {
    /// Data for the current picture as extracted from the stream.
    pic: PictureData,
    /// Backend-specific data for that picture.
    backend_pic: B::Picture,
    /// List of reference pictures, used once per slice.
    ref_pic_lists: ReferencePicLists<B::Handle>,
}

/// State of the H.264 decoder.
///
/// `H` is the [`DecodedHandle`] type produced by the backend, used to keep reference frames and
/// pass them to the backend again when needed.
pub struct H264DecoderState<H> {
    /// H.264 bitstream parser.
    parser: Parser,

    /// The decoded picture buffer.
    dpb: Dpb<H>,

    /// The current active SPS id.
    cur_sps_id: u8,
    /// The current active PPS id.
    cur_pps_id: u8,

    /// Cached variables from the previous reference picture.
    prev_ref_pic_info: PrevReferencePicInfo,
    /// Cached variables from the previous picture.
    prev_pic_info: PrevPicInfo,
    /// Maximum index of the long-term frame.
    max_long_term_frame_idx: i32,

    /// A cached, non-reference first field that did not make it into the DPB
    /// because it was full even after bumping the smaller POC. This field will
    /// be cached until the second field is processed so they can be output
    /// together.
    ///
    /// We are not using `DbpEntry<T>` as the type because contrary to a DPB entry,
    /// the handle of this member is always valid.
    last_field: Option<(Rc<RefCell<PictureData>>, H)>,
}

impl<H: Clone> Default for H264DecoderState<H> {
    fn default() -> Self {
        H264DecoderState {
            parser: Default::default(),
            dpb: Default::default(),
            cur_sps_id: Default::default(),
            cur_pps_id: Default::default(),
            prev_ref_pic_info: Default::default(),
            prev_pic_info: Default::default(),
            max_long_term_frame_idx: Default::default(),
            last_field: Default::default(),
        }
    }
}

pub struct H264;

impl StatelessCodec for H264 {
    type FormatInfo = Sps;
    type DecoderState<H> = H264DecoderState<H>;
}

impl<H: Clone> H264DecoderState<H> {
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

                let max_pic_order_cnt_lsb = 1 << (sps.log2_max_pic_order_cnt_lsb_minus4 + 4);

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
                        self.prev_pic_info.frame_num_offset + sps.max_frame_num() as i32;
                } else {
                    pic.frame_num_offset = self.prev_pic_info.frame_num_offset;
                }

                let mut abs_frame_num = if sps.num_ref_frames_in_pic_order_cnt_cycle != 0 {
                    pic.frame_num_offset + pic.frame_num
                } else {
                    0
                };

                if pic.nal_ref_idc == 0 && abs_frame_num > 0 {
                    abs_frame_num -= 1;
                }

                let mut expected_pic_order_cnt = 0;

                if abs_frame_num > 0 {
                    if sps.num_ref_frames_in_pic_order_cnt_cycle == 0 {
                        return Err(anyhow!("Invalid num_ref_frames_in_pic_order_cnt_cycle"));
                    }

                    let pic_order_cnt_cycle_cnt =
                        (abs_frame_num - 1) / sps.num_ref_frames_in_pic_order_cnt_cycle as i32;
                    let frame_num_in_pic_order_cnt_cycle =
                        (abs_frame_num - 1) % sps.num_ref_frames_in_pic_order_cnt_cycle as i32;
                    expected_pic_order_cnt =
                        pic_order_cnt_cycle_cnt * sps.expected_delta_per_pic_order_cnt_cycle;

                    assert!(frame_num_in_pic_order_cnt_cycle < 255);

                    for i in 0..sps.num_ref_frames_in_pic_order_cnt_cycle {
                        expected_pic_order_cnt += sps.offset_for_ref_frame[i as usize];
                    }
                }

                if pic.nal_ref_idc == 0 {
                    expected_pic_order_cnt += sps.offset_for_non_ref_pic;
                }

                if matches!(pic.field, Field::Frame) {
                    pic.top_field_order_cnt = expected_pic_order_cnt + pic.delta_pic_order_cnt0;

                    pic.bottom_field_order_cnt = pic.top_field_order_cnt
                        + sps.offset_for_top_to_bottom_field
                        + pic.delta_pic_order_cnt1;
                } else if !matches!(pic.field, Field::Bottom) {
                    pic.top_field_order_cnt = expected_pic_order_cnt + pic.delta_pic_order_cnt0;
                } else {
                    pic.bottom_field_order_cnt = expected_pic_order_cnt
                        + sps.offset_for_top_to_bottom_field
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
                        self.prev_pic_info.frame_num_offset + sps.max_frame_num() as i32;
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
                    sps.pic_order_cnt_type
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

    fn sliding_window_marking(&self, pic: &mut PictureData) -> anyhow::Result<()> {
        // If the current picture is a coded field that is the second field in
        // decoding order of a complementary reference field pair, and the first
        // field has been marked as "used for short-term reference", the current
        // picture and the complementary reference field pair are also marked as
        // "used for short-term reference".
        if pic.is_second_field()
            && matches!(
                pic.other_field().unwrap().borrow().reference(),
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
        let max_num_ref_frames = usize::try_from(std::cmp::max(1, sps.max_num_ref_frames)).unwrap();

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

    /// Returns an iterator of the handles of the frames that need to be bumped into the ready
    /// queue.
    fn bump_as_needed(&mut self, current_pic: &PictureData) -> impl Iterator<Item = H> {
        self.dpb
            .bump_as_needed(current_pic)
            .into_iter()
            .filter_map(|p| p.1)
    }

    /// Returns an iterator of the handles of all the frames still present in the DPB.
    fn drain(&mut self) -> impl Iterator<Item = H> {
        let pics = self.dpb.drain();

        self.dpb.clear();
        self.last_field = None;

        pics.into_iter().filter_map(|h| h.1)
    }

    /// Find the first field for the picture started by `slice`, if any.
    #[allow(clippy::type_complexity)]
    fn find_first_field(
        &self,
        slice: &Slice<impl AsRef<[u8]>>,
    ) -> anyhow::Result<Option<(Rc<RefCell<PictureData>>, H)>> {
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
}

impl<B> StatelessDecoder<H264, B>
where
    B: StatelessH264DecoderBackend,
    B::Handle: Clone,
{
    fn negotiation_possible(
        sps: &Sps,
        dpb: &Dpb<B::Handle>,
        current_resolution: Resolution,
    ) -> bool {
        let max_dpb_frames = sps.max_dpb_frames();
        let interlaced = !sps.frame_mbs_only_flag;

        let prev_max_dpb_frames = dpb.max_num_pics();
        let prev_interlaced = dpb.interlaced();

        let resolution = Resolution {
            width: sps.width,
            height: sps.height,
        };

        current_resolution != resolution
            || prev_max_dpb_frames != max_dpb_frames
            || prev_interlaced != interlaced
    }

    // Apply the parameters of `sps` to the decoder.
    fn apply_sps(&mut self, sps: &Sps) {
        let max_dpb_frames = sps.max_dpb_frames();
        let interlaced = !sps.frame_mbs_only_flag;
        let resolution = Resolution {
            width: sps.width,
            height: sps.height,
        };

        let max_num_order_frames = sps.max_num_order_frames() as usize;
        let max_num_reorder_frames = if max_num_order_frames > max_dpb_frames {
            0
        } else {
            max_num_order_frames
        };

        self.ready_queue.extend(self.codec.drain());

        self.coded_resolution = resolution;

        self.codec
            .dpb
            .set_limits(max_dpb_frames, max_num_reorder_frames);
        self.codec.dpb.set_interlaced(interlaced);
    }

    fn sort_pic_num_descending(pics: &mut [DpbEntry<B::Handle>]) {
        pics.sort_by_key(|h| std::cmp::Reverse(h.0.borrow().pic_num));
    }

    fn sort_long_term_pic_num_ascending(pics: &mut [DpbEntry<B::Handle>]) {
        pics.sort_by_key(|h| h.0.borrow().long_term_pic_num);
    }

    fn sort_frame_num_wrap_descending(pics: &mut [DpbEntry<B::Handle>]) {
        pics.sort_by_key(|h| std::cmp::Reverse(h.0.borrow().frame_num_wrap));
    }

    fn sort_long_term_frame_idx_ascending(pics: &mut [DpbEntry<B::Handle>]) {
        pics.sort_by_key(|h| h.0.borrow().long_term_frame_idx);
    }

    #[cfg(debug_assertions)]
    fn debug_ref_list_p(ref_pic_list: &[DpbEntry<B::Handle>], field_pic: bool) {
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
    fn debug_ref_list_b(ref_pic_list: &[DpbEntry<B::Handle>], ref_pic_list_name: &str) {
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
    fn build_ref_pic_list_p(dpb: &Dpb<B::Handle>) -> DpbPicList<B::Handle> {
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
    fn build_ref_field_pic_list_p(
        dpb: &Dpb<B::Handle>,
        cur_pic: &PictureData,
    ) -> DpbPicList<B::Handle> {
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

    fn sort_poc_descending(pics: &mut [DpbEntry<B::Handle>]) {
        pics.sort_by_key(|h| std::cmp::Reverse(h.0.borrow().pic_order_cnt));
    }

    fn sort_poc_ascending(pics: &mut [DpbEntry<B::Handle>]) {
        pics.sort_by_key(|h| h.0.borrow().pic_order_cnt);
    }

    // When the reference picture list RefPicList1 has more than one entry
    // and RefPicList1 is identical to the reference picture list
    // RefPicList0, the first two entries RefPicList1[0] and RefPicList1[1]
    // are switched.
    fn swap_b1_if_needed(b0: &DpbPicList<B::Handle>, b1: &mut DpbPicList<B::Handle>) {
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
    fn build_ref_pic_list_b(
        dpb: &Dpb<B::Handle>,
        cur_pic: &PictureData,
    ) -> (DpbPicList<B::Handle>, DpbPicList<B::Handle>) {
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
    fn build_ref_field_pic_list_b(
        dpb: &Dpb<B::Handle>,
        cur_pic: &PictureData,
    ) -> (DpbPicList<B::Handle>, DpbPicList<B::Handle>) {
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
        ref_frame_list: &mut DpbPicList<B::Handle>,
        ref_pic_list: &mut DpbPicList<B::Handle>,
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

    fn build_ref_pic_lists(
        dpb: &Dpb<B::Handle>,
        cur_pic: &PictureData,
    ) -> ReferencePicLists<B::Handle> {
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
            return Default::default();
        }

        let (ref_pic_list_p0, (ref_pic_list_b0, ref_pic_list_b1)) =
            if matches!(cur_pic.field, Field::Frame) {
                (
                    Self::build_ref_pic_list_p(dpb),
                    Self::build_ref_pic_list_b(dpb, cur_pic),
                )
            } else {
                (
                    Self::build_ref_field_pic_list_p(dpb, cur_pic),
                    Self::build_ref_field_pic_list_b(dpb, cur_pic),
                )
            };

        ReferencePicLists {
            ref_pic_list_p0,
            ref_pic_list_b0,
            ref_pic_list_b1,
        }
    }

    fn handle_memory_management_ops(&mut self, pic: &mut PictureData) -> anyhow::Result<()> {
        let markings = pic.ref_pic_marking.clone();

        for (i, marking) in markings.inner().iter().enumerate() {
            match marking.memory_management_control_operation() {
                0 => break,
                1 => self.codec.dpb.mmco_op_1(pic, i)?,
                2 => self.codec.dpb.mmco_op_2(pic, i)?,
                3 => self.codec.dpb.mmco_op_3(pic, i)?,
                4 => self.codec.max_long_term_frame_idx = self.codec.dpb.mmco_op_4(pic, i),
                5 => self.codec.max_long_term_frame_idx = self.codec.dpb.mmco_op_5(pic),
                6 => self.codec.dpb.mmco_op_6(pic, i),
                other => anyhow::bail!("unknown MMCO={}", other),
            }
        }

        Ok(())
    }

    /// Store some variables related to the previous reference picture. These
    /// will be used in the decoding of future pictures.
    fn fill_prev_ref_info(&mut self, pic: &PictureData) {
        let prev = &mut self.codec.prev_ref_pic_info;

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
        let prev = &mut self.codec.prev_pic_info;

        prev.frame_num = pic.frame_num;
        prev.has_mmco_5 = pic.has_mmco_5;
        prev.frame_num_offset = pic.frame_num_offset;
    }

    fn reference_pic_marking(&mut self, pic: &mut PictureData) -> anyhow::Result<()> {
        /* 8.2.5.1 */
        if matches!(pic.is_idr, IsIdr::Yes { .. }) {
            self.codec.dpb.mark_all_as_unused_for_ref();

            if pic.ref_pic_marking.long_term_reference_flag() {
                pic.set_reference(Reference::LongTerm, false);
                pic.long_term_frame_idx = 0;
                self.codec.max_long_term_frame_idx = 0;
            } else {
                pic.set_reference(Reference::ShortTerm, false);
                self.codec.max_long_term_frame_idx = -1;
            }

            return Ok(());
        }

        if pic.ref_pic_marking.adaptive_ref_pic_marking_mode_flag() {
            self.handle_memory_management_ops(pic)?;
        } else {
            self.codec.sliding_window_marking(pic)?;
        }

        Ok(())
    }

    fn add_to_dpb(
        dpb: &mut Dpb<B::Handle>,
        pic: Rc<RefCell<PictureData>>,
        handle: Option<B::Handle>,
        last_field: &mut Option<(Rc<RefCell<PictureData>>, B::Handle)>,
    ) -> anyhow::Result<()> {
        if !dpb.interlaced() {
            assert!(last_field.is_none());

            dpb.store_picture(pic, handle)?;
        } else {
            // If we have a cached field for this picture, we must combine
            // them before insertion.
            if pic
                .borrow()
                .other_field()
                .zip(last_field.as_ref().map(|f| &f.0))
                .map_or_else(
                    || false,
                    |(other_field, last_field)| Rc::ptr_eq(&other_field, last_field),
                )
            {
                if let Some((last_field, last_field_handle)) = last_field.take() {
                    dpb.store_picture(last_field, Some(last_field_handle))?;
                }
            }

            dpb.store_picture(pic, handle)?;
        }

        Ok(())
    }

    /// Adds picture to the ready queue if it could not be added to the DPB.
    fn add_to_ready_queue(&mut self, pic_rc: Rc<RefCell<PictureData>>, handle: B::Handle) {
        let pic = pic_rc.borrow();

        if matches!(pic.field, Field::Frame) {
            assert!(self.codec.last_field.is_none());

            self.ready_queue.push(handle);
        } else {
            match &self.codec.last_field {
                None => {
                    assert!(!pic.is_second_field());
                    drop(pic);

                    // Cache the field, wait for its pair.
                    self.codec.last_field = Some((pic_rc, handle));
                }
                Some(last_field)
                    if pic.is_second_field()
                        && pic.other_field().is_some()
                        && Rc::ptr_eq(&pic.other_field().unwrap(), &last_field.0) =>
                {
                    if let Some((field_pic, field_handle)) = self.codec.last_field.take() {
                        field_pic.borrow_mut().set_second_field_to(&pic_rc);
                        self.ready_queue.push(field_handle);
                    }
                }
                _ => {
                    // Somehow, the last field is not paired with the current field.
                    self.codec.last_field = None;
                }
            }
        }
    }

    fn finish_picture(&mut self, pic: CurrentPicState<B>) -> anyhow::Result<()> {
        debug!("Finishing picture POC {:?}", pic.pic.pic_order_cnt);

        // Submit the picture to the backend.
        let handle = self.submit_picture(pic.backend_pic)?;
        let mut pic = pic.pic;

        if matches!(pic.reference(), Reference::ShortTerm | Reference::LongTerm) {
            self.reference_pic_marking(&mut pic)?;
            self.fill_prev_ref_info(&pic);
        }

        self.fill_prev_info(&pic);

        self.codec.dpb.remove_unused();

        if pic.has_mmco_5 {
            // C.4.5.3 "Bumping process"
            // The bumping process is invoked in the following cases:
            // Clause 3:
            // The current picture has memory_management_control_operation equal
            // to 5, as specified in clause C.4.4.
            self.ready_queue.extend(self.codec.drain());
        }

        // Bump the DPB as per C.4.5.3 to cover clauses 1, 4, 5 and 6.
        self.ready_queue.extend(self.codec.bump_as_needed(&pic));

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
            || self.codec.dpb.has_empty_frame_buffer()
        {
            if self.codec.dpb.interlaced() && matches!(pic.field, Field::Frame) {
                drop(pic);

                // Split the Frame into two complementary fields so reference
                // marking is easier. This is inspired by the GStreamer implementation.
                let other_field = PictureData::split_frame(&pic_rc);
                let other_field_handle = handle.clone();

                Self::add_to_dpb(
                    &mut self.codec.dpb,
                    pic_rc,
                    Some(handle),
                    &mut self.codec.last_field,
                )?;
                Self::add_to_dpb(
                    &mut self.codec.dpb,
                    other_field,
                    Some(other_field_handle),
                    &mut self.codec.last_field,
                )?;
            } else {
                drop(pic);
                Self::add_to_dpb(
                    &mut self.codec.dpb,
                    pic_rc,
                    Some(handle),
                    &mut self.codec.last_field,
                )?;
            }
        } else {
            drop(pic);
            self.add_to_ready_queue(pic_rc, handle);
        }

        Ok(())
    }

    fn handle_frame_num_gap(&mut self, frame_num: i32, timestamp: u64) -> anyhow::Result<()> {
        if self.codec.dpb.is_empty() {
            return Ok(());
        }

        debug!("frame_num gap detected.");

        let sps = self
            .codec
            .parser
            .get_sps(self.codec.cur_sps_id)
            .context("Invalid SPS while handling a frame_num gap")?;

        if !sps.gaps_in_frame_num_value_allowed_flag {
            return Err(anyhow!(
                "Invalid frame_num: {}. Assuming unintentional loss of pictures",
                frame_num
            ));
        }

        let mut unused_short_term_frame_num =
            (self.codec.prev_ref_pic_info.frame_num + 1) % sps.max_frame_num() as i32;
        while unused_short_term_frame_num != frame_num {
            let sps = self
                .codec
                .parser
                .get_sps(self.codec.cur_sps_id)
                .context("Invalid SPS while handling a frame_num gap")?;
            let max_frame_num = sps.max_frame_num() as i32;

            let mut pic = PictureData::new_non_existing(unused_short_term_frame_num, timestamp);
            self.codec.compute_pic_order_count(&mut pic)?;

            self.codec
                .dpb
                .update_pic_nums(unused_short_term_frame_num, max_frame_num, &pic);

            self.codec.sliding_window_marking(&mut pic)?;

            self.codec.dpb.remove_unused();
            self.ready_queue.extend(self.codec.bump_as_needed(&pic));

            let pic_rc = Rc::new(RefCell::new(pic));

            if self.codec.dpb.interlaced() {
                let other_field = PictureData::split_frame(&pic_rc);

                Self::add_to_dpb(
                    &mut self.codec.dpb,
                    pic_rc,
                    None,
                    &mut self.codec.last_field,
                )?;
                Self::add_to_dpb(
                    &mut self.codec.dpb,
                    other_field,
                    None,
                    &mut self.codec.last_field,
                )?;
            } else {
                Self::add_to_dpb(
                    &mut self.codec.dpb,
                    pic_rc,
                    None,
                    &mut self.codec.last_field,
                )?;
            }

            unused_short_term_frame_num += 1;
            unused_short_term_frame_num %= max_frame_num;
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
            .codec
            .parser
            .get_pps(slice.header().pic_parameter_set_id)
            .context("Invalid SPS in init_current_pic")?;

        let sps = self
            .codec
            .parser
            .get_sps(pps.seq_parameter_set_id())
            .context("Invalid PPS in init_current_pic")?;
        let max_frame_num = sps.max_frame_num() as i32;

        let mut pic = PictureData::new_from_slice(slice, sps, timestamp);

        if let Some(first_field) = first_field {
            pic.set_first_field_to(first_field);
        }

        self.codec.compute_pic_order_count(&mut pic)?;

        if matches!(pic.is_idr, IsIdr::Yes { .. }) {
            // C.4.5.3 "Bumping process"
            // The bumping process is invoked in the following cases:
            // Clause 2:
            // The current picture is an IDR picture and
            // no_output_of_prior_pics_flag is not equal to 1 and is not
            // inferred to be equal to 1, as specified in clause C.4.4.
            if !pic.ref_pic_marking.no_output_of_prior_pics_flag() {
                self.ready_queue.extend(self.codec.drain());
            } else {
                // C.4.4 When no_output_of_prior_pics_flag is equal to 1 or is
                // inferred to be equal to 1, all frame buffers in the DPB are
                // emptied without output of the pictures they contain, and DPB
                // fullness is set to 0.
                self.codec.dpb.clear();
            }
        }

        self.codec
            .dpb
            .update_pic_nums(i32::from(slice.header().frame_num), max_frame_num, &pic);

        Ok(pic)
    }

    /// Called once per picture to start it.
    fn begin_picture(
        &mut self,
        timestamp: u64,
        slice: &Slice<&[u8]>,
    ) -> anyhow::Result<CurrentPicState<B>> {
        let nalu_hdr = slice.nalu().header();

        if nalu_hdr.idr_pic_flag() {
            self.codec.prev_ref_pic_info.frame_num = 0;
        }

        let hdr = slice.header();
        let frame_num = i32::from(hdr.frame_num);

        self.codec.cur_pps_id = hdr.pic_parameter_set_id;

        let pps = self
            .codec
            .parser
            .get_pps(self.codec.cur_pps_id)
            .context("Invalid PPS in handle_picture")?;

        self.codec.cur_sps_id = pps.seq_parameter_set_id();

        let sps = self
            .codec
            .parser
            .get_sps(self.codec.cur_sps_id)
            .context("Invalid SPS in handle_picture")?;

        if frame_num != self.codec.prev_ref_pic_info.frame_num
            && frame_num
                != (self.codec.prev_ref_pic_info.frame_num + 1) % sps.max_frame_num() as i32
        {
            self.handle_frame_num_gap(frame_num, timestamp)?;
        }

        let first_field = self.codec.find_first_field(slice)?;

        let pic = self.init_current_pic(slice, first_field.as_ref().map(|f| &f.0), timestamp)?;
        let ref_pic_lists = Self::build_ref_pic_lists(&self.codec.dpb, &pic);

        debug!("Decode picture POC {:?}", pic.pic_order_cnt);

        let mut backend_pic = if let Some(first_field) = first_field {
            self.backend
                .new_field_picture(&pic, timestamp, &first_field.1)?
        } else {
            self.backend.new_picture(&pic, timestamp)?
        };

        self.backend.start_picture(
            &mut backend_pic,
            &pic,
            self.codec
                .parser
                .get_sps(self.codec.cur_sps_id)
                .context("Invalid SPS in handle_picture")?,
            self.codec
                .parser
                .get_pps(self.codec.cur_pps_id)
                .context("invalid PPS in handle_picture")?,
            &self.codec.dpb,
            slice,
        )?;

        Ok(CurrentPicState {
            pic,
            backend_pic,
            ref_pic_lists,
        })
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
        dpb: &Dpb<B::Handle>,
        ref_pic_list_x: &mut DpbPicList<B::Handle>,
        num_ref_idx_lx_active_minus1: u8,
        max_pic_num: i32,
        rplm: &RefPicListModification,
        pic_num_lx_pred: &mut i32,
        ref_idx_lx: &mut usize,
    ) -> anyhow::Result<()> {
        let pic_num_lx_no_wrap;
        let abs_diff_pic_num = rplm.abs_diff_pic_num_minus1() as i32 + 1;
        let modification_of_pic_nums_idc = rplm.modification_of_pic_nums_idc();

        if modification_of_pic_nums_idc == 0 {
            if *pic_num_lx_pred - abs_diff_pic_num < 0 {
                pic_num_lx_no_wrap = *pic_num_lx_pred - abs_diff_pic_num + max_pic_num;
            } else {
                pic_num_lx_no_wrap = *pic_num_lx_pred - abs_diff_pic_num;
            }
        } else if modification_of_pic_nums_idc == 1 {
            if *pic_num_lx_pred + abs_diff_pic_num >= max_pic_num {
                pic_num_lx_no_wrap = *pic_num_lx_pred + abs_diff_pic_num - max_pic_num;
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
            pic_num_lx_no_wrap - max_pic_num
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
        dpb: &Dpb<B::Handle>,
        ref_pic_list_x: &mut DpbPicList<B::Handle>,
        num_ref_idx_lx_active_minus1: u8,
        max_long_term_frame_idx: i32,
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
        mut ref_pic_list_x: DpbPicList<B::Handle>,
    ) -> anyhow::Result<DpbPicList<B::Handle>> {
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
                        &self.codec.dpb,
                        &mut ref_pic_list_x,
                        num_ref_idx_lx_active_minus1,
                        hdr.max_pic_num as i32,
                        modification,
                        &mut pic_num_lx_pred,
                        &mut ref_idx_lx,
                    )?;
                }
                2 => Self::long_term_pic_list_modification(
                    &self.codec.dpb,
                    &mut ref_pic_list_x,
                    num_ref_idx_lx_active_minus1,
                    self.codec.max_long_term_frame_idx,
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
        hdr: &SliceHeader,
        ref_pic_lists: &ReferencePicLists<B::Handle>,
    ) -> anyhow::Result<RefPicLists<B::Handle>> {
        let mut ref_pic_list0 = Vec::new();
        let mut ref_pic_list1 = Vec::new();

        if let SliceType::P | SliceType::Sp = hdr.slice_type {
            ref_pic_list0 = self.modify_ref_pic_list(
                cur_pic,
                hdr,
                RefPicList::RefPicList0,
                ref_pic_lists.ref_pic_list_p0.clone(),
            )?;
        } else if let SliceType::B = hdr.slice_type {
            ref_pic_list0 = self.modify_ref_pic_list(
                cur_pic,
                hdr,
                RefPicList::RefPicList0,
                ref_pic_lists.ref_pic_list_b0.clone(),
            )?;
            ref_pic_list1 = self.modify_ref_pic_list(
                cur_pic,
                hdr,
                RefPicList::RefPicList1,
                ref_pic_lists.ref_pic_list_b1.clone(),
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
        cur_pic: &mut CurrentPicState<B>,
        slice: &Slice<&[u8]>,
    ) -> anyhow::Result<()> {
        let RefPicLists {
            ref_pic_list0,
            ref_pic_list1,
        } = self.create_ref_pic_lists(&cur_pic.pic, slice.header(), &cur_pic.ref_pic_lists)?;

        let sps = self
            .codec
            .parser
            .get_sps(self.codec.cur_sps_id)
            .context("Invalid SPS in handle_slice")?;

        let pps = self
            .codec
            .parser
            .get_pps(self.codec.cur_pps_id)
            .context("Invalid PPS in handle_slice")?;

        self.backend.decode_slice(
            &mut cur_pic.backend_pic,
            slice,
            sps,
            pps,
            &self.codec.dpb,
            &ref_pic_list0,
            &ref_pic_list1,
        )?;

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
            return Err(DecodeError::NotEnoughOutputBuffers(1));
        }

        let mut cursor = Cursor::new(bitstream);

        let mut cur_pic_opt: Option<CurrentPicState<B>> = None;

        while let Ok(Some(nalu)) = Nalu::next(&mut cursor) {
            match nalu.header().nalu_type() {
                NaluType::Sps => {
                    self.codec.parser.parse_sps(&nalu)?;
                }

                NaluType::Pps => {
                    self.codec.parser.parse_pps(&nalu)?;
                }

                NaluType::Slice
                | NaluType::SliceDpa
                | NaluType::SliceDpb
                | NaluType::SliceDpc
                | NaluType::SliceIdr
                | NaluType::SliceExt => {
                    let slice = self.codec.parser.parse_slice_header(nalu)?;
                    let mut cur_pic = match cur_pic_opt {
                        // No current picture, start a new one.
                        None => self.begin_picture(timestamp, &slice)?,
                        // We have a current picture but are starting a new field: finish it and
                        // start a new one.
                        Some(cur_pic)
                            if self.codec.dpb.interlaced()
                                && matches!(cur_pic.pic.field, Field::Frame)
                                && !cur_pic.pic.is_second_field()
                                && cur_pic.pic.field != slice.header().field() =>
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

impl<B> StatelessVideoDecoder<<B::Handle as DecodedHandle>::Descriptor>
    for StatelessDecoder<H264, B>
where
    B: StatelessH264DecoderBackend,
    B::Handle: Clone + 'static,
{
    fn decode(&mut self, timestamp: u64, mut bitstream: &[u8]) -> Result<(), DecodeError> {
        let sps = Self::peek_sps(&mut self.codec.parser, bitstream);

        if let Some(sps) = sps {
            if Self::negotiation_possible(&sps, &self.codec.dpb, self.coded_resolution) {
                // Make sure all the frames we decoded so far are in the ready queue.
                self.ready_queue.extend(self.codec.drain());
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
        self.ready_queue.extend(self.codec.drain());
        self.decoding_state = DecodingState::Reset;
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
                            decoder.apply_sps(sps);
                            decoder.decoding_state = DecodingState::Decoding;
                        }),
                    )))
                } else {
                    None
                }
            })
    }

    fn surface_pool(&mut self) -> &mut dyn SurfacePool<<B::Handle as DecodedHandle>::Descriptor> {
        self.surface_pool()
    }

    fn stream_info(&self) -> Option<&StreamInfo> {
        self.stream_info()
    }
}

#[cfg(test)]
pub mod tests {
    use crate::decoder::stateless::h264::H264;
    use crate::decoder::stateless::tests::test_decode_stream;
    use crate::decoder::stateless::tests::TestStream;
    use crate::decoder::stateless::StatelessDecoder;
    use crate::decoder::BlockingMode;
    use crate::utils::simple_playback_loop;
    use crate::utils::simple_playback_loop_owned_surfaces;
    use crate::utils::H264FrameIterator;
    use crate::DecodedFormat;

    /// Run `test` using the dummy decoder, in both blocking and non-blocking modes.
    fn test_decoder_dummy(test: &TestStream, blocking_mode: BlockingMode) {
        let decoder = StatelessDecoder::<H264, _>::new_dummy(blocking_mode);

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
