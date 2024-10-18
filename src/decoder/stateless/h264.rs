// Copyright 2023 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#[cfg(any(test, fuzzing))]
mod dummy;
#[cfg(feature = "v4l2")]
mod v4l2;
#[cfg(feature = "vaapi")]
mod vaapi;

use std::collections::btree_map::Entry;
use std::io::Cursor;
use std::os::fd::AsFd;
use std::os::fd::BorrowedFd;
use std::rc::Rc;

use anyhow::anyhow;
use anyhow::Context;
use log::debug;
use thiserror::Error;

use crate::codec::h264::dpb::Dpb;
use crate::codec::h264::dpb::DpbEntry;
use crate::codec::h264::dpb::DpbPicRefList;
use crate::codec::h264::dpb::MmcoError;
use crate::codec::h264::dpb::ReferencePicLists;
use crate::codec::h264::parser::MaxLongTermFrameIdx;
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
use crate::codec::h264::picture::FieldRank;
use crate::codec::h264::picture::IsIdr;
use crate::codec::h264::picture::PictureData;
use crate::codec::h264::picture::RcPictureData;
use crate::codec::h264::picture::Reference;
use crate::decoder::stateless::DecodeError;
use crate::decoder::stateless::DecodingState;
use crate::decoder::stateless::NewPictureResult;
use crate::decoder::stateless::PoolLayer;
use crate::decoder::stateless::StatelessBackendResult;
use crate::decoder::stateless::StatelessCodec;
use crate::decoder::stateless::StatelessDecoder;
use crate::decoder::stateless::StatelessDecoderBackend;
use crate::decoder::stateless::StatelessDecoderBackendPicture;
use crate::decoder::stateless::StatelessVideoDecoder;
use crate::decoder::stateless::TryFormat;
use crate::decoder::BlockingMode;
use crate::decoder::DecodedHandle;
use crate::decoder::DecoderEvent;
use crate::decoder::StreamInfo;
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
pub trait StatelessH264DecoderBackend:
    StatelessDecoderBackend + StatelessDecoderBackendPicture<H264>
{
    /// Called when a new SPS is parsed.
    fn new_sequence(&mut self, sps: &Rc<Sps>) -> StatelessBackendResult<()>;

    /// Called when the decoder determines that a frame or field was found.
    fn new_picture(&mut self, timestamp: u64) -> NewPictureResult<Self::Picture>;

    /// Called when the decoder determines that a second field was found.
    /// Indicates that the underlying BackendHandle is to be shared between the
    /// two pictures. This is so both fields decode to the same underlying
    /// resource and can thus be presented together as a single frame.
    fn new_field_picture(
        &mut self,
        timestamp: u64,
        first_field: &Self::Handle,
    ) -> NewPictureResult<Self::Picture>;

    /// Called by the decoder when starting a new frame or field.
    fn start_picture(
        &mut self,
        picture: &mut Self::Picture,
        picture_data: &PictureData,
        sps: &Sps,
        pps: &Pps,
        dpb: &Dpb<Self::Handle>,
        hdr: &SliceHeader,
    ) -> StatelessBackendResult<()>;

    /// Called to dispatch a decode operation to the backend.
    #[allow(clippy::too_many_arguments)]
    fn decode_slice(
        &mut self,
        picture: &mut Self::Picture,
        slice: &Slice,
        sps: &Sps,
        pps: &Pps,
        ref_pic_list0: &[&DpbEntry<Self::Handle>],
        ref_pic_list1: &[&DpbEntry<Self::Handle>],
    ) -> StatelessBackendResult<()>;

    /// Called when the decoder wants the backend to finish the decoding
    /// operations for `picture`. At this point, `decode_slice` has been called
    /// for all slices.
    ///
    /// This call will assign the ownership of the BackendHandle to the Picture
    /// and then assign the ownership of the Picture to the Handle.
    fn submit_picture(&mut self, picture: Self::Picture) -> StatelessBackendResult<Self::Handle>;
}

/// Keeps track of the last values seen for negotiation purposes.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
struct NegotiationInfo {
    /// The current coded resolution
    coded_resolution: Resolution,
    /// Same meaning as the specification.
    profile_idc: u8,
    /// Same meaning as the specification.
    bit_depth_luma_minus8: u8,
    /// Same meaning as the specification.
    bit_depth_chroma_minus8: u8,
    /// Same meaning as the specification.
    chroma_format_idc: u8,
    /// The maximum size of the dpb in frames.
    max_dpb_frames: usize,
    /// Whether this is an interlaced stream
    interlaced: bool,
}

impl From<&Sps> for NegotiationInfo {
    fn from(sps: &Sps) -> Self {
        NegotiationInfo {
            coded_resolution: Resolution::from((sps.width(), sps.height())),
            profile_idc: sps.profile_idc,
            bit_depth_luma_minus8: sps.bit_depth_luma_minus8,
            bit_depth_chroma_minus8: sps.bit_depth_chroma_minus8,
            chroma_format_idc: sps.chroma_format_idc,
            max_dpb_frames: sps.max_dpb_frames(),
            interlaced: !sps.frame_mbs_only_flag,
        }
    }
}

#[derive(Copy, Clone, Debug)]
enum RefPicList {
    RefPicList0,
    RefPicList1,
}

pub struct PrevReferencePicInfo {
    frame_num: u32,
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

impl PrevReferencePicInfo {
    /// Store some variables related to the previous reference picture. These
    /// will be used in the decoding of future pictures.
    fn fill(&mut self, pic: &PictureData) {
        self.has_mmco_5 = pic.has_mmco_5;
        self.top_field_order_cnt = pic.top_field_order_cnt;
        self.pic_order_cnt_msb = pic.pic_order_cnt_msb;
        self.pic_order_cnt_lsb = pic.pic_order_cnt_lsb;
        self.field = pic.field;
        self.frame_num = pic.frame_num;
    }
}

#[derive(Default)]
pub struct PrevPicInfo {
    frame_num: u32,
    frame_num_offset: u32,
    has_mmco_5: bool,
}

impl PrevPicInfo {
    /// Store some variables related to the previous picture. These will be used
    /// in the decoding of future pictures.
    fn fill(&mut self, pic: &PictureData) {
        self.frame_num = pic.frame_num;
        self.has_mmco_5 = pic.has_mmco_5;
        self.frame_num_offset = pic.frame_num_offset;
    }
}

/// Corresponds to RefPicList0 and RefPicList1 in the specification. Computed for every slice,
/// points to the pictures in the DPB.
struct RefPicLists<'a, T> {
    ref_pic_list0: DpbPicRefList<'a, T>,
    ref_pic_list1: DpbPicRefList<'a, T>,
}

/// Used to track that first_mb_in_slice increases monotonically.
enum CurrentMacroblockTracking {
    SeparateColorPlane(std::collections::BTreeMap<u8, u32>),
    NonSeparateColorPlane(u32),
}

/// State of the picture being currently decoded.
///
/// Stored between calls to [`StatelessDecoder::handle_slice`] that belong to the same picture.
struct CurrentPicState<P> {
    /// Data for the current picture as extracted from the stream.
    pic: PictureData,
    /// PPS at the time of the current picture.
    pps: Rc<Pps>,
    /// Backend-specific data for that picture.
    backend_pic: P,
    /// List of reference pictures, used once per slice.
    ref_pic_lists: ReferencePicLists,
    /// The current macroblock we are processing
    current_macroblock: CurrentMacroblockTracking,
}

/// State of the H.264 decoder.
///
/// `B` is the backend used for this decoder.
pub struct H264DecoderState<H: DecodedHandle, P> {
    /// H.264 bitstream parser.
    parser: Parser,
    /// Keeps track of the last stream parameters seen for negotiation purposes.
    negotiation_info: NegotiationInfo,

    /// The decoded picture buffer.
    dpb: Dpb<H>,

    /// Cached variables from the previous reference picture.
    prev_ref_pic_info: PrevReferencePicInfo,
    /// Cached variables from the previous picture.
    prev_pic_info: PrevPicInfo,
    /// Maximum index of the long-term frame.
    max_long_term_frame_idx: MaxLongTermFrameIdx,

    /// The picture currently being decoded. We need to preserve it between calls to `decode`
    /// because multiple slices will be processed in different calls to `decode`.
    current_pic: Option<CurrentPicState<P>>,
}

impl<H, P> Default for H264DecoderState<H, P>
where
    H: DecodedHandle,
{
    fn default() -> Self {
        H264DecoderState {
            parser: Default::default(),
            negotiation_info: Default::default(),
            dpb: Default::default(),
            prev_ref_pic_info: Default::default(),
            prev_pic_info: Default::default(),
            max_long_term_frame_idx: Default::default(),
            current_pic: None,
        }
    }
}

/// [`StatelessCodec`] structure to use in order to create a H.264 stateless decoder.
///
/// # Accepted input
///
/// A decoder using this codec processes exactly one NAL unit of input per call to
/// [`StatelessDecoder::decode`], and returns the number of bytes until the end of this NAL unit.
/// This makes it possible to call [`Decode`](StatelessDecoder::decode) repeatedly on some unsplit
/// Annex B stream and shrinking it by the number of bytes processed after each call, until the
/// stream ends up being empty.
pub struct H264;

impl StatelessCodec for H264 {
    type FormatInfo = Rc<Sps>;
    type DecoderState<H: DecodedHandle, P> = H264DecoderState<H, P>;
}

#[derive(Debug, Error)]
enum FindFirstFieldError {
    #[error("expected complementary field {0:?}, got {1:?}")]
    ExpectedComplementaryField(Field, Field),
    #[error("the previous field's frame_num value {0} differs from the current one's {1}")]
    FrameNumDiffers(u32, u32),
}

impl<H, P> H264DecoderState<H, P>
where
    H: DecodedHandle + Clone,
{
    fn compute_pic_order_count(&mut self, pic: &mut PictureData, sps: &Sps) -> anyhow::Result<()> {
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

                pic.pic_order_cnt_msb = if (pic.pic_order_cnt_lsb
                    < self.prev_ref_pic_info.pic_order_cnt_lsb)
                    && (prev_pic_order_cnt_lsb - pic.pic_order_cnt_lsb >= max_pic_order_cnt_lsb / 2)
                {
                    prev_pic_order_cnt_msb + max_pic_order_cnt_lsb
                } else if (pic.pic_order_cnt_lsb > prev_pic_order_cnt_lsb)
                    && (pic.pic_order_cnt_lsb - prev_pic_order_cnt_lsb > max_pic_order_cnt_lsb / 2)
                {
                    prev_pic_order_cnt_msb - max_pic_order_cnt_lsb
                } else {
                    prev_pic_order_cnt_msb
                };

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
                        self.prev_pic_info.frame_num_offset + sps.max_frame_num();
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
                        (abs_frame_num - 1) / sps.num_ref_frames_in_pic_order_cnt_cycle as u32;
                    let frame_num_in_pic_order_cnt_cycle =
                        (abs_frame_num - 1) % sps.num_ref_frames_in_pic_order_cnt_cycle as u32;
                    expected_pic_order_cnt =
                        pic_order_cnt_cycle_cnt as i32 * sps.expected_delta_per_pic_order_cnt_cycle;

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
                        self.prev_pic_info.frame_num_offset + sps.max_frame_num();
                } else {
                    pic.frame_num_offset = self.prev_pic_info.frame_num_offset;
                }

                let pic_order_cnt = if matches!(pic.is_idr, IsIdr::Yes { .. }) {
                    0
                } else if pic.nal_ref_idc == 0 {
                    2 * (pic.frame_num_offset + pic.frame_num) as i32 - 1
                } else {
                    2 * (pic.frame_num_offset + pic.frame_num) as i32
                };

                if matches!(pic.field, Field::Frame | Field::Top) {
                    pic.top_field_order_cnt = pic_order_cnt;
                }
                if matches!(pic.field, Field::Frame | Field::Bottom) {
                    pic.bottom_field_order_cnt = pic_order_cnt;
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

    /// Returns an iterator of the handles of the frames that need to be bumped into the ready
    /// queue.
    fn bump_as_needed(&mut self, current_pic: &PictureData) -> impl Iterator<Item = H> {
        self.dpb.bump_as_needed(current_pic).into_iter().flatten()
    }

    /// Returns an iterator of the handles of all the frames still present in the DPB.
    fn drain(&mut self) -> impl Iterator<Item = H> {
        let pics = self.dpb.drain();

        pics.into_iter().flatten()
    }

    /// Find the first field for the picture started by `slice`, if any.
    fn find_first_field(
        &self,
        hdr: &SliceHeader,
    ) -> Result<Option<(RcPictureData, H)>, FindFirstFieldError> {
        let mut prev_field = None;

        if self.dpb.interlaced() {
            if let Some(last_dpb_entry) = self.dpb.entries().last() {
                // Use the last entry in the DPB
                let last_pic = last_dpb_entry.pic.borrow();

                // If the picture is interlaced but doesn't have its other field set yet, then it must
                // be the first field.
                if !matches!(last_pic.field, Field::Frame)
                    && matches!(last_pic.field_rank(), FieldRank::Single)
                {
                    if let Some(handle) = &last_dpb_entry.reference {
                        // Still waiting for the second field
                        prev_field = Some((last_dpb_entry.pic.clone(), handle.clone()));
                    }
                }
            }
        }

        let prev_field = match prev_field {
            None => return Ok(None),
            Some(prev_field) => prev_field,
        };

        let prev_field_pic = prev_field.0.borrow();

        if prev_field_pic.frame_num != u32::from(hdr.frame_num) {
            return Err(FindFirstFieldError::FrameNumDiffers(
                prev_field_pic.frame_num,
                hdr.frame_num as u32,
            ));
        }

        let cur_field = if hdr.bottom_field_flag {
            Field::Bottom
        } else {
            Field::Top
        };

        if !hdr.field_pic_flag || cur_field == prev_field_pic.field {
            let field = prev_field_pic.field;

            return Err(FindFirstFieldError::ExpectedComplementaryField(
                field.opposite(),
                field,
            ));
        }

        drop(prev_field_pic);
        Ok(Some(prev_field))
    }

    // 8.2.4.3.1 Modification process of reference picture lists for short-term
    // reference pictures
    #[allow(clippy::too_many_arguments)]
    fn short_term_pic_list_modification<'a>(
        cur_pic: &PictureData,
        dpb: &'a Dpb<H>,
        ref_pic_list_x: &mut DpbPicRefList<'a, H>,
        num_ref_idx_lx_active_minus1: u8,
        max_pic_num: i32,
        rplm: &RefPicListModification,
        pic_num_lx_pred: &mut i32,
        ref_idx_lx: &mut usize,
    ) -> anyhow::Result<()> {
        let pic_num_lx_no_wrap;
        let abs_diff_pic_num = rplm.abs_diff_pic_num_minus1 as i32 + 1;
        let modification_of_pic_nums_idc = rplm.modification_of_pic_nums_idc;

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
                rplm.modification_of_pic_nums_idc
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

        if *ref_idx_lx >= ref_pic_list_x.len() {
            anyhow::bail!("invalid ref_idx_lx index");
        }
        ref_pic_list_x.insert(*ref_idx_lx, handle);
        *ref_idx_lx += 1;

        let mut nidx = *ref_idx_lx;

        for cidx in *ref_idx_lx..=usize::from(num_ref_idx_lx_active_minus1) + 1 {
            if cidx == ref_pic_list_x.len() {
                break;
            }

            let target = &ref_pic_list_x[cidx].pic;

            if target.borrow().pic_num_f(max_pic_num) != pic_num_lx {
                ref_pic_list_x[nidx] = ref_pic_list_x[cidx];
                nidx += 1;
            }
        }

        while ref_pic_list_x.len() > (usize::from(num_ref_idx_lx_active_minus1) + 1) {
            ref_pic_list_x.pop();
        }

        Ok(())
    }

    fn long_term_pic_list_modification<'a>(
        dpb: &'a Dpb<H>,
        ref_pic_list_x: &mut DpbPicRefList<'a, H>,
        num_ref_idx_lx_active_minus1: u8,
        max_long_term_frame_idx: MaxLongTermFrameIdx,
        rplm: &RefPicListModification,
        ref_idx_lx: &mut usize,
    ) -> anyhow::Result<()> {
        let long_term_pic_num = rplm.long_term_pic_num;

        let handle = dpb
            .find_long_term_with_long_term_pic_num(long_term_pic_num)
            .with_context(|| {
                format!(
                    "No LongTerm reference found with long_term_pic_num {}",
                    long_term_pic_num
                )
            })?;

        if *ref_idx_lx >= ref_pic_list_x.len() {
            anyhow::bail!("invalid ref_idx_lx index");
        }
        ref_pic_list_x.insert(*ref_idx_lx, handle);
        *ref_idx_lx += 1;

        let mut nidx = *ref_idx_lx;

        for cidx in *ref_idx_lx..=usize::from(num_ref_idx_lx_active_minus1) + 1 {
            if cidx == ref_pic_list_x.len() {
                break;
            }

            let target = &ref_pic_list_x[cidx].pic;
            if target.borrow().long_term_pic_num_f(max_long_term_frame_idx) != long_term_pic_num {
                ref_pic_list_x[nidx] = ref_pic_list_x[cidx];
                nidx += 1;
            }
        }

        while ref_pic_list_x.len() > (usize::from(num_ref_idx_lx_active_minus1) + 1) {
            ref_pic_list_x.pop();
        }

        Ok(())
    }

    fn modify_ref_pic_list(
        &self,
        cur_pic: &PictureData,
        hdr: &SliceHeader,
        ref_pic_list_type: RefPicList,
        ref_pic_list_indices: &[usize],
    ) -> anyhow::Result<DpbPicRefList<H>> {
        let (ref_pic_list_modification_flag_lx, num_ref_idx_lx_active_minus1, rplm) =
            match ref_pic_list_type {
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

        let mut ref_pic_list: Vec<_> = ref_pic_list_indices
            .iter()
            .map(|&i| &self.dpb.entries()[i])
            .take(usize::from(num_ref_idx_lx_active_minus1) + 1)
            .collect();

        if !ref_pic_list_modification_flag_lx {
            return Ok(ref_pic_list);
        }

        let mut pic_num_lx_pred = cur_pic.pic_num;
        let mut ref_idx_lx = 0;

        for modification in rplm {
            let idc = modification.modification_of_pic_nums_idc;

            match idc {
                0 | 1 => {
                    Self::short_term_pic_list_modification(
                        cur_pic,
                        &self.dpb,
                        &mut ref_pic_list,
                        num_ref_idx_lx_active_minus1,
                        hdr.max_pic_num as i32,
                        modification,
                        &mut pic_num_lx_pred,
                        &mut ref_idx_lx,
                    )?;
                }
                2 => Self::long_term_pic_list_modification(
                    &self.dpb,
                    &mut ref_pic_list,
                    num_ref_idx_lx_active_minus1,
                    self.max_long_term_frame_idx,
                    modification,
                    &mut ref_idx_lx,
                )?,
                3 => break,
                _ => anyhow::bail!("unexpected modification_of_pic_nums_idc {:?}", idc),
            }
        }

        Ok(ref_pic_list)
    }

    /// Generate RefPicList0 and RefPicList1 in the specification. Computed for every slice, points
    /// to the pictures in the DPB.
    fn create_ref_pic_lists(
        &mut self,
        cur_pic: &PictureData,
        hdr: &SliceHeader,
        ref_pic_lists: &ReferencePicLists,
    ) -> anyhow::Result<RefPicLists<H>> {
        let ref_pic_list0 = match hdr.slice_type {
            SliceType::P | SliceType::Sp => self.modify_ref_pic_list(
                cur_pic,
                hdr,
                RefPicList::RefPicList0,
                &ref_pic_lists.ref_pic_list_p0,
            )?,
            SliceType::B => self.modify_ref_pic_list(
                cur_pic,
                hdr,
                RefPicList::RefPicList0,
                &ref_pic_lists.ref_pic_list_b0,
            )?,
            _ => Vec::new(),
        };

        let ref_pic_list1 = match hdr.slice_type {
            SliceType::B => self.modify_ref_pic_list(
                cur_pic,
                hdr,
                RefPicList::RefPicList1,
                &ref_pic_lists.ref_pic_list_b1,
            )?,
            _ => Vec::new(),
        };

        Ok(RefPicLists {
            ref_pic_list0,
            ref_pic_list1,
        })
    }

    fn handle_memory_management_ops(&mut self, pic: &mut PictureData) -> Result<(), MmcoError> {
        let markings = pic.ref_pic_marking.clone();

        for marking in &markings.inner {
            match marking.memory_management_control_operation {
                0 => break,
                1 => self.dpb.mmco_op_1(pic, marking)?,
                2 => self.dpb.mmco_op_2(pic, marking)?,
                3 => self.dpb.mmco_op_3(pic, marking)?,
                4 => self.max_long_term_frame_idx = self.dpb.mmco_op_4(marking),
                5 => self.max_long_term_frame_idx = self.dpb.mmco_op_5(pic),
                6 => self.dpb.mmco_op_6(pic, marking),
                other => return Err(MmcoError::UnknownMmco(other)),
            }
        }

        Ok(())
    }

    fn reference_pic_marking(&mut self, pic: &mut PictureData, sps: &Sps) -> anyhow::Result<()> {
        /* 8.2.5.1 */
        if matches!(pic.is_idr, IsIdr::Yes { .. }) {
            self.dpb.mark_all_as_unused_for_ref();

            if pic.ref_pic_marking.long_term_reference_flag {
                pic.set_reference(Reference::LongTerm, false);
                pic.long_term_frame_idx = 0;
                self.max_long_term_frame_idx = MaxLongTermFrameIdx::Idx(0);
            } else {
                pic.set_reference(Reference::ShortTerm, false);
                self.max_long_term_frame_idx = MaxLongTermFrameIdx::NoLongTermFrameIndices;
            }

            return Ok(());
        }

        if pic.ref_pic_marking.adaptive_ref_pic_marking_mode_flag {
            self.handle_memory_management_ops(pic)?;
        } else {
            self.dpb.sliding_window_marking(pic, sps);
        }

        Ok(())
    }

    // Apply the parameters of `sps` to the decoding state.
    fn apply_sps(&mut self, sps: &Sps) {
        self.negotiation_info = NegotiationInfo::from(sps);

        let max_dpb_frames = sps.max_dpb_frames();
        let interlaced = !sps.frame_mbs_only_flag;
        let max_num_order_frames = sps.max_num_order_frames() as usize;
        let max_num_reorder_frames = if max_num_order_frames > max_dpb_frames {
            0
        } else {
            max_num_order_frames
        };

        self.dpb.set_limits(max_dpb_frames, max_num_reorder_frames);
        self.dpb.set_interlaced(interlaced);
    }
}

impl<B> StatelessDecoder<H264, B>
where
    B: StatelessH264DecoderBackend + TryFormat<H264>,
    B::Handle: Clone,
{
    fn negotiation_possible(sps: &Sps, old_negotiation_info: &NegotiationInfo) -> bool {
        let negotiation_info = NegotiationInfo::from(sps);
        *old_negotiation_info != negotiation_info
    }

    fn renegotiate_if_needed(&mut self, sps: &Rc<Sps>) -> anyhow::Result<()> {
        if Self::negotiation_possible(sps, &self.codec.negotiation_info) {
            // Make sure all the frames we decoded so far are in the ready queue.
            self.drain()?;
            self.backend.new_sequence(sps)?;
            self.await_format_change(sps.clone());
        }

        Ok(())
    }

    // Apply the parameters of `sps` to the decoder.
    fn apply_sps(&mut self, sps: &Sps) {
        self.codec.apply_sps(sps);

        self.coded_resolution = Resolution::from((sps.width(), sps.height()));
    }

    fn drain(&mut self) -> anyhow::Result<()> {
        // Finish the current picture if there is one pending.
        if let Some(cur_pic) = self.codec.current_pic.take() {
            self.finish_picture(cur_pic)?;
        }

        self.ready_queue.extend(self.codec.drain());

        Ok(())
    }

    /// Adds picture to the ready queue if it could not be added to the DPB.
    fn add_to_ready_queue(&mut self, pic: PictureData, handle: B::Handle) {
        if matches!(pic.field, Field::Frame) {
            self.ready_queue.push(handle);
        } else if let FieldRank::Second(..) = pic.field_rank() {
            self.ready_queue.push(handle)
        }
    }

    fn finish_picture(&mut self, pic: CurrentPicState<B::Picture>) -> anyhow::Result<()> {
        debug!("Finishing picture POC {:?}", pic.pic.pic_order_cnt);

        // Submit the picture to the backend.
        let handle = self.submit_picture(pic.backend_pic)?;
        let pps = pic.pps;
        let mut pic = pic.pic;

        if matches!(pic.reference(), Reference::ShortTerm | Reference::LongTerm) {
            self.codec.reference_pic_marking(&mut pic, &pps.sps)?;
            self.codec.prev_ref_pic_info.fill(&pic);
        }

        self.codec.prev_pic_info.fill(&pic);

        if pic.has_mmco_5 {
            // C.4.5.3 "Bumping process"
            // The bumping process is invoked in the following cases:
            // Clause 3:
            // The current picture has memory_management_control_operation equal
            // to 5, as specified in clause C.4.4.
            self.drain()?;
        }

        // Bump the DPB as per C.4.5.3 to cover clauses 1, 4, 5 and 6.
        self.ready_queue.extend(self.codec.bump_as_needed(&pic));

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
                // Split the Frame into two complementary fields so reference
                // marking is easier. This is inspired by the GStreamer implementation.
                let (first_field, second_field) = PictureData::split_frame(pic);

                self.codec
                    .dpb
                    .store_picture(first_field, Some(handle.clone()))?;
                self.codec.dpb.store_picture(second_field, Some(handle))?;
            } else {
                self.codec.dpb.store_picture(pic.into_rc(), Some(handle))?;
            }
        } else {
            self.add_to_ready_queue(pic, handle);
        }

        Ok(())
    }

    fn handle_frame_num_gap(
        &mut self,
        sps: &Sps,
        frame_num: u32,
        timestamp: u64,
    ) -> anyhow::Result<()> {
        if self.codec.dpb.is_empty() {
            return Ok(());
        }

        debug!("frame_num gap detected.");

        if !sps.gaps_in_frame_num_value_allowed_flag {
            return Err(anyhow!(
                "Invalid frame_num: {}. Assuming unintentional loss of pictures",
                frame_num
            ));
        }

        let mut unused_short_term_frame_num =
            (self.codec.prev_ref_pic_info.frame_num + 1) % sps.max_frame_num();
        while unused_short_term_frame_num != frame_num {
            let max_frame_num = sps.max_frame_num();

            let mut pic = PictureData::new_non_existing(unused_short_term_frame_num, timestamp);
            self.codec.compute_pic_order_count(&mut pic, sps)?;

            self.codec
                .dpb
                .update_pic_nums(unused_short_term_frame_num, max_frame_num, &pic);

            self.codec.dpb.sliding_window_marking(&mut pic, sps);

            self.ready_queue.extend(self.codec.bump_as_needed(&pic));

            if self.codec.dpb.interlaced() {
                let (first_field, second_field) = PictureData::split_frame(pic);

                self.codec.dpb.store_picture(first_field, None)?;
                self.codec.dpb.store_picture(second_field, None)?;
            } else {
                self.codec.dpb.store_picture(pic.into_rc(), None)?;
            }

            unused_short_term_frame_num += 1;
            unused_short_term_frame_num %= max_frame_num;
        }

        Ok(())
    }

    /// Init the current picture being decoded.
    fn init_current_pic(
        &mut self,
        slice: &Slice,
        sps: &Sps,
        first_field: Option<&RcPictureData>,
        timestamp: u64,
    ) -> anyhow::Result<PictureData> {
        let mut pic = PictureData::new_from_slice(slice, sps, timestamp, first_field);
        self.codec.compute_pic_order_count(&mut pic, sps)?;

        if matches!(pic.is_idr, IsIdr::Yes { .. }) {
            // C.4.5.3 "Bumping process"
            // The bumping process is invoked in the following cases:
            // Clause 2:
            // The current picture is an IDR picture and
            // no_output_of_prior_pics_flag is not equal to 1 and is not
            // inferred to be equal to 1, as specified in clause C.4.4.
            if !pic.ref_pic_marking.no_output_of_prior_pics_flag {
                self.drain()?;
            } else {
                // C.4.4 When no_output_of_prior_pics_flag is equal to 1 or is
                // inferred to be equal to 1, all frame buffers in the DPB are
                // emptied without output of the pictures they contain, and DPB
                // fullness is set to 0.
                self.codec.dpb.clear();
            }
        }

        self.codec.dpb.update_pic_nums(
            u32::from(slice.header.frame_num),
            sps.max_frame_num(),
            &pic,
        );

        Ok(pic)
    }

    /// Called once per picture to start it.
    fn begin_picture(
        &mut self,
        timestamp: u64,
        slice: &Slice,
    ) -> Result<CurrentPicState<B::Picture>, DecodeError> {
        let hdr = &slice.header;
        let pps = Rc::clone(
            self.codec
                .parser
                .get_pps(hdr.pic_parameter_set_id)
                .context("Invalid PPS in handle_picture")?,
        );

        // A picture's SPS may require negociation.
        self.renegotiate_if_needed(&pps.sps)?;
        if let DecodingState::AwaitingFormat(_) = &self.decoding_state {
            return Err(DecodeError::CheckEvents);
        }

        // Start by securing the backend picture before modifying our state.
        let first_field = self
            .codec
            .find_first_field(&slice.header)
            .context("while looking for first field")?;
        let mut backend_pic = if let Some(first_field) = &first_field {
            self.backend.new_field_picture(timestamp, &first_field.1)
        } else {
            self.backend.new_picture(timestamp)
        }?;

        let nalu_hdr = &slice.nalu.header;

        if nalu_hdr.idr_pic_flag {
            self.codec.prev_ref_pic_info.frame_num = 0;
        }

        let frame_num = u32::from(hdr.frame_num);

        let current_macroblock = match pps.sps.separate_colour_plane_flag {
            true => CurrentMacroblockTracking::SeparateColorPlane(Default::default()),
            false => CurrentMacroblockTracking::NonSeparateColorPlane(0),
        };

        if frame_num != self.codec.prev_ref_pic_info.frame_num
            && frame_num != (self.codec.prev_ref_pic_info.frame_num + 1) % pps.sps.max_frame_num()
        {
            self.handle_frame_num_gap(&pps.sps, frame_num, timestamp)?;
        }

        let pic = self.init_current_pic(
            slice,
            &pps.sps,
            first_field.as_ref().map(|f| &f.0),
            timestamp,
        )?;
        let ref_pic_lists = self.codec.dpb.build_ref_pic_lists(&pic);

        debug!("Decode picture POC {:?}", pic.pic_order_cnt);

        self.backend.start_picture(
            &mut backend_pic,
            &pic,
            pps.sps.as_ref(),
            pps.as_ref(),
            &self.codec.dpb,
            &slice.header,
        )?;

        Ok(CurrentPicState {
            pic,
            pps,
            backend_pic,
            ref_pic_lists,
            current_macroblock,
        })
    }

    // Check whether first_mb_in_slice increases monotonically for the current
    // picture as required by the specification.
    fn check_first_mb_in_slice(
        &mut self,
        current_macroblock: &mut CurrentMacroblockTracking,
        slice: &Slice,
    ) {
        match current_macroblock {
            CurrentMacroblockTracking::SeparateColorPlane(current_macroblock) => {
                match current_macroblock.entry(slice.header.colour_plane_id) {
                    Entry::Vacant(current_macroblock) => {
                        current_macroblock.insert(slice.header.first_mb_in_slice);
                    }
                    Entry::Occupied(mut current_macroblock) => {
                        let current_macroblock = current_macroblock.get_mut();
                        if slice.header.first_mb_in_slice >= *current_macroblock {
                            log::trace!("first_mb_in_slice does not increase monotically, expect corrupted output");
                        }
                        *current_macroblock = slice.header.first_mb_in_slice;
                    }
                }
            }
            CurrentMacroblockTracking::NonSeparateColorPlane(current_macroblock) => {
                if slice.header.first_mb_in_slice >= *current_macroblock {
                    log::trace!(
                        "first_mb_in_slice does not increase monotically, expect corrupted output"
                    );
                }
                *current_macroblock = slice.header.first_mb_in_slice;
            }
        }
    }

    /// Handle a slice. Called once per slice NALU.
    fn handle_slice(
        &mut self,
        cur_pic: &mut CurrentPicState<B::Picture>,
        slice: &Slice,
    ) -> anyhow::Result<()> {
        self.check_first_mb_in_slice(&mut cur_pic.current_macroblock, slice);

        // A slice can technically refer to another PPS.
        let pps = self
            .codec
            .parser
            .get_pps(slice.header.pic_parameter_set_id)
            .context("Invalid PPS")?;
        cur_pic.pps = Rc::clone(pps);

        // Make sure that no negotiation is possible mid-picture. How could it?
        // We'd lose the context with the previous slices on it.
        if Self::negotiation_possible(&cur_pic.pps.sps, &self.codec.negotiation_info) {
            anyhow::bail!("invalid stream: inter-frame renegotiation requested");
        }

        let RefPicLists {
            ref_pic_list0,
            ref_pic_list1,
        } = self
            .codec
            .create_ref_pic_lists(&cur_pic.pic, &slice.header, &cur_pic.ref_pic_lists)?;

        self.backend.decode_slice(
            &mut cur_pic.backend_pic,
            slice,
            cur_pic.pps.sps.as_ref(),
            cur_pic.pps.as_ref(),
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

    fn process_nalu(&mut self, timestamp: u64, nalu: Nalu) -> Result<(), DecodeError> {
        match nalu.header.type_ {
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
                let mut cur_pic = match self.codec.current_pic.take() {
                    // No current picture, start a new one.
                    None => self.begin_picture(timestamp, &slice)?,
                    // We have a current picture but are starting a new field, or first_mb_in_slice
                    // indicates that a new picture is starting: finish the current picture and
                    // start a new one.
                    Some(cur_pic)
                        if (self.codec.dpb.interlaced()
                            && matches!(cur_pic.pic.field, Field::Frame)
                            && !cur_pic.pic.is_second_field()
                            && cur_pic.pic.field != slice.header.field())
                            || (slice.header.first_mb_in_slice == 0) =>
                    {
                        self.finish_picture(cur_pic)?;
                        self.begin_picture(timestamp, &slice)?
                    }
                    // This slice is part of the current picture.
                    Some(cur_pic) => cur_pic,
                };

                self.handle_slice(&mut cur_pic, &slice)?;
                self.codec.current_pic = Some(cur_pic);
            }
            other => {
                debug!("Unsupported NAL unit type {:?}", other,);
            }
        }

        Ok(())
    }
}

impl<B> StatelessVideoDecoder for StatelessDecoder<H264, B>
where
    B: StatelessH264DecoderBackend + TryFormat<H264>,
    B::Handle: Clone + 'static,
{
    type Handle = B::Handle;
    type FramePool = B::FramePool;

    fn decode(&mut self, timestamp: u64, bitstream: &[u8]) -> Result<usize, DecodeError> {
        let mut cursor = Cursor::new(bitstream);
        let nalu = Nalu::next(&mut cursor)?;

        if nalu.header.type_ == NaluType::Sps {
            let sps = self.codec.parser.parse_sps(&nalu)?.clone();
            if matches!(self.decoding_state, DecodingState::AwaitingStreamInfo) {
                // If more SPS come along we will renegotiate in begin_picture().
                self.renegotiate_if_needed(&sps)?;
            } else if matches!(self.decoding_state, DecodingState::Reset) {
                // We can resume decoding since the decoding parameters have not changed.
                self.decoding_state = DecodingState::Decoding;
            }
        } else if matches!(self.decoding_state, DecodingState::Reset) {
            let mut cursor = Cursor::new(bitstream);

            while let Ok(nalu) = Nalu::next(&mut cursor) {
                // In the Reset state we can resume decoding from any key frame.
                if matches!(nalu.header.type_, NaluType::SliceIdr) {
                    self.decoding_state = DecodingState::Decoding;
                    break;
                }
            }
        }

        let nalu_len = nalu.offset + nalu.size;

        match &mut self.decoding_state {
            // Process parameter sets, but skip input until we get information
            // from the stream.
            DecodingState::AwaitingStreamInfo | DecodingState::Reset => {
                if matches!(nalu.header.type_, NaluType::Pps) {
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

    fn next_event(&mut self) -> Option<DecoderEvent<B::Handle>> {
        self.query_next_event(|decoder, sps| {
            // Apply the SPS settings to the decoder so we don't enter the AwaitingFormat state
            // on the next decode() call.
            decoder.apply_sps(sps);
        })
    }

    fn frame_pool(&mut self, layer: PoolLayer) -> Vec<&mut B::FramePool> {
        self.backend.frame_pool(layer)
    }

    fn stream_info(&self) -> Option<&StreamInfo> {
        self.backend.stream_info()
    }

    fn poll_fd(&self) -> BorrowedFd {
        self.epoll_fd.0.as_fd()
    }
}

#[cfg(test)]
pub mod tests {
    use crate::codec::h264::parser::Nalu;
    use crate::decoder::stateless::h264::H264;
    use crate::decoder::stateless::tests::test_decode_stream;
    use crate::decoder::stateless::tests::TestStream;
    use crate::decoder::stateless::StatelessDecoder;
    use crate::decoder::BlockingMode;
    use crate::utils::simple_playback_loop;
    use crate::utils::simple_playback_loop_owned_frames;
    use crate::utils::NalIterator;
    use crate::DecodedFormat;

    /// Run `test` using the dummy decoder, in both blocking and non-blocking modes.
    fn test_decoder_dummy(test: &TestStream, blocking_mode: BlockingMode) {
        let decoder = StatelessDecoder::<H264, _>::new_dummy(blocking_mode).unwrap();

        test_decode_stream(
            |d, s, f| {
                simple_playback_loop(
                    d,
                    NalIterator::<Nalu>::new(s),
                    f,
                    &mut simple_playback_loop_owned_frames,
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
