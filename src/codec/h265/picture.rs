// Copyright 2023 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use crate::codec::h265::parser::NaluType;
use crate::codec::h265::parser::Pps;
use crate::codec::h265::parser::Slice;

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
pub enum Reference {
    #[default]
    None,
    ShortTerm,
    LongTerm,
}

/// Data associated with an h.265 picture. Most fields are extracted from the
/// slice header and kept for future processing.
#[derive(Debug, Default, Clone)]
pub struct PictureData {
    // Fields extracted from the slice header. These are the CamelCase
    // variables, unless noted otherwise.
    pub nal_unit_type: NaluType,
    pub no_rasl_output_flag: bool,
    pub pic_output_flag: bool,
    pub valid_for_prev_tid0_pic: bool,
    pub slice_pic_order_cnt_lsb: i32,
    pub pic_order_cnt_msb: i32,
    pub pic_order_cnt_val: i32,
    pub no_output_of_prior_pics_flag: bool,

    // Internal state.
    pub is_irap: bool,
    pub is_first_picture: bool,
    pub reference: Reference,
    pub pic_latency_cnt: i32,
    pub needed_for_output: bool,
}

impl PictureData {
    /// Instantiates a new `PictureData` from a slice.
    ///
    /// See 8.1.3 Decoding process for a coded picture with nuh_layer_id equal
    /// to 0.
    ///
    /// This will also call the picture order count process (clause 8.3.1) to
    /// correctly initialize the POC values.
    pub fn new_from_slice(
        slice: &Slice<&[u8]>,
        pps: &Pps,
        is_first_picture_in_au: bool,
        prev_tid0_pic: Option<&PictureData>,
        max_pic_order_cnt_lsb: i32,
        _timestamp: u64,
    ) -> Self {
        let hdr = slice.header();
        let nalu_type = slice.nalu().header().type_();
        let is_irap = nalu_type.is_irap();

        // We assume HandleCraAsBlafFLag == 0, as it is only set through
        // external means, which we do not provide.

        let mut pic_order_cnt_msb = 0;
        let slice_pic_order_cnt_lsb: i32 = hdr.pic_order_cnt_lsb().into();

        // Compute the output flags:
        // The value of NoRaslOutputFlag is equal to 1 for each IDR access
        // unit, each BLA access unit, and each CRA access unit that is the
        // first access unit in the bitstream in decoding order, is the first
        // access unit that follows an end of sequence NAL unit in decoding
        // order, or has HandleCraAsBlaFlag equal to 1.
        let no_rasl_output_flag = nalu_type.is_idr()
            || nalu_type.is_bla()
            || nalu_type.is_cra()
            || is_first_picture_in_au;

        let pic_output_flag = if slice.nalu().header().type_().is_rasl() && no_rasl_output_flag {
            false
        } else {
            hdr.pic_output_flag()
        };

        // Compute the Picture Order Count. See 8.3.1 Decoding Process for
        // Picture Order Count
        if !(is_irap && no_rasl_output_flag) {
            if let Some(prev_tid0_pic) = prev_tid0_pic {
                // Equation (8-1)
                let prev_pic_order_cnt_lsb = prev_tid0_pic.slice_pic_order_cnt_lsb;
                let prev_pic_order_cnt_msb = prev_tid0_pic.pic_order_cnt_msb;
                if (slice_pic_order_cnt_lsb < prev_pic_order_cnt_lsb)
                    && (prev_pic_order_cnt_lsb - slice_pic_order_cnt_lsb)
                        >= (max_pic_order_cnt_lsb / 2)
                {
                    pic_order_cnt_msb = prev_pic_order_cnt_msb + max_pic_order_cnt_lsb;
                } else if (slice_pic_order_cnt_lsb > prev_pic_order_cnt_lsb)
                    && (slice_pic_order_cnt_lsb - prev_pic_order_cnt_lsb)
                        > (max_pic_order_cnt_lsb / 2)
                {
                    pic_order_cnt_msb = prev_pic_order_cnt_msb - max_pic_order_cnt_lsb;
                } else {
                    pic_order_cnt_msb = prev_pic_order_cnt_msb;
                }
            }
        }

        // Compute whether this picture will be a valid prevTid0Pic, i.e.:
        //
        // Let prevTid0Pic be the previous picture in decoding order that has
        // TemporalId equal to 0 and that is not a RASL, RADL or SLNR picture.
        //
        // Use this flag to correctly set up the field in the decoder during
        // `finish_picture`.
        let valid_for_prev_tid0_pic = pps.temporal_id() == 0
            && !nalu_type.is_radl()
            && !nalu_type.is_rasl()
            && !nalu_type.is_slnr();

        let no_output_of_prior_pics_flag =
            if nalu_type.is_irap() && no_rasl_output_flag && !is_first_picture_in_au {
                nalu_type.is_cra() || hdr.no_output_of_prior_pics_flag()
            } else {
                false
            };

        Self {
            nal_unit_type: nalu_type,
            no_rasl_output_flag,
            no_output_of_prior_pics_flag,
            pic_output_flag,
            valid_for_prev_tid0_pic,
            slice_pic_order_cnt_lsb,
            pic_order_cnt_msb,
            // Equation (8-2)
            pic_order_cnt_val: pic_order_cnt_msb + slice_pic_order_cnt_lsb,
            is_irap,
            is_first_picture: is_first_picture_in_au,
            reference: Default::default(),
            pic_latency_cnt: 0,
            needed_for_output: false,
        }
    }

    /// Whether the current picture is a reference, either ShortTerm or LongTerm.
    pub fn is_ref(&self) -> bool {
        !matches!(self.reference, Reference::None)
    }
}
