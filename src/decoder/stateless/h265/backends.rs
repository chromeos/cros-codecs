// Copyright 2023 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use crate::decoder::stateless::h265::dpb::Dpb;
use crate::decoder::stateless::h265::parser::Pps;
use crate::decoder::stateless::h265::parser::Slice;
use crate::decoder::stateless::h265::parser::Sps;
use crate::decoder::stateless::h265::picture::PictureData;
use crate::decoder::stateless::h265::RefPicListEntry;
use crate::decoder::stateless::StatelessDecoderBackend;

pub type Result<T> = crate::decoder::stateless::StatelessBackendResult<T>;

// #[cfg(test)]
// pub mod dummy;
// #[cfg(feature = "vaapi")]
// pub mod vaapi;

/// Stateless backend methods specific to H.265.
pub(crate) trait StatelessH265DecoderBackend: StatelessDecoderBackend<Sps> {
    /// Type used by the backend to represent a picture in the process of being decoded.
    type Picture;

    /// Called when a new SPS is parsed.
    fn new_sequence(&mut self, sps: &Sps) -> Result<()>;

    /// Called when the decoder determines that a frame or field was found.
    fn new_picture(&mut self, picture: &PictureData, timestamp: u64) -> Result<Self::Picture>;

    /// Called by the decoder for every frame or field found.
    fn handle_picture(
        &mut self,
        picture: &mut Self::Picture,
        picture_data: &PictureData,
        sps: &Sps,
        pps: &Pps,
        dpb: &Dpb<Self::Handle>,
        slice: &Slice<&[u8]>,
    ) -> Result<()>;

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
    ) -> Result<()>;

    /// Called when the decoder wants the backend to finish the decoding
    /// operations for `picture`. At this point, `decode_slice` has been called
    /// for all slices.
    ///
    /// This call will assign the ownership of the BackendHandle to the Picture
    /// and then assign the ownership of the Picture to the Handle.
    fn submit_picture(&mut self, picture: Self::Picture) -> Result<Self::Handle>;
}
