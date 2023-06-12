// Copyright 2022 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use crate::decoder::stateless::h264::dpb::Dpb;
use crate::decoder::stateless::h264::dpb::DpbEntry;
use crate::decoder::stateless::h264::parser::Pps;
use crate::decoder::stateless::h264::parser::Slice;
use crate::decoder::stateless::h264::parser::Sps;
use crate::decoder::stateless::h264::picture::PictureData;
use crate::decoder::stateless::StatelessDecoderBackend;

pub type Result<T> = crate::decoder::stateless::StatelessBackendResult<T>;

#[cfg(test)]
pub mod dummy;
#[cfg(feature = "vaapi")]
pub mod vaapi;

/// Stateless backend methods specific to H.264.
pub(crate) trait StatelessH264DecoderBackend: StatelessDecoderBackend<Sps> {
    /// Type used by the backend to represent a picture in the process of being decoded.
    type Picture;

    /// Called when a new SPS is parsed.
    fn new_sequence(&mut self, sps: &Sps) -> Result<()>;

    /// Called when the decoder determines that a frame or field was found.
    fn new_picture(&mut self, picture: &PictureData, timestamp: u64) -> Result<Self::Picture>;

    /// Called when the decoder determines that a second field was found.
    /// Indicates that the underlying BackendHandle is to be shared between the
    /// two pictures. This is so both fields decode to the same underlying
    /// resource and can thus be presented together as a single frame.
    fn new_field_picture(
        &mut self,
        picture: &PictureData,
        timestamp: u64,
        first_field: &Self::Handle,
    ) -> Result<Self::Picture>;

    /// Called by the decoder when starting a new frame or field.
    fn start_picture(
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
        ref_pic_list0: &[DpbEntry<Self::Handle>],
        ref_pic_list1: &[DpbEntry<Self::Handle>],
    ) -> Result<()>;

    /// Called when the decoder wants the backend to finish the decoding
    /// operations for `picture`. At this point, `decode_slice` has been called
    /// for all slices.
    ///
    /// This call will assign the ownership of the BackendHandle to the Picture
    /// and then assign the ownership of the Picture to the Handle.
    fn submit_picture(&mut self, picture: Self::Picture) -> Result<Self::Handle>;
}
