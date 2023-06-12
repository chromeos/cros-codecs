// Copyright 2022 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use crate::decoder::stateless::vp9::parser::Header;
use crate::decoder::stateless::vp9::parser::MAX_SEGMENTS;
use crate::decoder::stateless::vp9::parser::NUM_REF_FRAMES;
use crate::decoder::stateless::vp9::Segmentation;
use crate::decoder::stateless::StatelessDecoderBackend;

#[cfg(test)]
pub mod dummy;
#[cfg(feature = "vaapi")]
pub mod vaapi;

pub type Result<T> = crate::decoder::stateless::StatelessBackendResult<T>;

/// Stateless backend methods specific to VP9.
pub(crate) trait StatelessVp9DecoderBackend: StatelessDecoderBackend<Header> {
    /// Called when new stream parameters are found.
    fn new_sequence(&mut self, header: &Header) -> Result<()>;

    /// Called when the decoder wants the backend to finish the decoding
    /// operations for `picture`.
    ///
    /// This call will assign the ownership of the BackendHandle to the Picture
    /// and then assign the ownership of the Picture to the Handle.
    fn submit_picture(
        &mut self,
        picture: &Header,
        reference_frames: &[Option<Self::Handle>; NUM_REF_FRAMES],
        bitstream: &[u8],
        timestamp: u64,
        segmentation: &[Segmentation; MAX_SEGMENTS],
    ) -> Result<Self::Handle>;
}
