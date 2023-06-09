// Copyright 2022 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use crate::decoder::stateless::vp8::parser::Header;
use crate::decoder::stateless::vp8::parser::MbLfAdjustments;
use crate::decoder::stateless::vp8::parser::Segmentation;
use crate::decoder::stateless::VideoDecoderBackend;

#[cfg(test)]
pub mod dummy;
#[cfg(feature = "vaapi")]
pub mod vaapi;

pub type Result<T> = crate::decoder::stateless::StatelessBackendResult<T>;

/// Trait for stateless decoder backends. The decoder will call into the backend
/// to request decode operations. The backend can operate in blocking mode,
/// where it will wait until the current decode finishes, or in non-blocking
/// mode, where it should return immediately with any previously decoded frames
/// that happen to be ready.
pub(crate) trait StatelessDecoderBackend: VideoDecoderBackend<Header> {
    /// Called when new stream parameters are found.
    fn new_sequence(&mut self, header: &Header) -> Result<()>;

    /// Called when the decoder wants the backend to finish the decoding
    /// operations for `picture`.
    ///
    /// This call will assign the ownership of the BackendHandle to the Picture
    /// and then assign the ownership of the Picture to the Handle.
    #[allow(clippy::too_many_arguments)]
    fn submit_picture(
        &mut self,
        picture: &Header,
        last_ref: Option<&Self::Handle>,
        golden_ref: Option<&Self::Handle>,
        alt_ref: Option<&Self::Handle>,
        bitstream: &[u8],
        segmentation: &Segmentation,
        mb_lf_adjust: &MbLfAdjustments,
        timestamp: u64,
    ) -> Result<Self::Handle>;
}
