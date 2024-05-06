// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

pub mod stateless;

use crate::FrameLayout;
use crate::Resolution;

/// Specifies the encoder operation
#[derive(Clone)]
pub enum RateControl {
    /// The encoder shall maintain the constant bitrate
    ConstantBitrate(u64),
}

impl RateControl {
    pub(crate) fn bitrate_target(&self) -> Option<u64> {
        match self {
            RateControl::ConstantBitrate(target) => Some(*target),
        }
    }
}

/// Encoder's input metadata
#[derive(Clone)]
pub struct FrameMetadata {
    pub timestamp: u64,
    pub display_resolution: Resolution,
    pub layout: FrameLayout,
    pub force_keyframe: bool,
}

/// Encoder's coded output with contained frame.
pub struct CodedBitstreamBuffer {
    /// [`FrameMetadata`] of the frame that is compressed in [`Self::bitstream`]
    pub metadata: FrameMetadata,

    /// Bitstream with compressed frame together with optionally other compressed control messages
    pub bitstream: Vec<u8>,
}

impl CodedBitstreamBuffer {
    pub fn new(metadata: FrameMetadata, bitstream: Vec<u8>) -> Self {
        Self {
            metadata,
            bitstream,
        }
    }
}

impl From<CodedBitstreamBuffer> for Vec<u8> {
    fn from(value: CodedBitstreamBuffer) -> Self {
        value.bitstream
    }
}
