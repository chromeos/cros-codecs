// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

pub mod stateless;

use crate::FrameLayout;
use crate::Resolution;

/// Specifies the encoder operation
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RateControl {
    /// The encoder shall maintain the constant bitrate
    ConstantBitrate(u64),

    /// The encoder shall maintain codec specific quality parameter constant (eg. QP for H.264)
    /// disregarding bitrate.
    ConstantQuality(u32),
}

impl RateControl {
    pub(crate) fn is_same_variant(left: &Self, right: &Self) -> bool {
        std::mem::discriminant(left) == std::mem::discriminant(right)
    }

    pub(crate) fn bitrate_target(&self) -> Option<u64> {
        match self {
            RateControl::ConstantBitrate(target) => Some(*target),
            RateControl::ConstantQuality(_) => None,
        }
    }
}

/// Dynamic parameters of the encoded stream that client may choose to change during the encoding
/// session without recreating the entire encoder instance.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Tunings {
    /// The stream's [`RateControl`]
    pub rate_control: RateControl,
    /// Stream framerate in frames per second
    pub framerate: u32,
    /// Minimum value of codec specific quality parameter constant (eg. QP for H.264)
    pub min_quality: u32,
    /// Maximum value of codec specific quality parameter constant (eg. QP for H.264)
    pub max_quality: u32,
}

impl Default for Tunings {
    fn default() -> Self {
        Self {
            rate_control: RateControl::ConstantBitrate(200_000),
            framerate: 30,
            min_quality: 0,
            max_quality: u32::MAX,
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
