// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use crate::codec::av1::parser::BitDepth;
use crate::codec::av1::parser::Profile;
use crate::encoder::PredictionStructure;
use crate::encoder::Tunings;
use crate::Resolution;

pub struct AV1;

#[derive(Clone)]
pub struct EncoderConfig {
    pub profile: Profile,
    pub bit_depth: BitDepth,
    pub resolution: Resolution,
    pub pred_structure: PredictionStructure,
    /// Initial tunings values
    pub initial_tunings: Tunings,
}

impl Default for EncoderConfig {
    fn default() -> Self {
        // Artificially encoder configuration with intent to be widely supported.
        Self {
            profile: Profile::Profile0,
            bit_depth: BitDepth::Depth8,
            resolution: Resolution {
                width: 320,
                height: 240,
            },
            pred_structure: PredictionStructure::LowDelay { limit: 1024 },
            initial_tunings: Default::default(),
        }
    }
}
