// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use crate::codec::h264::parser::Level;
use crate::codec::h264::parser::Profile;
use crate::encoder::stateless::PredictionStructure;
use crate::encoder::Tunings;
use crate::Resolution;

pub struct H264;

#[derive(Clone)]
pub struct EncoderConfig {
    pub resolution: Resolution,
    pub profile: Profile,
    pub level: Level,
    pub pred_structure: PredictionStructure,
    /// Initial tunings values
    pub initial_tunings: Tunings,
}

impl Default for EncoderConfig {
    fn default() -> Self {
        // Artificially encoder configuration with intent to be widely supported.
        Self {
            resolution: Resolution {
                width: 320,
                height: 240,
            },
            profile: Profile::Baseline,
            level: Level::L4,
            pred_structure: PredictionStructure::LowDelay { limit: 2048 },
            initial_tunings: Default::default(),
        }
    }
}
