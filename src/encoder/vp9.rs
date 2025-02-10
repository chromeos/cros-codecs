// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
//
use crate::codec::vp9::parser::BitDepth;
use crate::encoder::PredictionStructure;
use crate::encoder::Tunings;
use crate::Resolution;

pub struct VP9;

#[derive(Clone)]
pub struct EncoderConfig {
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
            bit_depth: BitDepth::Depth8,
            resolution: Resolution { width: 320, height: 240 },
            pred_structure: PredictionStructure::LowDelay { limit: 2048 },
            initial_tunings: Default::default(),
        }
    }
}
