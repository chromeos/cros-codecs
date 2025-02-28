// Copyright 2025 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use v4l2r::bindings::v4l2_ctrl_av1_film_grain;
use v4l2r::bindings::v4l2_ctrl_av1_frame;
use v4l2r::bindings::v4l2_ctrl_av1_sequence;
use v4l2r::bindings::v4l2_ctrl_av1_tile_group_entry;

#[derive(Default)]
pub struct V4l2CtrlAv1FilmGrainParams {
    #[allow(dead_code)]
    handle: v4l2_ctrl_av1_film_grain,
}

impl V4l2CtrlAv1FilmGrainParams {
    pub fn new() -> Self {
        Default::default()
    }
}

#[derive(Default)]
pub struct V4l2CtrlAv1FrameParams {
    #[allow(dead_code)]
    handle: v4l2_ctrl_av1_frame,
}

impl V4l2CtrlAv1FrameParams {
    pub fn new() -> Self {
        Default::default()
    }
}

#[derive(Default)]
pub struct V4l2CtrlAv1SequenceParams {
    #[allow(dead_code)]
    handle: v4l2_ctrl_av1_sequence,
}

impl V4l2CtrlAv1SequenceParams {
    pub fn new() -> Self {
        Default::default()
    }
}

#[derive(Default)]
pub struct V4l2CtrlAv1TileGroupEntryParams {
    #[allow(dead_code)]
    handle: v4l2_ctrl_av1_tile_group_entry,
}

impl V4l2CtrlAv1TileGroupEntryParams {
    pub fn new() -> Self {
        Default::default()
    }
}
