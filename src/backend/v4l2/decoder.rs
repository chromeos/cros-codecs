// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#[cfg(feature = "v4l2")]
use crate::Rect;
use crate::Resolution;

pub const ADDITIONAL_REFERENCE_FRAME_BUFFER: usize = 4;

pub mod stateless;

pub trait V4l2StreamInfo {
    /// Returns the minimum number of surfaces required to decode the stream.
    // name was chosen to match vaapi
    fn min_num_frames(&self) -> usize;
    /// Returns the coded size of the surfaces required to decode the stream.
    fn coded_size(&self) -> Resolution;
    /// Returns the visible rectangle within the coded size for the stream.
    fn visible_rect(&self) -> Rect;
}
