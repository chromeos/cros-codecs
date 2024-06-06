// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

//! V4L2 backend

pub mod decoder;
pub mod encoder;

impl From<v4l2r::PixelFormat> for crate::Fourcc {
    fn from(value: v4l2r::PixelFormat) -> Self {
        crate::Fourcc(value.to_u32())
    }
}

impl From<crate::Fourcc> for v4l2r::PixelFormat {
    fn from(value: crate::Fourcc) -> Self {
        v4l2r::PixelFormat::from_u32(value.0)
    }
}
