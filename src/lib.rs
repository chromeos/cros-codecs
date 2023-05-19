// Copyright 2022 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

pub mod decoders;
pub mod utils;

use std::str::FromStr;

#[cfg(feature = "vaapi")]
pub use libva;

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
pub struct Resolution {
    pub width: u32,
    pub height: u32,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum DecodedFormat {
    NV12,
    I420,
}

impl FromStr for DecodedFormat {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "nv12" | "NV12" => Ok(DecodedFormat::NV12),
            "i420" | "I420" => Ok(DecodedFormat::I420),
            _ => Err("unrecognized output format. Valid values: nv12, i420"),
        }
    }
}

/// Copies `src` into `dst` as NV12, removing any extra padding.
pub fn nv12_copy(
    src: &[u8],
    dst: &mut [u8],
    width: usize,
    height: usize,
    strides: [usize; 3],
    offsets: [usize; 3],
) {
    // Copy Y.
    let src_y_lines = src[offsets[0]..]
        .chunks(strides[0])
        .map(|line| &line[..width]);
    let dst_y_lines = dst.chunks_mut(width);

    for (src_line, dst_line) in src_y_lines.zip(dst_y_lines).take(height) {
        dst_line.copy_from_slice(src_line);
    }

    let dst_u_offset = width * height;

    // Align width and height to 2 for UV plane.
    let width = if width % 2 == 1 { width + 1 } else { width };
    let height = if height % 2 == 1 { height + 1 } else { height };
    // 1 sample per 4 pixels, but we have two components per line so width can remain as-is.
    let height = height / 2;

    // Copy UV.
    let src_uv_lines = src[offsets[1]..]
        .chunks(strides[1])
        .map(|line| &line[..width]);
    let dst_uv_lines = dst[dst_u_offset..].chunks_mut(width);
    for (src_line, dst_line) in src_uv_lines.zip(dst_uv_lines).take(height) {
        dst_line.copy_from_slice(src_line);
    }
}

/// Copies `src` into `dst` as I420, removing any extra padding.
pub fn i420_copy(
    src: &[u8],
    dst: &mut [u8],
    width: usize,
    height: usize,
    strides: [usize; 3],
    offsets: [usize; 3],
) {
    // Copy Y.
    let src_y_lines = src[offsets[0]..]
        .chunks(strides[0])
        .map(|line| &line[..width]);
    let dst_y_lines = dst.chunks_mut(width);

    for (src_line, dst_line) in src_y_lines.zip(dst_y_lines).take(height) {
        dst_line.copy_from_slice(src_line);
    }

    let dst_u_offset = width * height;

    // Align width and height to 2 for U and V planes.
    let width = if width % 2 == 1 { width + 1 } else { width };
    let height = if height % 2 == 1 { height + 1 } else { height };
    // 1 sample per 4 pixels.
    let width = width / 2;
    let height = height / 2;

    // Copy U.
    let src_u_lines = src[offsets[1]..]
        .chunks(strides[1])
        .map(|line| &line[..width]);
    let dst_u_lines = dst[dst_u_offset..].chunks_mut(width);
    for (src_line, dst_line) in src_u_lines.zip(dst_u_lines).take(height) {
        dst_line.copy_from_slice(src_line);
    }

    let dst_v_offset = dst_u_offset + width * height;

    // Copy V.
    let src_v_lines = src[offsets[2]..]
        .chunks(strides[2])
        .map(|line| &line[..width]);
    let dst_v_lines = dst[dst_v_offset..].chunks_mut(width);
    for (src_line, dst_line) in src_v_lines.zip(dst_v_lines).take(height) {
        dst_line.copy_from_slice(src_line);
    }
}

/// Returns the size required to store a frame of `format` with size `width`x`height`, without any
/// padding. This is the minimum size of the destination buffer passed to `nv12_copy` or
/// `i420_copy`.
pub fn decoded_frame_size(format: DecodedFormat, width: usize, height: usize) -> usize {
    match format {
        DecodedFormat::I420 | DecodedFormat::NV12 => {
            let u_size = width * height;
            // U and V planes need to be aligned to 2.
            let uv_size = ((width + 1) / 2) * ((height + 1) / 2) * 2;

            u_size + uv_size
        }
    }
}
