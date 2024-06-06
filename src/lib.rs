// Copyright 2022 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

//! This crate provides tools to help decode and encode various video codecs, leveraging the
//! hardware acceleration available on the target.
//!
//! The [codec] module contains tools to parse encoded video streams like H.264 or VP9 and extract
//! the information useful in order to perform e.g. hardware-accelerated decoding.
//!
//! The [backend] module contains common backend code. A backend is a provider of some way to
//! decode or encode a particular codec, like VAAPI.
//!
//! The [decoder] module contains decoders that can turn an encoded video stream into a sequence of
//! decoded frames using the hardware acceleration available on the host.
//!
//! The [encoder] module contains encoder that can turn a picture sequence into a compressed
//! sequence of decodable encoded packets using the hardware acceleration available on the host.
//!
//! The [utils] module contains some useful code that is shared between different parts of this
//! crate and didn't fit any of the modules above.

pub mod backend;
pub mod codec;
pub mod decoder;
pub mod device;
pub mod encoder;
pub mod utils;

use std::str::FromStr;

use byteorder::ByteOrder;
use byteorder::LittleEndian;
#[cfg(feature = "vaapi")]
pub use libva;
#[cfg(feature = "v4l2")]
pub use v4l2r;

/// Rounding modes for `Resolution`
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ResolutionRoundMode {
    /// Rounds component-wise to the next even value.
    Even,
}

/// A frame resolution in pixels.
#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
pub struct Resolution {
    pub width: u32,
    pub height: u32,
}

impl Resolution {
    /// Whether `self` can contain `other`.
    pub fn can_contain(&self, other: Self) -> bool {
        self.width >= other.width && self.height >= other.height
    }

    /// Rounds `self` according to `rnd_mode`.
    pub fn round(mut self, rnd_mode: ResolutionRoundMode) -> Self {
        match rnd_mode {
            ResolutionRoundMode::Even => {
                if self.width % 2 != 0 {
                    self.width += 1;
                }

                if self.height % 2 != 0 {
                    self.height += 1;
                }
            }
        }

        self
    }
}

impl From<(u32, u32)> for Resolution {
    fn from(value: (u32, u32)) -> Self {
        Self {
            width: value.0,
            height: value.1,
        }
    }
}

impl From<Resolution> for (u32, u32) {
    fn from(value: Resolution) -> Self {
        (value.width, value.height)
    }
}

/// Wrapper around u32 when they are meant to be a fourcc.
///
/// Provides conversion and display/debug implementations useful when dealing with fourcc codes.
#[derive(Clone, Copy, PartialEq)]
pub struct Fourcc(u32);

impl From<u32> for Fourcc {
    fn from(fourcc: u32) -> Self {
        Self(fourcc)
    }
}

impl From<Fourcc> for u32 {
    fn from(fourcc: Fourcc) -> Self {
        fourcc.0
    }
}

impl From<&[u8; 4]> for Fourcc {
    fn from(n: &[u8; 4]) -> Self {
        Self(n[0] as u32 | (n[1] as u32) << 8 | (n[2] as u32) << 16 | (n[3] as u32) << 24)
    }
}

impl From<Fourcc> for [u8; 4] {
    fn from(n: Fourcc) -> Self {
        [
            n.0 as u8,
            (n.0 >> 8) as u8,
            (n.0 >> 16) as u8,
            (n.0 >> 24) as u8,
        ]
    }
}

impl std::fmt::Display for Fourcc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let c: [u8; 4] = (*self).into();

        f.write_fmt(format_args!(
            "{}{}{}{}",
            c[0] as char, c[1] as char, c[2] as char, c[3] as char
        ))
    }
}

impl std::fmt::Debug for Fourcc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("0x{:08x} ({})", self.0, self))
    }
}

/// Formats that buffers can be mapped into for the CPU to read.
///
/// The conventions here largely follow these of libyuv.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum DecodedFormat {
    /// Y, U and V planes, 4:2:0 sampling, 8 bits per sample.
    I420,
    /// One Y and one interleaved UV plane, 4:2:0 sampling, 8 bits per sample.
    NV12,
    /// Y, U and V planes, 4:2:2 sampling, 8 bits per sample.
    I422,
    /// Y, U and V planes, 4:4:4 sampling, 8 bits per sample.
    I444,
    /// Y, U and V planes, 4:2:0 sampling, 16 bits per sample, LE. Only the 10 LSBs are used.
    I010,
    /// Y, U and V planes, 4:2:0 sampling, 16 bits per sample, LE. Only the 12 LSBs are used.
    I012,
    /// Y, U and V planes, 4:2:2 sampling, 16 bits per sample, LE. Only the 10 LSBs are used.
    I210,
    /// Y, U and V planes, 4:2:2 sampling, 16 bits per sample, LE. Only the 12 LSBs are used.
    I212,
    /// Y, U and V planes, 4:4:4 sampling, 16 bits per sample, LE. Only the 10 LSBs are used.
    I410,
    /// Y, U and V planes, 4:4:4 sampling, 16 bits per sample, LE. Only the 12 LSBs are used.
    I412,
}

impl FromStr for DecodedFormat {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "i420" | "I420" => Ok(DecodedFormat::I420),
            "i422" | "I422" => Ok(DecodedFormat::I422),
            "i444" | "I444" => Ok(DecodedFormat::I444),
            "nv12" | "NV12" => Ok(DecodedFormat::NV12),
            "i010" | "I010" => Ok(DecodedFormat::I010),
            "i012" | "I012" => Ok(DecodedFormat::I012),
            "i210" | "I210" => Ok(DecodedFormat::I210),
            "i212" | "I212" => Ok(DecodedFormat::I212),
            "i410" | "I410" => Ok(DecodedFormat::I410),
            "i412" | "I412" => Ok(DecodedFormat::I412),
            _ => {
                Err("unrecognized output format. Valid values: i420, nv12, i422, i444, i010, i012, i210, i212, i410, i412")
            }
        }
    }
}

/// Describes the layout of a plane within a frame.
#[derive(Debug, Clone, PartialEq)]
pub struct PlaneLayout {
    /// Index of the memory buffer the plane belongs to.
    pub buffer_index: usize,
    /// Start offset of the plane within its buffer.
    pub offset: usize,
    /// Distance in bytes between two lines of data in this plane.
    pub stride: usize,
}

/// Unambiguously describes the layout of a frame.
///
/// A frame can be made of one or several memory buffers, each containing one or several planes.
/// For a given frame, this structure defines where each plane can be found.
#[derive(Debug, Clone, PartialEq)]
pub struct FrameLayout {
    /// `(Fourcc, modifier)` tuple describing the arrangement of the planes.
    ///
    /// This member is enough to infer how many planes and buffers the frame has, and which
    /// buffer each plane belongs into.
    pub format: (Fourcc, u64),
    /// Size in pixels of the frame.
    pub size: Resolution,
    /// Layout of each individual plane.
    pub planes: Vec<PlaneLayout>,
}

/// Build a frame memory descriptor enum that supports multiple descriptor types.
///
/// This is useful for the case where the frames' memory backing is not decided at compile-time.
/// In this case, this macro can be used to list all the potential types supported at run-time, and
/// the selected one can be built as the program is run.
///
/// # Example
///
/// ```
/// use cros_codecs::multiple_desc_type;
/// use cros_codecs::utils::DmabufFrame;
///
/// /// Frames' memory can be provided either by the backend, or via PRIME DMABUF handles.
/// multiple_desc_type! {
///     enum OwnedOrDmaDescriptor {
///         Owned(()),
///         Dmabuf(DmabufFrame),
///     }
/// }
/// ```
#[macro_export]
macro_rules! multiple_desc_type {
    (enum $s:ident { $($v:ident($t:ty),)* } ) => {
        enum $s {
            $($v($t),)*
        }

        #[cfg(feature = "vaapi")]
        impl libva::SurfaceMemoryDescriptor for $s {
            fn add_attrs(&mut self, attrs: &mut Vec<libva::VASurfaceAttrib>) -> Option<Box<dyn std::any::Any>> {
                match self {
                    $($s::$v(desc) => desc.add_attrs(attrs),)*
                }
            }
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
    // 1 sample per 4 pixels, but we have two components per line so width can remain as-is.
    let uv_width = if width % 2 == 1 { width + 1 } else { width };
    let uv_height = if height % 2 == 1 { height + 1 } else { height } / 2;

    // Copy UV.
    let src_uv_lines = src[offsets[1]..]
        .chunks(strides[1])
        .map(|line| &line[..uv_width]);
    let dst_uv_lines = dst[dst_u_offset..].chunks_mut(uv_width);
    for (src_line, dst_line) in src_uv_lines.zip(dst_uv_lines).take(uv_height) {
        dst_line.copy_from_slice(src_line);
    }
}

/// Copies `src` into `dst` as I4xx (YUV tri-planar).
///
/// This function does not change the data layout beyond removing any padding in the source, i.e.
/// both `src` and `dst` are 3-planar YUV buffers.
///
/// `strides` and `offsets` give the stride and starting position of each plane in `src`. In `dst`
/// each plane will be put sequentially one after the other.
///
/// `sub_h` and `sub_v` enable horizontal and vertical sub-sampling, respectively. E.g, if both
/// `sub_h` and `sub_v` are `true` the data will be `4:2:0`, if only `sub_v` is `true` then it will be
/// `4:2:2`, and if both are `false` then we have `4:4:4`.
pub fn i4xx_copy(
    src: &[u8],
    dst: &mut [u8],
    width: usize,
    height: usize,
    strides: [usize; 3],
    offsets: [usize; 3],
    (sub_h, sub_v): (bool, bool),
) {
    // Align width and height of UV planes to 2 if sub-sampling is used.
    let uv_width = if sub_h { (width + 1) / 2 } else { width };
    let uv_height = if sub_v { (height + 1) / 2 } else { height };

    let dst_y_size = width * height;
    let dst_u_size = uv_width * uv_height;
    let (dst_y_plane, dst_uv_planes) = dst.split_at_mut(dst_y_size);
    let (dst_u_plane, dst_v_plane) = dst_uv_planes.split_at_mut(dst_u_size);

    // Copy Y.
    let src_y_lines = src[offsets[0]..]
        .chunks(strides[0])
        .map(|line| &line[..width]);
    let dst_y_lines = dst_y_plane.chunks_mut(width);
    for (src_line, dst_line) in src_y_lines.zip(dst_y_lines).take(height) {
        dst_line.copy_from_slice(src_line);
    }

    // Copy U.
    let src_u_lines = src[offsets[1]..]
        .chunks(strides[1])
        .map(|line| &line[..uv_width]);
    let dst_u_lines = dst_u_plane.chunks_mut(uv_width);
    for (src_line, dst_line) in src_u_lines.zip(dst_u_lines).take(uv_height) {
        dst_line.copy_from_slice(src_line);
    }

    // Copy V.
    let src_v_lines = src[offsets[2]..]
        .chunks(strides[2])
        .map(|line| &line[..uv_width]);
    let dst_v_lines = dst_v_plane.chunks_mut(uv_width);
    for (src_line, dst_line) in src_v_lines.zip(dst_v_lines).take(uv_height) {
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
        DecodedFormat::I422 => {
            let u_size = width * height;
            // U and V planes need to be aligned to 2.
            let uv_size = ((width + 1) / 2) * ((height + 1) / 2) * 2 * 2;

            u_size + uv_size
        }
        DecodedFormat::I444 => (width * height) * 3,
        DecodedFormat::I010 | DecodedFormat::I012 => {
            decoded_frame_size(DecodedFormat::I420, width, height) * 2
        }
        DecodedFormat::I210 | DecodedFormat::I212 => {
            let u_size = width * height * 2;
            // U and V planes need to be aligned to 2.
            let uv_size = ((width + 1) / 2) * ((height + 1) / 2) * 2 * 2;

            u_size + uv_size
        }
        DecodedFormat::I410 | DecodedFormat::I412 => (width * height * 2) * 3,
    }
}

/// Copies `src` into `dst` as I410, removing all padding and changing the layout from packed to
/// triplanar. Also drops the alpha channel.
fn y410_to_i410(
    src: &[u8],
    dst: &mut [u8],
    width: usize,
    height: usize,
    strides: [usize; 3],
    offsets: [usize; 3],
) {
    let src_lines = src[offsets[0]..]
        .chunks(strides[0])
        .map(|line| &line[..width * 4]);

    let dst_y_size = width * 2 * height;
    let dst_u_size = width * 2 * height;

    let (dst_y_plane, dst_uv_planes) = dst.split_at_mut(dst_y_size);
    let (dst_u_plane, dst_v_plane) = dst_uv_planes.split_at_mut(dst_u_size);
    let dst_y_lines = dst_y_plane.chunks_mut(width * 2);
    let dst_u_lines = dst_u_plane.chunks_mut(width * 2);
    let dst_v_lines = dst_v_plane.chunks_mut(width * 2);

    for (src_line, (dst_y_line, (dst_u_line, dst_v_line))) in src_lines
        .zip(dst_y_lines.zip(dst_u_lines.zip(dst_v_lines)))
        .take(height)
    {
        for (src, (dst_y, (dst_u, dst_v))) in src_line.chunks(4).zip(
            dst_y_line
                .chunks_mut(2)
                .zip(dst_u_line.chunks_mut(2).zip(dst_v_line.chunks_mut(2))),
        ) {
            let y = LittleEndian::read_u16(&[src[1] >> 2 | src[2] << 6, src[2] >> 2 & 0b11]);
            let u = LittleEndian::read_u16(&[src[0], src[1] & 0b11]);
            let v = LittleEndian::read_u16(&[src[2] >> 4 | src[3] << 4, src[3] >> 4 & 0b11]);
            LittleEndian::write_u16(dst_y, y);
            LittleEndian::write_u16(dst_u, u);
            LittleEndian::write_u16(dst_v, v);
        }
    }
}

/// Instructs on whether it should block on the operation(s).
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub enum BlockingMode {
    Blocking,
    #[default]
    NonBlocking,
}

#[cfg(test)]
mod tests {
    use super::Fourcc;

    const NV12_FOURCC: u32 = 0x3231564E;

    #[test]
    fn fourcc_u32() {
        let fourcc = Fourcc::from(NV12_FOURCC);
        let value: u32 = fourcc.into();
        assert_eq!(value, NV12_FOURCC);
    }

    #[test]
    fn fourcc_u8_4() {
        let fourcc = Fourcc::from(NV12_FOURCC);
        let value: [u8; 4] = fourcc.into();
        assert_eq!(value, *b"NV12");
    }

    #[test]
    fn fourcc_display() {
        let fourcc = Fourcc::from(NV12_FOURCC);
        assert_eq!(fourcc.to_string(), "NV12");
    }

    #[test]
    fn fourcc_debug() {
        let fourcc = Fourcc::from(NV12_FOURCC);
        assert_eq!(format!("{:?}", fourcc), "0x3231564e (NV12)");
    }
}
