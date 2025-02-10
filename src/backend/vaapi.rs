// Copyright 2022 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

//! VAAPI backend for both stateless decoders and encoders.

use std::collections::HashSet;
use std::fmt::Debug;
use std::os::fd::AsRawFd;

use anyhow::anyhow;
use byteorder::ByteOrder;
use byteorder::LittleEndian;
use libva::Display;
use libva::VAConfigAttrib;
use libva::VAConfigAttribType;

use crate::utils::DmabufFrame;
use crate::utils::UserPtrFrame;
use crate::DecodedFormat;

pub mod decoder;
pub mod encoder;
pub mod surface_pool;

fn va_rt_format_to_string(va_rt_format: u32) -> String {
    String::from(match va_rt_format {
        libva::VA_RT_FORMAT_YUV420 => "YUV420",
        libva::VA_RT_FORMAT_YUV422 => "YUV422",
        libva::VA_RT_FORMAT_YUV444 => "YUV444",
        libva::VA_RT_FORMAT_YUV420_10 => "YUV420_10",
        libva::VA_RT_FORMAT_YUV420_12 => "YUV420_12",
        libva::VA_RT_FORMAT_YUV422_10 => "YUV422_10",
        libva::VA_RT_FORMAT_YUV422_12 => "YUV422_12",
        libva::VA_RT_FORMAT_YUV444_10 => "YUV444_10",
        libva::VA_RT_FORMAT_YUV444_12 => "YUV444_12",
        other => return format!("unknown VA rt_format {}", other),
    })
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
struct FormatMap {
    pub rt_format: u32,
    pub va_fourcc: u32,
    pub decoded_format: DecodedFormat,
}

/// Maps a given VA_RT_FORMAT to a compatible decoded format in an arbitrary
/// preferred order.
const FORMAT_MAP: [FormatMap; 10] = [
    FormatMap {
        rt_format: libva::VA_RT_FORMAT_YUV420,
        va_fourcc: libva::VA_FOURCC_NV12,
        decoded_format: DecodedFormat::NV12,
    },
    FormatMap {
        rt_format: libva::VA_RT_FORMAT_YUV420,
        va_fourcc: libva::VA_FOURCC_I420,
        decoded_format: DecodedFormat::I420,
    },
    FormatMap {
        rt_format: libva::VA_RT_FORMAT_YUV422,
        va_fourcc: libva::VA_FOURCC_422H,
        decoded_format: DecodedFormat::I422,
    },
    FormatMap {
        rt_format: libva::VA_RT_FORMAT_YUV444,
        va_fourcc: libva::VA_FOURCC_444P,
        decoded_format: DecodedFormat::I444,
    },
    FormatMap {
        rt_format: libva::VA_RT_FORMAT_YUV420_10,
        va_fourcc: libva::VA_FOURCC_P010,
        decoded_format: DecodedFormat::I010,
    },
    FormatMap {
        rt_format: libva::VA_RT_FORMAT_YUV420_12,
        va_fourcc: libva::VA_FOURCC_P012,
        decoded_format: DecodedFormat::I012,
    },
    FormatMap {
        rt_format: libva::VA_RT_FORMAT_YUV422_10,
        va_fourcc: libva::VA_FOURCC_Y210,
        decoded_format: DecodedFormat::I210,
    },
    FormatMap {
        rt_format: libva::VA_RT_FORMAT_YUV422_12,
        va_fourcc: libva::VA_FOURCC_Y212,
        decoded_format: DecodedFormat::I212,
    },
    FormatMap {
        rt_format: libva::VA_RT_FORMAT_YUV444_10,
        va_fourcc: libva::VA_FOURCC_Y410,
        decoded_format: DecodedFormat::I410,
    },
    FormatMap {
        rt_format: libva::VA_RT_FORMAT_YUV444_12,
        va_fourcc: libva::VA_FOURCC_Y412,
        decoded_format: DecodedFormat::I412,
    },
];

/// Returns a set of supported decoded formats given `rt_format`
fn supported_formats_for_rt_format(
    display: &Display,
    rt_format: u32,
    profile: i32,
    entrypoint: u32,
    image_formats: &[libva::VAImageFormat],
) -> anyhow::Result<HashSet<FormatMap>> {
    let mut attrs =
        vec![VAConfigAttrib { type_: VAConfigAttribType::VAConfigAttribRTFormat, value: 0 }];

    display.get_config_attributes(profile, entrypoint, &mut attrs)?;

    // See whether this RT_FORMAT is supported by the given VAProfile and
    // VAEntrypoint pair.
    if attrs[0].value == libva::VA_ATTRIB_NOT_SUPPORTED || attrs[0].value & rt_format == 0 {
        return Err(anyhow!(
            "rt_format {:?} not supported for profile {:?} and entrypoint {:?}",
            rt_format,
            profile,
            entrypoint
        ));
    }

    let mut supported_formats = HashSet::new();

    for format in FORMAT_MAP {
        if format.rt_format == rt_format {
            supported_formats.insert(format);
        }
    }

    // Only retain those that the hardware can actually map into.
    supported_formats
        .retain(|&entry| image_formats.iter().any(|fmt| fmt.fourcc == entry.va_fourcc));

    Ok(supported_formats)
}

impl TryFrom<&libva::VAImageFormat> for DecodedFormat {
    type Error = anyhow::Error;

    fn try_from(value: &libva::VAImageFormat) -> Result<Self, Self::Error> {
        match value.fourcc {
            libva::VA_FOURCC_I420 => Ok(DecodedFormat::I420),
            libva::VA_FOURCC_NV12 => Ok(DecodedFormat::NV12),
            libva::VA_FOURCC_P010 => Ok(DecodedFormat::I010),
            libva::VA_FOURCC_P012 => Ok(DecodedFormat::I012),
            libva::VA_FOURCC_Y210 => Ok(DecodedFormat::I210),
            libva::VA_FOURCC_Y212 => Ok(DecodedFormat::I212),
            libva::VA_FOURCC_Y410 => Ok(DecodedFormat::I410),
            libva::VA_FOURCC_Y412 => Ok(DecodedFormat::I412),
            _ => Err(anyhow!("Unsupported format")),
        }
    }
}

/// Copies `src` into `dst` removing all padding and converting from biplanar to triplanar format.
///
/// `useful_pixels` is the number of useful pixels in each sample, e.g. `10` for `P010`, `12` for
/// `P012`, etc.
///
/// This function is VAAPI-specific because of the unusual the source pixels are laid out: VAAPI
/// writes the `useful_pixels` MSBs, but software generally expects the LSBs to contain the data.
fn p01x_to_i01x(
    src: &[u8],
    dst: &mut [u8],
    useful_pixels: usize,
    width: usize,
    height: usize,
    strides: [usize; 3],
    offsets: [usize; 3],
) {
    let sample_shift = 16 - useful_pixels;

    // Copy Y.
    //
    // VAAPI's Y samples are two byte little endian with the bottom six bits ignored. We need to
    // convert that to two byte little endian with top 6 bits ignored.

    let src_y_lines = src[offsets[0]..].chunks(strides[0]).map(|line| &line[..width * 2]);
    let dst_y_lines = dst.chunks_mut(width * 2);

    for (src_line, dst_line) in src_y_lines.zip(dst_y_lines).take(height) {
        for (src_y, dst_y) in src_line.chunks(2).zip(dst_line.chunks_mut(2)) {
            LittleEndian::write_u16(dst_y, LittleEndian::read_u16(src_y) >> sample_shift);
        }
    }

    let dst_u_offset = width * 2 * height;

    // Align width and height to 2 for UV plane.
    let width = if width % 2 == 1 { width + 1 } else { width };
    let height = if height % 2 == 1 { height + 1 } else { height };
    // 1 sample per 4 pixels, but we have two components per line so width remains as-is.
    let height = height / 2;

    let dst_u_size = width * height;

    // Copy U and V and deinterleave into different planes.
    //
    // We need to perform the same bit shift as luma, but also to de-interleave the data.
    let src_uv_lines = src[offsets[1]..].chunks(strides[1]).map(|line| &line[..width * 2]);
    let (dst_u_plane, dst_v_plane) = dst[dst_u_offset..].split_at_mut(dst_u_size);
    let dst_u_lines = dst_u_plane.chunks_mut(width);
    let dst_v_lines = dst_v_plane.chunks_mut(width);
    for (src_line, (dst_u_line, dst_v_line)) in
        src_uv_lines.zip(dst_u_lines.zip(dst_v_lines)).take(height)
    {
        for ((src_u, src_v), (dst_u, dst_v)) in src_line
            .chunks(4)
            .map(|chunk| (&chunk[0..2], &chunk[2..4]))
            .zip(dst_u_line.chunks_mut(2).zip(dst_v_line.chunks_mut(2)))
        {
            LittleEndian::write_u16(dst_u, LittleEndian::read_u16(src_u) >> sample_shift);
            LittleEndian::write_u16(dst_v, LittleEndian::read_u16(src_v) >> sample_shift);
        }
    }
}

/// Copies `src` into `dst` as I21x, removing all padding and changing the layout from packed to
/// triplanar.
///
/// `useful_pixels` is the number of useful pixels in each sample, e.g. `10` for `Y210` or `16` for
/// `Y216`.
///
/// This function is VAAPI-specific because of the unusual the source pixels are laid out: VAAPI
/// writes the `useful_pixels` MSBs, but software generally expects the LSBs to contain the data.
///
/// WARNING: this function could not be tested for lack of supporting hardware.
fn y21x_to_i21x(
    src: &[u8],
    dst: &mut [u8],
    useful_pixels: usize,
    width: usize,
    height: usize,
    strides: [usize; 3],
    offsets: [usize; 3],
) {
    let sample_shift = 16 - useful_pixels;
    // Align width to 2 for U and V planes and divide by 2.
    // This should not be necessary as the sampling method requires that width is a multiple of 2
    // to begin with.
    let uv_width = if width % 2 == 1 { width + 1 } else { width } / 2;

    // YUYV representation, i.e. 4 16-bit words per two Y samples meaning we have 4 * width bytes
    // of data per line.
    let src_lines = src[offsets[0]..].chunks(strides[0]).map(|line| &line[..width * 4]);

    let dst_y_size = width * 2 * height;
    let dst_u_size = uv_width * 2 * height;

    let (dst_y_plane, dst_uv_planes) = dst.split_at_mut(dst_y_size);
    let (dst_u_plane, dst_v_plane) = dst_uv_planes.split_at_mut(dst_u_size);
    let dst_y_lines = dst_y_plane.chunks_mut(width * 2);
    let dst_u_lines = dst_u_plane.chunks_mut(uv_width * 2);
    let dst_v_lines = dst_v_plane.chunks_mut(uv_width * 2);

    for (src_line, (dst_y_line, (dst_u_line, dst_v_line))) in
        src_lines.zip(dst_y_lines.zip(dst_u_lines.zip(dst_v_lines))).take(height)
    {
        for (src, (dst_y, (dst_u, dst_v))) in src_line.chunks(8).zip(
            dst_y_line.chunks_mut(4).zip(dst_u_line.chunks_mut(2).zip(dst_v_line.chunks_mut(2))),
        ) {
            let y0 = LittleEndian::read_u16(&src[0..2]) >> sample_shift;
            let u = LittleEndian::read_u16(&src[2..4]) >> sample_shift;
            let y1 = LittleEndian::read_u16(&src[4..6]) >> sample_shift;
            let v = LittleEndian::read_u16(&src[6..8]) >> sample_shift;

            LittleEndian::write_u16(&mut dst_y[0..2], y0);
            LittleEndian::write_u16(&mut dst_y[2..4], y1);
            LittleEndian::write_u16(dst_u, u);
            LittleEndian::write_u16(dst_v, v);
        }
    }
}

/// Copies `src` into `dst` as I412, removing all padding and changing the layout from packed to
/// triplanar. Also drops the alpha channel.
///
/// This function is VAAPI-specific because the samples need to be rolled somehow...
fn y412_to_i412(
    src: &[u8],
    dst: &mut [u8],
    width: usize,
    height: usize,
    strides: [usize; 3],
    offsets: [usize; 3],
) {
    let src_lines = src[offsets[0]..].chunks(strides[0]).map(|line| &line[..width * 8]);

    let dst_y_size = width * 2 * height;
    let dst_u_size = width * 2 * height;

    let (dst_y_plane, dst_uv_planes) = dst.split_at_mut(dst_y_size);
    let (dst_u_plane, dst_v_plane) = dst_uv_planes.split_at_mut(dst_u_size);
    let dst_y_lines = dst_y_plane.chunks_mut(width * 2);
    let dst_u_lines = dst_u_plane.chunks_mut(width * 2);
    let dst_v_lines = dst_v_plane.chunks_mut(width * 2);

    for (src_line, (dst_y_line, (dst_u_line, dst_v_line))) in
        src_lines.zip(dst_y_lines.zip(dst_u_lines.zip(dst_v_lines))).take(height)
    {
        for (src, (dst_y, (dst_u, dst_v))) in src_line.chunks(8).zip(
            dst_y_line.chunks_mut(2).zip(dst_u_line.chunks_mut(2).zip(dst_v_line.chunks_mut(2))),
        ) {
            let y = LittleEndian::read_u16(&src[2..4]);
            let u = LittleEndian::read_u16(&src[0..2]);
            let v = LittleEndian::read_u16(&src[4..6]);
            // Why is that rotate_right neeed??
            LittleEndian::write_u16(dst_y, y.rotate_right(4));
            LittleEndian::write_u16(dst_u, u.rotate_right(4));
            LittleEndian::write_u16(dst_v, v.rotate_right(4));
        }
    }
}

impl libva::ExternalBufferDescriptor for UserPtrFrame {
    const MEMORY_TYPE: libva::MemoryType = libva::MemoryType::UserPtr;
    type DescriptorAttribute = libva::VASurfaceAttribExternalBuffers;

    fn va_surface_attribute(&mut self) -> Self::DescriptorAttribute {
        let pitches = self
            .layout
            .planes
            .iter()
            .map(|p| p.stride as u32)
            .chain(std::iter::repeat(0))
            .take(4)
            .collect::<Vec<_>>()
            .try_into()
            .unwrap();
        let offsets = self
            .layout
            .planes
            .iter()
            .map(|p| p.offset as u32)
            .chain(std::iter::repeat(0))
            .take(4)
            .collect::<Vec<_>>()
            .try_into()
            .unwrap();

        libva::VASurfaceAttribExternalBuffers {
            pixel_format: self.layout.format.0.into(),
            width: self.layout.size.width,
            height: self.layout.size.height,
            data_size: self.mem_layout.size() as u32,
            num_planes: self.layout.planes.len() as u32,
            pitches,
            offsets,
            buffers: self.buffers.as_mut_ptr() as *mut _,
            num_buffers: self.buffers.len() as u32,
            flags: 0,
            private_data: std::ptr::null_mut(),
        }
    }
}

impl libva::ExternalBufferDescriptor for DmabufFrame {
    const MEMORY_TYPE: libva::MemoryType = libva::MemoryType::DrmPrime2;
    type DescriptorAttribute = libva::VADRMPRIMESurfaceDescriptor;

    fn va_surface_attribute(&mut self) -> Self::DescriptorAttribute {
        let objects = self
            .fds
            .iter()
            .map(|fd| libva::VADRMPRIMESurfaceDescriptorObject {
                fd: fd.as_raw_fd(),
                size: nix::sys::stat::fstat(fd.as_raw_fd())
                    .map(|stat| stat.st_size as u32)
                    // If we don't have the information about the plane fd size, fallback to 0.
                    // Libva seems to be *sometimes* "happy" with zero.
                    .unwrap_or(0),
                // TODO should the descriptor be moved to individual objects?
                drm_format_modifier: self.layout.format.1,
            })
            .chain(std::iter::repeat(Default::default()))
            .take(4)
            .collect::<Vec<_>>()
            .try_into()
            .unwrap();

        let layers = [
            libva::VADRMPRIMESurfaceDescriptorLayer {
                drm_format: self.layout.format.0.into(),
                num_planes: self.layout.planes.len() as u32,
                object_index: [0, 0, 0, 0],
                offset: self
                    .layout
                    .planes
                    .iter()
                    .map(|p| p.offset as u32)
                    .chain(std::iter::repeat(0))
                    .take(4)
                    .collect::<Vec<_>>()
                    .try_into()
                    .unwrap(),
                pitch: self
                    .layout
                    .planes
                    .iter()
                    .map(|p| p.stride as u32)
                    .chain(std::iter::repeat(0))
                    .take(4)
                    .collect::<Vec<_>>()
                    .try_into()
                    .unwrap(),
            },
            Default::default(),
            Default::default(),
            Default::default(),
        ];

        libva::VADRMPRIMESurfaceDescriptor {
            // TODO should we match and use VA_FOURCC_* here?
            fourcc: self.layout.format.0.into(),
            width: self.layout.size.width,
            height: self.layout.size.height,
            num_objects: 1,
            objects,
            num_layers: 1,
            layers,
        }
    }
}
