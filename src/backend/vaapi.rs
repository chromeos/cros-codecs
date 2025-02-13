// Copyright 2022 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

//! VAAPI backend for both stateless decoders and encoders.

use std::collections::HashSet;
use std::fmt::Debug;
use std::os::fd::AsRawFd;

use anyhow::anyhow;
use libva::Display;
use libva::VAConfigAttrib;
use libva::VAConfigAttribType;

use crate::utils::DmabufFrame;
use crate::utils::UserPtrFrame;
use crate::DecodedFormat;

pub mod decoder;
pub mod encoder;
pub mod surface_pool;

#[allow(dead_code)]
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

#[allow(dead_code)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
struct FormatMap {
    pub rt_format: u32,
    pub va_fourcc: u32,
    pub decoded_format: DecodedFormat,
}

/// Maps a given VA_RT_FORMAT to a compatible decoded format in an arbitrary
/// preferred order.
#[allow(dead_code)]
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
#[allow(dead_code)]
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
