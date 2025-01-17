// Copyright 2025 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::cell::RefCell;

use crate::utils::align_up;
use crate::DecodedFormat;
use crate::EncodedFormat;
use crate::Fourcc;
use crate::Resolution;

#[cfg(feature = "backend")]
pub mod gbm_video_frame;

pub const Y_PLANE: usize = 0;
pub const UV_PLANE: usize = 1;
pub const U_PLANE: usize = 1;
pub const V_PLANE: usize = 2;

// RAII wrappers for video memory mappings. The Drop method should implement any necessary
// munmap()'ing and cache flushing.
pub trait ReadMapping<'a> {
    fn get(&self) -> Vec<&[u8]>;
}

pub trait WriteMapping<'a> {
    fn get(&self) -> Vec<RefCell<&'a mut [u8]>>;
}

// Unified abstraction for any kind of frame data that might be sent to the hardware.
pub trait VideoFrame {
    type NativeHandle;

    fn fourcc(&self) -> Fourcc;

    // Outputs visible resolution. Use pitch and plane size for coded resolution calculations.
    fn resolution(&self) -> Resolution;

    fn is_compressed(&self) -> bool {
        match self.fourcc().to_string().as_str() {
            "H264" | "HEVC" | "VP80" | "VP90" | "AV1F" => true,
            _ => false,
        }
    }

    // Whether or not all the planes are in a contiguous memory allocation. For example, returns
    // true for NV12 and false for NM12.
    fn is_contiguous(&self) -> bool {
        if self.is_compressed() {
            return false;
        }

        // TODO: Add more formats.
        match self.fourcc().to_string().as_str() {
            "MM21" | "NM12" => false,
            _ => true,
        }
    }

    fn decoded_format(&self) -> Result<DecodedFormat, String> {
        if self.is_compressed() {
            return Err("Cannot convert compressed format into decoded format".to_string());
        }

        Ok(DecodedFormat::from(self.fourcc()))
    }

    fn encoded_format(&self) -> Result<EncodedFormat, String> {
        if !self.is_compressed() {
            return Err("Cannot convert uncompressed format into encoded format".to_string());
        }

        Ok(EncodedFormat::from(self.fourcc()))
    }

    fn num_planes(&self) -> usize {
        if self.is_compressed() {
            return 1;
        }

        match self.decoded_format().unwrap() {
            DecodedFormat::I420
            | DecodedFormat::I422
            | DecodedFormat::I444
            | DecodedFormat::I010
            | DecodedFormat::I012
            | DecodedFormat::I210
            | DecodedFormat::I212
            | DecodedFormat::I410
            | DecodedFormat::I412 => 3,
            DecodedFormat::NV12 | DecodedFormat::MM21 => 2,
        }
    }

    fn get_horizontal_subsampling(&self) -> Vec<usize> {
        let mut ret: Vec<usize> = vec![];
        for plane_idx in 0..self.num_planes() {
            if self.is_compressed() {
                ret.push(1);
            } else {
                ret.push(match self.decoded_format().unwrap() {
                    DecodedFormat::I420
                    | DecodedFormat::NV12
                    | DecodedFormat::I422
                    | DecodedFormat::I010
                    | DecodedFormat::I012
                    | DecodedFormat::I210
                    | DecodedFormat::I212
                    | DecodedFormat::MM21 => {
                        if plane_idx == 0 {
                            1
                        } else {
                            2
                        }
                    }
                    DecodedFormat::I444 | DecodedFormat::I410 | DecodedFormat::I412 => 1,
                });
            }
        }
        ret
    }

    fn get_vertical_subsampling(&self) -> Vec<usize> {
        let mut ret: Vec<usize> = vec![];
        for plane_idx in 0..self.num_planes() {
            if self.is_compressed() {
                ret.push(1);
            } else {
                ret.push(match self.decoded_format().unwrap() {
                    DecodedFormat::I420
                    | DecodedFormat::NV12
                    | DecodedFormat::I010
                    | DecodedFormat::I012
                    | DecodedFormat::MM21 => {
                        if plane_idx == 0 {
                            1
                        } else {
                            2
                        }
                    }
                    DecodedFormat::I422
                    | DecodedFormat::I444
                    | DecodedFormat::I210
                    | DecodedFormat::I212
                    | DecodedFormat::I410
                    | DecodedFormat::I412 => 1,
                })
            }
        }
        ret
    }

    fn get_bytes_per_element(&self) -> Vec<usize> {
        let mut ret: Vec<usize> = vec![];
        for plane_idx in 0..self.num_planes() {
            if self.is_compressed() {
                ret.push(1);
            } else {
                ret.push(match self.decoded_format().unwrap() {
                    DecodedFormat::I420 | DecodedFormat::I422 | DecodedFormat::I444 => 1,
                    DecodedFormat::I010
                    | DecodedFormat::I012
                    | DecodedFormat::I210
                    | DecodedFormat::I212
                    | DecodedFormat::I410
                    | DecodedFormat::I412 => 2,
                    DecodedFormat::NV12 | DecodedFormat::MM21 => {
                        if plane_idx == 0 {
                            1
                        } else {
                            2
                        }
                    }
                })
            }
        }
        ret
    }

    fn get_plane_size(&self) -> Vec<usize>;

    // Pitch is measured in bytes while stride is measured in pixels.
    fn get_plane_pitch(&self) -> Vec<usize>;

    fn validate_frame(&self) -> Result<(), String> {
        if self.is_compressed() {
            return Ok(());
        }

        let horizontal_subsampling = self.get_horizontal_subsampling();
        let vertical_subsampling = self.get_vertical_subsampling();
        let bytes_per_element = self.get_bytes_per_element();
        let plane_pitch = self.get_plane_pitch();
        let plane_size = self.get_plane_size();

        for plane in 0..self.num_planes() {
            let minimum_pitch = align_up(
                self.resolution().width as usize,
                horizontal_subsampling[plane],
            ) * bytes_per_element[plane]
                / horizontal_subsampling[plane];
            if plane_pitch[plane] < minimum_pitch {
                return Err(
                    "Pitch of plane {plane} is insufficient to accomodate format!".to_string(),
                );
            }
            let minimum_size = align_up(
                self.resolution().height as usize,
                vertical_subsampling[plane],
            ) / vertical_subsampling[plane]
                * plane_pitch[plane];
            if plane_size[plane] < minimum_size {
                return Err(
                    "Size of plane {plane} is insufficient to accomodate format!".to_string(),
                );
            }
        }

        Ok(())
    }

    fn map<'a>(&'a self) -> Result<Box<dyn ReadMapping<'a> + 'a>, String>;

    fn map_mut<'a>(&'a mut self) -> Result<Box<dyn WriteMapping<'a> + 'a>, String>;

    fn to_native_handle(self: Box<Self>) -> Result<Self::NativeHandle, String>;
}
