// Copyright 2025 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::cell::RefCell;
use std::fs::File;
use std::iter::zip;
use std::os::fd::AsRawFd;
use std::os::fd::BorrowedFd;
use std::os::fd::FromRawFd;
use std::path::Path;
use std::ptr;
use std::slice;
use std::sync::Arc;

use crate::utils::align_up;
use crate::utils::buffer_size_for_area;
use crate::video_frame::ReadMapping;
use crate::video_frame::VideoFrame;
use crate::video_frame::WriteMapping;
use crate::Fourcc;
use crate::Resolution;

use drm_fourcc::DrmFourcc;
use gbm_sys::{
    gbm_bo, gbm_bo_create, gbm_bo_destroy, gbm_bo_flags, gbm_bo_get_fd, gbm_bo_get_height,
    gbm_bo_get_offset, gbm_bo_get_stride_for_plane, gbm_bo_get_width, gbm_bo_import, gbm_bo_map,
    gbm_bo_transfer_flags, gbm_bo_unmap, gbm_create_device, gbm_device, gbm_device_destroy,
    gbm_import_fd_data,
};
use nix::libc;

#[cfg(feature = "vaapi")]
use libva::Surface;
#[cfg(feature = "vaapi")]
use libva::SurfaceMemoryDescriptor;
#[cfg(feature = "v4l2")]
use v4l2r::memory::DmaBufHandle;
#[cfg(feature = "v4l2")]
use v4l2r::memory::DmaBufSource;

// The gbm crate's wrapper for map() doesn't have the lifetime semantics that we want, so we need
// to implement our own.
// Unfortunately, because the GBM crate's BufferObject class does not expose the raw gbm_bo
// pointer, we are going to need to reimplement a lot of its functionality. This means a lot of
// "unsafe" code will be needed in this module. We guarantee the safety of this code by using
// Rust's native lifetime system to ensure that parent objects do not get deallocated before all of
// their children. For example, map objects maintain a reference to the underlying video frame,
// and video frames maintain a reference to the GBM device that allocated them.

fn map_bo(
    bo: *mut gbm_bo,
    is_writable: bool,
) -> Result<(*mut libc::c_void, *mut libc::c_void), String> {
    let permissions = if is_writable {
        gbm_bo_transfer_flags::GBM_BO_TRANSFER_READ_WRITE
    } else {
        gbm_bo_transfer_flags::GBM_BO_TRANSFER_READ
    };
    let mut stride: u32 = 0;
    let mut map_data: *mut libc::c_void = ptr::null_mut();
    unsafe {
        // This should be safe because we validate that bo is non-null on creation and we tie the
        // lifetimes of the VideoFrames to the GBM device.
        let map_result = gbm_bo_map(
            bo,
            0,
            0,
            gbm_bo_get_width(bo),
            gbm_bo_get_height(bo),
            permissions,
            &mut stride as *mut u32,
            &mut map_data as *mut *mut libc::c_void,
        );
        if map_result.is_null() {
            Err(format!(
                "Failed to map GBM buffer object read {}!",
                if is_writable { "write" } else { "only" }
            ))
        } else {
            Ok((map_result, map_data))
        }
    }
}

fn import_bo_from_dmabuf_fd(
    device: *mut gbm_device,
    dma: BorrowedFd<'_>,
    width: u32,
    height: u32,
    stride: u32,
    format: u32,
    usage: u32,
) -> *mut gbm_bo {
    let mut import_data = gbm_import_fd_data {
        fd: dma.as_raw_fd(),
        width: width,
        height: height,
        stride: stride,
        format: format,
    };
    unsafe {
        gbm_bo_import(
            device,
            gbm_sys::GBM_BO_IMPORT_FD,
            &mut import_data as *mut gbm_import_fd_data as *mut libc::c_void,
            usage,
        )
    }
}

pub struct GbmMapping<'a> {
    map_datas: Vec<*mut libc::c_void>,
    raw_mems: Vec<*mut libc::c_void>,
    lens: Vec<usize>,
    is_writable: bool,
    frame: &'a GbmVideoFrame,
}

impl<'a> ReadMapping<'a> for GbmMapping<'a> {
    fn get(&self) -> Vec<&[u8]> {
        unsafe {
            zip(self.raw_mems.iter(), self.lens.iter())
                .map(|x| slice::from_raw_parts(*x.0 as *const u8, *x.1))
                .collect()
        }
    }
}

impl<'a> WriteMapping<'a> for GbmMapping<'a> {
    fn get(&self) -> Vec<RefCell<&'a mut [u8]>> {
        if !self.is_writable {
            panic!("Attempted to get writable slice to read only mapping!");
        }

        // The above check prevents us from undefined behavior in the event that the user attempts
        // to coerce a ReadMapping into a WriteMapping.
        unsafe {
            zip(self.raw_mems.iter(), self.lens.iter())
                .map(|x| RefCell::new(slice::from_raw_parts_mut(*x.0 as *mut u8, *x.1)))
                .collect()
        }
    }
}

impl<'a> Drop for GbmMapping<'a> {
    fn drop(&mut self) {
        for plane_idx in 0..self.map_datas.len() {
            let bo = if self.frame.bo.len() == 1 {
                self.frame.bo[0]
            } else {
                self.frame.bo[plane_idx]
            };
            let map_data = self.map_datas[plane_idx];
            unsafe { gbm_bo_unmap(bo, map_data) };
        }
    }
}

pub struct GbmVideoFrame {
    fourcc: Fourcc,
    resolution: Resolution,
    bo: Vec<*mut gbm_bo>,
    // This reference is just to create a lifetime constraint. We don't want someone to close the
    // device before free'ing up all allocated VideoFrames.
    _device: Arc<GbmDevice>,
}

impl GbmVideoFrame {
    fn map_helper(&self, is_writable: bool) -> Result<GbmMapping, String> {
        let mut ret = GbmMapping {
            map_datas: vec![],
            raw_mems: vec![],
            lens: self.get_plane_size(),
            is_writable: is_writable,
            frame: self,
        };
        for plane_idx in 0..self.num_planes() {
            let (bo, offset) = if self.bo.len() == 1 {
                (self.bo[0], unsafe {
                    gbm_bo_get_offset(self.bo[0], plane_idx as libc::c_int) as isize
                })
            } else {
                (self.bo[plane_idx], unsafe {
                    gbm_bo_get_offset(self.bo[plane_idx], 0) as isize
                })
            };
            let (raw_mem, map_data) = map_bo(bo, is_writable)?;
            ret.map_datas.push(map_data);
            ret.raw_mems.push(unsafe { raw_mem.offset(offset) });
        }
        Ok(ret)
    }
}

impl VideoFrame for GbmVideoFrame {
    #[cfg(feature = "v4l2")]
    type NativeHandle = Vec<DmaBufHandle<File>>;

    // TODO
    #[cfg(feature = "vaapi")]
    type NativeHandle = ();

    fn fourcc(&self) -> Fourcc {
        self.fourcc.clone()
    }

    fn resolution(&self) -> Resolution {
        self.resolution.clone()
    }

    // GBM allocates all the planes in one buffer and doesn't expose a means to get the size of a
    // single plane, so just stub this method.
    fn get_plane_size(&self) -> Vec<usize> {
        let mut ret: Vec<usize> = vec![];
        let vertical_subsampling = self.get_vertical_subsampling();
        let plane_pitch = self.get_plane_pitch();
        for plane_idx in 0..self.num_planes() {
            ret.push(
                plane_pitch[plane_idx]
                    * align_up(
                        self.resolution().height as usize,
                        vertical_subsampling[plane_idx],
                    )
                    / vertical_subsampling[plane_idx],
            );
        }
        ret
    }

    fn get_plane_pitch(&self) -> Vec<usize> {
        let mut ret: Vec<usize> = vec![];
        for plane_idx in 0..self.num_planes() {
            if self.bo.len() > 1 {
                ret.push(unsafe { gbm_bo_get_stride_for_plane(self.bo[plane_idx], 0) as usize });
            } else {
                ret.push(unsafe {
                    gbm_bo_get_stride_for_plane(self.bo[0], plane_idx as libc::c_int) as usize
                });
            }
        }
        ret
    }

    fn map<'a>(&'a self) -> Result<Box<dyn ReadMapping<'a> + 'a>, String> {
        Ok(Box::new(self.map_helper(false)?))
    }

    fn map_mut<'a>(&'a mut self) -> Result<Box<dyn WriteMapping<'a> + 'a>, String> {
        Ok(Box::new(self.map_helper(true)?))
    }

    #[cfg(feature = "v4l2")]
    fn to_native_handle(self: Box<Self>) -> Result<Self::NativeHandle, String> {
        Ok(self
            .bo
            .iter()
            .map(|bo| DmaBufHandle::from(unsafe { File::from_raw_fd(gbm_bo_get_fd(*bo)) }))
            .collect())
    }

    #[cfg(feature = "vaapi")]
    fn to_native_handle(self: Box<Self>) -> Result<Self::NativeHandle, String> {
        todo!("VA-API native handles not implemented yet!");
    }
}

impl Drop for GbmVideoFrame {
    fn drop(&mut self) {
        for bo in self.bo.iter() {
            unsafe { gbm_bo_destroy(*bo) };
        }
    }
}

pub struct GbmDevice {
    device: *mut gbm_device,
    // Keeps device file descriptors valid as long as the GbmDevice is alive.
    _device_file: std::fs::File,
}

impl GbmDevice {
    pub fn open<P: AsRef<Path>>(path: P) -> Result<Arc<Self>, String> {
        let device_file = std::fs::OpenOptions::new()
            .read(true)
            .write(true)
            .open(path)
            .map_err(|_| "Error opening GBM device!".to_string())?;
        let device = unsafe { gbm_create_device(device_file.as_raw_fd()) };
        if device.is_null() {
            Err("Could not create GBM device from file!".to_string())
        } else {
            Ok(Arc::new(Self {
                device: device,
                _device_file: device_file,
            }))
        }
    }

    pub fn new_frame(
        self: Arc<Self>,
        fourcc: Fourcc,
        resolution: Resolution,
    ) -> Result<GbmVideoFrame, String> {
        let mut ret = GbmVideoFrame {
            _device: Arc::clone(&self),
            fourcc: fourcc,
            resolution: resolution,
            bo: vec![],
        };

        // TODO: Pass in a parameter that determines this usage rather than just OR'ing everything
        // together.
        let usage = gbm_bo_flags::GBM_BO_USE_LINEAR as u32;

        if ret.is_compressed() {
            let buffer_size = buffer_size_for_area(resolution.width, resolution.height);
            // The width and height values are ultimately arbitrary, but we should try to pick some
            // that won't trigger the GEM driver to add padding, so we only allocate what we need.
            // 32 pixels is a common enough alignment that this provides us with a decent guess.
            let fake_width = align_up((buffer_size as f64).sqrt() as u32, 32);
            let fake_height = align_up(buffer_size as u32, fake_width) / fake_width;
            let bo = unsafe {
                gbm_bo_create(
                    self.device,
                    fake_width,
                    fake_height,
                    DrmFourcc::R8 as u32,
                    usage,
                )
            };
            if bo.is_null() {
                Err("Error allocating compressed buffer!".to_string())
            } else {
                ret.bo.push(bo);
                Ok(ret)
            }
        } else if ret.is_contiguous() {
            let bo = unsafe {
                gbm_bo_create(
                    self.device,
                    resolution.width,
                    resolution.height,
                    u32::from(fourcc),
                    usage,
                )
            };
            if bo.is_null() {
                Err(format!(
                    "Error allocating contiguous buffer! Fourcc: {} width: {} height: {}",
                    fourcc.to_string(),
                    resolution.width,
                    resolution.height
                ))
            } else {
                ret.bo.push(bo);
                Ok(ret)
            }
        } else {
            // We hack multiplanar formats into GBM by making a bunch of separate BO's. We use
            // either R8 or RG88 depending on the bytes per element. So NM12 for example would be
            // an R8 BO and then an RG88 BO of 1/4th the size.
            let horizontal_subsampling = ret.get_horizontal_subsampling();
            let vertical_subsampling = ret.get_vertical_subsampling();
            let bytes_per_element = ret.get_bytes_per_element();
            for plane_idx in 0..ret.num_planes() {
                let bo = unsafe {
                    gbm_bo_create(
                        self.device,
                        (align_up(resolution.width as usize, horizontal_subsampling[plane_idx])
                            / horizontal_subsampling[plane_idx]
                            * bytes_per_element[plane_idx]) as u32,
                        (align_up(resolution.height as usize, vertical_subsampling[plane_idx])
                            / vertical_subsampling[plane_idx]) as u32,
                        DrmFourcc::R8 as u32,
                        usage,
                    )
                };
                if bo.is_null() {
                    return Err(format!(
                        "Error allocating plane {} for format {}",
                        plane_idx,
                        fourcc.to_string()
                    ));
                }
                ret.bo.push(bo);
            }
            Ok(ret)
        }
    }

    #[cfg(feature = "v4l2")]
    pub fn import_from_v4l2<S: DmaBufSource>(
        self: Arc<Self>,
        fourcc: Fourcc,
        resolution: Resolution,
        strides: Vec<usize>,
        native_handle: Vec<DmaBufHandle<S>>,
    ) -> Result<GbmVideoFrame, String> {
        let mut ret = GbmVideoFrame {
            _device: Arc::clone(&self),
            fourcc: fourcc,
            resolution: resolution,
            bo: vec![],
        };

        if strides.is_empty() || native_handle.is_empty() {
            return Err("Cannot import empty V4L2 handle!".to_string());
        }
        if !ret.is_contiguous() && strides.len() != ret.num_planes() {
            return Err(format!(
                "Invalid number of strides for format {}",
                fourcc.to_string()
            ));
        }
        if !ret.is_contiguous() && native_handle.len() != ret.num_planes() {
            return Err(format!(
                "Invalid number of V4L2 planes for format {}",
                fourcc.to_string()
            ));
        }

        // TODO: Pass in a parameter that determines this usage rather than just OR'ing everything
        // together.
        let usage = gbm_bo_flags::GBM_BO_USE_LINEAR as u32;
        if ret.is_contiguous() {
            let bo = import_bo_from_dmabuf_fd(
                self.device,
                native_handle[0].0.as_fd(),
                ret.resolution.width,
                ret.resolution.height,
                strides[0] as u32,
                u32::from(fourcc),
                usage,
            );
            if bo.is_null() {
                return Err("Error importing contiguous V4L2 buffer!".to_string());
            }
            ret.bo.push(bo);
        } else {
            let horizontal_subsampling = ret.get_horizontal_subsampling();
            let vertical_subsampling = ret.get_vertical_subsampling();
            let bytes_per_element = ret.get_bytes_per_element();
            for plane_idx in 0..ret.num_planes() {
                let bo = import_bo_from_dmabuf_fd(
                    self.device,
                    native_handle[plane_idx].0.as_fd(),
                    (align_up(resolution.width as usize, horizontal_subsampling[plane_idx])
                        / horizontal_subsampling[plane_idx]
                        * bytes_per_element[plane_idx]) as u32,
                    (align_up(resolution.height as usize, vertical_subsampling[plane_idx])
                        / vertical_subsampling[plane_idx]) as u32,
                    strides[plane_idx] as u32,
                    DrmFourcc::R8 as u32,
                    usage,
                );
                if bo.is_null() {
                    return Err(format!("Error importing plane {}", plane_idx));
                }
                ret.bo.push(bo);
            }
        }

        Ok(ret)
    }
}

impl Drop for GbmDevice {
    fn drop(&mut self) {
        unsafe { gbm_device_destroy(self.device) }
    }
}

// TODO: Add unit tests
