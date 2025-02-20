// Copyright 2025 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::cell::RefCell;
use std::fs::File;
use std::iter::zip;
use std::os::fd::AsRawFd;
#[cfg(feature = "v4l2")]
use std::os::fd::BorrowedFd;
use std::os::fd::FromRawFd;
use std::path::Path;
use std::ptr;
#[cfg(feature = "vaapi")]
use std::rc::Rc;
use std::slice;
use std::sync::Arc;

use crate::utils::align_up;
use crate::utils::buffer_size_for_area;
use crate::video_frame::ReadMapping;
use crate::video_frame::VideoFrame;
use crate::video_frame::WriteMapping;
#[cfg(feature = "vaapi")]
use crate::DecodedFormat;
use crate::Fourcc;
use crate::Resolution;

use drm_fourcc::DrmFourcc;
#[cfg(feature = "v4l2")]
use gbm_sys::gbm_import_fd_data;
#[cfg(feature = "vaapi")]
use gbm_sys::gbm_import_fd_modifier_data;
use gbm_sys::{
    gbm_bo, gbm_bo_create, gbm_bo_destroy, gbm_bo_flags, gbm_bo_get_fd, gbm_bo_get_height,
    gbm_bo_get_modifier, gbm_bo_get_offset, gbm_bo_get_stride_for_plane, gbm_bo_get_width,
    gbm_bo_import, gbm_bo_map, gbm_bo_transfer_flags, gbm_bo_unmap, gbm_create_device, gbm_device,
    gbm_device_destroy,
};
use nix::libc;

#[cfg(feature = "v4l2")]
use crate::v4l2r::device::Device;
#[cfg(feature = "vaapi")]
use libva::{
    Display, ExternalBufferDescriptor, MemoryType, Surface, UsageHint, VADRMPRIMESurfaceDescriptor,
    VADRMPRIMESurfaceDescriptorLayer, VADRMPRIMESurfaceDescriptorObject,
};
#[cfg(feature = "v4l2")]
use v4l2r::ioctl::V4l2Buffer;
#[cfg(feature = "v4l2")]
use v4l2r::memory::{DmaBufHandle, DmaBufSource};
#[cfg(feature = "v4l2")]
use v4l2r::Format;

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

#[cfg(feature = "v4l2")]
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
            let bo =
                if self.frame.bo.len() == 1 { self.frame.bo[0] } else { self.frame.bo[plane_idx] };
            let map_data = self.map_datas[plane_idx];
            unsafe { gbm_bo_unmap(bo, map_data) };
        }
    }
}

#[derive(Debug)]
pub struct GbmVideoFrame {
    fourcc: Fourcc,
    resolution: Resolution,
    bo: Vec<*mut gbm_bo>,
    // This reference is just to create a lifetime constraint. We don't want someone to close the
    // device before free'ing up all allocated VideoFrames. We keep it optional just so that we can
    // create default video frames for the purpose of signaling a drain event.
    _device: Option<Arc<GbmDevice>>,
    // This ties the lifetime of the DRM FDs to the lifetime of this object so that we can export
    // to V4L2.
    #[cfg(feature = "v4l2")]
    export_handles: Vec<DmaBufHandle<File>>,
}

impl GbmVideoFrame {
    fn get_plane_offset(&self) -> Vec<usize> {
        let mut ret: Vec<usize> = vec![];
        for plane_idx in 0..self.num_planes() {
            if self.bo.len() > 1 {
                ret.push(unsafe { gbm_bo_get_offset(self.bo[plane_idx], 0) as usize });
            } else {
                ret.push(unsafe {
                    gbm_bo_get_offset(self.bo[0], plane_idx as libc::c_int) as usize
                });
            }
        }
        ret
    }

    #[allow(dead_code)]
    fn get_modifier(&self) -> u64 {
        unsafe { gbm_bo_get_modifier(self.bo[0]) }
    }

    fn map_helper(&self, is_writable: bool) -> Result<GbmMapping, String> {
        let offsets = self.get_plane_offset();
        let mut ret = GbmMapping {
            map_datas: vec![],
            raw_mems: vec![],
            lens: self.get_plane_size(),
            is_writable: is_writable,
            frame: self,
        };
        for plane_idx in 0..self.num_planes() {
            let bo = if self.bo.len() == 1 { self.bo[0] } else { self.bo[plane_idx] };
            let (raw_mem, map_data) = map_bo(bo, is_writable)?;
            ret.map_datas.push(map_data);
            ret.raw_mems.push(unsafe { raw_mem.offset(offsets[plane_idx] as isize) });
        }
        Ok(ret)
    }
}

#[cfg(feature = "vaapi")]
pub struct GbmExternalBufferDescriptor {
    fourcc: Fourcc,
    modifier: u64,
    resolution: Resolution,
    pitches: Vec<usize>,
    offsets: Vec<usize>,
    // We use a proper File object here to correctly manage the lifetimes of the exported FDs.
    // Otherwise we risk exhausting the FD limit on the machine for certain test vectors.
    export_file: File,
}

#[cfg(feature = "vaapi")]
impl ExternalBufferDescriptor for GbmExternalBufferDescriptor {
    const MEMORY_TYPE: MemoryType = MemoryType::DrmPrime2;
    type DescriptorAttribute = VADRMPRIMESurfaceDescriptor;

    fn va_surface_attribute(&mut self) -> Self::DescriptorAttribute {
        let objects = [
            VADRMPRIMESurfaceDescriptorObject {
                fd: self.export_file.as_raw_fd(),
                size: self.export_file.metadata().unwrap().len() as u32,
                drm_format_modifier: self.modifier,
            },
            Default::default(),
            Default::default(),
            Default::default(),
        ];

        let layers = [
            VADRMPRIMESurfaceDescriptorLayer {
                drm_format: u32::from(self.fourcc),
                num_planes: self.pitches.len() as u32,
                object_index: [0, 0, 0, 0],
                offset: self
                    .offsets
                    .iter()
                    .map(|x| *x as u32)
                    .chain(std::iter::repeat(0))
                    .take(4)
                    .collect::<Vec<_>>()
                    .try_into()
                    .unwrap(),
                pitch: self
                    .pitches
                    .iter()
                    .map(|x| *x as u32)
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
        let resolution = self.resolution;
        let ret = VADRMPRIMESurfaceDescriptor {
            fourcc: u32::from(self.fourcc),
            width: resolution.width,
            height: resolution.height,
            num_objects: 1,
            objects,
            num_layers: 1,
            layers,
        };
        ret
    }
}

impl VideoFrame for GbmVideoFrame {
    #[cfg(feature = "v4l2")]
    type NativeHandle = DmaBufHandle<File>;

    #[cfg(feature = "vaapi")]
    type MemDescriptor = GbmExternalBufferDescriptor;
    #[cfg(feature = "vaapi")]
    type NativeHandle = Surface<GbmExternalBufferDescriptor>;

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
                    * align_up(self.resolution().height as usize, vertical_subsampling[plane_idx])
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
    fn to_native_handle(&self, plane: usize) -> Result<&Self::NativeHandle, String> {
        if plane >= self.export_handles.len() {
            Err("No such plane {plane}".to_string())
        } else {
            Ok(&self.export_handles[plane])
        }
    }

    // No-op for GBM buffers since the backing FD already disambiguates them.
    #[cfg(feature = "v4l2")]
    fn process_dqbuf(&mut self, _device: Arc<Device>, _format: &Format, _buf: &V4l2Buffer) {}

    #[cfg(feature = "vaapi")]
    fn to_native_handle(&self, display: &Rc<Display>) -> Result<Self::NativeHandle, String> {
        if self.is_compressed() {
            return Err("Compressed buffer export to VA-API is not currently supported".to_string());
        }
        if !self.is_contiguous() {
            return Err(
                "Exporting non-contiguous GBM buffers to VA-API is not currently supported"
                    .to_string(),
            );
        }

        // TODO: Add more supported formats
        let rt_format = match self.decoded_format().unwrap() {
            DecodedFormat::I420 | DecodedFormat::NV12 => libva::VA_RT_FORMAT_YUV420,
            _ => return Err("Format unsupported for VA-API export".to_string()),
        };

        let export_descriptor = vec![GbmExternalBufferDescriptor {
            fourcc: self.fourcc.clone(),
            modifier: self.get_modifier(),
            resolution: self.resolution(),
            pitches: self.get_plane_pitch(),
            offsets: self.get_plane_offset(),
            // SAFETY: gbm_bo_get_fd() gives us a fresh, owned fd on every call.
            export_file: unsafe { File::from_raw_fd(gbm_bo_get_fd(self.bo[0])) },
        }];

        let mut ret = display
            .create_surfaces(
                rt_format,
                Some(u32::from(self.fourcc)),
                self.resolution().width,
                self.resolution().height,
                Some(UsageHint::USAGE_HINT_DECODER),
                export_descriptor,
            )
            .map_err(|_| "Error importing GbmVideoFrame to VA-API".to_string())?;

        Ok(ret.pop().unwrap())
    }
}

impl Drop for GbmVideoFrame {
    fn drop(&mut self) {
        for bo in self.bo.iter() {
            unsafe { gbm_bo_destroy(*bo) };
        }
    }
}

// UNSAFE: We will only access the raw BOs from the worker thread, and there are no other copies of
// these pointers.
unsafe impl Send for GbmVideoFrame {}
unsafe impl Sync for GbmVideoFrame {}

#[derive(Debug)]
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
            Ok(Arc::new(Self { device: device, _device_file: device_file }))
        }
    }

    pub fn new_frame(
        self: Arc<Self>,
        fourcc: Fourcc,
        visible_resolution: Resolution,
        coded_resolution: Resolution,
    ) -> Result<GbmVideoFrame, String> {
        let mut ret = GbmVideoFrame {
            fourcc: fourcc,
            resolution: visible_resolution,
            bo: vec![],
            _device: Some(Arc::clone(&self)),
            #[cfg(feature = "v4l2")]
            export_handles: vec![],
        };

        if ret.is_compressed() {
            let buffer_size = buffer_size_for_area(coded_resolution.width, coded_resolution.height);
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
                    gbm_bo_flags::GBM_BO_USE_LINEAR as u32,
                )
            };
            if bo.is_null() {
                return Err("Error allocating compressed buffer!".to_string());
            }

            ret.bo.push(bo);
        } else if ret.is_contiguous() {
            // gbm_sys is missing this use flag for some reason.
            const GBM_BO_USE_HW_VIDEO_DECODER: u32 = 1 << 13;
            // It's important that we use the correct use flag for platforms that support directly
            // importing GBM allocated frame buffers to the video decoding hardware because the
            // video decoding hardware sometimes makes assumptions about the modifier flags. If we
            // try to force everything to be linear, we can end up getting a tiled frame when we
            // try to map it.
            let bo = unsafe {
                gbm_bo_create(
                    self.device,
                    coded_resolution.width,
                    coded_resolution.height,
                    u32::from(fourcc),
                    GBM_BO_USE_HW_VIDEO_DECODER,
                )
            };
            if bo.is_null() {
                return Err(format!(
                    "Error allocating contiguous buffer! Fourcc: {} width: {} height: {}",
                    fourcc.to_string(),
                    coded_resolution.width,
                    coded_resolution.height
                ));
            }

            ret.bo.push(bo);
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
                        (align_up(
                            coded_resolution.width as usize,
                            horizontal_subsampling[plane_idx],
                        ) / horizontal_subsampling[plane_idx]
                            * bytes_per_element[plane_idx]) as u32,
                        (align_up(
                            coded_resolution.height as usize,
                            vertical_subsampling[plane_idx],
                        ) / vertical_subsampling[plane_idx]) as u32,
                        DrmFourcc::R8 as u32,
                        gbm_bo_flags::GBM_BO_USE_LINEAR as u32,
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
        }

        #[cfg(feature = "v4l2")]
        {
            ret.export_handles = ret
                .bo
                .iter()
                .map(|bo| unsafe {
                    DmaBufHandle::from(File::from_raw_fd(gbm_bo_get_fd(bo.clone())))
                })
                .collect::<_>();
        }

        Ok(ret)
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
            fourcc: fourcc,
            resolution: resolution,
            bo: vec![],
            export_handles: vec![],
            _device: Some(Arc::clone(&self)),
        };

        if strides.is_empty() || native_handle.is_empty() {
            return Err("Cannot import empty V4L2 handle!".to_string());
        }
        if !ret.is_contiguous() && strides.len() != ret.num_planes() {
            return Err(format!("Invalid number of strides for format {}", fourcc.to_string()));
        }
        if !ret.is_contiguous() && native_handle.len() != ret.num_planes() {
            return Err(format!("Invalid number of V4L2 planes for format {}", fourcc.to_string()));
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

        ret.export_handles = ret
            .bo
            .iter()
            .map(|bo| unsafe { DmaBufHandle::from(File::from_raw_fd(gbm_bo_get_fd(bo.clone()))) })
            .collect::<_>();

        Ok(ret)
    }

    #[cfg(feature = "vaapi")]
    pub fn import_from_vaapi(
        self: Arc<Self>,
        surface: &Surface<GbmExternalBufferDescriptor>,
    ) -> Result<GbmVideoFrame, String> {
        let descriptor = surface
            .export_prime()
            .map_err(|err| format!("Could not export VA-API surface! {err:?}"))?;

        if descriptor.layers.len() != 1 {
            return Err("Cannot import more than 1 layers as a single GBM buffer".to_string());
        }

        for idx in descriptor.layers[0].object_index {
            if idx as usize >= descriptor.objects.len() {
                return Err("Object index {idx} out of bounds".to_string());
            }
        }

        let mut ret = GbmVideoFrame {
            _device: Some(Arc::clone(&self)),
            fourcc: Fourcc::from(descriptor.fourcc),
            resolution: Resolution { width: descriptor.width, height: descriptor.height },
            bo: vec![],
        };

        let buffers = [
            descriptor.objects[descriptor.layers[0].object_index[0] as usize].fd.as_raw_fd()
                as libc::c_int,
            -1,
            -1,
            -1,
        ];
        let mut import_data = gbm_import_fd_modifier_data {
            width: descriptor.width,
            height: descriptor.height,
            format: descriptor.fourcc,
            num_fds: 1,
            fds: buffers,
            strides: [
                descriptor.layers[0].pitch[0] as libc::c_int,
                descriptor.layers[0].pitch[1] as libc::c_int,
                descriptor.layers[0].pitch[2] as libc::c_int,
                descriptor.layers[0].pitch[3] as libc::c_int,
            ],
            offsets: [
                descriptor.layers[0].offset[0] as libc::c_int,
                descriptor.layers[0].offset[1] as libc::c_int,
                descriptor.layers[0].offset[2] as libc::c_int,
                descriptor.layers[0].offset[3] as libc::c_int,
            ],
            modifier: descriptor.objects[0].drm_format_modifier,
        };

        // TODO: Plumb real usage flags in here.
        let usage = gbm_bo_flags::GBM_BO_USE_SCANOUT as u32;

        // The constant in gbm_sys is wrong for some reason
        const GBM_BO_IMPORT_FD_MODIFIER: u32 = 0x5505;
        let bo = unsafe {
            gbm_bo_import(
                self.device,
                GBM_BO_IMPORT_FD_MODIFIER,
                &mut import_data as *mut gbm_import_fd_modifier_data as *mut libc::c_void,
                usage,
            )
        };

        if bo.is_null() {
            Err("Error importing VA-API surface!".to_string())
        } else {
            ret.bo.push(bo);
            Ok(ret)
        }
    }
}

impl Drop for GbmDevice {
    fn drop(&mut self) {
        unsafe { gbm_device_destroy(self.device) }
    }
}

// UNSAFE: We will only access the raw GBM device from the worker thread, and there are no other
// copies of the raw pointer available.
unsafe impl Send for GbmDevice {}
unsafe impl Sync for GbmDevice {}

// TODO: Add unit tests
