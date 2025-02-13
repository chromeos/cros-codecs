// Copyright 2025 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::cell::RefCell;
use std::fs::File;
use std::iter::zip;
use std::num::NonZeroUsize;
use std::os::fd::{AsFd, AsRawFd, BorrowedFd, FromRawFd};
use std::ptr::NonNull;
#[cfg(feature = "vaapi")]
use std::rc::Rc;
use std::slice;
#[cfg(feature = "v4l2")]
use std::sync::Arc;

use crate::video_frame::{ReadMapping, VideoFrame, WriteMapping};
#[cfg(feature = "vaapi")]
use crate::DecodedFormat;
use crate::{Fourcc, FrameLayout, Resolution};

use drm_fourcc::DrmModifier;
use nix::errno::Errno;
use nix::ioctl_write_ptr;
use nix::libc;
use nix::poll::poll;
use nix::poll::PollFd;
use nix::poll::PollFlags;
use nix::poll::PollTimeout;
use nix::sys::mman::mmap;
use nix::sys::mman::munmap;
use nix::sys::mman::MapFlags;
use nix::sys::mman::ProtFlags;
use nix::unistd::dup;

#[cfg(feature = "vaapi")]
use libva::{
    Display, ExternalBufferDescriptor, MemoryType, Surface, UsageHint, VADRMPRIMESurfaceDescriptor,
    VADRMPRIMESurfaceDescriptorLayer, VADRMPRIMESurfaceDescriptorObject,
};
#[cfg(feature = "v4l2")]
use v4l2r::bindings::v4l2_plane;
#[cfg(feature = "v4l2")]
use v4l2r::device::Device;
#[cfg(feature = "v4l2")]
use v4l2r::ioctl::V4l2Buffer;
#[cfg(feature = "v4l2")]
use v4l2r::memory::DmaBufHandle;
#[cfg(feature = "v4l2")]
use v4l2r::Format;

// UNSAFE: This file uses tons of unsafe code because we are directly interacting with the kernel's
// DMA infrastructure. The core assumption is that GenericDmaVideoFrame is initialized with a
// valid DRM Prime File Descriptor, and that the FrameLayout given accurately describes the memory
// layout of the frame. We leverage Rust's lifetime system and RAII design patterns to guarantee
// that mappings will not last longer than the underlying DMA buffer.

// Defined in include/linux/dma-buf.h
const DMA_BUF_BASE: u8 = b'b';
const DMA_BUF_IOCTL_SYNC: u8 = 0;
const DMA_BUF_SYNC_READ: u64 = 1 << 0;
const DMA_BUF_SYNC_WRITE: u64 = 2 << 0;
const DMA_BUF_SYNC_START: u64 = 0 << 2;
const DMA_BUF_SYNC_END: u64 = 1 << 2;
#[repr(C)]
struct dma_buf_sync {
    flags: u64,
}
ioctl_write_ptr!(dma_buf_ioctl_sync, DMA_BUF_BASE, DMA_BUF_IOCTL_SYNC, dma_buf_sync);

fn handle_eintr<T>(cb: &mut impl FnMut() -> nix::Result<T>) -> Result<T, String> {
    loop {
        match cb() {
            Ok(ret) => return Ok(ret),
            Err(errno) => {
                if errno != Errno::EINTR {
                    return Err(format!("Error executing DMA buf sync! {errno}"));
                }
            }
        }
    }
}

// Because we are limited to executing raw mmap instead of leveraging the GEM driver, all of our
// buffers will be mapped linear even if the backing frame has a modifier. So, we have to manually
// detile the buffers.
const Y_SUBTILE_WIDTH: usize = 16;
const Y_SUBTILE_HEIGHT: usize = 4;
const Y_SUBTILE_SIZE: usize = Y_SUBTILE_WIDTH * Y_SUBTILE_HEIGHT;
const Y_TILE_WIDTH_IN_SUBTILES: usize = 8;
const Y_TILE_HEIGHT_IN_SUBTILES: usize = 8;
const Y_TILE_WIDTH: usize = Y_TILE_WIDTH_IN_SUBTILES * Y_SUBTILE_WIDTH;
const Y_TILE_HEIGHT: usize = Y_TILE_HEIGHT_IN_SUBTILES * Y_SUBTILE_HEIGHT;
const Y_TILE_SIZE: usize = Y_TILE_WIDTH * Y_TILE_HEIGHT;

fn detile_y_tile(dst: &mut [u8], src: &[u8], width: usize, height: usize) {
    let tiles_per_row = width / Y_TILE_WIDTH;
    for y in 0..height {
        for x in 0..width {
            let tile_x = x / Y_TILE_WIDTH;
            let tile_y = y / Y_TILE_HEIGHT;
            let intra_tile_x = x % Y_TILE_WIDTH;
            let intra_tile_y = y % Y_TILE_HEIGHT;
            let subtile_x = intra_tile_x / Y_SUBTILE_WIDTH;
            let subtile_y = intra_tile_y / Y_SUBTILE_HEIGHT;
            let intra_subtile_x = intra_tile_x % Y_SUBTILE_WIDTH;
            let intra_subtile_y = intra_tile_y % Y_SUBTILE_HEIGHT;
            // TODO: We should batch up the writes since subtile rows are contiguous. Also consider
            // SIMD'ifying this function.
            dst[y * width + x] = src[(tile_y * tiles_per_row + tile_x) * Y_TILE_SIZE
                + (subtile_x * Y_TILE_HEIGHT_IN_SUBTILES + subtile_y) * Y_SUBTILE_SIZE
                + intra_subtile_y * Y_SUBTILE_WIDTH
                + intra_subtile_x]
        }
    }
}

pub struct DmaMapping<'a> {
    dma_handles: Vec<BorrowedFd<'a>>,
    addrs: Vec<NonNull<libc::c_void>>,
    detiled_bufs: Vec<Vec<u8>>,
    lens: Vec<usize>,
    is_writable: bool,
}

impl<'a> DmaMapping<'a> {
    fn new(
        dma_handles: &'a Vec<File>,
        offsets: Vec<usize>,
        pitches: Vec<usize>,
        lens: Vec<usize>,
        modifier: DrmModifier,
        is_writable: bool,
    ) -> Result<Self, String> {
        if is_writable && modifier != DrmModifier::Linear {
            return Err(
                "Writable mappings currently only supported for linear buffers!".to_string()
            );
        }
        if modifier != DrmModifier::Linear && modifier != DrmModifier::I915_y_tiled {
            return Err(
                "Only linear and Y tile buffers are currently supported for mapping!".to_string()
            );
        }

        let borrowed_dma_handles: Vec<BorrowedFd> = dma_handles.iter().map(|x| x.as_fd()).collect();

        // Wait on all memory fences to finish before attempting to map this DMA buffer.
        for fd in borrowed_dma_handles.iter() {
            let mut fence_poll_fd =
                [PollFd::new(fd.clone(), PollFlags::POLLIN | PollFlags::POLLOUT)];
            poll(&mut fence_poll_fd, PollTimeout::NONE).unwrap();
        }

        // Some architectures do not put DMA in the same coherency zone as CPU, so we need to
        // invalidate cache lines corresponding to this memory. The DMA infrastructure provides
        // this convenient IOCTL for doing so.
        let sync_struct =
            dma_buf_sync { flags: DMA_BUF_SYNC_START | DMA_BUF_SYNC_READ | DMA_BUF_SYNC_WRITE };

        for fd in borrowed_dma_handles.iter() {
            handle_eintr(&mut || unsafe { dma_buf_ioctl_sync(fd.as_raw_fd(), &sync_struct) })?;
        }

        // Offsets aren't guaranteed to page aligned, so we have to map the entire FD and then
        // do pointer arithmetic to get the right buffer.
        let mut addrs: Vec<NonNull<libc::c_void>> = vec![];
        if borrowed_dma_handles.len() > 1 {
            for i in 0..offsets.len() {
                addrs.push(unsafe {
                    mmap(
                        None,
                        NonZeroUsize::new(lens[i] + offsets[i])
                            .ok_or("Attempted to map plane of length 0!")?,
                        if is_writable {
                            ProtFlags::PROT_READ | ProtFlags::PROT_WRITE
                        } else {
                            ProtFlags::PROT_READ
                        },
                        MapFlags::MAP_SHARED,
                        borrowed_dma_handles[i].as_fd(),
                        0,
                    )
                    .map_err(|err| format!("Error mapping plane {err}"))?
                    .add(offsets[i])
                });
            }
        } else {
            let total_size = NonZeroUsize::new(lens.iter().sum::<usize>() + offsets[0])
                .ok_or("Attempted to map VideoFrame of length 0")?;
            unsafe {
                let base_addr = mmap(
                    None,
                    total_size,
                    if is_writable {
                        ProtFlags::PROT_READ | ProtFlags::PROT_WRITE
                    } else {
                        ProtFlags::PROT_READ
                    },
                    MapFlags::MAP_SHARED,
                    borrowed_dma_handles[0].as_fd(),
                    0,
                )
                .map_err(|err| format!("Error mapping plane {err}"))?;
                for i in 0..offsets.len() {
                    addrs.push(base_addr.add(offsets[i]));
                }
            }
        }

        let mut detiled_bufs = vec![];
        if modifier == DrmModifier::I915_y_tiled {
            let tiled_bufs: Vec<&[u8]> = unsafe {
                zip(addrs.iter(), lens.iter())
                    .map(|x| slice::from_raw_parts(x.0.as_ptr() as *const u8, *x.1))
                    .collect()
            };
            for i in 0..tiled_bufs.len() {
                let mut detiled_buf: Vec<u8> = vec![];
                detiled_buf.resize(tiled_bufs[i].len(), 0);
                detile_y_tile(
                    detiled_buf.as_mut_slice(),
                    tiled_bufs[i],
                    pitches[i],
                    lens[i] / pitches[i],
                );
                detiled_bufs.push(detiled_buf);
            }
        }

        Ok(DmaMapping {
            dma_handles: borrowed_dma_handles.clone(),
            addrs: addrs,
            detiled_bufs: detiled_bufs,
            lens: lens.clone(),
            is_writable: is_writable,
        })
    }
}

impl<'a> ReadMapping<'a> for DmaMapping<'a> {
    fn get(&self) -> Vec<&[u8]> {
        if self.detiled_bufs.len() > 0 {
            self.detiled_bufs.iter().map(|x| x.as_slice()).collect()
        } else {
            unsafe {
                zip(self.addrs.iter(), self.lens.iter())
                    .map(|x| slice::from_raw_parts(x.0.as_ptr() as *const u8, *x.1))
                    .collect()
            }
        }
    }
}

impl<'a> WriteMapping<'a> for DmaMapping<'a> {
    fn get(&self) -> Vec<RefCell<&'a mut [u8]>> {
        if !self.is_writable {
            panic!("Attempted to get writable slice to read only mapping!");
        }

        // The above check prevents us from undefined behavior in the event that the user attempts
        // to coerce a ReadMapping into a WriteMapping.
        unsafe {
            zip(self.addrs.iter(), self.lens.iter())
                .map(|x| RefCell::new(slice::from_raw_parts_mut(x.0.as_ptr() as *mut u8, *x.1)))
                .collect()
        }
    }
}

impl<'a> Drop for DmaMapping<'a> {
    fn drop(&mut self) {
        unsafe {
            let _ = zip(self.addrs.iter(), self.lens.iter()).map(|x| munmap(*x.0, *x.1).unwrap());

            // Flush all cache lines back to main memory.
            let sync_struct =
                dma_buf_sync { flags: DMA_BUF_SYNC_END | DMA_BUF_SYNC_READ | DMA_BUF_SYNC_WRITE };
            for fd in self.dma_handles.iter() {
                let _ = handle_eintr(&mut || dma_buf_ioctl_sync(fd.as_raw_fd(), &sync_struct));
            }
        }
    }
}

#[derive(Debug)]
pub struct GenericDmaVideoFrame {
    dma_handles: Vec<File>,
    layout: FrameLayout,
}

impl Clone for GenericDmaVideoFrame {
    fn clone(&self) -> Self {
        Self {
            dma_handles: self
                .dma_handles
                .iter()
                .map(|x| unsafe {
                    File::from_raw_fd(dup(x.as_raw_fd()).expect("Could not dup DMAbuf FD!"))
                })
                .collect(),
            layout: self.layout.clone(),
        }
    }
}

impl GenericDmaVideoFrame {
    pub fn new(
        dma_handles: Vec<File>,
        layout: FrameLayout,
    ) -> Result<GenericDmaVideoFrame, String> {
        let ret = GenericDmaVideoFrame { dma_handles: dma_handles, layout: layout };
        ret.validate_frame()?;
        Ok(ret)
    }

    fn get_single_plane_size(&self, index: usize) -> usize {
        if index >= self.num_planes() {
            panic!("Invalid plane index {index}!");
        }

        if self.dma_handles.len() == 1 {
            if index == self.num_planes() - 1 {
                let total_size = self.dma_handles[0].metadata().unwrap().len() as usize;
                total_size - self.layout.planes[index].offset
            } else {
                self.layout.planes[index + 1].offset - self.layout.planes[index].offset
            }
        } else {
            self.dma_handles[index].metadata().unwrap().len() as usize
        }
    }

    fn get_plane_offset(&self) -> Vec<usize> {
        self.layout.planes.iter().map(|x| x.offset).collect()
    }

    fn map_helper(&self, is_writable: bool) -> Result<DmaMapping, String> {
        let lens = self.get_plane_size();
        let pitches = self.get_plane_pitch();
        let offsets = self.get_plane_offset();
        DmaMapping::new(
            &self.dma_handles,
            offsets,
            pitches,
            lens,
            DrmModifier::from(self.layout.format.1),
            is_writable,
        )
    }
}

#[cfg(feature = "vaapi")]
impl ExternalBufferDescriptor for GenericDmaVideoFrame {
    const MEMORY_TYPE: MemoryType = MemoryType::DrmPrime2;
    type DescriptorAttribute = VADRMPRIMESurfaceDescriptor;

    fn va_surface_attribute(&mut self) -> Self::DescriptorAttribute {
        let objects = self
            .dma_handles
            .iter()
            .map(|fd| VADRMPRIMESurfaceDescriptorObject {
                fd: fd.as_raw_fd(),
                size: fd.metadata().unwrap().len() as u32,
                drm_format_modifier: self.layout.format.1,
            })
            .chain(std::iter::repeat(Default::default()))
            .take(4)
            .collect::<Vec<_>>();
        let layers = [
            VADRMPRIMESurfaceDescriptorLayer {
                drm_format: u32::from(self.layout.format.0),
                num_planes: self.num_planes() as u32,
                object_index: (0..self.dma_handles.len() as u32)
                    .chain(std::iter::repeat(0))
                    .take(4)
                    .collect::<Vec<_>>()
                    .try_into()
                    .unwrap(),
                offset: self
                    .get_plane_offset()
                    .iter()
                    .map(|x| *x as u32)
                    .chain(std::iter::repeat(0))
                    .take(4)
                    .collect::<Vec<_>>()
                    .try_into()
                    .unwrap(),
                pitch: self
                    .get_plane_pitch()
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
        VADRMPRIMESurfaceDescriptor {
            fourcc: u32::from(self.layout.format.0),
            width: self.layout.size.width,
            height: self.layout.size.height,
            num_objects: self.dma_handles.len() as u32,
            objects: objects.try_into().unwrap(),
            num_layers: 1,
            layers: layers,
        }
    }
}

impl VideoFrame for GenericDmaVideoFrame {
    #[cfg(feature = "v4l2")]
    type NativeHandle = DmaBufHandle<File>;

    #[cfg(feature = "vaapi")]
    type MemDescriptor = GenericDmaVideoFrame;
    #[cfg(feature = "vaapi")]
    type NativeHandle = Surface<GenericDmaVideoFrame>;

    fn fourcc(&self) -> Fourcc {
        self.layout.format.0.clone()
    }

    fn resolution(&self) -> Resolution {
        self.layout.size.clone()
    }

    fn get_plane_size(&self) -> Vec<usize> {
        (0..self.num_planes()).map(|idx| self.get_single_plane_size(idx)).collect()
    }

    fn get_plane_pitch(&self) -> Vec<usize> {
        self.layout.planes.iter().map(|x| x.stride).collect()
    }

    fn map<'a>(&'a self) -> Result<Box<dyn ReadMapping<'a> + 'a>, String> {
        Ok(Box::new(self.map_helper(false)?))
    }

    fn map_mut<'a>(&'a mut self) -> Result<Box<dyn WriteMapping<'a> + 'a>, String> {
        Ok(Box::new(self.map_helper(true)?))
    }

    #[cfg(feature = "v4l2")]
    fn fill_v4l2_plane(&self, index: usize, plane: &mut v4l2_plane) {
        if self.dma_handles.len() == 1 {
            plane.m.fd = self.dma_handles[0].as_raw_fd();
            plane.length = self.dma_handles[0].metadata().unwrap().len() as u32;
        } else {
            plane.m.fd = self.dma_handles[index].as_raw_fd();
            plane.length = self.get_single_plane_size(index) as u32;
        }
        // WARNING: Importing DMA buffers with an offset is not officially supported by V4L2, but
        // several drivers (including MTK venc) will respect the data_offset field.
        plane.data_offset = self.layout.planes[index].offset as u32;
    }

    // No-op for DMA buffers since the backing FD already disambiguates them.
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

        let mut ret = display
            .create_surfaces(
                rt_format,
                Some(u32::from(self.layout.format.0)),
                self.resolution().width,
                self.resolution().height,
                // TODO: Should we add USAGE_HINT_ENCODER support?
                Some(UsageHint::USAGE_HINT_DECODER),
                vec![self.clone()],
            )
            .map_err(|_| "Error importing GenericDmaVideoFrame to VA-API".to_string())?;

        Ok(ret.pop().unwrap())
    }
}
