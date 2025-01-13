// Copyright 2025 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::os::fd::AsFd;
use std::os::fd::BorrowedFd;
use std::path::Path;
use std::path::PathBuf;
use std::rc::Rc;

use crate::c2_wrapper::c2_decoder::C2DecoderBackend;
use crate::decoder::stateless::av1::Av1;
use crate::decoder::stateless::h264::H264;
use crate::decoder::stateless::h265::H265;
use crate::decoder::stateless::vp8::Vp8;
use crate::decoder::stateless::vp9::Vp9;
use crate::decoder::stateless::DynStatelessVideoDecoder;
use crate::decoder::stateless::StatelessDecoder;
use crate::decoder::stateless::StatelessVideoDecoder;
use crate::decoder::BlockingMode;
use crate::decoder::DynDecodedHandle;
use crate::decoder::FramePool;
use crate::decoder::StreamInfo;
use crate::multiple_desc_type;
use crate::utils::DmabufFrame;
use crate::utils::UserPtrFrame;
use crate::DecodedFormat;
use crate::EncodedFormat;
use crate::Fourcc;
use crate::FrameLayout;
use crate::FrameMemoryType;
use crate::PlaneLayout;
use crate::Resolution;

/// A simple wrapper for a GBM device node.
pub struct GbmDevice(std::fs::File);

impl AsFd for GbmDevice {
    fn as_fd(&self) -> BorrowedFd<'_> {
        self.0.as_fd()
    }
}
impl drm::Device for GbmDevice {}

/// Simple helper methods for opening a `Card`.
impl GbmDevice {
    pub fn open<P: AsRef<Path>>(path: P) -> std::io::Result<Self> {
        std::fs::OpenOptions::new()
            .read(true)
            .write(true)
            .open(path)
            .map(GbmDevice)
    }
}

// Our buffer descriptor type.
//
// We support buffers which memory is managed by the backend, or imported from user memory or a
// PRIME buffer.
multiple_desc_type! {
    enum BufferDescriptor {
        Managed(()),
        Dmabuf(DmabufFrame),
        User(UserPtrFrame),
    }
}

// Export a file descriptor from a GBM `BufferObject` and turn it into a `DmabufFrame` suitable
// for using as the target of a decoder.
// TODO: Reconsider the error handling here.
fn export_gbm_bo<T>(obj: &gbm::BufferObject<T>) -> DmabufFrame {
    let fd = obj.fd().unwrap();
    let modifier = obj.modifier().unwrap();
    let format = obj.format().unwrap();
    let planes = (0..obj.plane_count().unwrap() as i32)
        .map(|i| PlaneLayout {
            buffer_index: 0,
            offset: obj.offset(i).unwrap() as usize,
            stride: obj.stride_for_plane(i).unwrap() as usize,
        })
        .collect();
    let size = Resolution::from((obj.width().unwrap(), obj.height().unwrap()));

    DmabufFrame {
        fds: vec![fd],
        layout: FrameLayout {
            format: (Fourcc::from(format as u32), modifier.into()),
            size,
            planes,
        },
    }
}

#[derive(Clone, Debug)]
pub struct C2VaapiDecoderOptions {
    pub libva_device_path: Option<PathBuf>,
    pub gbm_device_path: Option<PathBuf>,
    pub frame_memory_type: FrameMemoryType,
}

pub struct C2VaapiDecoder {
    display: Rc<libva::Display>,
    frame_memory_type: FrameMemoryType,
    gbm: Option<gbm::Device<GbmDevice>>,
}

impl
    C2DecoderBackend<
        dyn StatelessVideoDecoder<
            Handle = DynDecodedHandle<BufferDescriptor>,
            FramePool = dyn FramePool<Descriptor = BufferDescriptor>,
        >,
        DynDecodedHandle<BufferDescriptor>,
        dyn FramePool<Descriptor = BufferDescriptor>,
        C2VaapiDecoderOptions,
    > for C2VaapiDecoder
{
    fn new(options: C2VaapiDecoderOptions) -> Result<Self, String> {
        let gbm = match options.frame_memory_type {
            FrameMemoryType::Prime => {
                let gbm_path = options
                    .gbm_device_path
                    .unwrap_or(PathBuf::from("/dev/dri/renderD128"));
                let gbm = GbmDevice::open(gbm_path)
                    .and_then(gbm::Device::new)
                    .map_err(|_| "failed to create GBM device")?;

                Some(gbm)
            }
            _ => None,
        };

        let display = match options.libva_device_path {
            Some(libva_device_path) => libva::Display::open_drm_display(libva_device_path.clone())
                .map_err(|_| format!("failed to open libva display {libva_device_path:?}"))?,
            None => libva::Display::open().ok_or("failed to open libva display")?,
        };

        Ok(Self {
            display: display,
            frame_memory_type: options.frame_memory_type,
            gbm: gbm,
        })
    }

    fn get_decoder(
        &mut self,
        format: EncodedFormat,
    ) -> Result<DynStatelessVideoDecoder<BufferDescriptor>, String> {
        Ok(match format {
            EncodedFormat::H264 => StatelessDecoder::<H264, _>::new_vaapi(
                self.display.clone(),
                BlockingMode::NonBlocking,
            )
            .map_err(|_| "Failed to instantiate H264 encoder")?
            .into_trait_object(),
            EncodedFormat::H265 => StatelessDecoder::<H265, _>::new_vaapi(
                self.display.clone(),
                BlockingMode::NonBlocking,
            )
            .map_err(|_| "Failed to instantiate H265 encoder")?
            .into_trait_object(),
            EncodedFormat::VP8 => StatelessDecoder::<Vp8, _>::new_vaapi(
                self.display.clone(),
                BlockingMode::NonBlocking,
            )
            .map_err(|_| "Failed to instantiate VP8 encoder")?
            .into_trait_object(),
            EncodedFormat::VP9 => StatelessDecoder::<Vp9, _>::new_vaapi(
                self.display.clone(),
                BlockingMode::NonBlocking,
            )
            .map_err(|_| "Failed to instantiate VP9 encoder")?
            .into_trait_object(),
            EncodedFormat::AV1 => StatelessDecoder::<Av1, _>::new_vaapi(
                self.display.clone(),
                BlockingMode::NonBlocking,
            )
            .map_err(|_| "Failed to instantiate AV1 encoder")?
            .into_trait_object(),
        })
    }

    fn allocate_new_frames(
        &mut self,
        stream_info: &StreamInfo,
        num_frames: usize,
    ) -> Result<Vec<BufferDescriptor>, String> {
        // TODO: Support more formats
        if stream_info.format != DecodedFormat::I420 {
            return Err("Invalid stream decoded format".into());
        }

        Ok(match self.frame_memory_type {
            FrameMemoryType::Managed => (0..num_frames)
                .map(|_| BufferDescriptor::Managed(()))
                .collect(),
            FrameMemoryType::Prime => {
                let mut ret: Vec<BufferDescriptor> = Vec::new();
                for _i in 0..num_frames {
                    ret.push(BufferDescriptor::Dmabuf(export_gbm_bo(
                        &self
                            .gbm
                            .as_ref()
                            .unwrap()
                            .create_buffer_object::<()>(
                                stream_info.coded_resolution.width,
                                stream_info.coded_resolution.height,
                                gbm::Format::Nv12,
                                gbm::BufferObjectFlags::SCANOUT,
                            )
                            .map_err(|_| "Error creating GBM buffer")?,
                    )));
                }

                ret
            }
            FrameMemoryType::User => {
                let mut ret: Vec<BufferDescriptor> = Vec::new();
                for _i in 0..num_frames {
                    ret.push(BufferDescriptor::User(UserPtrFrame::new_nv12(
                        stream_info.coded_resolution,
                    )));
                }

                ret
            }
        })
    }
}
