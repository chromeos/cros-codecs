// Copyright 2025 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

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
use crate::decoder::StreamInfo;
use crate::multiple_desc_type;
use crate::utils::DmabufFrame;
use crate::utils::UserPtrFrame;
use crate::video_frame::gbm_video_frame::GbmVideoFrame;
use crate::DecodedFormat;
use crate::EncodedFormat;
use crate::FrameMemoryType;
use crate::PlaneLayout;
use crate::Resolution;

#[derive(Clone, Debug)]
pub struct C2VaapiDecoderOptions {
    pub libva_device_path: Option<PathBuf>,
    pub frame_memory_type: FrameMemoryType,
}

pub struct C2VaapiDecoder {
    display: Rc<libva::Display>,
    frame_memory_type: FrameMemoryType,
}

impl C2DecoderBackend for C2VaapiDecoder {
    type DecodedHandle = GbmVideoFrame;
    type DecoderOptions = C2VaapiDecoderOptions;

    fn new(options: C2VaapiDecoderOptions) -> Result<Self, String> {
        let display = match options.libva_device_path {
            Some(libva_device_path) => libva::Display::open_drm_display(libva_device_path.clone())
                .map_err(|_| format!("failed to open libva display {libva_device_path:?}"))?,
            None => libva::Display::open().ok_or("failed to open libva display")?,
        };

        Ok(Self { display: display, frame_memory_type: options.frame_memory_type })
    }

    fn get_decoder(
        &mut self,
        format: EncodedFormat,
    ) -> Result<DynStatelessVideoDecoder<GbmVideoFrame>, String> {
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
}
