// Copyright 2025 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::rc::Rc;

use crate::c2_wrapper::c2_encoder::C2EncoderBackend;
use crate::encoder::av1::EncoderConfig as AV1EncoderConfig;
use crate::encoder::h264::EncoderConfig as H264EncoderConfig;
use crate::encoder::stateless::av1;
use crate::encoder::stateless::h264;
use crate::encoder::stateless::vp9;
use crate::encoder::vp9::EncoderConfig as VP9EncoderConfig;
use crate::encoder::VideoEncoder;
use crate::utils::align_up;
use crate::video_frame::VideoFrame;
use crate::BlockingMode;
use crate::DecodedFormat;
use crate::EncodedFormat;
use crate::Fourcc;
use crate::Resolution;

#[derive(Clone, Debug)]
pub struct C2VaapiEncoderOptions {
    pub low_power: bool,
    pub visible_resolution: Resolution,
}

pub struct C2VaapiEncoder {
    display: Rc<libva::Display>,
    low_power: bool,
    visible_resolution: Resolution,
    coded_resolution: Resolution,
}

impl C2EncoderBackend for C2VaapiEncoder {
    type EncoderOptions = C2VaapiEncoderOptions;

    fn new(options: C2VaapiEncoderOptions) -> Result<Self, String> {
        const VAAPI_WIDTH_ALIGN: u32 = 16;
        const VAAPI_HEIGHT_ALIGN: u32 = 16;

        // TODO: Support alternative display paths
        let display = libva::Display::open().ok_or("Error opening LibVA display!".to_string())?;
        Ok(Self {
            display: display,
            low_power: options.low_power,
            visible_resolution: options.visible_resolution.clone(),
            // TODO: This really shouldn't be necessary, but for some reason minigbm gets the
            // vertical alignment wrong when we omit it :(
            coded_resolution: Resolution {
                width: align_up(options.visible_resolution.width, VAAPI_WIDTH_ALIGN),
                height: align_up(options.visible_resolution.height, VAAPI_HEIGHT_ALIGN),
            },
        })
    }

    fn get_encoder<V: VideoFrame>(
        &mut self,
        input_format: DecodedFormat,
        output_format: EncodedFormat,
    ) -> Result<(Box<dyn VideoEncoder<V>>, Resolution, Resolution), String> {
        Ok(match output_format {
            EncodedFormat::H264 => {
                let encoder = h264::StatelessEncoder::new_vaapi(
                    self.display.clone(),
                    H264EncoderConfig {
                        resolution: self.visible_resolution.clone(),
                        ..Default::default()
                    },
                    Fourcc::from(input_format),
                    self.visible_resolution.clone(),
                    self.low_power,
                    BlockingMode::Blocking,
                )
                .map_err(|err| format!("Error initializing encoder! {:?}", err))?;
                (Box::new(encoder), self.visible_resolution.clone(), self.coded_resolution.clone())
            }
            EncodedFormat::VP9 => {
                let encoder = vp9::StatelessEncoder::new_vaapi(
                    self.display.clone(),
                    VP9EncoderConfig {
                        resolution: self.visible_resolution.clone(),
                        ..Default::default()
                    },
                    Fourcc::from(input_format),
                    self.visible_resolution.clone(),
                    self.low_power,
                    BlockingMode::Blocking,
                )
                .map_err(|err| format!("Error initializing encoder! {:?}", err))?;
                (Box::new(encoder), self.visible_resolution.clone(), self.coded_resolution.clone())
            }
            EncodedFormat::AV1 => {
                let encoder = av1::StatelessEncoder::new_vaapi(
                    self.display.clone(),
                    AV1EncoderConfig {
                        resolution: self.visible_resolution.clone(),
                        ..Default::default()
                    },
                    Fourcc::from(input_format),
                    self.visible_resolution.clone(),
                    self.low_power,
                    BlockingMode::Blocking,
                )
                .map_err(|err| format!("Error initializing encoder! {:?}", err))?;
                (Box::new(encoder), self.visible_resolution.clone(), self.coded_resolution.clone())
            }
            _ => return Err(format!("Format not supported by V4L2! {:?}", output_format)),
        })
    }
}
