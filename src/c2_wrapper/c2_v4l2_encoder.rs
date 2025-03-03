// Copyright 2025 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::sync::Arc;

use crate::backend::v4l2::encoder::find_device_with_capture;
use crate::backend::v4l2::encoder::MmapingCapture;
use crate::c2_wrapper::c2_encoder::C2EncoderBackend;
use crate::encoder::stateful::h264::v4l2::V4L2StatefulH264Encoder;
use crate::encoder::stateful::vp8::v4l2::V4L2StatefulVP8Encoder;
use crate::encoder::stateful::vp9::v4l2::V4L2StatefulVP9Encoder;
use crate::encoder::VideoEncoder;
use crate::video_frame::V4l2VideoFrame;
use crate::video_frame::VideoFrame;
use crate::DecodedFormat;
use crate::EncodedFormat;
use crate::Fourcc;
use crate::Resolution;

use v4l2r::device::Device;
use v4l2r::device::DeviceConfig;

fn v4l2_format_to_coded_size(format: &v4l2r::Format) -> Resolution {
    let stride = format.plane_fmt[0].bytesperline;
    let size = format.plane_fmt[0].sizeimage;
    Resolution { width: stride, height: size / stride }
}

#[derive(Clone, Debug)]
pub struct C2V4L2EncoderOptions {
    pub output_fourcc: Fourcc,
    pub visible_resolution: Resolution,
}

pub struct C2V4L2Encoder {
    visible_resolution: Resolution,
    device: Arc<Device>,
}

impl C2EncoderBackend for C2V4L2Encoder {
    type EncoderOptions = C2V4L2EncoderOptions;

    fn new(options: C2V4L2EncoderOptions) -> Result<Self, String> {
        let pixel_format = v4l2r::PixelFormat::from_u32(u32::from(options.output_fourcc));
        let device = find_device_with_capture(pixel_format)
            .ok_or("Could not find V4L2 device!".to_string())?;
        let device = Device::open(&device, DeviceConfig::new().non_blocking_dqbuf())
            .map_err(|err| format!("Error opening V4L2 device! {:?}", err))?;
        Ok(Self { visible_resolution: options.visible_resolution, device: Arc::new(device) })
    }

    fn get_encoder<V: VideoFrame>(
        &mut self,
        input_format: DecodedFormat,
        output_format: EncodedFormat,
    ) -> Result<(Box<dyn VideoEncoder<V4l2VideoFrame<V>>>, Resolution, Resolution), String> {
        Ok(match output_format {
            EncodedFormat::H264 => {
                let mut encoder = V4L2StatefulH264Encoder::new(
                    self.device.clone(),
                    MmapingCapture,
                    crate::encoder::h264::EncoderConfig {
                        resolution: self.visible_resolution.clone(),
                        ..Default::default()
                    },
                    Fourcc::from(input_format),
                    self.visible_resolution.clone(),
                    Default::default(),
                )
                .map_err(|err| format!("Error initializing encoder! {:?}", err))?;
                let coded_format = encoder
                    .backend()
                    .output_format()
                    .map_err(|err| format!("Error querying backend format! {:?}", err))?;
                (
                    Box::new(encoder),
                    self.visible_resolution.clone(),
                    v4l2_format_to_coded_size(&coded_format),
                )
            }
            EncodedFormat::VP8 => {
                let mut encoder = V4L2StatefulVP8Encoder::new(
                    self.device.clone(),
                    MmapingCapture,
                    crate::encoder::vp8::EncoderConfig {
                        resolution: self.visible_resolution.clone(),
                        ..Default::default()
                    },
                    Fourcc::from(input_format),
                    self.visible_resolution.clone(),
                    Default::default(),
                )
                .map_err(|err| format!("Error initializing encoder! {:?}", err))?;
                let coded_format = encoder
                    .backend()
                    .output_format()
                    .map_err(|err| format!("Error querying backend format! {:?}", err))?;
                (
                    Box::new(encoder),
                    self.visible_resolution.clone(),
                    v4l2_format_to_coded_size(&coded_format),
                )
            }
            EncodedFormat::VP9 => {
                let mut encoder = V4L2StatefulVP9Encoder::new(
                    self.device.clone(),
                    MmapingCapture,
                    crate::encoder::vp9::EncoderConfig {
                        resolution: self.visible_resolution.clone(),
                        ..Default::default()
                    },
                    Fourcc::from(input_format),
                    self.visible_resolution.clone(),
                    Default::default(),
                )
                .map_err(|err| format!("Error initializing encoder! {:?}", err))?;
                let coded_format = encoder
                    .backend()
                    .output_format()
                    .map_err(|err| format!("Error querying backend format! {:?}", err))?;
                (
                    Box::new(encoder),
                    self.visible_resolution.clone(),
                    v4l2_format_to_coded_size(&coded_format),
                )
            }
            _ => return Err(format!("Format not supported by V4L2! {:?}", output_format)),
        })
    }
}
