// Copyright 2025 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::path::PathBuf;

use crate::c2_wrapper::c2_decoder::C2DecoderBackend;
use crate::decoder::stateless::h264::H264;
use crate::decoder::stateless::vp8::Vp8;
use crate::decoder::stateless::vp9::Vp9;
use crate::decoder::stateless::DynStatelessVideoDecoder;
use crate::decoder::stateless::StatelessDecoder;
use crate::decoder::stateless::StatelessVideoDecoder;
use crate::decoder::BlockingMode;
use crate::decoder::StreamInfo;
use crate::video_frame::VideoFrame;
use crate::EncodedFormat;
use crate::Fourcc;

#[derive(Clone, Debug)]
pub struct C2V4L2DecoderOptions {
    // TODO: This is currently unused, but we should plumb it to V4L2Device initialization.
    pub video_device_path: Option<PathBuf>,
}

pub struct C2V4L2Decoder {}

impl C2DecoderBackend for C2V4L2Decoder {
    type DecoderOptions = C2V4L2DecoderOptions;

    fn new(_options: C2V4L2DecoderOptions) -> Result<Self, String> {
        Ok(Self {})
    }

    // TODO: Actually query the driver for this information.
    fn supported_output_formats(&self) -> Vec<Fourcc> {
        vec![Fourcc::from(b"MM21")]
    }

    fn get_decoder<V: VideoFrame + 'static>(
        &mut self,
        format: EncodedFormat,
    ) -> Result<DynStatelessVideoDecoder<V>, String> {
        Ok(match format {
            EncodedFormat::H264 => {
                StatelessDecoder::<H264, _>::new_v4l2(BlockingMode::NonBlocking).into_trait_object()
            }
            EncodedFormat::VP8 => {
                StatelessDecoder::<Vp8, _>::new_v4l2(BlockingMode::NonBlocking).into_trait_object()
            }
            EncodedFormat::VP9 => {
                StatelessDecoder::<Vp9, _>::new_v4l2(BlockingMode::NonBlocking).into_trait_object()
            }
            _ => return Err(format!("Unsupported format {format:?}")),
        })
    }
}
