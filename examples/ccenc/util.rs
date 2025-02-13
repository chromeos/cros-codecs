// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::path::PathBuf;
use std::str::FromStr;
use std::sync::Arc;
use std::{thread, time};

use argh::FromArgs;

use cros_codecs::image_processing::extend_border_nv12;
use cros_codecs::image_processing::i420_to_nv12_chroma;
use cros_codecs::image_processing::nv12_copy;
use cros_codecs::video_frame::gbm_video_frame::{GbmDevice, GbmUsage};
use cros_codecs::video_frame::generic_dma_video_frame::GenericDmaVideoFrame;
use cros_codecs::video_frame::VideoFrame;
use cros_codecs::video_frame::UV_PLANE;
use cros_codecs::video_frame::Y_PLANE;
use cros_codecs::DecodedFormat;
use cros_codecs::Fourcc;
use cros_codecs::Resolution;

#[derive(Debug, PartialEq, Eq, Copy, Clone, Default)]
pub enum Codec {
    #[default]
    H264,
    H265,
    VP8,
    VP9,
    AV1,
}

impl FromStr for Codec {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "h264" | "H264" => Ok(Self::H264),
            "h265" | "H265" => Ok(Self::H265),
            "vp8" | "VP8" => Ok(Self::VP8),
            "vp9" | "VP9" => Ok(Self::VP9),
            "av1" | "AV1" => Ok(Self::AV1),
            _ => Err("unrecognized codec. Valid values: h264, h265, vp8, vp9, av1"),
        }
    }
}

/// Simple encoder
#[derive(Debug, FromArgs)]
pub struct Args {
    /// input file
    #[argh(positional)]
    pub input: PathBuf,

    /// input frames width
    #[argh(option)]
    pub width: u32,

    /// input frames height
    #[argh(option)]
    pub height: u32,

    /// input frame coded width
    #[argh(option)]
    pub coded_width: Option<u32>,

    /// input frame coded height
    #[argh(option)]
    pub coded_height: Option<u32>,

    /// input frames count
    #[argh(option)]
    pub count: usize,

    /// input fourcc
    #[argh(option)]
    pub fourcc: DecodedFormat,

    /// codec
    #[argh(option)]
    pub codec: Option<Codec>,

    /// framerate
    #[argh(option, default = "30")]
    pub framerate: u32,

    /// bitrate
    #[argh(option, default = "200000")]
    pub bitrate: u64,

    /// output file to write the decoded frames to
    #[argh(option)]
    pub output: Option<PathBuf>,

    /// set to true if low power version of the API shall be used
    #[argh(switch)]
    pub low_power: bool,
}

pub fn upload_img(
    gbm_device: &Arc<GbmDevice>,
    resolution: Resolution,
    input_coded_resolution: Resolution,
    gbm_coded_resolution: Resolution,
    data: &[u8],
    input_fourcc: DecodedFormat,
    frame_fourcc: Fourcc,
) -> GenericDmaVideoFrame {
    assert!(
        input_coded_resolution.width % 2 == 0,
        "Input coded resolution must have even dimensions!"
    );
    assert!(
        input_coded_resolution.height % 2 == 0,
        "Input coded resolution must have even dimensions!"
    );
    assert!(gbm_coded_resolution.width % 2 == 0, "GBM coded resolution must have even dimensions!");
    assert!(
        gbm_coded_resolution.height % 2 == 0,
        "GBM coded resolution must have even dimensions!"
    );
    let input_y = &data[0..input_coded_resolution.get_area()];
    let mut tmp_input_uv: Vec<u8> = Vec::new();
    let input_uv = match input_fourcc {
        DecodedFormat::NV12 => {
            &data[input_coded_resolution.get_area()..(input_coded_resolution.get_area() * 3 / 2)]
        }
        DecodedFormat::I420 => {
            tmp_input_uv.resize(input_coded_resolution.get_area() / 2, 0);
            let input_u = &data
                [input_coded_resolution.get_area()..(input_coded_resolution.get_area() * 5 / 4)];
            let input_v = &data[(input_coded_resolution.get_area() * 5 / 4)
                ..(input_coded_resolution.get_area() * 3 / 2)];
            i420_to_nv12_chroma(input_u, input_v, tmp_input_uv.as_mut_slice());
            tmp_input_uv.as_slice()
        }
        _ => panic!("Unsupported input format!"),
    };

    let mut ret = gbm_device
        .clone()
        .new_frame(frame_fourcc, resolution.clone(), gbm_coded_resolution.clone(), GbmUsage::Encode)
        .expect("Could not allocate input frame!")
        .to_generic_dma_video_frame()
        .expect("Could not export GBM frame to DMA frame!");
    {
        let dst_pitches = ret.get_plane_pitch();
        let dst_sizes = ret.get_plane_size();
        let dst_mapping = ret.map_mut().expect("Failed to map input frame!");
        let dst_planes = dst_mapping.get();
        nv12_copy(
            input_y,
            input_coded_resolution.width as usize,
            *dst_planes[Y_PLANE].borrow_mut(),
            dst_pitches[Y_PLANE],
            input_uv,
            input_coded_resolution.width as usize,
            *dst_planes[UV_PLANE].borrow_mut(),
            dst_pitches[UV_PLANE],
            resolution.width as usize,
            resolution.height as usize,
        );
        extend_border_nv12(
            *dst_planes[Y_PLANE].borrow_mut(),
            *dst_planes[UV_PLANE].borrow_mut(),
            resolution.width as usize,
            resolution.height as usize,
            gbm_coded_resolution.width as usize,
            dst_sizes[Y_PLANE] / (gbm_coded_resolution.width as usize),
        );
    }

    // HACK: If frame creation greatly out-paces the encoder, we can exhaust available file
    // descriptors. This sleep intentionally throttles the frame creation so that our tests don't
    // flake for this reason. We intend on replacing all of this infrastructure with a proper
    // C2Wrapper based system in the near future, so it probably isn't worth spending too much time
    // figuring out how to fix this.
    thread::sleep(time::Duration::from_millis(10));

    ret
}
