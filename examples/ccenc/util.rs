// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::path::PathBuf;
use std::str::FromStr;

use argh::FromArgs;

use cros_codecs::DecodedFormat;

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
