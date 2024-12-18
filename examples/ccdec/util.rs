// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::ffi::OsStr;
use std::path::Path;
use std::path::PathBuf;
use std::str::FromStr;

use argh::FromArgs;

use cros_codecs::DecodedFormat;

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum EncodedFormat {
    H264,
    H265,
    VP8,
    VP9,
    AV1,
}

impl FromStr for EncodedFormat {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "h264" | "H264" => Ok(EncodedFormat::H264),
            "h265" | "H265" => Ok(EncodedFormat::H265),
            "vp8" | "VP8" => Ok(EncodedFormat::VP8),
            "vp9" | "VP9" => Ok(EncodedFormat::VP9),
            "av1" | "AV1" => Ok(EncodedFormat::AV1),
            _ => Err("unrecognized input format. Valid values: h264, h265, vp8, vp9, av1"),
        }
    }
}

#[derive(Debug)]
pub enum Md5Computation {
    Stream,
    Frame,
}

impl FromStr for Md5Computation {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "stream" => Ok(Md5Computation::Stream),
            "frame" => Ok(Md5Computation::Frame),
            _ => Err("unrecognized MD5 computation option. Valid values: stream, frame"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum FrameMemoryType {
    Managed,
    Prime,
    User,
}

impl FromStr for FrameMemoryType {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "managed" => Ok(FrameMemoryType::Managed),
            "prime" => Ok(FrameMemoryType::Prime),
            "user" => Ok(FrameMemoryType::User),
            _ => Err("unrecognized memory type. Valid values: managed, prime, user"),
        }
    }
}

/// Simple player using cros-codecs
#[derive(Debug, FromArgs)]
pub struct Args {
    /// input file
    #[argh(positional)]
    pub input: PathBuf,

    /// output file to write the decoded frames to
    #[argh(option)]
    pub output: Option<PathBuf>,

    /// whether to decode a frame per file. Requires "output" to be set.
    #[argh(switch)]
    pub multiple_output_files: bool,

    /// input format to decode from.
    #[argh(option)]
    pub input_format: EncodedFormat,

    /// pixel format to decode into. Default: i420
    #[argh(option, default = "DecodedFormat::I420")]
    pub output_format: DecodedFormat,

    /// origin of the memory for decoded buffers (managed, prime or user). Default: managed.
    #[argh(option, default = "FrameMemoryType::Managed")]
    pub frame_memory: FrameMemoryType,

    /// path to the GBM device to use if frame-memory=prime
    #[allow(dead_code)]
    #[argh(option)]
    pub gbm_device: Option<PathBuf>,

    /// path to VA-API device. This option is ignored on V4L2 systems.
    #[argh(option)]
    #[allow(dead_code)]
    pub libva_device: Option<PathBuf>,

    /// whether to decode frames synchronously
    #[argh(switch)]
    pub synchronous: bool,

    /// whether to display the MD5 of the decoded stream, and at which granularity (stream or
    /// frame)
    #[argh(option)]
    pub compute_md5: Option<Md5Computation>,

    /// path to JSON file containing golden MD5 sums of each frame.
    #[argh(option)]
    pub golden: Option<PathBuf>,
}

/// Decide the output file name when multiple_output_files is set
pub fn decide_output_file_name<'a>(output: &'a Path, index: i32) -> PathBuf {
    let extract_str = |s: Option<&'a OsStr>| s.and_then(|s| s.to_str()).expect("malformed file");

    let [file_name, stem] = [output.file_name(), output.file_stem()].map(extract_str);

    if output.extension().is_some() {
        let [extension] = [output.extension()].map(extract_str);
        let new_file_name = format!("{}_{}.{}", stem, index, extension);
        PathBuf::from(String::from(output.to_str().unwrap()).replace(file_name, &new_file_name))
    } else {
        let new_file_name = format!("{}_{}", stem, index);
        PathBuf::from(String::from(output.to_str().unwrap()).replace(file_name, &new_file_name))
    }
}
