// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::borrow::Cow;
use std::fs::File;
use std::io::Read;
use std::io::Write;
use std::path::PathBuf;
use std::str::FromStr;

use argh::FromArgs;
use cros_codecs::backend::v4l2::decoder::stateless::V4l2StatelessDecoderHandle;
use cros_codecs::codec::h264::parser::Nalu as H264Nalu;
use cros_codecs::decoder::stateless::h264::H264;
use cros_codecs::decoder::stateless::StatelessDecoder;
use cros_codecs::decoder::stateless::StatelessVideoDecoder;
use cros_codecs::decoder::BlockingMode;
use cros_codecs::decoder::DecodedHandle;
use cros_codecs::multiple_desc_type;
use cros_codecs::utils::simple_playback_loop;
use cros_codecs::utils::simple_playback_loop_owned_frames;
use cros_codecs::utils::DmabufFrame;
use cros_codecs::utils::NalIterator;
use cros_codecs::utils::UserPtrFrame;
use cros_codecs::DecodedFormat;

multiple_desc_type! {
    enum BufferDescriptor {
        Managed(()),
        Dmabuf(DmabufFrame),
        User(UserPtrFrame),
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
enum EncodedFormat {
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

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
enum FrameMemoryType {
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
struct Args {
    /// input file
    #[argh(positional)]
    input: PathBuf,

    /// output file to write the decoded frames to
    #[argh(option)]
    output: Option<PathBuf>,

    /// input format to decode from.
    #[argh(option)]
    input_format: EncodedFormat,

    //TODO    /// pixel format to decode into. Default: i420
    //TODO    #[argh(option, default = "DecodedFormat::I420")]
    //TODO    output_format: DecodedFormat,
    /// origin of the memory for decoded buffers (managed, prime or user). Default: managed.
    #[argh(option, default = "FrameMemoryType::Managed")]
    frame_memory: FrameMemoryType,

    //TODO    /// path to the GBM device to use if frame-memory=prime
    //TODO    #[argh(option)]
    //TODO    gbm_device: Option<PathBuf>,
    /// whether to decode frames synchronously
    #[argh(switch)]
    synchronous: bool,
    //TODO    /// whether to display the MD5 of the decoded stream, and at which granularity (stream or
    //TODO    /// frame)
    //TODO    #[argh(option)]
    //TODO    compute_md5: Option<Md5Computation>,
}

fn main() {
    env_logger::init();

    let args: Args = argh::from_env();

    let input = {
        let mut buf = Vec::new();
        File::open(args.input)
            .expect("error opening input file")
            .read_to_end(&mut buf)
            .expect("error reading input file");
        buf
    };

    let mut output = args
        .output
        .as_ref()
        .map(|p| File::create(p).expect("Failed to create output file"));

    let blocking_mode = if args.synchronous {
        todo!() // BlockingMode::Blocking
    } else {
        BlockingMode::NonBlocking
    };

    let (mut decoder, frame_iter) = match args.input_format {
        EncodedFormat::H264 => {
            let frame_iter = Box::new(NalIterator::<H264Nalu>::new(&input))
                as Box<dyn Iterator<Item = Cow<[u8]>>>;

            let decoder = Box::new(StatelessDecoder::<H264, _>::new_v4l2(blocking_mode))
                as Box<dyn StatelessVideoDecoder<_>>;

            (decoder, frame_iter)
        }
        EncodedFormat::VP8 => todo!(),
        EncodedFormat::VP9 => todo!(),
        EncodedFormat::H265 => todo!(),
        EncodedFormat::AV1 => todo!(),
    };

    let mut on_new_frame = |handle: V4l2StatelessDecoderHandle| {
        let picture = handle.dyn_picture();
        let mut handle = picture.dyn_mappable_handle().unwrap();
        let buffer_size = handle.image_size();
        let mut frame_data = vec![0; buffer_size];
        handle.read(&mut frame_data).unwrap();
        if let Some(output) = &mut output {
            output
                .write_all(&frame_data)
                .expect("Failed to write output file");
        }
    };

    simple_playback_loop(
        decoder.as_mut(),
        frame_iter,
        &mut on_new_frame,
        &mut |stream_info, nb_frames| {
            Ok(match args.frame_memory {
                FrameMemoryType::Managed => {
                    simple_playback_loop_owned_frames(stream_info, nb_frames)?
                        .into_iter()
                        .collect()
                }
                FrameMemoryType::Prime => todo!(),
                FrameMemoryType::User => todo!(),
            })
        },
        DecodedFormat::NV12,
        blocking_mode,
    )
    .expect("error during playback loop");
}
