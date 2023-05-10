// Copyright 2023 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::fs::File;
use std::io::Read;
use std::io::Write;
/// Simple player example that can write decoded frames into a file.
///
/// Also useful for test suites like e.g. Fluster.
use std::path::PathBuf;
use std::str::FromStr;

use argh::FromArgs;
use cros_codecs::decoders::BlockingMode;
use cros_codecs::decoders::DecodedHandle;
use cros_codecs::decoders::VideoDecoder;
use cros_codecs::utils::simple_playback_loop;
use cros_codecs::utils::H264FrameIterator;
use cros_codecs::utils::IvfIterator;
use cros_codecs::DecodedFormat;

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
enum EncodedFormat {
    H264,
    VP8,
    VP9,
}

impl FromStr for EncodedFormat {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "h264" | "H264" => Ok(EncodedFormat::H264),
            "vp8" | "VP8" => Ok(EncodedFormat::VP8),
            "vp9" | "VP9" => Ok(EncodedFormat::VP9),
            _ => Err("unrecognized input format. Valid values: h264, vp8, vp9"),
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

    /// input format to decode from. Default: h264
    #[argh(option, default = "EncodedFormat::H264")]
    input_format: EncodedFormat,

    /// pixel format to decode into. Default: i420
    #[argh(option, default = "DecodedFormat::I420")]
    output_format: DecodedFormat,

    /// whether to decode frames synchronously
    #[argh(switch)]
    synchronous: bool,
}

fn main() {
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
        .map(|p| File::create(p).expect("error creating output file"));

    let blocking_mode = if args.synchronous {
        BlockingMode::Blocking
    } else {
        BlockingMode::NonBlocking
    };

    let display = libva::Display::open().expect("failed to open libva display");
    let (mut decoder, frame_iter) = match args.input_format {
        EncodedFormat::H264 => {
            let decoder = Box::new(
                cros_codecs::decoders::h264::decoder::Decoder::new_vaapi(display, blocking_mode)
                    .expect("failed to create decoder"),
            ) as Box<dyn VideoDecoder>;

            let frame_iter =
                Box::new(H264FrameIterator::new(&input)) as Box<dyn Iterator<Item = &[u8]>>;

            (decoder, frame_iter)
        }
        EncodedFormat::VP8 => {
            let decoder = Box::new(
                cros_codecs::decoders::vp8::decoder::Decoder::new_vaapi(display, blocking_mode)
                    .expect("failed to create decoder"),
            ) as Box<dyn VideoDecoder>;

            let frame_iter = Box::new(IvfIterator::new(&input)) as Box<dyn Iterator<Item = &[u8]>>;

            (decoder, frame_iter)
        }
        EncodedFormat::VP9 => {
            let decoder = Box::new(
                cros_codecs::decoders::vp9::decoder::Decoder::new_vaapi(display, blocking_mode)
                    .expect("failed to create decoder"),
            ) as Box<dyn VideoDecoder>;

            let frame_iter = Box::new(IvfIterator::new(&input)) as Box<dyn Iterator<Item = &[u8]>>;

            (decoder, frame_iter)
        }
    };

    let mut on_new_frame = move |handle: Box<dyn DecodedHandle>| {
        if let Some(output) = &mut output {
            let mut picture = handle.dyn_picture_mut();
            let mut handle = picture.dyn_mappable_handle_mut();
            let buffer_size = handle.image_size();
            let mut frame_data = vec![0; buffer_size];

            handle.read(&mut frame_data).unwrap();
            output
                .write_all(&frame_data)
                .expect("failed to write to output file");
        }
    };

    simple_playback_loop(
        decoder.as_mut(),
        frame_iter,
        &mut on_new_frame,
        args.output_format,
        blocking_mode,
    );
}
