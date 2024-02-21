// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::borrow::Borrow;
use std::fs::File;
use std::io::Read;
use std::io::Write;
use std::path::PathBuf;
use std::rc::Rc;

use argh::FromArgs;
use cros_codecs::backend::vaapi::surface_pool::VaSurfacePool;
use cros_codecs::codec::h264::parser::Profile;
use cros_codecs::decoder::FramePool;
use cros_codecs::encoder::stateless::h264::EncoderConfig;
use cros_codecs::encoder::stateless::h264::H264;
use cros_codecs::encoder::stateless::StatelessEncoder;
use cros_codecs::encoder::stateless::StatelessVideoEncoder;
use cros_codecs::encoder::Bitrate;
use cros_codecs::encoder::FrameMetadata;
use cros_codecs::BlockingMode;
use cros_codecs::Fourcc;
use cros_codecs::FrameLayout;
use cros_codecs::PlaneLayout;
use cros_codecs::Resolution;

/// Simple encoder
#[derive(Debug, FromArgs)]
struct Args {
    /// input file
    #[argh(positional)]
    input: PathBuf,

    /// input frames width
    #[argh(option)]
    width: u32,

    /// input frames height
    #[argh(option)]
    height: u32,

    /// input frames count
    #[argh(option)]
    count: usize,

    /// default quantization parameter
    #[argh(option)]
    default_qp: Option<u8>,

    /// framerate
    #[argh(option)]
    framerate: Option<u32>,

    /// output file to write the decoded frames to
    #[argh(option)]
    output: Option<PathBuf>,

    /// set to true if low power version of the API shall be used
    #[argh(switch)]
    low_power: bool,
}

fn upload_img<M: libva::SurfaceMemoryDescriptor>(
    display: &Rc<libva::Display>,
    surface: &libva::Surface<M>,
    width: u32,
    height: u32,
    data: &[u8],
) -> FrameLayout {
    let image_fmts = display.query_image_formats().unwrap();
    let image_fmt = image_fmts
        .into_iter()
        .find(|f| f.fourcc == libva::constants::VA_FOURCC_NV12)
        .unwrap();

    let mut image =
        libva::Image::create_from(surface, image_fmt, (width, height), (width, height)).unwrap();

    let va_image = *image.image();
    let dest = image.as_mut();
    let width = width as usize;
    let height = height as usize;
    let orig_height = height;

    let mut src: &[u8] = data;
    let mut dst = &mut dest[va_image.offsets[0] as usize..];

    // Copy luma
    for _ in 0..height {
        dst[..width].copy_from_slice(&src[..width]);
        dst = &mut dst[va_image.pitches[0] as usize..];
        src = &src[width..];
    }

    // Advance to the offset of the chroma plane
    let mut src = &data[width * height..];
    let mut dst = &mut dest[va_image.offsets[1] as usize..];

    let height = height / 2;

    // Copy chroma
    for _ in 0..height {
        dst[..width].copy_from_slice(&src[..width]);
        dst = &mut dst[va_image.pitches[1] as usize..];
        src = &src[width..];
    }
    drop(image);

    surface.sync().unwrap();

    FrameLayout {
        format: (Fourcc::from(b"NV12"), 0),
        size: Resolution::from((width as u32, orig_height as u32)),
        planes: vec![
            PlaneLayout {
                buffer_index: 0,
                offset: 0,
                stride: va_image.pitches[0] as usize,
            },
            PlaneLayout {
                buffer_index: 0,
                offset: va_image.offsets[0] as usize,
                stride: va_image.pitches[1] as usize,
            },
        ],
    }
}

fn main() {
    env_logger::init();

    let args: Args = argh::from_env();

    let mut input = File::open(args.input).expect("error opening input file");

    let resolution = Resolution {
        width: args.width,
        height: args.height,
    };

    let mut config = EncoderConfig {
        bitrate: Bitrate::Constant(2_000_000_000),
        profile: Profile::Baseline,
        framerate: 30,
        resolution,

        ..Default::default()
    };

    if let Some(default_qp) = args.default_qp {
        config.default_qp = default_qp;
    }

    if let Some(framerate) = args.framerate {
        config.framerate = framerate;
    }

    let display = libva::Display::open().unwrap();
    let fourcc = b"NV12".into();
    let mut encoder = StatelessEncoder::<H264, _, _>::new_vaapi(
        Rc::clone(&display),
        config,
        fourcc,
        resolution,
        args.low_power,
        BlockingMode::Blocking,
    )
    .expect("Unable to crate encoder");

    let mut pool = VaSurfacePool::new(
        Rc::clone(&display),
        libva::constants::VA_RT_FORMAT_YUV420,
        Some(libva::UsageHint::USAGE_HINT_ENCODER),
        Resolution {
            width: args.width,
            height: args.height,
        },
    );

    pool.add_frames(vec![(); 16]).unwrap();

    let frame_size: usize = (args.width * args.height + args.width * args.height / 2) as usize;

    let mut output = args.output.map(|output| File::create(output).unwrap());

    let mut buf = vec![0u8; frame_size];
    for i in 0..args.count {
        input.read_exact(&mut buf[..]).unwrap();
        let handle = pool.get_surface().unwrap();
        let layout = upload_img(&display, handle.borrow(), args.width, args.height, &buf[..]);

        let input_frame = FrameMetadata {
            display_resolution: Resolution {
                width: args.width,
                height: args.height,
            },
            layout,
            timestamp: i as u64,
            force_keyframe: false,
        };

        encoder.encode(input_frame, handle).unwrap();
        while let Some(coded) = encoder.poll().unwrap() {
            if let Some(ref mut output) = output {
                output.write_all(&coded.bitstream).unwrap();
            }
        }
    }

    encoder.drain().unwrap();
    while let Some(coded) = encoder.poll().unwrap() {
        if let Some(ref mut output) = output {
            output.write_all(&coded.bitstream).unwrap();
        }
    }
}
