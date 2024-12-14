// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::borrow::Borrow;
use std::fs::File;
use std::io::Read;
use std::io::Write;
use std::rc::Rc;

use cros_codecs::backend::vaapi::surface_pool::PooledVaSurface;
use cros_codecs::backend::vaapi::surface_pool::VaSurfacePool;
use cros_codecs::bitstream_utils::IvfFileHeader;
use cros_codecs::bitstream_utils::IvfFrameHeader;
use cros_codecs::decoder::FramePool;
use cros_codecs::encoder::av1::EncoderConfig as AV1EncoderConfig;
use cros_codecs::encoder::h264::EncoderConfig as H264EncoderConfig;
use cros_codecs::encoder::stateless::av1;
use cros_codecs::encoder::stateless::h264;
use cros_codecs::encoder::stateless::vp9;
use cros_codecs::encoder::vp9::EncoderConfig as VP9EncoderConfig;
use cros_codecs::encoder::FrameMetadata;
use cros_codecs::encoder::RateControl;
use cros_codecs::encoder::Tunings;
use cros_codecs::encoder::VideoEncoder;
use cros_codecs::image_processing::extend_border_nv12;
use cros_codecs::image_processing::i420_to_nv12_chroma;
use cros_codecs::image_processing::nv12_copy;
use cros_codecs::BlockingMode;
use cros_codecs::DecodedFormat;
use cros_codecs::Fourcc;
use cros_codecs::FrameLayout;
use cros_codecs::PlaneLayout;
use cros_codecs::Resolution;

use crate::util::Args;
use crate::util::Codec;

fn upload_img<M: libva::SurfaceMemoryDescriptor>(
    display: &Rc<libva::Display>,
    surface: &libva::Surface<M>,
    resolution: Resolution,
    input_coded_resolution: Resolution,
    data: &[u8],
    input_fourcc: DecodedFormat,
) -> FrameLayout {
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

    let image_fmts = display.query_image_formats().unwrap();
    let image_fmt = image_fmts
        .into_iter()
        .find(|f| f.fourcc == libva::VA_FOURCC_NV12)
        .unwrap();
    let mut image = libva::Image::create_from(
        surface,
        image_fmt,
        (resolution.width, resolution.height),
        (resolution.width, resolution.height),
    )
    .unwrap();
    let va_image = *image.image();
    let dst = image.as_mut();
    let (dst_y, dst_uv) =
        (&mut dst[va_image.offsets[0] as usize..]).split_at_mut(va_image.offsets[1] as usize);

    nv12_copy(
        input_y,
        input_coded_resolution.width as usize,
        dst_y,
        va_image.pitches[0] as usize,
        input_uv,
        input_coded_resolution.width as usize,
        dst_uv,
        va_image.pitches[1] as usize,
        resolution.width as usize,
        resolution.height as usize,
    );
    extend_border_nv12(
        dst_y,
        dst_uv,
        resolution.width as usize,
        resolution.height as usize,
        va_image.pitches[0] as usize,
        va_image.height as usize,
    );

    drop(image);

    surface.sync().unwrap();

    FrameLayout {
        format: (Fourcc::from(b"NV12"), 0),
        size: resolution,
        planes: vec![
            PlaneLayout {
                buffer_index: 0,
                offset: va_image.offsets[0] as usize,
                stride: va_image.pitches[0] as usize,
            },
            PlaneLayout {
                buffer_index: 0,
                offset: va_image.offsets[1] as usize,
                stride: va_image.pitches[1] as usize,
            },
        ],
    }
}

fn new_h264_vaapi_encoder(
    args: &Args,
    display: &Rc<libva::Display>,
) -> Box<dyn VideoEncoder<PooledVaSurface<()>>> {
    let resolution = Resolution {
        width: args.width,
        height: args.height,
    };

    let config = H264EncoderConfig {
        resolution,
        initial_tunings: Tunings {
            framerate: args.framerate,
            rate_control: RateControl::ConstantBitrate(args.bitrate),
            ..Default::default()
        },
        ..Default::default()
    };

    let fourcc = b"NV12".into();
    let encoder = h264::StatelessEncoder::new_vaapi(
        Rc::clone(display),
        config,
        fourcc,
        resolution,
        args.low_power,
        BlockingMode::Blocking,
    )
    .expect("Unable to create encoder");

    Box::new(encoder)
}

fn new_vp9_vaapi_encoder(
    args: &Args,
    display: &Rc<libva::Display>,
) -> Box<dyn VideoEncoder<PooledVaSurface<()>>> {
    let resolution = Resolution {
        width: args.width,
        height: args.height,
    };

    let config = VP9EncoderConfig {
        resolution,
        initial_tunings: Tunings {
            framerate: args.framerate,
            rate_control: RateControl::ConstantBitrate(args.bitrate),
            ..Default::default()
        },
        ..Default::default()
    };

    let fourcc = b"NV12".into();
    let encoder = vp9::StatelessEncoder::new_vaapi(
        Rc::clone(display),
        config,
        fourcc,
        resolution,
        args.low_power,
        BlockingMode::Blocking,
    )
    .expect("Unable to create encoder");

    Box::new(encoder)
}

fn new_av1_vaapi_encoder(
    args: &Args,
    display: &Rc<libva::Display>,
) -> Box<dyn VideoEncoder<PooledVaSurface<()>>> {
    let resolution = Resolution {
        width: args.width,
        height: args.height,
    };

    let config = AV1EncoderConfig {
        resolution,
        initial_tunings: Tunings {
            framerate: args.framerate,
            rate_control: RateControl::ConstantBitrate(args.bitrate),
            ..Default::default()
        },
        ..Default::default()
    };

    let fourcc = b"NV12".into();
    let encoder = av1::StatelessEncoder::new_vaapi(
        Rc::clone(display),
        config,
        fourcc,
        resolution,
        args.low_power,
        BlockingMode::Blocking,
    )
    .expect("Unable to create encoder");

    Box::new(encoder)
}

pub fn do_encode(mut input: File, args: Args) -> () {
    let display = libva::Display::open().unwrap();

    let codec = args.codec.unwrap_or_default();

    let mut encoder = match codec {
        Codec::H264 => new_h264_vaapi_encoder(&args, &display),
        Codec::VP9 => new_vp9_vaapi_encoder(&args, &display),
        Codec::AV1 => new_av1_vaapi_encoder(&args, &display),
        _ => panic!("Unsupported format!"),
    };

    let mut pool = VaSurfacePool::new(
        Rc::clone(&display),
        libva::VA_RT_FORMAT_YUV420,
        Some(libva::UsageHint::USAGE_HINT_ENCODER),
        Resolution {
            width: args.width,
            height: args.height,
        },
    );

    pool.add_frames(vec![(); 16]).unwrap();

    let coded_width = args.coded_width.unwrap_or(args.width);
    let coded_height = args.coded_height.unwrap_or(args.height);
    let coded_frame_size: usize = (coded_width * coded_height * 3 / 2) as usize;

    let mut output = args.output.map(|output| File::create(output).unwrap());

    if let Some(ref mut output) = output {
        if codec == Codec::VP9 {
            let hdr = IvfFileHeader::new(
                IvfFileHeader::CODEC_VP9,
                args.width as u16,
                args.height as u16,
                args.framerate,
                args.count as u32,
            );
            hdr.writo_into(output).unwrap();
        }
    }

    let mut buf = vec![0u8; coded_frame_size];
    for i in 0..args.count {
        input.read_exact(&mut buf[..]).unwrap();
        let handle = pool.get_surface().unwrap();
        let layout = upload_img(
            &display,
            handle.borrow(),
            (args.width, args.height).into(),
            (coded_width, coded_height).into(),
            &buf[..],
            args.fourcc,
        );

        let input_frame = FrameMetadata {
            layout,
            timestamp: i as u64,
            force_keyframe: false,
        };

        encoder.encode(input_frame, handle).unwrap();
        while let Some(coded) = encoder.poll().unwrap() {
            if let Some(ref mut output) = output {
                if codec == Codec::VP9 {
                    let hdr = IvfFrameHeader {
                        timestamp: coded.metadata.timestamp,
                        frame_size: coded.bitstream.len() as u32,
                    };

                    hdr.writo_into(output).unwrap();
                }

                output.write_all(&coded.bitstream).unwrap();
            }
        }
    }

    encoder.drain().unwrap();
    while let Some(coded) = encoder.poll().unwrap() {
        if let Some(ref mut output) = output {
            if codec == Codec::VP9 {
                let hdr = IvfFrameHeader {
                    timestamp: coded.metadata.timestamp,
                    frame_size: coded.bitstream.len() as u32,
                };

                hdr.writo_into(output).unwrap();
            }

            output.write_all(&coded.bitstream).unwrap();
        }
    }
}
