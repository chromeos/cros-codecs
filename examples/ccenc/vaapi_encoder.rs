// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::fs::File;
use std::io::Read;
use std::io::Write;
use std::path::PathBuf;
use std::rc::Rc;

use cros_codecs::bitstream_utils::IvfFileHeader;
use cros_codecs::bitstream_utils::IvfFrameHeader;
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
use cros_codecs::video_frame::gbm_video_frame::GbmDevice;
use cros_codecs::video_frame::generic_dma_video_frame::GenericDmaVideoFrame;
use cros_codecs::BlockingMode;
use cros_codecs::Fourcc;
use cros_codecs::Resolution;

use crate::util::upload_img;
use crate::util::Args;
use crate::util::Codec;

fn new_h264_vaapi_encoder(
    args: &Args,
    display: &Rc<libva::Display>,
) -> Box<dyn VideoEncoder<GenericDmaVideoFrame>> {
    let resolution = Resolution { width: args.width, height: args.height };

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
) -> Box<dyn VideoEncoder<GenericDmaVideoFrame>> {
    let resolution = Resolution { width: args.width, height: args.height };

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
) -> Box<dyn VideoEncoder<GenericDmaVideoFrame>> {
    let resolution = Resolution { width: args.width, height: args.height };

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
    let gbm_device =
        GbmDevice::open(PathBuf::from("/dev/dri/renderD128")).expect("Could not open GBM device!");

    let codec = args.codec.unwrap_or_default();

    let mut encoder = match codec {
        Codec::H264 => new_h264_vaapi_encoder(&args, &display),
        Codec::VP9 => new_vp9_vaapi_encoder(&args, &display),
        Codec::AV1 => new_av1_vaapi_encoder(&args, &display),
        _ => panic!("Unsupported format!"),
    };

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
        let handle = upload_img(
            &gbm_device,
            (args.width, args.height).into(),
            (coded_width, coded_height).into(),
            (coded_width, coded_height).into(),
            &buf[..],
            args.fourcc,
            Fourcc::from(b"NV12"),
        );

        let input_frame = FrameMetadata {
            layout: Default::default(),
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
