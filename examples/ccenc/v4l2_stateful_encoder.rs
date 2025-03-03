// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::fs::File;
use std::io::Read;
use std::io::Write;
use std::path::PathBuf;
use std::sync::Arc;

use cros_codecs::backend::v4l2::encoder::find_device_with_capture;
use cros_codecs::backend::v4l2::encoder::EncoderCodec;
use cros_codecs::backend::v4l2::encoder::MmapingCapture;
use cros_codecs::backend::v4l2::encoder::V4L2Backend;
use cros_codecs::bitstream_utils::IvfFileHeader;
use cros_codecs::bitstream_utils::IvfFrameHeader;
use cros_codecs::encoder::simple_encode_loop;
use cros_codecs::encoder::stateful::h264::v4l2::V4L2StatefulH264Encoder;
use cros_codecs::encoder::stateful::h265::v4l2::V4L2StatefulH265Encoder;
use cros_codecs::encoder::stateful::vp8::v4l2::V4L2StatefulVP8Encoder;
use cros_codecs::encoder::stateful::vp9::v4l2::V4L2StatefulVP9Encoder;
use cros_codecs::encoder::stateful::StatefulEncoder;
use cros_codecs::encoder::CodedBitstreamBuffer;
use cros_codecs::encoder::FrameMetadata;
use cros_codecs::encoder::RateControl;
use cros_codecs::encoder::Tunings;
use cros_codecs::video_frame::gbm_video_frame::GbmDevice;
use cros_codecs::video_frame::generic_dma_video_frame::GenericDmaVideoFrame;
use cros_codecs::video_frame::V4l2VideoFrame;
use cros_codecs::DecodedFormat;
use cros_codecs::Fourcc;
use cros_codecs::Resolution;

use v4l2r::device::Device;
use v4l2r::device::DeviceConfig;

use crate::util::upload_img;
use crate::util::Args;
use crate::util::Codec;

struct DiskFrameReader<'a> {
    file: &'a File,
    gbm_device: Arc<GbmDevice>,
    input_fourcc: DecodedFormat,
    visible_size: Resolution,
    input_coded_size: Resolution,
    queue_format: v4l2r::Format,
    frame_num: usize,
    total_frames: usize,
}

impl<'a> Iterator for DiskFrameReader<'a> {
    type Item = (FrameMetadata, V4l2VideoFrame<GenericDmaVideoFrame>);

    fn next(&mut self) -> Option<Self::Item> {
        if self.frame_num >= self.total_frames {
            return None;
        }
        let timestamp = self.frame_num as u64;
        self.frame_num += 1;

        // TODO: Support formats that aren't 4:2:0
        let mut buf = vec![0u8; self.input_coded_size.get_area() * 3 / 2];
        self.file.read_exact(buf.as_mut_slice()).unwrap();

        Some((
            FrameMetadata {
                timestamp: timestamp,
                layout: Default::default(),
                force_keyframe: false,
            },
            V4l2VideoFrame(upload_img(
                &self.gbm_device,
                self.visible_size.clone(),
                self.input_coded_size.clone(),
                // TODO: This isn't guaranteed to be right. Sometimes, especially when we don't use
                // minigbm, we will frames that are improperly aligned for DMA import. So, we query
                // the V4L2 queue directly to figure out what the alignment should be. This
                // workaround assumes that all planes have similar alignment requirements, though.
                Resolution {
                    width: self.queue_format.plane_fmt[0].bytesperline as u32,
                    height: (self.queue_format.plane_fmt[0].sizeimage
                        / self.queue_format.plane_fmt[0].bytesperline)
                        as u32,
                },
                buf.as_slice(),
                self.input_fourcc.clone(),
                Fourcc::from(b"NV12"),
            )),
        ))
    }
}

impl<'a> DiskFrameReader<'_> {
    pub fn new(
        file: &'a File,
        input_fourcc: DecodedFormat,
        visible_size: Resolution,
        input_coded_size: Resolution,
        queue_format: v4l2r::Format,
        total_frames: usize,
    ) -> DiskFrameReader<'a> {
        DiskFrameReader {
            file: file,
            gbm_device: GbmDevice::open(PathBuf::from("/dev/dri/renderD128"))
                .expect("Could not open GBM device!"),
            input_fourcc: input_fourcc,
            visible_size: visible_size,
            input_coded_size: input_coded_size,
            queue_format: queue_format,
            frame_num: 0,
            total_frames: total_frames,
        }
    }
}

fn codec_to_pixelformat(codec: Codec) -> v4l2r::PixelFormat {
    match codec {
        Codec::H264 => v4l2r::PixelFormat::from_fourcc(b"H264"),
        Codec::H265 => v4l2r::PixelFormat::from_fourcc(b"HEVC"),
        Codec::VP9 => v4l2r::PixelFormat::from_fourcc(b"VP90"),
        Codec::VP8 => v4l2r::PixelFormat::from_fourcc(b"VP80"),
        _ => panic!("Unsupported format!"),
    }
}

fn codec_to_ivf_magic(codec: Codec) -> [u8; 4] {
    match codec {
        // Note that H264 does not generally use IVF containers.
        Codec::VP8 => IvfFileHeader::CODEC_VP8,
        Codec::VP9 => IvfFileHeader::CODEC_VP9,
        _ => panic!("Unsupported format!"),
    }
}

fn do_encode_loop<'a, Codecz>(
    mut encoder: StatefulEncoder<
        V4l2VideoFrame<GenericDmaVideoFrame>,
        V4L2Backend<V4l2VideoFrame<GenericDmaVideoFrame>, MmapingCapture, Codecz>,
    >,
    input: &'a File,
    args: Args,
) -> ()
where
    V4L2Backend<V4l2VideoFrame<GenericDmaVideoFrame>, MmapingCapture, Codecz>: EncoderCodec,
{
    let mut frame_reader = DiskFrameReader::new(
        &input,
        args.fourcc,
        (args.width, args.height).into(),
        (args.coded_width.unwrap_or(args.width), args.coded_height.unwrap_or(args.height)).into(),
        encoder.backend().output_format().unwrap(),
        args.count,
    );

    let codec = args.codec.unwrap_or_default();
    let output_file = args.output.map(|path| {
        let mut output = File::create(path).expect("Error opening output file!");

        if codec != Codec::H264 {
            let hdr = IvfFileHeader::new(
                codec_to_ivf_magic(codec),
                args.width as u16,
                args.height as u16,
                args.framerate,
                args.count as u32,
            );
            hdr.writo_into(&mut output).expect("Error writing IVF file header!");
        }

        output
    });

    // Unwrapping an optional takes ownership of it, so we do that outside of the lambda so we
    // don't violate FnMut's lifetime requirements.
    match output_file {
        Some(mut output_file) => {
            let frame_consumer = |coded_chunk: CodedBitstreamBuffer| {
                if codec != Codec::H264 {
                    let hdr = IvfFrameHeader {
                        timestamp: coded_chunk.metadata.timestamp,
                        frame_size: coded_chunk.bitstream.len() as u32,
                    };
                    hdr.writo_into(&mut output_file).expect("Error writing IVF frame header!");
                }

                let _ = output_file
                    .write(&coded_chunk.bitstream[..])
                    .expect("Error writing output file!");
            };
            simple_encode_loop(&mut encoder, &mut frame_reader, frame_consumer)
                .expect("Failed to encode!");
        }
        None => {
            simple_encode_loop(&mut encoder, &mut frame_reader, |_| ()).expect("Failed to encode!")
        }
    };
}

pub fn do_encode(input: File, args: Args) -> () {
    let codec = args.codec.unwrap_or_default();
    let device = find_device_with_capture(codec_to_pixelformat(codec))
        .expect("Could not find an encoder for codec");
    let device = Device::open(&device, DeviceConfig::new().non_blocking_dqbuf()).expect("open");
    let device = Arc::new(device);

    let resolution = Resolution { width: args.width, height: args.height };
    let queue_fourcc = Fourcc::from(b"NV12");
    let tunings: Tunings = Tunings {
        rate_control: RateControl::ConstantBitrate(args.bitrate),
        framerate: args.framerate,
        ..Default::default()
    };

    match codec {
        Codec::H264 => do_encode_loop(
            V4L2StatefulH264Encoder::new(
                device,
                MmapingCapture,
                cros_codecs::encoder::h264::EncoderConfig {
                    resolution: resolution.clone(),
                    initial_tunings: tunings.clone(),
                    ..Default::default()
                },
                queue_fourcc,
                resolution,
                tunings,
            )
            .expect("Failed to create encoder"),
            &input,
            args,
        ),
        Codec::H265 => do_encode_loop(
            V4L2StatefulH265Encoder::new(
                device,
                MmapingCapture,
                cros_codecs::encoder::h265::EncoderConfig {
                    resolution: resolution.clone(),
                    ..Default::default()
                },
                queue_fourcc,
                resolution,
                tunings,
            )
            .expect("Failed to create encoder"),
            &input,
            args,
        ),
        Codec::VP8 => do_encode_loop(
            V4L2StatefulVP8Encoder::new(
                device,
                MmapingCapture,
                cros_codecs::encoder::vp8::EncoderConfig {
                    resolution: resolution.clone(),
                    ..Default::default()
                },
                queue_fourcc,
                resolution,
                tunings,
            )
            .expect("Failed to create encoder"),
            &input,
            args,
        ),
        Codec::VP9 => do_encode_loop(
            V4L2StatefulVP9Encoder::new(
                device,
                MmapingCapture,
                cros_codecs::encoder::vp9::EncoderConfig {
                    resolution: resolution.clone(),
                    initial_tunings: tunings.clone(),
                    ..Default::default()
                },
                queue_fourcc,
                resolution,
                tunings,
            )
            .expect("Failed to create encoder"),
            &input,
            args,
        ),
        _ => panic!("Unsupported format!"),
    };
}
