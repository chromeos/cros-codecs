// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::borrow::Cow;
use std::fs::File;
use std::io::Read;
use std::io::Write;

use cros_codecs::backend::v4l2::decoder::stateless::V4l2StatelessDecoderHandle;
use cros_codecs::bitstream_utils::NalIterator;
use cros_codecs::codec::h264::parser::Nalu as H264Nalu;
use cros_codecs::decoder::stateless::h264::H264;
use cros_codecs::decoder::stateless::StatelessDecoder;
use cros_codecs::decoder::stateless::StatelessVideoDecoder;
use cros_codecs::decoder::BlockingMode;
use cros_codecs::decoder::DecodedHandle;
use cros_codecs::decoder::DynDecodedHandle;
use cros_codecs::multiple_desc_type;
use cros_codecs::utils::simple_playback_loop;
use cros_codecs::utils::simple_playback_loop_owned_frames;
use cros_codecs::utils::DmabufFrame;
use cros_codecs::utils::UserPtrFrame;
use cros_codecs::DecodedFormat;

use crate::md5::md5_digest;
use crate::md5::MD5Context;
use crate::util::decide_output_file_name;
use crate::util::Args;
use crate::util::EncodedFormat;
use crate::util::FrameMemoryType;
use crate::util::Md5Computation;

multiple_desc_type! {
    enum BufferDescriptor {
        Managed(()),
        Dmabuf(DmabufFrame),
        User(UserPtrFrame),
    }
}

pub fn do_decode(mut input: File, args: Args) -> () {
    let input = {
        let mut buf = Vec::new();
        input
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

            let decoder = StatelessDecoder::<H264, _>::new_v4l2(blocking_mode).into_trait_object();

            (decoder, frame_iter)
        }
        EncodedFormat::VP8 => todo!(),
        EncodedFormat::VP9 => todo!(),
        EncodedFormat::H265 => todo!(),
        EncodedFormat::AV1 => todo!(),
    };

    let mut md5_context = MD5Context::new();
    let mut output_filename_idx = 0;
    let need_per_frame_md5 = match args.compute_md5 {
        Some(Md5Computation::Frame) => true,
        _ => args.golden.is_some(),
    };

    let mut on_new_frame = |handle: DynDecodedHandle<()>| {
        let timestamp = handle.timestamp(); //handle.handle.borrow().timestamp;
        log::debug!("{:<20} {:?}\n", "on_new_frame", timestamp);

        let picture = handle.dyn_picture();
        let mut handle = picture.dyn_mappable_handle().unwrap();
        let buffer_size = handle.image_size();
        let mut frame_data = vec![0; buffer_size];

        handle.read(&mut frame_data).unwrap();
        log::debug!(
            "{:<20} {:?}, {} bytes\n",
            "on_new_frame",
            timestamp,
            buffer_size
        );

        if args.multiple_output_files {
            let file_name = decide_output_file_name(
                args.output
                    .as_ref()
                    .expect("multiple_output_files need output to be set"),
                output_filename_idx,
            );

            let mut output = File::create(file_name).expect("error creating output file");
            output_filename_idx += 1;
            output
                .write_all(&frame_data)
                .expect("failed to write to output file");
        } else if let Some(output) = &mut output {
            output
                .write_all(&frame_data)
                .expect("failed to write to output file");
        }

        let frame_md5: String = if need_per_frame_md5 {
            md5_digest(&frame_data)
        } else {
            "".to_string()
        };

        match args.compute_md5 {
            None => (),
            Some(Md5Computation::Frame) => println!("{}", frame_md5),
            Some(Md5Computation::Stream) => md5_context.consume(&frame_data),
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

    if let Some(Md5Computation::Stream) = args.compute_md5 {
        println!("{}", md5_context.flush());
    }
}
