// Copyright 2023 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

//! ccdec, a simple decoder program using cros-codecs. Capable of computing MD5 checksums from the
//! input and writing the raw decoded frames to a file.

use std::borrow::Cow;
use std::fs::File;
use std::io::Read;
use std::io::Write;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::Mutex;
use std::thread;
use std::time::Duration;

use cros_codecs::bitstream_utils::IvfIterator;
use cros_codecs::bitstream_utils::NalIterator;
use cros_codecs::c2_wrapper::c2_decoder::C2DecoderWorker;
#[cfg(feature = "v4l2")]
use cros_codecs::c2_wrapper::c2_v4l2_decoder::C2V4L2Decoder;
#[cfg(feature = "v4l2")]
use cros_codecs::c2_wrapper::c2_v4l2_decoder::C2V4L2DecoderOptions;
#[cfg(feature = "vaapi")]
use cros_codecs::c2_wrapper::c2_vaapi_decoder::C2VaapiDecoder;
#[cfg(feature = "vaapi")]
use cros_codecs::c2_wrapper::c2_vaapi_decoder::C2VaapiDecoderOptions;
use cros_codecs::c2_wrapper::C2DecodeJob;
use cros_codecs::c2_wrapper::C2Status;
use cros_codecs::c2_wrapper::C2Wrapper;
use cros_codecs::c2_wrapper::DrainMode;
use cros_codecs::codec::h264::parser::Nalu as H264Nalu;
use cros_codecs::codec::h265::parser::Nalu as H265Nalu;
use cros_codecs::decoder::StreamInfo;
use cros_codecs::image_processing::nv12_to_i420;
use cros_codecs::utils::align_up;
use cros_codecs::video_frame::frame_pool::FramePool;
use cros_codecs::video_frame::frame_pool::PooledVideoFrame;
use cros_codecs::video_frame::gbm_video_frame::GbmDevice;
use cros_codecs::video_frame::gbm_video_frame::GbmUsage;
use cros_codecs::video_frame::generic_dma_video_frame::GenericDmaVideoFrame;
use cros_codecs::video_frame::VideoFrame;
use cros_codecs::video_frame::UV_PLANE;
use cros_codecs::video_frame::Y_PLANE;
use cros_codecs::DecodedFormat;
use cros_codecs::EncodedFormat;
use cros_codecs::Fourcc;

use crate::md5::md5_digest;
use crate::md5::MD5Context;
use crate::util::decide_output_file_name;
use crate::util::golden_md5s;
use crate::util::Args;
use crate::util::Md5Computation;

mod md5;
mod util;

// Returns the frame iterator for IVF file.
fn create_vpx_frame_iterator(input: &[u8]) -> Box<dyn Iterator<Item = Cow<[u8]>> + '_> {
    Box::new(IvfIterator::new(input).map(Cow::Borrowed))
}

fn main() {
    env_logger::init();

    let args: Args = argh::from_env();

    let mut input = File::open(&args.input).expect("error opening input file");

    assert!(
        args.output_format == DecodedFormat::I420,
        "Only I420 currently supported by VA-API ccdec"
    );

    let input = {
        let mut buf = Vec::new();
        input.read_to_end(&mut buf).expect("error reading input file");
        buf
    };

    let mut output = if !args.multiple_output_files {
        args.output.as_ref().map(|p| File::create(p).expect("error creating output file"))
    } else {
        None
    };

    let golden_iter = Arc::new(Mutex::new(golden_md5s(&args.golden).into_iter()));

    let frame_iter =
        match args.input_format {
            EncodedFormat::H264 => Box::new(NalIterator::<H264Nalu>::new(&input))
                as Box<dyn Iterator<Item = Cow<[u8]>>>,
            EncodedFormat::H265 => Box::new(NalIterator::<H265Nalu>::new(&input))
                as Box<dyn Iterator<Item = Cow<[u8]>>>,
            _ => create_vpx_frame_iterator(&input),
        };

    let mut _md5_context = Arc::new(Mutex::new(MD5Context::new()));
    let md5_context = _md5_context.clone();

    let mut output_filename_idx = 0;
    let need_per_frame_md5 = match args.compute_md5 {
        Some(Md5Computation::Frame) => true,
        _ => args.golden.is_some(),
    };
    let stream_mode = Some(Md5Computation::Stream) == args.compute_md5;

    let gbm_device = Arc::new(
        GbmDevice::open(PathBuf::from("/dev/dri/renderD128")).expect("Could not open GBM device!"),
    );
    let framepool = Arc::new(Mutex::new(FramePool::new(move |stream_info: &StreamInfo| {
        <Arc<GbmDevice> as Clone>::clone(&gbm_device)
            .new_frame(
                Fourcc::from(b"NV12"),
                stream_info.display_resolution,
                stream_info.coded_resolution,
                GbmUsage::Decode,
            )
            .expect("Could not allocate frame for frame pool!")
            .to_generic_dma_video_frame()
            .expect("Could not export GBM frame to DMA frame!")
    })));
    // This is a workaround to get "copy by clone" semantics for closure variable capture since
    // Rust lacks the appropriate syntax to express this concept.
    let _framepool = framepool.clone();
    let framepool_hint_cb = move |stream_info: StreamInfo| {
        (*_framepool.lock().unwrap()).resize(&stream_info);
    };
    let alloc_cb = move || (*framepool.lock().unwrap()).alloc();

    let on_new_frame = move |job: C2DecodeJob<PooledVideoFrame<GenericDmaVideoFrame>>| {
        if args.output.is_none() && args.compute_md5.is_none() && args.golden.is_none() {
            return;
        }
        let width = job.output.as_ref().unwrap().resolution().width as usize;
        let height = job.output.as_ref().unwrap().resolution().height as usize;
        let luma_size = job.output.as_ref().unwrap().resolution().get_area();
        let chroma_size = align_up(width, 2) / 2 * align_up(height, 2) / 2;
        let mut frame_data: Vec<u8> = vec![0; luma_size + 2 * chroma_size];
        let (dst_y, dst_uv) = frame_data.split_at_mut(luma_size);
        let (dst_u, dst_v) = dst_uv.split_at_mut(chroma_size);
        {
            let src_pitches = job.output.as_ref().unwrap().get_plane_pitch();
            let src_mapping =
                job.output.as_ref().unwrap().map().expect("Failed to map output frame!");
            let src_planes = src_mapping.get();
            nv12_to_i420(
                src_planes[Y_PLANE],
                src_pitches[Y_PLANE],
                dst_y,
                width,
                src_planes[UV_PLANE],
                src_pitches[UV_PLANE],
                dst_u,
                align_up(width, 2) / 2,
                dst_v,
                align_up(width, 2) / 2,
                width,
                height,
            );
        }

        if args.multiple_output_files {
            let file_name = decide_output_file_name(
                args.output.as_ref().expect("multiple_output_files need output to be set"),
                output_filename_idx,
            );

            let mut output = File::create(file_name).expect("error creating output file");
            output_filename_idx += 1;
            output.write_all(&frame_data).expect("failed to write to output file");
        } else if let Some(output) = &mut output {
            output.write_all(&frame_data).expect("failed to write to output file");
        }

        let frame_md5: String =
            if need_per_frame_md5 { md5_digest(&frame_data) } else { "".to_string() };

        match args.compute_md5 {
            None => (),
            Some(Md5Computation::Frame) => println!("{}", frame_md5),
            Some(Md5Computation::Stream) => (*md5_context.lock().unwrap()).consume(&frame_data),
        }

        if args.golden.is_some() {
            assert_eq!(frame_md5, (*golden_iter.lock().unwrap()).next().unwrap());
        }
    };

    let error_cb = move |_status: C2Status| {
        panic!("Unrecoverable decoding error!");
    };

    #[cfg(feature = "vaapi")]
    let mut decoder: C2Wrapper<_, C2DecoderWorker<_, C2VaapiDecoder>> = C2Wrapper::new(
        Fourcc::from(args.input_format),
        // TODO: Support other pixel formats
        Fourcc::from(b"NV12"),
        error_cb,
        on_new_frame,
        framepool_hint_cb,
        alloc_cb,
        C2VaapiDecoderOptions { libva_device_path: args.libva_device },
    );
    #[cfg(feature = "v4l2")]
    let mut decoder: C2Wrapper<_, C2DecoderWorker<_, C2V4L2Decoder>> = C2Wrapper::new(
        Fourcc::from(args.input_format),
        // TODO: Support other pixel formats
        Fourcc::from(b"NV12"),
        error_cb,
        on_new_frame,
        framepool_hint_cb,
        alloc_cb,
        C2V4L2DecoderOptions { video_device_path: None },
    );
    let _ = decoder.start();

    for input_frame in frame_iter {
        decoder.queue(vec![C2DecodeJob {
            input: input_frame.as_ref().to_vec(),
            ..Default::default()
        }]);
    }
    decoder.drain(DrainMode::EOSDrain);

    while decoder.is_alive() {
        thread::sleep(Duration::from_millis(10));
    }
    decoder.stop();

    if stream_mode {
        println!("{}", (*_md5_context.lock().unwrap()).flush());
    }
}
