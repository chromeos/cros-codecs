// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::fs::File;
use std::io::ErrorKind;
use std::io::Read;
use std::io::Write;
use std::path::PathBuf;
use std::sync::atomic::AtomicU32;
use std::sync::Arc;
use std::sync::Mutex;
use std::thread;
use std::time::Duration;

use cros_codecs::bitstream_utils::IvfFileHeader;
use cros_codecs::bitstream_utils::IvfFrameHeader;
use cros_codecs::c2_wrapper::c2_encoder::C2EncoderWorker;
#[cfg(feature = "v4l2")]
use cros_codecs::c2_wrapper::c2_v4l2_encoder::C2V4L2Encoder;
#[cfg(feature = "v4l2")]
use cros_codecs::c2_wrapper::c2_v4l2_encoder::C2V4L2EncoderOptions;
#[cfg(feature = "vaapi")]
use cros_codecs::c2_wrapper::c2_vaapi_encoder::C2VaapiEncoder;
#[cfg(feature = "vaapi")]
use cros_codecs::c2_wrapper::c2_vaapi_encoder::C2VaapiEncoderOptions;
use cros_codecs::c2_wrapper::C2EncodeJob;
use cros_codecs::c2_wrapper::C2Status;
use cros_codecs::c2_wrapper::C2Worker;
use cros_codecs::c2_wrapper::C2Wrapper;
use cros_codecs::c2_wrapper::DrainMode;
use cros_codecs::decoder::StreamInfo;
use cros_codecs::image_processing::extend_border_nv12;
use cros_codecs::image_processing::i420_to_nv12_chroma;
use cros_codecs::image_processing::nv12_copy;
use cros_codecs::video_frame::frame_pool::FramePool;
use cros_codecs::video_frame::frame_pool::PooledVideoFrame;
use cros_codecs::video_frame::gbm_video_frame::GbmDevice;
use cros_codecs::video_frame::gbm_video_frame::GbmUsage;
use cros_codecs::video_frame::generic_dma_video_frame::GenericDmaVideoFrame;
use cros_codecs::video_frame::VideoFrame;
use cros_codecs::video_frame::UV_PLANE;
use cros_codecs::video_frame::Y_PLANE;
use cros_codecs::DecodedFormat;
use cros_codecs::Fourcc;
use cros_codecs::Resolution;

mod util;

use crate::util::Args;
use crate::util::Codec;

// We use pooled video frames here because it prevents us from exhausting the FD limit on the
// machine. Unfortunately this does require us to cap the pipeline depth to the frame pool size.
const PIPELINE_DEPTH: usize = 64;

fn codec_to_fourcc(codec: &Codec) -> Fourcc {
    match codec {
        Codec::H264 => Fourcc::from(b"H264"),
        Codec::VP8 => Fourcc::from(b"VP80"),
        Codec::VP9 => Fourcc::from(b"VP90"),
        _ => panic!("Unsupported format!"),
    }
}

fn codec_to_ivf_magic(codec: &Codec) -> [u8; 4] {
    match codec {
        // Note that H264 does not generally use IVF containers.
        Codec::VP8 => IvfFileHeader::CODEC_VP8,
        Codec::VP9 => IvfFileHeader::CODEC_VP9,
        _ => panic!("Unsupported format!"),
    }
}

fn enqueue_work<W>(
    encoder: &mut C2Wrapper<C2EncodeJob<PooledVideoFrame<GenericDmaVideoFrame>>, W>,
    file: &mut File,
    framepool: Arc<Mutex<FramePool<GenericDmaVideoFrame>>>,
    input_format: DecodedFormat,
    input_coded_resolution: Resolution,
    num_frames: u64,
    bitrate: u64,
    framerate: u32,
    timestamp: &mut u64,
) -> bool
where
    W: C2Worker<C2EncodeJob<PooledVideoFrame<GenericDmaVideoFrame>>>,
{
    assert!(input_coded_resolution.width % 2 == 0);
    assert!(input_coded_resolution.height % 2 == 0);

    if *timestamp >= num_frames {
        if *timestamp == num_frames {
            encoder.drain(DrainMode::EOSDrain);
            *timestamp += 1;
        }
        return false;
    }

    let mut new_frame = match (*framepool.lock().unwrap()).alloc() {
        Some(frame) => frame,
        // We've exhausted the pipeline depth
        None => return false,
    };

    let mut buf = vec![0u8; input_coded_resolution.get_area() * 3 / 2];
    match file.read_exact(buf.as_mut_slice()) {
        Ok(_) => (),
        Err(e) => {
            if e.kind() == ErrorKind::UnexpectedEof {
                // We've reached the end of the input file, start draining.
                encoder.drain(DrainMode::EOSDrain);
                *timestamp = u64::MAX;
                return false;
            } else {
                panic!("Error reading input file! {:?}", e);
            }
        }
    }

    let input_y = &buf[0..input_coded_resolution.get_area()];
    let mut tmp_input_uv: Vec<u8> = Vec::new();
    let input_uv = match input_format {
        DecodedFormat::NV12 => {
            &buf[input_coded_resolution.get_area()..(input_coded_resolution.get_area() * 3 / 2)]
        }
        DecodedFormat::I420 => {
            tmp_input_uv.resize(input_coded_resolution.get_area() / 2, 0);
            let input_u = &buf
                [input_coded_resolution.get_area()..(input_coded_resolution.get_area() * 5 / 4)];
            let input_v = &buf[(input_coded_resolution.get_area() * 5 / 4)
                ..(input_coded_resolution.get_area() * 3 / 2)];
            i420_to_nv12_chroma(input_u, input_v, tmp_input_uv.as_mut_slice());
            tmp_input_uv.as_slice()
        }
        _ => panic!("Unsupported input format!"),
    };

    {
        let visible_resolution = new_frame.resolution();
        let dst_pitches = new_frame.get_plane_pitch();
        let dst_sizes = new_frame.get_plane_size();
        let coded_resolution = Resolution {
            width: dst_pitches[0] as u32,
            height: (dst_sizes[0] / dst_pitches[0]) as u32,
        };
        let dst_mapping = new_frame.map_mut().expect("Failed to map input frame!");
        let dst_planes = dst_mapping.get();
        nv12_copy(
            input_y,
            input_coded_resolution.width as usize,
            *dst_planes[Y_PLANE].borrow_mut(),
            dst_pitches[Y_PLANE],
            input_uv,
            input_coded_resolution.width as usize,
            *dst_planes[UV_PLANE].borrow_mut(),
            dst_pitches[UV_PLANE],
            visible_resolution.width as usize,
            visible_resolution.height as usize,
        );
        extend_border_nv12(
            *dst_planes[Y_PLANE].borrow_mut(),
            *dst_planes[UV_PLANE].borrow_mut(),
            visible_resolution.width as usize,
            visible_resolution.height as usize,
            coded_resolution.width as usize,
            coded_resolution.height as usize,
        );
    }

    let job = C2EncodeJob {
        input: Some(new_frame),
        output: vec![],
        timestamp: *timestamp,
        bitrate: bitrate,
        framerate: Arc::new(AtomicU32::new(framerate)),
        drain: DrainMode::NoDrain,
    };
    encoder.queue(vec![job]);

    *timestamp += 1;

    true
}

fn main() {
    env_logger::init();

    let args: Args = argh::from_env();

    let mut input = File::open(&args.input).expect("error opening input file!");

    let input_fourcc = Fourcc::from(b"NV12");
    let codec = args.codec.unwrap_or_default();
    let output_fourcc = codec_to_fourcc(&codec);

    let gbm_device = Arc::new(
        GbmDevice::open(PathBuf::from("/dev/dri/renderD128")).expect("Could not open GBM device!"),
    );
    let framepool = Arc::new(Mutex::new(FramePool::new(move |stream_info: &StreamInfo| {
        <Arc<GbmDevice> as Clone>::clone(&gbm_device)
            .new_frame(
                Fourcc::from(b"NV12"),
                stream_info.display_resolution,
                stream_info.coded_resolution,
                GbmUsage::Encode,
            )
            .expect("Could not allocate frame for frame pool!")
            .to_generic_dma_video_frame()
            .expect("Could not export GBM frame to DMA frame!")
    })));
    let framepool_ = framepool.clone();
    let framepool_hint_cb = move |stream_info: StreamInfo| {
        (*framepool_.lock().unwrap()).resize(&StreamInfo {
            format: stream_info.format,
            coded_resolution: stream_info.coded_resolution,
            display_resolution: stream_info.display_resolution,
            min_num_frames: PIPELINE_DEPTH,
        });
    };

    // We shouldn't need temp frames for GBM.
    let alloc_cb = move || None;

    let error_cb = move |_status: C2Status| {
        panic!("Unrecoverable encoding error!");
    };

    let output_file = Arc::new(Mutex::new(
        File::create(args.output.unwrap()).expect("Error creating output file"),
    ));
    if codec != Codec::H264 {
        let hdr = IvfFileHeader::new(
            codec_to_ivf_magic(&codec),
            args.width as u16,
            args.height as u16,
            args.framerate,
            args.count as u32,
        );
        hdr.writo_into(&mut *output_file.lock().unwrap()).expect("Error writing IVF file header!");
    }
    let codec_ = codec.clone();
    let work_done_cb = move |job: C2EncodeJob<PooledVideoFrame<GenericDmaVideoFrame>>| {
        if codec_ != Codec::H264 {
            let hdr =
                IvfFrameHeader { timestamp: job.timestamp, frame_size: job.output.len() as u32 };
            hdr.writo_into(&mut *output_file.lock().unwrap())
                .expect("Error writing IVF frame header!");
        }
        let _ = (*output_file.lock().unwrap())
            .write(job.output.as_slice())
            .expect("Error writing output file!");
    };

    let input_coded_resolution = Resolution {
        width: args.coded_width.unwrap_or(args.width),
        height: args.coded_height.unwrap_or(args.height),
    };
    assert!(input_coded_resolution.width >= args.width);
    assert!(input_coded_resolution.height >= args.height);

    #[cfg(feature = "v4l2")]
    let mut encoder: C2Wrapper<_, C2EncoderWorker<_, C2V4L2Encoder>> = C2Wrapper::new(
        input_fourcc,
        output_fourcc.clone(),
        error_cb,
        work_done_cb,
        framepool_hint_cb,
        alloc_cb,
        C2V4L2EncoderOptions {
            output_fourcc: output_fourcc,
            visible_resolution: Resolution { width: args.width, height: args.height },
        },
    );
    #[cfg(feature = "vaapi")]
    let mut encoder: C2Wrapper<_, C2EncoderWorker<_, C2VaapiEncoder>> = C2Wrapper::new(
        input_fourcc,
        output_fourcc.clone(),
        error_cb,
        work_done_cb,
        framepool_hint_cb,
        alloc_cb,
        C2VaapiEncoderOptions {
            low_power: args.low_power,
            visible_resolution: Resolution { width: args.width, height: args.height },
        },
    );

    let mut timestamp: u64 = 0;

    // Start the encoder
    encoder.start();

    // Run the encode job
    while encoder.is_alive() {
        // Enqueue as much as we can until the framepool is exhausted
        while enqueue_work(
            &mut encoder,
            &mut input,
            framepool.clone(),
            args.fourcc,
            input_coded_resolution.clone(),
            args.count as u64,
            args.bitrate,
            args.framerate,
            &mut timestamp,
        ) {}

        thread::sleep(Duration::from_millis(10));
    }

    // Shut down the encoder
    encoder.stop();
}
