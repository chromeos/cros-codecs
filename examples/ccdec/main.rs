// Copyright 2023 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

//! ccdec, a simple decoder program using cros-codecs. Capable of computing MD5 checksums from the
//! input and writing the raw decoded frames to a file.

use std::borrow::Cow;
use std::fs::File;
use std::io::Read;
use std::io::Write;
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
use cros_codecs::c2_wrapper::C2DecodeWorkObject;
use cros_codecs::c2_wrapper::C2Status;
use cros_codecs::c2_wrapper::C2VideoFrame;
use cros_codecs::c2_wrapper::C2Wrapper;
use cros_codecs::codec::h264::parser::Nalu as H264Nalu;
use cros_codecs::codec::h265::parser::Nalu as H265Nalu;
use cros_codecs::DecodedFormat;
use cros_codecs::EncodedFormat;
use cros_codecs::Fourcc;
use cros_codecs::FrameLayout;
use cros_codecs::PlaneLayout;
use cros_codecs::Resolution;

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

/// Object holds the necessary data for a single decode operation during testing.
#[derive(Debug, Clone, Default)]
pub struct TestDecodeWorkObject {
    /// Input data for decode
    pub input: Vec<u8>,
    /// Output data after decode
    pub output: Vec<u8>,
}

impl C2DecodeWorkObject for TestDecodeWorkObject {
    fn input(&mut self) -> &[u8] {
        self.input.as_slice()
    }

    fn output(
        &mut self,
        fourcc: Fourcc,
        visible_width: usize,
        visible_height: usize,
    ) -> Result<C2VideoFrame, String> {
        if fourcc.to_string() != "I420" {
            return Err("Unsupported pixel format!".to_string());
        }

        let aligned_width = (visible_width + 1) & (!1);
        let aligned_height = (visible_height + 1) & (!1);
        self.output = vec![0; aligned_width * aligned_height * 3 / 2];
        let (y, uv) = self.output.as_mut_slice().split_at_mut(visible_width * visible_height);
        let y_layout = PlaneLayout { buffer_index: 0, offset: 0, stride: aligned_width };
        let (u, v) = uv.split_at_mut(aligned_width * aligned_height / 4);
        let u_layout = PlaneLayout { buffer_index: 1, offset: 0, stride: aligned_width / 2 };
        let v_layout = PlaneLayout { buffer_index: 2, offset: 0, stride: aligned_width / 2 };
        Ok(C2VideoFrame {
            planes: vec![y, u, v],
            layout: FrameLayout {
                format: (fourcc, 0),
                size: Resolution::from((visible_width as u32, visible_height as u32)),
                planes: vec![y_layout, u_layout, v_layout],
            },
        })
    }
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

    let on_new_frame = move |job: C2DecodeJob<TestDecodeWorkObject>| {
        if args.output.is_some() || args.compute_md5.is_some() || args.golden.is_some() {
            let frame_data = job.work_object.output.as_slice();

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
        }
    };

    let error_cb = move |_status: C2Status| {
        panic!("Unrecoverable decoding error!");
    };

    #[cfg(feature = "vaapi")]
    let mut decoder: C2Wrapper<_, _, _, C2DecoderWorker<C2VaapiDecoder, _, _, _>> = C2Wrapper::new(
        Fourcc::from(args.input_format),
        error_cb,
        on_new_frame,
        C2VaapiDecoderOptions {
            libva_device_path: args.libva_device,
            frame_memory_type: args.frame_memory,
        },
    );
    #[cfg(feature = "v4l2")]
    let mut decoder: C2Wrapper<_, _, _, C2DecoderWorker<C2V4L2Decoder, _, _, _>> = C2Wrapper::new(
        Fourcc::from(args.input_format),
        error_cb,
        on_new_frame,
        C2V4L2DecoderOptions { video_device_path: None },
    );
    let _ = decoder.start();

    for input_frame in frame_iter {
        decoder.queue(vec![C2DecodeJob {
            work_object: TestDecodeWorkObject {
                input: input_frame.as_ref().to_vec(),
                output: Vec::new(),
            },
            drain: false,
        }]);
    }
    decoder.drain();

    while decoder.is_alive() {
        thread::sleep(Duration::from_millis(10));
    }
    decoder.stop();

    if stream_mode {
        println!("{}", (*_md5_context.lock().unwrap()).flush());
    }
}
