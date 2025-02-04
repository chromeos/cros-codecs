// Copyright 2023 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

//! ccdec, a simple decoder program using cros-codecs. Capable of computing MD5 checksums from the
//! input and writing the raw decoded frames to a file.

use std::borrow::Cow;
use std::fs::File;
use std::io::Cursor;
use std::io::Read;
use std::io::Write;
use std::os::fd::AsFd;
use std::os::fd::BorrowedFd;
use std::path::Path;
use std::path::PathBuf;

use cros_codecs::bitstream_utils::IvfIterator;
use cros_codecs::bitstream_utils::NalIterator;
use cros_codecs::codec::h264::parser::Nalu as H264Nalu;
use cros_codecs::codec::h265::parser::Nalu as H265Nalu;
use cros_codecs::decoder::stateless::av1::Av1;
use cros_codecs::decoder::stateless::h264::H264;
use cros_codecs::decoder::stateless::h265::H265;
use cros_codecs::decoder::stateless::vp8::Vp8;
use cros_codecs::decoder::stateless::vp9::Vp9;
use cros_codecs::decoder::stateless::StatelessDecoder;
use cros_codecs::decoder::stateless::StatelessVideoDecoder;
use cros_codecs::decoder::BlockingMode;
use cros_codecs::decoder::DecodedHandle;
use cros_codecs::decoder::DynDecodedHandle;
use cros_codecs::decoder::StreamInfo;
use cros_codecs::multiple_desc_type;
use cros_codecs::utils::simple_playback_loop;
use cros_codecs::utils::simple_playback_loop_owned_frames;
use cros_codecs::utils::simple_playback_loop_userptr_frames;
use cros_codecs::utils::DmabufFrame;
use cros_codecs::utils::UserPtrFrame;
use cros_codecs::DecodedFormat;
use cros_codecs::Fourcc;
use cros_codecs::FrameLayout;
use cros_codecs::PlaneLayout;
use cros_codecs::Resolution;
use matroska_demuxer::Frame;
use matroska_demuxer::MatroskaFile;

use crate::md5::md5_digest;
use crate::md5::MD5Context;
use crate::util::decide_output_file_name;
use crate::util::golden_md5s;
use crate::util::Args;
use crate::util::EncodedFormat;
use crate::util::FrameMemoryType;
use crate::util::Md5Computation;

// Our buffer descriptor type.
//
// We support buffers which memory is managed by the backend, or imported from user memory or a
// PRIME buffer.
multiple_desc_type! {
    enum BufferDescriptor {
        Managed(()),
        Dmabuf(DmabufFrame),
        User(UserPtrFrame),
    }
}

/// Export a file descriptor from a GBM `BufferObject` and turn it into a `DmabufFrame` suitable
/// for using as the target of a decoder.
fn export_gbm_bo<T>(obj: &gbm::BufferObject<T>) -> anyhow::Result<DmabufFrame> {
    let fd = obj.fd()?;
    let modifier = obj.modifier()?;
    let format = obj.format()?;
    let planes = (0..obj.plane_count()? as i32)
        .map(|i| PlaneLayout {
            buffer_index: 0,
            offset: obj.offset(i).unwrap() as usize,
            stride: obj.stride_for_plane(i).unwrap() as usize,
        })
        .collect();
    let size = Resolution::from((obj.width().unwrap(), obj.height().unwrap()));

    Ok(DmabufFrame {
        fds: vec![fd],
        layout: FrameLayout {
            format: (Fourcc::from(format as u32), modifier.into()),
            size,
            planes,
        },
    })
}

/// Buffer allocation callback for `simple_playback_loop` to allocate and export buffers from a GBM
/// device.
fn simple_playback_loop_prime_frames<D: AsFd>(
    device: &gbm::Device<D>,
    stream_info: &StreamInfo,
    nb_frames: usize,
) -> anyhow::Result<Vec<DmabufFrame>> {
    let gbm_fourcc = match stream_info.format {
        DecodedFormat::I420 | DecodedFormat::NV12 => gbm::Format::Nv12,
        _ => anyhow::bail!(
            "{:?} format is unsupported with GBM memory",
            stream_info.format
        ),
    };

    (0..nb_frames)
        .map(|_| {
            device
                .create_buffer_object::<()>(
                    stream_info.coded_resolution.width,
                    stream_info.coded_resolution.height,
                    gbm_fourcc,
                    gbm::BufferObjectFlags::SCANOUT,
                )
                .map_err(|e| anyhow::anyhow!(e))
                .and_then(|o| export_gbm_bo(&o))
        })
        .collect()
}
struct MkvFrameIterator<T: AsRef<[u8]>> {
    input: MatroskaFile<Cursor<T>>,
    video_track: u64,
}

impl<T: AsRef<[u8]>> MkvFrameIterator<T> {
    fn new(input: T) -> anyhow::Result<Self> {
        let input = MatroskaFile::open(Cursor::new(input))?;
        let video_track = input
            .tracks()
            .iter()
            .find(|t| t.track_type() == matroska_demuxer::TrackType::Video)
            .map(|t| t.track_number().get())
            .ok_or_else(|| anyhow::anyhow!("no video track in input file"))?;

        Ok(Self { input, video_track })
    }
}

impl<T: AsRef<[u8]>> Iterator for MkvFrameIterator<T> {
    type Item = Vec<u8>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut frame = Frame::default();
        while self.input.next_frame(&mut frame).unwrap() {
            if frame.track == self.video_track {
                return Some(frame.data);
            }
        }

        None
    }
}

/// Detects the container type (IVF or MKV) and returns the corresponding frame iterator.
fn create_vpx_frame_iterator(input: &[u8]) -> Box<dyn Iterator<Item = Cow<[u8]>> + '_> {
    if input.starts_with(&[0x1a, 0x45, 0xdf, 0xa3]) {
        Box::new(MkvFrameIterator::new(input).unwrap().map(Cow::Owned))
    } else {
        Box::new(IvfIterator::new(input).map(Cow::Borrowed))
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

    let mut output = if !args.multiple_output_files {
        args.output
            .as_ref()
            .map(|p| File::create(p).expect("error creating output file"))
    } else {
        None
    };

    let blocking_mode = if args.synchronous {
        BlockingMode::Blocking
    } else {
        BlockingMode::NonBlocking
    };

    let mut golden_iter = golden_md5s(&args.golden).into_iter();

    let gbm = match args.frame_memory {
        FrameMemoryType::Managed | FrameMemoryType::User => None,
        FrameMemoryType::Prime => {
            /// A simple wrapper for a GBM device node.
            pub struct GbmDevice(std::fs::File);

            impl AsFd for GbmDevice {
                fn as_fd(&self) -> BorrowedFd<'_> {
                    self.0.as_fd()
                }
            }
            impl drm::Device for GbmDevice {}

            /// Simple helper methods for opening a `Card`.
            impl GbmDevice {
                pub fn open<P: AsRef<Path>>(path: P) -> std::io::Result<Self> {
                    std::fs::OpenOptions::new()
                        .read(true)
                        .write(true)
                        .open(path)
                        .map(GbmDevice)
                }
            }

            let gbm_path = args
                .gbm_device
                .unwrap_or(PathBuf::from("/dev/dri/renderD128"));
            let gbm = GbmDevice::open(gbm_path)
                .and_then(gbm::Device::new)
                .expect("failed to create GBM device");

            Some(gbm)
        }
    };

    let display = match args.libva_device {
        Some(libva_device_path) => libva::Display::open_drm_display(libva_device_path.clone())
            .expect(&format!(
                "failed to open libva display {}",
                libva_device_path.display()
            )),
        None => libva::Display::open().expect("failed to open libva display"),
    };

    // The created `decoder` is turned into a `DynStatelessVideoDecoder` trait object. This allows
    // the same code to control the decoder no matter what codec or backend we are using.
    let (mut decoder, frame_iter) = match args.input_format {
        EncodedFormat::H264 => {
            let frame_iter = Box::new(NalIterator::<H264Nalu>::new(&input))
                as Box<dyn Iterator<Item = Cow<[u8]>>>;

            let decoder = StatelessDecoder::<H264, _>::new_vaapi(display, blocking_mode)
                .unwrap()
                .into_trait_object();

            (decoder, frame_iter)
        }
        EncodedFormat::VP8 => {
            let frame_iter = create_vpx_frame_iterator(&input);

            let decoder = StatelessDecoder::<Vp8, _>::new_vaapi(display, blocking_mode)
                .unwrap()
                .into_trait_object();

            (decoder, frame_iter)
        }
        EncodedFormat::VP9 => {
            let frame_iter = create_vpx_frame_iterator(&input);

            let decoder = StatelessDecoder::<Vp9, _>::new_vaapi(display, blocking_mode)
                .unwrap()
                .into_trait_object();

            (decoder, frame_iter)
        }
        EncodedFormat::H265 => {
            let frame_iter = Box::new(NalIterator::<H265Nalu>::new(&input))
                as Box<dyn Iterator<Item = Cow<[u8]>>>;

            let decoder = StatelessDecoder::<H265, _>::new_vaapi(display, blocking_mode)
                .unwrap()
                .into_trait_object();

            (decoder, frame_iter)
        }
        EncodedFormat::AV1 => {
            let frame_iter = create_vpx_frame_iterator(&input);

            let decoder = StatelessDecoder::<Av1, _>::new_vaapi(display, blocking_mode)
                .unwrap()
                .into_trait_object();

            (decoder, frame_iter)
        }
    };

    let mut md5_context = MD5Context::new();
    let mut output_filename_idx = 0;
    let need_per_frame_md5 = match args.compute_md5 {
        Some(Md5Computation::Frame) => true,
        _ => args.golden.is_some(),
    };

    let mut on_new_frame = |handle: DynDecodedHandle<BufferDescriptor>| {
        if args.output.is_some() || args.compute_md5.is_some() || args.golden.is_some() {
            handle.sync().unwrap();
            let picture = handle.dyn_picture();
            let mut handle = picture.dyn_mappable_handle().unwrap();
            let buffer_size = handle.image_size();
            let mut frame_data = vec![0; buffer_size];
            handle.read(&mut frame_data).unwrap();

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

            if args.golden.is_some() {
                assert_eq!(&frame_md5, &golden_iter.next().unwrap());
            }
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
                        .map(BufferDescriptor::Managed)
                        .collect()
                }
                FrameMemoryType::Prime => simple_playback_loop_prime_frames(
                    gbm.as_ref().unwrap(),
                    stream_info,
                    nb_frames,
                )?
                .into_iter()
                .map(BufferDescriptor::Dmabuf)
                .collect(),
                FrameMemoryType::User => {
                    simple_playback_loop_userptr_frames(stream_info, nb_frames)?
                        .into_iter()
                        .map(BufferDescriptor::User)
                        .collect()
                }
            })
        },
        args.output_format,
        blocking_mode,
    )
    .expect("error during playback loop");

    if let Some(Md5Computation::Stream) = args.compute_md5 {
        println!("{}", md5_context.flush());
    }
}
