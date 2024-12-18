// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::sync::Arc;

use v4l2r::controls::codec::VideoGopSize;
use v4l2r::controls::codec::VideoVP8Profile;
use v4l2r::controls::codec::VideoVPXMaxQp;
use v4l2r::controls::codec::VideoVPXMinQp;
use v4l2r::device::Device;

use crate::backend::v4l2::encoder::CaptureBuffers;
use crate::backend::v4l2::encoder::ControlError;
use crate::backend::v4l2::encoder::EncoderCodec;
use crate::backend::v4l2::encoder::InitializationError;
use crate::backend::v4l2::encoder::OutputBufferHandle;
use crate::backend::v4l2::encoder::V4L2Backend;
use crate::encoder::stateful::StatefulEncoder;
use crate::encoder::vp8::EncoderConfig;
use crate::encoder::vp8::VP8;
use crate::encoder::PredictionStructure;
use crate::encoder::Tunings;
use crate::Fourcc;
use crate::Resolution;

const PIXEL_FORMAT_VP8: v4l2r::PixelFormat = v4l2r::PixelFormat::from_fourcc(b"VP80");

pub type V4L2VP8Backend<Handle, CaptureBufferz> = V4L2Backend<Handle, CaptureBufferz, VP8>;

pub type V4L2StatefulVP8Encoder<Handle, CaptureBufferz> =
    StatefulEncoder<Handle, V4L2VP8Backend<Handle, CaptureBufferz>>;

impl<Handle, CaptureBufferz> EncoderCodec for V4L2VP8Backend<Handle, CaptureBufferz>
where
    Handle: OutputBufferHandle,
    CaptureBufferz: CaptureBuffers,
{
    fn apply_tunings(device: &Device, tunings: &Tunings) -> Result<(), ControlError> {
        let min_qp = VideoVPXMinQp(tunings.min_quality.clamp(0, 127) as i32);
        Self::apply_ctrl(device, "vpx min qp", min_qp)?;

        let max_qp = VideoVPXMaxQp(tunings.max_quality.clamp(0, 127) as i32);
        Self::apply_ctrl(device, "vpx max qp", max_qp)?;

        Ok(())
    }
}

impl<Handle, CaptureBufferz> V4L2VP8Backend<Handle, CaptureBufferz>
where
    Handle: OutputBufferHandle,
    CaptureBufferz: CaptureBuffers,
{
    pub fn new(
        device: Arc<Device>,
        capture_buffers: CaptureBufferz,
        config: EncoderConfig,
        fourcc: Fourcc,
        coded_size: Resolution,
        tunings: Tunings,
    ) -> Result<Self, InitializationError> {
        match config.pred_structure {
            PredictionStructure::LowDelay { limit } => {
                let limit = limit as i32;

                Self::apply_ctrl(&device, "gop size", VideoGopSize(limit))?;
            }
        }

        // TODO: allow picking profile
        Self::apply_ctrl(&device, "vp8 profile", VideoVP8Profile::Profile0)?;

        Self::create(
            device,
            capture_buffers,
            fourcc,
            coded_size,
            config.resolution,
            PIXEL_FORMAT_VP8,
            tunings,
        )
    }
}

impl<Handle, CaptureBufferz> V4L2StatefulVP8Encoder<Handle, CaptureBufferz>
where
    Handle: OutputBufferHandle,
    CaptureBufferz: CaptureBuffers,
{
    pub fn new(
        device: Arc<Device>,
        capture_buffers: CaptureBufferz,
        config: EncoderConfig,
        fourcc: Fourcc,
        coded_size: Resolution,
        tunings: Tunings,
    ) -> Result<Self, InitializationError> {
        Ok(Self::create(
            tunings.clone(),
            V4L2VP8Backend::new(device, capture_buffers, config, fourcc, coded_size, tunings)?,
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::path::PathBuf;
    use std::sync::Arc;

    use v4l2r::device::Device;
    use v4l2r::device::DeviceConfig;

    use crate::backend::v4l2::encoder::find_device_with_capture;
    use crate::backend::v4l2::encoder::tests::perform_v4l2_encoder_dmabuf_test;
    use crate::backend::v4l2::encoder::tests::perform_v4l2_encoder_mmap_test;
    use crate::backend::v4l2::encoder::tests::BoPoolAllocator;
    use crate::backend::v4l2::encoder::tests::GbmDevice;
    use crate::backend::v4l2::encoder::v4l2_format_to_frame_layout;
    use crate::backend::v4l2::encoder::MmapingCapture;
    use crate::encoder::simple_encode_loop;
    use crate::encoder::tests::userptr_test_frame_generator;
    use crate::encoder::RateControl;
    use crate::utils::DmabufFrame;
    use crate::utils::IvfFileHeader;
    use crate::utils::IvfFrameHeader;
    use crate::Resolution;

    #[ignore]
    // Ignore this test by default as it requires v4l2m2m-compatible hardware.
    #[test]
    fn test_v4l2_encoder_userptr() {
        const VISIBLE_SIZE: Resolution = Resolution {
            width: 500,
            height: 500,
        };
        const CODED_SIZE: Resolution = Resolution {
            width: 512,
            height: 512,
        };
        const FRAME_COUNT: u64 = 100;

        let _ = env_logger::try_init();

        let device = find_device_with_capture(PIXEL_FORMAT_VP8).expect("no VP8 encoder found");
        let device = Device::open(&device, DeviceConfig::new().non_blocking_dqbuf()).expect("open");
        let device = Arc::new(device);

        let mut encoder = V4L2StatefulVP8Encoder::new(
            device,
            MmapingCapture,
            EncoderConfig {
                resolution: VISIBLE_SIZE,
                ..Default::default()
            },
            Fourcc::from(b"NM12"),
            CODED_SIZE,
            Tunings {
                rate_control: RateControl::ConstantBitrate(400_000),
                ..Default::default()
            },
        )
        .unwrap();

        let format: v4l2r::Format = encoder.backend().output_format().unwrap();
        let layout = v4l2_format_to_frame_layout(&format);

        let mut bitstream = Vec::new();

        let file_header = IvfFileHeader::new(
            IvfFileHeader::CODEC_VP8,
            VISIBLE_SIZE.width as u16,
            VISIBLE_SIZE.height as u16,
            30,
            FRAME_COUNT as u32,
        );

        file_header.writo_into(&mut bitstream).unwrap();

        let buffer_size = format
            .plane_fmt
            .iter()
            .map(|plane| plane.sizeimage)
            .max()
            .unwrap() as usize;
        let mut frame_producer = userptr_test_frame_generator(FRAME_COUNT, layout, buffer_size);

        simple_encode_loop(&mut encoder, &mut frame_producer, |coded| {
            // Skip Duck IVF frame header from v4l2 stream, and replace with ours with correct
            // timestamp.
            let frame_bitstream = &coded.bitstream[4 + 8..];

            let header = IvfFrameHeader {
                timestamp: coded.metadata.timestamp,
                frame_size: frame_bitstream.len() as u32,
            };

            header.writo_into(&mut bitstream).unwrap();
            bitstream.extend(frame_bitstream);
        })
        .expect("encode loop");

        let write_to_file = std::option_env!("CROS_CODECS_TEST_WRITE_TO_FILE") == Some("true");
        if write_to_file {
            use std::io::Write;
            let mut out = std::fs::File::create("test_v4l2_encoder_userptr.vp8.ivf").unwrap();
            out.write_all(&bitstream).unwrap();
            out.flush().unwrap();
        }
    }

    #[ignore]
    // Ignore this test by default as it requires v4l2m2m-compatible hardware.
    #[test]
    fn test_v4l2_encoder_mmap() {
        const VISIBLE_SIZE: Resolution = Resolution {
            width: 500,
            height: 500,
        };
        const CODED_SIZE: Resolution = Resolution {
            width: 512,
            height: 512,
        };
        const FRAME_COUNT: u64 = 100;

        let _ = env_logger::try_init();

        let device = find_device_with_capture(PIXEL_FORMAT_VP8).expect("no VP8 encoder found");
        let device = Device::open(&device, DeviceConfig::new().non_blocking_dqbuf()).expect("open");
        let device = Arc::new(device);

        let encoder = V4L2StatefulVP8Encoder::new(
            device,
            MmapingCapture,
            EncoderConfig {
                resolution: VISIBLE_SIZE,
                ..Default::default()
            },
            Fourcc::from(b"NM12"),
            CODED_SIZE,
            Tunings {
                rate_control: RateControl::ConstantBitrate(2_000_000),
                ..Default::default()
            },
        )
        .unwrap();

        let mut bitstream = Vec::new();

        let file_header = IvfFileHeader::new(
            IvfFileHeader::CODEC_VP8,
            VISIBLE_SIZE.width as u16,
            VISIBLE_SIZE.height as u16,
            30,
            FRAME_COUNT as u32,
        );

        file_header.writo_into(&mut bitstream).unwrap();

        perform_v4l2_encoder_mmap_test(FRAME_COUNT, encoder, |coded| {
            // Skip Duck IVF frame header from v4l2 stream, and replace with ours with correct
            // timestamp.
            let frame_bitstream = &coded.bitstream[4 + 8..];

            let header = IvfFrameHeader {
                timestamp: coded.metadata.timestamp,
                frame_size: frame_bitstream.len() as u32,
            };

            header.writo_into(&mut bitstream).unwrap();
            bitstream.extend(frame_bitstream);
        });

        let write_to_file = std::option_env!("CROS_CODECS_TEST_WRITE_TO_FILE") == Some("true");
        if write_to_file {
            use std::io::Write;
            let mut out = std::fs::File::create("test_v4l2_encoder_mmap.vp8.ivf").unwrap();
            out.write_all(&bitstream).unwrap();
            out.flush().unwrap();
        }
    }

    #[ignore]
    // Ignore this test by default as it requires v4l2m2m-compatible hardware.
    #[test]
    fn test_v4l2_encoder_dmabuf() {
        const VISIBLE_SIZE: Resolution = Resolution {
            width: 500,
            height: 500,
        };
        const CODED_SIZE: Resolution = Resolution {
            width: 512,
            height: 512,
        };
        const FRAME_COUNT: u64 = 100;

        let _ = env_logger::try_init();

        let device = find_device_with_capture(PIXEL_FORMAT_VP8).expect("no VP8 encoder found");
        let device = Device::open(&device, DeviceConfig::new().non_blocking_dqbuf()).expect("open");
        let device = Arc::new(device);

        let gbm = GbmDevice::open(PathBuf::from("/dev/dri/renderD128"))
            .and_then(gbm::Device::new)
            .expect("failed to create GBM device");

        let gbm = Arc::new(gbm);

        let encoder = V4L2StatefulVP8Encoder::<DmabufFrame, _>::new(
            device.clone(),
            BoPoolAllocator::new(gbm.clone()),
            EncoderConfig {
                resolution: VISIBLE_SIZE,
                ..Default::default()
            },
            Fourcc::from(b"NV12"),
            CODED_SIZE,
            Tunings {
                framerate: 30,
                rate_control: RateControl::ConstantBitrate(400_000),
                ..Default::default()
            },
        )
        .unwrap();

        let mut bitstream = Vec::new();

        let file_header = IvfFileHeader::new(
            IvfFileHeader::CODEC_VP8,
            VISIBLE_SIZE.width as u16,
            VISIBLE_SIZE.height as u16,
            30,
            FRAME_COUNT as u32,
        );

        file_header.writo_into(&mut bitstream).unwrap();

        perform_v4l2_encoder_dmabuf_test(
            CODED_SIZE,
            VISIBLE_SIZE,
            FRAME_COUNT,
            gbm,
            encoder,
            |coded| {
                // Skip Duck IVF frame header from v4l2 stream, and replace with ours with correct
                // timestamp.
                let frame_bitstream = &coded.bitstream[4 + 8..];

                let header = IvfFrameHeader {
                    timestamp: coded.metadata.timestamp,
                    frame_size: frame_bitstream.len() as u32,
                };

                header.writo_into(&mut bitstream).unwrap();
                bitstream.extend(frame_bitstream);
            },
        );

        let write_to_file = std::option_env!("CROS_CODECS_TEST_WRITE_TO_FILE") == Some("true");
        if write_to_file {
            use std::io::Write;
            let mut out = std::fs::File::create("test_v4l2_encoder_dmabuf.vp8.ivf").unwrap();
            out.write_all(&bitstream).unwrap();
            out.flush().unwrap();
        }
    }
}
