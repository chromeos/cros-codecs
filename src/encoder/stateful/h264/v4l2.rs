// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::sync::Arc;

use v4l2r::controls::codec::VideoGopSize;
use v4l2r::controls::codec::VideoH264IPeriod;
use v4l2r::controls::codec::VideoH264Level;
use v4l2r::controls::codec::VideoH264MaxQp;
use v4l2r::controls::codec::VideoH264MinQp;
use v4l2r::controls::codec::VideoH264Profile;
use v4l2r::controls::codec::VideoPrependSpsPpsToIdr;
use v4l2r::device::Device;

use crate::backend::v4l2::encoder::CaptureBuffers;
use crate::backend::v4l2::encoder::ControlError;
use crate::backend::v4l2::encoder::EncoderCodec;
use crate::backend::v4l2::encoder::InitializationError;
use crate::backend::v4l2::encoder::OutputBufferHandle;
use crate::backend::v4l2::encoder::V4L2Backend;
use crate::codec::h264::parser::Level;
use crate::codec::h264::parser::Profile;
use crate::encoder::h264::EncoderConfig;
use crate::encoder::h264::H264;
use crate::encoder::stateful::StatefulEncoder;
use crate::encoder::PredictionStructure;
use crate::encoder::Tunings;
use crate::Fourcc;
use crate::Resolution;

const PIXEL_FORMAT_H264: v4l2r::PixelFormat = v4l2r::PixelFormat::from_fourcc(b"H264");

pub type V4L2H264Backend<Handle, CaptureBufferz> = V4L2Backend<Handle, CaptureBufferz, H264>;

pub type V4L2StatefulH264Encoder<Handle, CaptureBufferz> =
    StatefulEncoder<Handle, V4L2H264Backend<Handle, CaptureBufferz>>;

impl From<Level> for VideoH264Level {
    fn from(value: Level) -> Self {
        match value {
            Level::L1 => VideoH264Level::L1_0,
            Level::L1B => VideoH264Level::L1B,
            Level::L1_1 => VideoH264Level::L1_1,
            Level::L1_2 => VideoH264Level::L1_2,
            Level::L1_3 => VideoH264Level::L1_3,
            Level::L2_0 => VideoH264Level::L2_0,
            Level::L2_1 => VideoH264Level::L2_1,
            Level::L2_2 => VideoH264Level::L2_2,
            Level::L3 => VideoH264Level::L3_0,
            Level::L3_1 => VideoH264Level::L3_1,
            Level::L3_2 => VideoH264Level::L3_2,
            Level::L4 => VideoH264Level::L4_0,
            Level::L4_1 => VideoH264Level::L4_1,
            Level::L4_2 => VideoH264Level::L4_2,
            Level::L5 => VideoH264Level::L5_0,
            Level::L5_1 => VideoH264Level::L5_1,
            Level::L5_2 => VideoH264Level::L5_2,
            Level::L6 => VideoH264Level::L6_0,
            Level::L6_1 => VideoH264Level::L6_1,
            Level::L6_2 => VideoH264Level::L6_2,
        }
    }
}

impl From<Profile> for VideoH264Profile {
    fn from(value: Profile) -> Self {
        match value {
            Profile::Baseline => Self::Baseline,
            Profile::Main => Self::Main,
            Profile::Extended => Self::Extended,
            Profile::High => Self::High,
            Profile::High10 => Self::High10,
            Profile::High422P => Self::High422,
        }
    }
}

impl<Handle, CaptureBufferz> EncoderCodec for V4L2H264Backend<Handle, CaptureBufferz>
where
    Handle: OutputBufferHandle,
    CaptureBufferz: CaptureBuffers,
{
    fn apply_tunings(device: &Device, tunings: &Tunings) -> Result<(), ControlError> {
        let min_qp = VideoH264MinQp(tunings.min_quality.clamp(1, 51) as i32);
        Self::apply_ctrl(device, "h264 min qp", min_qp)?;

        let max_qp = VideoH264MaxQp(tunings.max_quality.clamp(1, 51) as i32);
        Self::apply_ctrl(device, "h264 max qp", max_qp)?;

        Ok(())
    }
}

impl<Handle, CaptureBufferz> V4L2H264Backend<Handle, CaptureBufferz>
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
                Self::apply_ctrl(&device, "h264 i period", VideoH264IPeriod(limit))?;
            }
        }

        Self::apply_ctrl(
            &device,
            "prepend SPS PPS to IDR",
            VideoPrependSpsPpsToIdr(true),
        )?;
        Self::apply_ctrl(
            &device,
            "h264 profile",
            VideoH264Profile::from(config.profile),
        )?;
        Self::apply_ctrl(&device, "h264 level", VideoH264Level::from(config.level))?;

        Self::create(
            device,
            capture_buffers,
            fourcc,
            coded_size,
            config.resolution,
            PIXEL_FORMAT_H264,
            tunings,
        )
    }
}

impl<Handle, CaptureBufferz> V4L2StatefulH264Encoder<Handle, CaptureBufferz>
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
            V4L2H264Backend::new(device, capture_buffers, config, fourcc, coded_size, tunings)?,
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

    use crate::backend::v4l2::encoder::tests::find_device_with_capture;
    use crate::backend::v4l2::encoder::tests::perform_v4l2_encoder_dmabuf_test;
    use crate::backend::v4l2::encoder::tests::perform_v4l2_encoder_mmap_test;
    use crate::backend::v4l2::encoder::tests::v4l2_format_to_frame_layout;
    use crate::backend::v4l2::encoder::tests::BoPoolAllocator;
    use crate::backend::v4l2::encoder::tests::GbmDevice;
    use crate::backend::v4l2::encoder::MmapingCapture;
    use crate::encoder::simple_encode_loop;
    use crate::encoder::tests::userptr_test_frame_generator;
    use crate::encoder::RateControl;
    use crate::utils::DmabufFrame;
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

        let device = find_device_with_capture(PIXEL_FORMAT_H264).expect("no H264 encoder found");
        let device = Device::open(&device, DeviceConfig::new().non_blocking_dqbuf()).expect("open");
        let device = Arc::new(device);

        let mut encoder = V4L2StatefulH264Encoder::new(
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
        let buffer_size = format
            .plane_fmt
            .iter()
            .map(|plane| plane.sizeimage)
            .max()
            .unwrap() as usize;
        let mut frame_producer = userptr_test_frame_generator(FRAME_COUNT, layout, buffer_size);

        simple_encode_loop(&mut encoder, &mut frame_producer, |coded| {
            bitstream.extend(coded.bitstream)
        })
        .expect("encode loop");

        let write_to_file = std::option_env!("CROS_CODECS_TEST_WRITE_TO_FILE") == Some("true");
        if write_to_file {
            use std::io::Write;
            let mut out = std::fs::File::create("test_v4l2_encoder_userptr.h264").unwrap();
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

        let device = find_device_with_capture(PIXEL_FORMAT_H264).expect("no H264 encoder found");
        let device = Device::open(&device, DeviceConfig::new().non_blocking_dqbuf()).expect("open");
        let device = Arc::new(device);

        let encoder = V4L2StatefulH264Encoder::new(
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

        let mut bitstream = Vec::new();
        perform_v4l2_encoder_mmap_test(FRAME_COUNT, encoder, |coded| {
            bitstream.extend(coded.bitstream)
        });

        let write_to_file = std::option_env!("CROS_CODECS_TEST_WRITE_TO_FILE") == Some("true");
        if write_to_file {
            use std::io::Write;
            let mut out = std::fs::File::create("test_v4l2_encoder_mmap.h264").unwrap();
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

        let device = find_device_with_capture(PIXEL_FORMAT_H264).expect("no H264 encoder found");
        let device = Device::open(&device, DeviceConfig::new().non_blocking_dqbuf()).expect("open");
        let device = Arc::new(device);

        let gbm = GbmDevice::open(PathBuf::from("/dev/dri/renderD128"))
            .and_then(gbm::Device::new)
            .expect("failed to create GBM device");

        let gbm = Arc::new(gbm);

        let encoder = V4L2StatefulH264Encoder::<DmabufFrame, _>::new(
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

        perform_v4l2_encoder_dmabuf_test(
            CODED_SIZE,
            VISIBLE_SIZE,
            FRAME_COUNT,
            gbm,
            encoder,
            |coded| bitstream.extend(coded.bitstream),
        );

        let write_to_file = std::option_env!("CROS_CODECS_TEST_WRITE_TO_FILE") == Some("true");
        if write_to_file {
            use std::io::Write;
            let mut out = std::fs::File::create("test_v4l2_encoder_dmabuf.h264").unwrap();
            out.write_all(&bitstream).unwrap();
            out.flush().unwrap();
        }
    }
}
