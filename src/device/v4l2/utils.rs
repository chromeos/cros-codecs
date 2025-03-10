// Copyright 2025 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::ffi::c_char;
use std::path::PathBuf;
use std::sync::Arc;

use v4l2r::device::queue::Queue;
use v4l2r::device::{Device as VideoDevice, DeviceConfig};
use v4l2r::ioctl::Capability;
use v4l2r::nix::fcntl::{open, OFlag};
use v4l2r::nix::sys::stat::Mode;
use zerocopy::FromZeros;

const MAX_DEVICE_NO: usize = 128;

/// Enumerate V4L2 (video and media) devices on the system.
pub fn enumerate_devices() -> Option<(PathBuf, PathBuf)> {
    let decoder_device_prefix = "/dev/video";

    for dev_no in 0..MAX_DEVICE_NO {
        let video_device_path = PathBuf::from(format!("{}{}", decoder_device_prefix, dev_no));
        let video_device_config = DeviceConfig::new().non_blocking_dqbuf();

        let device = match VideoDevice::open(&video_device_path, video_device_config) {
            Ok(device) => Arc::new(device),
            Err(_) => continue,
        };

        if Queue::get_output_mplane_queue(device.clone()).is_err() {
            continue;
        }

        let caps = device.caps();
        if let Some(media_device_path) = find_media_device(caps) {
            log::info!(
                "Using video device {:?} with media device {:?}",
                video_device_path,
                media_device_path
            );
            return Some((video_device_path, media_device_path));
        }
    }
    None
}

/// A struct that contains information about media device.
///
/// See: https://docs.kernel.org/userspace-api/media/mediactl/media-ioc-device-info.html
#[repr(C)]
#[derive(FromZeros)]
pub struct MediaDeviceInfo {
    driver: [c_char; 16],
    model: [c_char; 32],
    serial: [c_char; 40],
    bus_info: [c_char; 32],
    media_version: u32,
    hw_revision: u32,
    driver_version: u32,
    reserved: [u32; 31],
}

const IOCTL_MEDIA_COMMAND: u8 = b'|';
pub const MEDIA_IOC_DEVICE_INFO: u8 = 0x00;
use nix::ioctl_readwrite;
ioctl_readwrite!(
    media_ioc_device_info,
    IOCTL_MEDIA_COMMAND,
    MEDIA_IOC_DEVICE_INFO,
    MediaDeviceInfo
);

fn parse_c_str_from_array(arr: &[c_char]) -> String {
    let bytes: Vec<u8> = arr.iter().map(|&c| c as u8).take_while(|&c| c != 0).collect();
    String::from_utf8_lossy(&bytes).trim().to_string()
}

/// Find media device according to the decode device's capabilities.
pub fn find_media_device(cap: &Capability) -> Option<PathBuf> {
    let media_device_prefix = "/dev/media";

    for dev_no in 0..MAX_DEVICE_NO {
        let media_device_path = PathBuf::from(format!("{}{}", media_device_prefix, dev_no));
        let media_device =
            match open(&media_device_path, OFlag::O_RDWR | OFlag::O_CLOEXEC, Mode::empty()) {
                Ok(media_device) => media_device,
                Err(_) => continue,
            };

        // MediaDeviceInfo is a struct which contains only fields for which 0 is a valid
        // bit pattern.
        let mut media_device_info = MediaDeviceInfo::new_zeroed();
        let media_device_info = unsafe {
            // SAFETY: This should be safe, as the `media_ioc_device_info` ioctl is called with
            //  `media_device` - a valid file descriptor  and `media_device_info` - a valid pointer
            //  to `MediaDeviceInfo` struct.
            match media_ioc_device_info(media_device, &mut media_device_info) {
                Ok(_) => Some(media_device_info),
                Err(_) => None,
            }
        };

        // Match the video device and the media controller by the |bus_info|
        // field. This works better than |driver| field if there are multiple
        // instances of the same decoder driver in the system
        if let Some(media_bus_info) =
            media_device_info.as_ref().map(|info| parse_c_str_from_array(&info.bus_info))
        {
            if !cap.bus_info.is_empty()
                && !media_bus_info.is_empty()
                && cap.bus_info == *media_bus_info
            {
                return Some(media_device_path);
            }
        }

        // Fall back to matching the video device and the media controller by the
        // driver field. The mtk-vcodec driver does not fill the card and bus fields
        // properly, so those won't work.
        if let Some(driver) =
            media_device_info.as_ref().map(|info| parse_c_str_from_array(&info.driver))
        {
            if !cap.bus_info.is_empty() && !driver.is_empty() && cap.driver == *driver {
                return Some(media_device_path);
            }
        }
    }
    None
}
