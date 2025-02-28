// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;

use crate::backend::v4l2::decoder::V4l2StreamInfo;
use crate::decoder::stateless::NewStatelessDecoderError;
use crate::decoder::stateless::StatelessBackendResult;
use crate::decoder::stateless::StatelessDecoderBackend;
use crate::decoder::DecodedHandle;
use crate::decoder::StreamInfo;
use crate::device::v4l2::stateless::device::V4l2Device;
use crate::device::v4l2::stateless::request::V4l2Request;
use crate::video_frame::VideoFrame;
use crate::DecodedFormat;
use crate::Fourcc;
use crate::Resolution;

pub struct V4l2Picture<V: VideoFrame> {
    request: Rc<RefCell<V4l2Request<V>>>,
    // To properly decode stream while output and capture queues
    // are processed independently it's required for v4l2 backend
    // to maintain DPB buffer recycling. The following vector
    // is used to prevent reference pictures to be reused while
    // current picture is still being decoded.
    ref_pictures: Option<Vec<Rc<RefCell<V4l2Picture<V>>>>>,
}

impl<V: VideoFrame> V4l2Picture<V> {
    pub fn new(request: Rc<RefCell<V4l2Request<V>>>) -> Self {
        Self { request, ref_pictures: None }
    }
    pub fn video_frame(&self) -> Arc<V> {
        self.request.as_ref().borrow().result().capture_buffer.borrow().frame.clone()
    }
    pub fn timestamp(&self) -> u64 {
        self.request.as_ref().borrow().timestamp()
    }
    pub fn set_ref_pictures(
        &mut self,
        ref_pictures: Vec<Rc<RefCell<V4l2Picture<V>>>>,
    ) -> &mut Self {
        self.ref_pictures = Some(ref_pictures);
        self
    }
    pub fn sync(&mut self) -> &mut Self {
        self.request.as_ref().borrow_mut().sync();
        self.ref_pictures = None;
        self
    }
    pub fn request(&mut self) -> Rc<RefCell<V4l2Request<V>>> {
        self.request.clone()
    }
    pub fn drop_references(&mut self) {
        self.ref_pictures = None;
    }
}

pub struct V4l2StatelessDecoderHandle<V: VideoFrame> {
    pub picture: Rc<RefCell<V4l2Picture<V>>>,
    pub stream_info: StreamInfo,
}

impl<V: VideoFrame> Clone for V4l2StatelessDecoderHandle<V> {
    fn clone(&self) -> Self {
        Self { picture: Rc::clone(&self.picture), stream_info: self.stream_info.clone() }
    }
}

impl<V: VideoFrame> DecodedHandle for V4l2StatelessDecoderHandle<V> {
    type Frame = V;

    fn video_frame(&self) -> Arc<Self::Frame> {
        self.picture.borrow().video_frame()
    }

    fn coded_resolution(&self) -> Resolution {
        self.stream_info.coded_resolution.clone()
    }

    fn display_resolution(&self) -> Resolution {
        self.stream_info.display_resolution.clone()
    }

    fn timestamp(&self) -> u64 {
        self.picture.borrow().timestamp()
    }

    fn sync(&self) -> anyhow::Result<()> {
        self.picture.borrow_mut().sync();
        Ok(())
    }

    fn is_ready(&self) -> bool {
        todo!();
    }
}

pub struct V4l2StatelessDecoderBackend<V: VideoFrame> {
    pub device: V4l2Device<V>,
    pub stream_info: StreamInfo,
    pub frame_counter: u64,
}

impl<V: VideoFrame> V4l2StatelessDecoderBackend<V> {
    pub fn new() -> Result<Self, NewStatelessDecoderError> {
        Ok(Self {
            device: V4l2Device::new()?,
            stream_info: StreamInfo {
                format: DecodedFormat::I420,
                min_num_frames: 0,
                coded_resolution: Resolution::from((0, 0)),
                display_resolution: Resolution::from((0, 0)),
            },
            frame_counter: 0,
        })
    }

    pub(crate) fn new_sequence<StreamData>(
        &mut self,
        stream_params: &StreamData,
        fourcc: Fourcc,
    ) -> StatelessBackendResult<()>
    where
        for<'a> &'a StreamData: V4l2StreamInfo,
    {
        let coded_resolution = stream_params.coded_size().clone();
        let min_num_frames = stream_params.min_num_frames();

        // TODO: Query the driver for the format
        self.stream_info.format = DecodedFormat::MM21;
        self.stream_info.display_resolution = Resolution::from(stream_params.visible_rect());
        self.stream_info.coded_resolution = coded_resolution;
        self.stream_info.min_num_frames = min_num_frames;

        Ok(self.device.initialize_queues(fourcc, coded_resolution, min_num_frames as u32)?)
    }
}

impl<V: VideoFrame> StatelessDecoderBackend for V4l2StatelessDecoderBackend<V> {
    type Handle = V4l2StatelessDecoderHandle<V>;

    fn stream_info(&self) -> Option<&StreamInfo> {
        // TODO
        Some(&self.stream_info)
    }

    fn reset_backend(&mut self) -> anyhow::Result<()> {
        Ok(self.device.reset_queues()?)
    }
}
