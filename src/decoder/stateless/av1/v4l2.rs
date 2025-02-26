// Copyright 2025 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::cell::RefCell;
use std::rc::Rc;

use crate::backend::v4l2::decoder::stateless::V4l2Picture;
use crate::backend::v4l2::decoder::stateless::V4l2StatelessDecoderBackend;
use crate::codec::av1::parser::FrameHeaderObu;
use crate::codec::av1::parser::StreamInfo;
use crate::codec::av1::parser::TileGroupObu;
use crate::codec::av1::parser::NUM_REF_FRAMES;
use crate::decoder::stateless::av1::Av1;
use crate::decoder::stateless::av1::StatelessAV1DecoderBackend;
use crate::decoder::stateless::NewPictureResult;
use crate::decoder::stateless::NewStatelessDecoderError;
use crate::decoder::stateless::StatelessBackendResult;
use crate::decoder::stateless::StatelessDecoder;
use crate::decoder::stateless::StatelessDecoderBackendPicture;
use crate::decoder::BlockingMode;
use crate::video_frame::VideoFrame;

impl<V: VideoFrame> StatelessDecoderBackendPicture<Av1> for V4l2StatelessDecoderBackend<V> {
    type Picture = Rc<RefCell<V4l2Picture<V>>>;
}

impl<V: VideoFrame> StatelessAV1DecoderBackend for V4l2StatelessDecoderBackend<V> {
    fn change_stream_info(&mut self, _stream_info: &StreamInfo) -> StatelessBackendResult<()> {
        todo!()
    }

    fn new_picture(
        &mut self,
        _hdr: &FrameHeaderObu,
        _timestamp: u64,
        _alloc_cb: &mut dyn FnMut() -> Option<V>,
    ) -> NewPictureResult<Self::Picture> {
        todo!()
    }

    fn begin_picture(
        &mut self,
        _picture: &mut Self::Picture,
        _stream_info: &StreamInfo,
        _hdr: &FrameHeaderObu,
        _reference_frames: &[Option<Self::Handle>; NUM_REF_FRAMES],
    ) -> StatelessBackendResult<()> {
        todo!()
    }

    fn decode_tile_group(
        &mut self,
        _picture: &mut Self::Picture,
        _tile_group: TileGroupObu,
    ) -> crate::decoder::stateless::StatelessBackendResult<()> {
        todo!()
    }

    fn submit_picture(&mut self, _picture: Self::Picture) -> StatelessBackendResult<Self::Handle> {
        todo!()
    }
}

impl<V: VideoFrame> StatelessDecoder<Av1, V4l2StatelessDecoderBackend<V>> {
    pub fn new_v4l2(blocking_mode: BlockingMode) -> Result<Self, NewStatelessDecoderError> {
        Self::new(V4l2StatelessDecoderBackend::new()?, blocking_mode)
    }
}
