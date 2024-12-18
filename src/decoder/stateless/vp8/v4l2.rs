// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::cell::RefCell;
use std::rc::Rc;

use v4l2r::bindings::v4l2_ctrl_vp8_frame;
use v4l2r::controls::SafeExtControl;

use crate::backend::v4l2::decoder::stateless::BackendHandle;
use crate::backend::v4l2::decoder::stateless::V4l2Picture;
use crate::backend::v4l2::decoder::stateless::V4l2StatelessDecoderBackend;
use crate::backend::v4l2::decoder::stateless::V4l2StatelessDecoderHandle;
use crate::backend::v4l2::decoder::V4l2StreamInfo;

use crate::codec::vp8::parser::Header;
use crate::codec::vp8::parser::MbLfAdjustments;
use crate::codec::vp8::parser::Segmentation;

use crate::decoder::stateless::vp8::StatelessVp8DecoderBackend;
use crate::decoder::stateless::vp8::Vp8;

use crate::decoder::stateless::NewPictureError;
use crate::decoder::stateless::NewPictureResult;
use crate::decoder::stateless::StatelessBackendResult;
use crate::decoder::stateless::StatelessDecoder;
use crate::decoder::stateless::StatelessDecoderBackendPicture;
use crate::decoder::BlockingMode;

use crate::device::v4l2::stateless::controls::vp8::V4l2CtrlVp8FrameParams;

use crate::Resolution;

/// The number of frames to allocate for this codec. Same as GStreamer's vavp8dec.
const NUM_FRAMES: usize = 7;

impl V4l2StreamInfo for &Header {
    fn min_num_frames(&self) -> usize {
        NUM_FRAMES
    }

    fn coded_size(&self) -> Resolution {
        Resolution::from((self.width as u32, self.height as u32))
    }

    fn visible_rect(&self) -> ((u32, u32), (u32, u32)) {
        ((0, 0), self.coded_size().into())
    }
}

impl StatelessDecoderBackendPicture<Vp8> for V4l2StatelessDecoderBackend {
    type Picture = Rc<RefCell<V4l2Picture>>;
}

impl StatelessVp8DecoderBackend for V4l2StatelessDecoderBackend {
    fn new_sequence(&mut self, _: &Header) -> StatelessBackendResult<()> {
        Ok(())
    }

    fn new_picture(&mut self, timestamp: u64) -> NewPictureResult<Self::Picture> {
        let request_buffer = match self.device.alloc_request(timestamp) {
            Ok(buffer) => buffer,
            _ => return Err(NewPictureError::OutOfOutputBuffers),
        };
        Ok(Rc::new(RefCell::new(V4l2Picture::new(request_buffer))))
    }

    fn submit_picture(
        &mut self,
        picture: Self::Picture,
        hdr: &Header,
        _: &Option<Self::Handle>,
        _: &Option<Self::Handle>,
        _: &Option<Self::Handle>,
        _: &[u8],
        segmentation: &Segmentation,
        mb_lf_adjust: &MbLfAdjustments,
    ) -> StatelessBackendResult<Self::Handle> {
        let mut vp8_frame_params = V4l2CtrlVp8FrameParams::new();

        vp8_frame_params
            .set_loop_filter_params(hdr, mb_lf_adjust)
            .set_quantization_params(hdr)
            .set_segmentation_params(segmentation)
            .set_entropy_params(hdr);

        let handle = Rc::new(RefCell::new(BackendHandle {
            picture: picture.clone(),
        }));
        println!(
            "{:<20} {:?}\n",
            "submit_picture",
            picture.borrow().timestamp()
        );
        picture.borrow_mut().request().submit();
        Ok(V4l2StatelessDecoderHandle { handle })
    }
}

impl StatelessDecoder<Vp8, V4l2StatelessDecoderBackend> {
    // Creates a new instance of the decoder using the v4l2 backend.
    pub fn new_v4l2(blocking_mode: BlockingMode) -> Self {
        Self::new(V4l2StatelessDecoderBackend::new(), blocking_mode)
            .expect("Failed to create v4l2 stateless decoder backend")
    }
}
