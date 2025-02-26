// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::cell::RefCell;
use std::rc::Rc;

use v4l2r::bindings::v4l2_ctrl_h264_pps;
use v4l2r::bindings::v4l2_ctrl_h264_scaling_matrix;
use v4l2r::bindings::v4l2_ctrl_h264_sps;
use v4l2r::controls::codec::H264Pps;
use v4l2r::controls::codec::H264ScalingMatrix;
use v4l2r::controls::codec::H264Sps;
use v4l2r::controls::SafeExtControl;

use crate::backend::v4l2::decoder::stateless::V4l2Picture;
use crate::backend::v4l2::decoder::stateless::V4l2StatelessDecoderBackend;
use crate::backend::v4l2::decoder::stateless::V4l2StatelessDecoderHandle;
use crate::backend::v4l2::decoder::V4l2StreamInfo;
use crate::backend::v4l2::decoder::ADDITIONAL_REFERENCE_FRAME_BUFFER;
use crate::codec::h264::dpb::Dpb;
use crate::codec::h264::dpb::DpbEntry;
use crate::codec::h264::parser::Pps;
use crate::codec::h264::parser::Slice;
use crate::codec::h264::parser::SliceHeader;
use crate::codec::h264::parser::Sps;
use crate::codec::h264::picture::PictureData;
use crate::decoder::stateless::h264::StatelessH264DecoderBackend;
use crate::decoder::stateless::h264::H264;
use crate::decoder::stateless::NewPictureError;
use crate::decoder::stateless::NewPictureResult;
use crate::decoder::stateless::StatelessBackendResult;
use crate::decoder::stateless::StatelessDecoder;
use crate::decoder::stateless::StatelessDecoderBackend;
use crate::decoder::stateless::StatelessDecoderBackendPicture;
use crate::decoder::BlockingMode;
use crate::decoder::DecodedHandle;
use crate::device::v4l2::stateless::controls::h264::V4l2CtrlH264DecodeMode;
use crate::device::v4l2::stateless::controls::h264::V4l2CtrlH264DecodeParams;
use crate::device::v4l2::stateless::controls::h264::V4l2CtrlH264DpbEntry;
use crate::device::v4l2::stateless::controls::h264::V4l2CtrlH264StartCode;
use crate::video_frame::VideoFrame;
use crate::DecodedFormat;
use crate::Fourcc;
use crate::Rect;
use crate::Resolution;

impl V4l2StreamInfo for &Rc<Sps> {
    fn min_num_frames(&self) -> usize {
        self.max_dpb_frames() + ADDITIONAL_REFERENCE_FRAME_BUFFER
    }

    fn coded_size(&self) -> Resolution {
        Resolution::from((self.width(), self.height()))
    }

    fn visible_rect(&self) -> Rect {
        let rect = self.visible_rectangle();

        Rect { x: rect.min.x, y: rect.min.y, width: rect.max.x, height: rect.max.y }
    }
}

impl<V: VideoFrame> StatelessDecoderBackendPicture<H264> for V4l2StatelessDecoderBackend<V> {
    type Picture = Rc<RefCell<V4l2Picture<V>>>;
}

impl<V: VideoFrame> StatelessH264DecoderBackend for V4l2StatelessDecoderBackend<V> {
    fn new_sequence(&mut self, sps: &Rc<Sps>) -> StatelessBackendResult<()> {
        const MB_UNIT: u32 = 16;
        const MAP_UNIT: u32 = 16;
        const MAX_FRAMES_IN_PIPELINE: usize = 4;

        let resolution = Resolution::from((
            (sps.pic_width_in_mbs_minus1 + 1) as u32 * MB_UNIT,
            (sps.pic_height_in_map_units_minus1 + 1) as u32 * MAP_UNIT,
        ));

        let visible_rect = Rect::from((
            (sps.visible_rectangle().min.x, sps.visible_rectangle().min.y),
            (sps.visible_rectangle().max.x, sps.visible_rectangle().max.y),
        ));

        self.stream_info.format = DecodedFormat::MM21;
        self.stream_info.display_resolution = Resolution::from(visible_rect);
        self.stream_info.coded_resolution = resolution.clone();
        self.stream_info.min_num_frames = sps.max_dpb_frames() + MAX_FRAMES_IN_PIPELINE;

        self.device.initialize_queues(
            Fourcc::from(b"S264"),
            resolution,
            visible_rect,
            self.stream_info.min_num_frames as u32,
        )?;
        Ok(())
    }

    fn new_picture(
        &mut self,
        timestamp: u64,
        alloc_cb: &mut dyn FnMut() -> Option<
            <<Self as StatelessDecoderBackend>::Handle as DecodedHandle>::Frame,
        >,
    ) -> NewPictureResult<Self::Picture> {
        let frame = alloc_cb().ok_or(NewPictureError::OutOfOutputBuffers)?;
        let request_buffer = match self.device.alloc_request(timestamp, frame) {
            Ok(buffer) => buffer,
            _ => return Err(NewPictureError::OutOfOutputBuffers),
        };

        let picture = Rc::new(RefCell::new(V4l2Picture::new(request_buffer.clone())));
        request_buffer
            .as_ref()
            .borrow_mut()
            .set_picture_ref(Rc::<RefCell<V4l2Picture<V>>>::downgrade(&picture));
        Ok(picture)
    }

    fn new_field_picture(&mut self, _: u64, _: &Self::Handle) -> NewPictureResult<Self::Picture> {
        todo!()
    }

    fn start_picture(
        &mut self,
        picture: &mut Self::Picture,
        picture_data: &PictureData,
        sps: &Sps,
        pps: &Pps,
        dpb: &Dpb<Self::Handle>,
        slice_header: &SliceHeader,
    ) -> StatelessBackendResult<()> {
        let mut dpb_entries = Vec::<V4l2CtrlH264DpbEntry>::new();
        let mut ref_pictures = Vec::<Rc<RefCell<V4l2Picture<V>>>>::new();
        for entry in dpb.entries() {
            let ref_picture = match &entry.reference {
                Some(handle) => handle.picture.clone(),
                None => todo!(),
            };
            dpb_entries.push(V4l2CtrlH264DpbEntry {
                timestamp: ref_picture.borrow().timestamp(),
                pic: entry.pic.clone(),
            });
            ref_pictures.push(ref_picture);
        }

        let mut h264_decode_params = V4l2CtrlH264DecodeParams::new();
        let h264_sps = SafeExtControl::<H264Sps>::from(v4l2_ctrl_h264_sps::from(sps));
        let h264_pps = SafeExtControl::<H264Pps>::from(v4l2_ctrl_h264_pps::from(pps));
        let h264_scaling_matrix =
            SafeExtControl::<H264ScalingMatrix>::from(v4l2_ctrl_h264_scaling_matrix::from(pps));

        h264_decode_params
            .set_picture_data(picture_data)
            .set_dpb_entries(dpb_entries)
            .set_slice_header(slice_header);
        let mut picture = picture.borrow_mut();

        let request = picture.request();
        let mut request = request.as_ref().borrow_mut();
        request
            .ioctl(h264_sps)
            .ioctl(h264_pps)
            .ioctl(h264_scaling_matrix)
            .ioctl(&h264_decode_params)
            .ioctl(V4l2CtrlH264DecodeMode::FrameBased)
            .ioctl(V4l2CtrlH264StartCode::AnnexB);
        picture.set_ref_pictures(ref_pictures);
        Ok(())
    }

    fn decode_slice(
        &mut self,
        picture: &mut Self::Picture,
        slice: &Slice,
        _: &Sps,
        _: &Pps,
        _: &[&DpbEntry<Self::Handle>],
        _: &[&DpbEntry<Self::Handle>],
    ) -> StatelessBackendResult<()> {
        const START_CODE: [u8; 3] = [0, 0, 1];

        let request = picture.borrow_mut().request();
        let mut request = request.as_ref().borrow_mut();

        request.write(&START_CODE);
        request.write(slice.nalu.as_ref());
        Ok(())
    }

    fn submit_picture(&mut self, picture: Self::Picture) -> StatelessBackendResult<Self::Handle> {
        let request = picture.borrow_mut().request();
        let mut request = request.as_ref().borrow_mut();
        request.submit();
        Ok(V4l2StatelessDecoderHandle {
            picture: picture.clone(),
            stream_info: self.stream_info.clone(),
        })
    }
}

impl<V: VideoFrame> StatelessDecoder<H264, V4l2StatelessDecoderBackend<V>> {
    // Creates a new instance of the decoder using the v4l2 backend.
    pub fn new_v4l2(blocking_mode: BlockingMode) -> Self {
        Self::new(V4l2StatelessDecoderBackend::new(), blocking_mode)
            .expect("Failed to create v4l2 stateless decoder backend")
    }
}
