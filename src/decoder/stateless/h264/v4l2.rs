// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::cell::RefCell;
use std::rc::Rc;

use v4l2r::bindings::v4l2_ctrl_h264_pps;
use v4l2r::bindings::v4l2_ctrl_h264_sps;
use v4l2r::controls::codec::H264Pps;
use v4l2r::controls::codec::H264Sps;
use v4l2r::controls::SafeExtControl;

use crate::backend::v4l2::decoder::stateless::BackendHandle;
use crate::backend::v4l2::decoder::stateless::V4l2Picture;
use crate::backend::v4l2::decoder::stateless::V4l2StatelessDecoderBackend;
use crate::backend::v4l2::decoder::stateless::V4l2StatelessDecoderHandle;
use crate::codec::h264::dpb::Dpb;
use crate::codec::h264::dpb::DpbEntry;
use crate::codec::h264::parser::Pps;
use crate::codec::h264::parser::Slice;
use crate::codec::h264::parser::SliceHeader;
use crate::codec::h264::parser::Sps;
use crate::codec::h264::picture::PictureData;
use crate::decoder::stateless::h264::StatelessH264DecoderBackend;
use crate::decoder::stateless::h264::H264;
use crate::decoder::stateless::StatelessBackendResult;
use crate::decoder::stateless::StatelessDecoder;
use crate::decoder::stateless::StatelessDecoderBackendPicture;
use crate::decoder::BlockingMode;
use crate::device::v4l2::stateless::controls::h264::V4l2CtrlH264DecodeMode;
use crate::device::v4l2::stateless::controls::h264::V4l2CtrlH264DecodeParams;
use crate::device::v4l2::stateless::controls::h264::V4l2CtrlH264DpbEntry;
//TODO use crate::device::v4l2::stateless::controls::h264::V4l2CtrlH264ScalingMatrix;
use crate::Resolution;

impl StatelessDecoderBackendPicture<H264> for V4l2StatelessDecoderBackend {
    type Picture = Rc<RefCell<V4l2Picture>>;
}

impl StatelessH264DecoderBackend for V4l2StatelessDecoderBackend {
    fn new_sequence(&mut self, sps: &Rc<Sps>) -> StatelessBackendResult<()> {
        let mb_unit = 16;
        let map_unit = 16;
        let resolution = Resolution::from((
            (sps.pic_width_in_mbs_minus1 + 1) * mb_unit,
            (sps.pic_height_in_map_units_minus1 + 1) * map_unit,
        ));
        self.device.set_resolution(resolution);
        Ok(())
    }

    fn new_picture(
        &mut self,
        _: &PictureData,
        timestamp: u64,
    ) -> StatelessBackendResult<Self::Picture> {
        Ok(Rc::new(RefCell::new(V4l2Picture::new(
            self.device.alloc_request(timestamp),
        ))))
    }

    fn new_field_picture(
        &mut self,
        _: &PictureData,
        _: u64,
        _: &Self::Handle,
    ) -> StatelessBackendResult<Self::Picture> {
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
        let mut ref_pictures = Vec::<Rc<RefCell<V4l2Picture>>>::new();
        for entry in dpb.entries() {
            let ref_picture = match &entry.handle {
                Some(handle) => handle.handle.borrow().picture.clone(),
                None => todo!(),
            };
            dpb_entries.push(V4l2CtrlH264DpbEntry {
                timestamp: ref_picture.borrow().timestamp(),
                pic: entry.pic.clone(),
            });
            ref_pictures.push(ref_picture);
        }
        //TODO let mut h264_scaling_matrix = V4l2CtrlH264ScalingMatrix::new();
        let mut h264_decode_params = V4l2CtrlH264DecodeParams::new();
        let h264_sps = SafeExtControl::<H264Sps>::from(v4l2_ctrl_h264_sps::from(sps));
        let h264_pps = SafeExtControl::<H264Pps>::from(v4l2_ctrl_h264_pps::from(pps));
        h264_decode_params
            .set_picture_data(picture_data)
            .set_dpb_entries(dpb_entries)
            .set_slice_header(slice_header);
        let mut picture = picture.borrow_mut();
        picture
            .request()
            .ioctl(h264_sps)
            .ioctl(h264_pps)
            //TODO.ioctl(&h264_scaling_matrix)
            .ioctl(&h264_decode_params)
            .ioctl(V4l2CtrlH264DecodeMode::FrameBased);
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
        picture.borrow().request().write(slice.nalu.as_ref());
        Ok(())
    }

    fn submit_picture(&mut self, picture: Self::Picture) -> StatelessBackendResult<Self::Handle> {
        let handle = Rc::new(RefCell::new(BackendHandle {
            picture: picture.clone(),
        }));
        picture.borrow().request().submit();
        Ok(V4l2StatelessDecoderHandle { handle })
    }
}

impl StatelessDecoder<H264, V4l2StatelessDecoderBackend> {
    // Creates a new instance of the decoder using the v4l2 backend.
    pub fn new_v4l2(blocking_mode: BlockingMode) -> Self {
        Self::new(V4l2StatelessDecoderBackend::new(), blocking_mode)
            .expect("Failed to create v4l2 stateless decoder backend")
    }
}
