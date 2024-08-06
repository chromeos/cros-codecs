// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::cell::RefCell;
use std::rc::Rc;

use crate::decoder::stateless::PoolLayer;
use crate::decoder::stateless::StatelessCodec;
use crate::decoder::stateless::StatelessDecoderBackend;
use crate::decoder::stateless::TryFormat;
use crate::decoder::DecodedHandle;
use crate::decoder::DynHandle;
use crate::decoder::FramePool;
use crate::decoder::MappableHandle;
use crate::decoder::StreamInfo;
use crate::DecodedFormat;
use crate::Resolution;

use crate::device::v4l2::stateless::device::V4l2Device;
use crate::device::v4l2::stateless::request::V4l2Request;

pub struct V4l2Picture {
    request: V4l2Request,
    // To properly decode stream while output and capture queues
    // are processed independently it's required for v4l2 backend
    // to maintain DPB buffer recycling. The following vector
    // is used to prevent reference pictures to be reused while
    // current picture is still being decoded.
    // TODO: handle ref list inernally by V4l2Request.
    ref_pictures: Option<Vec<Rc<RefCell<V4l2Picture>>>>,
}

impl V4l2Picture {
    pub fn new(request: V4l2Request) -> Self {
        Self {
            request,
            ref_pictures: None,
        }
    }
    pub fn timestamp(&self) -> u64 {
        self.request.timestamp()
    }
    pub fn set_ref_pictures(&mut self, ref_pictures: Vec<Rc<RefCell<V4l2Picture>>>) -> &mut Self {
        self.ref_pictures = Some(ref_pictures);
        self
    }
    pub fn sync(&mut self) -> &mut Self {
        self.request.sync();
        self.ref_pictures = None;
        self
    }
    pub fn request(&mut self) -> &mut V4l2Request {
        &mut self.request
    }
}

impl<'a> MappableHandle for std::cell::Ref<'a, V4l2Picture> {
    fn read(&mut self, data: &mut [u8]) -> anyhow::Result<()> {
        self.request.result().read(data);
        Ok(())
    }
    fn image_size(&mut self) -> usize {
        self.request.result().length()
    }
}

pub struct BackendHandle {
    pub picture: Rc<RefCell<V4l2Picture>>,
}

impl<'a> DynHandle for std::cell::Ref<'a, BackendHandle> {
    fn dyn_mappable_handle<'b>(&'b self) -> anyhow::Result<Box<dyn MappableHandle + 'b>> {
        self.picture.borrow_mut().sync();
        Ok(Box::new(self.picture.borrow()))
    }
}

pub struct V4l2StatelessDecoderHandle {
    pub handle: Rc<RefCell<BackendHandle>>,
}

impl Clone for V4l2StatelessDecoderHandle {
    fn clone(&self) -> Self {
        Self {
            handle: Rc::clone(&self.handle),
        }
    }
}

impl DecodedHandle for V4l2StatelessDecoderHandle {
    type Descriptor = ();

    fn coded_resolution(&self) -> Resolution {
        todo!();
    }

    fn display_resolution(&self) -> Resolution {
        todo!();
    }

    fn timestamp(&self) -> u64 {
        self.handle.borrow().picture.borrow().timestamp()
    }

    fn dyn_picture<'a>(&'a self) -> Box<dyn DynHandle + 'a> {
        Box::new(self.handle.borrow())
    }

    fn sync(&self) -> anyhow::Result<()> {
        Ok(())
    }

    fn is_ready(&self) -> bool {
        todo!();
    }

    fn resource(&self) -> std::cell::Ref<()> {
        todo!();
    }
}

pub struct V4l2StatelessDecoderBackend {
    pub device: V4l2Device,
    stream_info: StreamInfo,
}

impl V4l2StatelessDecoderBackend {
    pub fn new() -> Self {
        Self {
            device: V4l2Device::new(),
            stream_info: StreamInfo {
                format: DecodedFormat::I420,
                min_num_frames: 4,
                coded_resolution: Resolution::from((320, 200)),
                display_resolution: Resolution::from((320, 200)),
            },
        }
    }
}

impl FramePool for V4l2StatelessDecoderBackend {
    type Descriptor = ();

    fn coded_resolution(&self) -> Resolution {
        todo!();
    }

    fn set_coded_resolution(&mut self, _resolution: Resolution) {
        todo!();
    }

    fn add_frames(&mut self, _descriptors: Vec<Self::Descriptor>) -> Result<(), anyhow::Error> {
        todo!();
    }

    fn num_free_frames(&self) -> usize {
        self.device.num_free_buffers()
    }

    fn num_managed_frames(&self) -> usize {
        self.device.num_buffers()
    }

    fn clear(&mut self) {
        todo!();
    }
}

impl<Codec: StatelessCodec> TryFormat<Codec> for V4l2StatelessDecoderBackend {
    fn try_format(&mut self, _: &Codec::FormatInfo, _: DecodedFormat) -> anyhow::Result<()> {
        // TODO
        Ok(())
    }
}

impl StatelessDecoderBackend for V4l2StatelessDecoderBackend {
    type Handle = V4l2StatelessDecoderHandle;

    type FramePool = Self;

    fn stream_info(&self) -> Option<&StreamInfo> {
        // TODO
        Some(&self.stream_info)
    }

    fn frame_pool(&mut self, _: PoolLayer) -> Vec<&mut Self::FramePool> {
        self.device.recycle_buffers();
        vec![self]
    }
}
