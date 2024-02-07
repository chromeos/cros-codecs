// Copyright 2022 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

//! This file contains a dummy backend whose only purpose is to let the decoder
//! run so we can test it in isolation.

use std::cell::RefCell;
use std::rc::Rc;

use crate::decoder::stateless::PoolLayer;
use crate::decoder::stateless::StatelessCodec;
use crate::decoder::stateless::StatelessDecoderBackend;
use crate::decoder::stateless::StatelessDecoderBackendPicture;
use crate::decoder::stateless::TryFormat;
use crate::decoder::DecodedHandle;
use crate::decoder::DynHandle;
use crate::decoder::FramePool;
use crate::decoder::MappableHandle;
use crate::decoder::StreamInfo;
use crate::DecodedFormat;
use crate::Resolution;

#[derive(Default)]
pub struct BackendHandle(());

impl MappableHandle for BackendHandle {
    fn read(&mut self, _: &mut [u8]) -> anyhow::Result<()> {
        Ok(())
    }

    fn image_size(&mut self) -> usize {
        1
    }
}

impl<'a> DynHandle for std::cell::Ref<'a, BackendHandle> {
    fn dyn_mappable_handle<'b>(&'b self) -> anyhow::Result<Box<dyn MappableHandle + 'b>> {
        Ok(Box::<BackendHandle>::default())
    }
}

pub struct Handle {
    pub handle: Rc<RefCell<BackendHandle>>,
}

impl Clone for Handle {
    fn clone(&self) -> Self {
        Self {
            handle: Rc::clone(&self.handle),
        }
    }
}

impl DecodedHandle for Handle {
    type Descriptor = ();

    fn coded_resolution(&self) -> Resolution {
        Default::default()
    }

    fn display_resolution(&self) -> Resolution {
        Default::default()
    }

    fn timestamp(&self) -> u64 {
        0
    }

    fn dyn_picture<'a>(&'a self) -> Box<dyn DynHandle + 'a> {
        Box::new(self.handle.borrow())
    }

    fn sync(&self) -> anyhow::Result<()> {
        Ok(())
    }

    fn is_ready(&self) -> bool {
        true
    }

    fn resource(&self) -> std::cell::Ref<()> {
        std::cell::Ref::map(self.handle.borrow(), |h| &h.0)
    }
}

/// Dummy backend that can be used for any codec.
pub(crate) struct Backend {
    stream_info: StreamInfo,
}

impl Backend {
    pub(crate) fn new() -> Self {
        Self {
            stream_info: StreamInfo {
                format: DecodedFormat::I420,
                min_num_frames: 4,
                coded_resolution: Resolution::from((320, 200)),
                display_resolution: Resolution::from((320, 200)),
            },
        }
    }
}

impl<M> FramePool<M> for Backend {
    fn coded_resolution(&self) -> Resolution {
        Resolution::from((320, 200))
    }

    fn set_coded_resolution(&mut self, _resolution: Resolution) {}

    fn add_frames(&mut self, _descriptors: Vec<M>) -> Result<(), anyhow::Error> {
        Ok(())
    }

    fn num_free_frames(&self) -> usize {
        4
    }

    fn num_managed_frames(&self) -> usize {
        4
    }

    fn clear(&mut self) {}
}

impl<Codec: StatelessCodec> StatelessDecoderBackendPicture<Codec> for Backend {
    type Picture = ();
}

impl<Codec: StatelessCodec> TryFormat<Codec> for Backend {
    fn try_format(&mut self, _: &Codec::FormatInfo, _: DecodedFormat) -> anyhow::Result<()> {
        Ok(())
    }
}

impl StatelessDecoderBackend for Backend {
    type Handle = Handle;

    type FramePool = Self;

    fn stream_info(&self) -> Option<&StreamInfo> {
        Some(&self.stream_info)
    }

    fn frame_pool(&mut self, _: PoolLayer) -> Vec<&mut Self::FramePool> {
        vec![self]
    }
}
