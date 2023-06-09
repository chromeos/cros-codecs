// Copyright 2022 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

//! This file contains a dummy backend whose only purpose is to let the decoder
//! run so we can test it in isolation.

use std::cell::RefCell;
use std::rc::Rc;

use crate::decoder::DecodedHandle;
use crate::decoder::DynHandle;
use crate::decoder::MappableHandle;
use crate::decoder::Result;
use crate::decoder::StatelessBackendResult;
use crate::decoder::VideoDecoderBackend;
use crate::DecodedFormat;
use crate::Resolution;

pub struct BackendHandle;

impl MappableHandle for BackendHandle {
    fn read(&mut self, _: &mut [u8]) -> Result<()> {
        Ok(())
    }

    fn image_size(&mut self) -> usize {
        1
    }
}

impl DynHandle for BackendHandle {
    fn dyn_mappable_handle_mut<'a>(&'a mut self) -> Box<dyn MappableHandle + 'a> {
        Box::new(BackendHandle)
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
    fn coded_resolution(&self) -> Resolution {
        Default::default()
    }

    fn display_resolution(&self) -> Resolution {
        Default::default()
    }

    fn timestamp(&self) -> u64 {
        0
    }

    fn dyn_picture_mut(&self) -> std::cell::RefMut<dyn DynHandle> {
        self.handle.borrow_mut()
    }

    fn sync(&self) -> StatelessBackendResult<()> {
        Ok(())
    }

    fn is_ready(&self) -> bool {
        true
    }
}

/// Dummy backend that can be used for any codec.
pub(crate) struct Backend;

impl Backend {
    pub(crate) fn new() -> Self {
        Self
    }
}

impl<FormatInfo> VideoDecoderBackend<FormatInfo> for Backend
where
    Handle: DecodedHandle,
{
    type Handle = Handle;

    fn num_resources_total(&self) -> usize {
        16
    }

    fn num_resources_left(&self) -> usize {
        16
    }

    fn format(&self) -> Option<DecodedFormat> {
        None
    }

    fn try_format(&mut self, _: &FormatInfo, _: DecodedFormat) -> crate::decoder::Result<()> {
        Ok(())
    }

    fn coded_resolution(&self) -> Option<Resolution> {
        None
    }

    fn display_resolution(&self) -> Option<Resolution> {
        None
    }
}
