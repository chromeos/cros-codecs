// Copyright 2022 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

// This file contains a dummy backend whose only purpose is to let the decoder
// run so we can test it in isolation.

use std::cell::RefCell;
use std::rc::Rc;

use crate::backend::dummy::decoder::Backend;
use crate::backend::dummy::decoder::Handle;
use crate::codec::vp8::parser::Header;
use crate::codec::vp8::parser::MbLfAdjustments;
use crate::codec::vp8::parser::Segmentation;
use crate::decoder::stateless::vp8::StatelessVp8DecoderBackend;
use crate::decoder::stateless::vp8::Vp8;
use crate::decoder::stateless::NewPictureResult;
use crate::decoder::stateless::NewStatelessDecoderError;
use crate::decoder::stateless::StatelessBackendResult;
use crate::decoder::stateless::StatelessDecoder;
use crate::decoder::BlockingMode;

impl StatelessVp8DecoderBackend for Backend {
    fn new_sequence(&mut self, _: &Header) -> StatelessBackendResult<()> {
        Ok(())
    }

    fn new_picture(&mut self, _: u64) -> NewPictureResult<Self::Picture> {
        Ok(())
    }

    fn submit_picture(
        &mut self,
        _: Self::Picture,
        _: &Header,
        _: &Option<Self::Handle>,
        _: &Option<Self::Handle>,
        _: &Option<Self::Handle>,
        _: &[u8],
        _: &Segmentation,
        _: &MbLfAdjustments,
    ) -> StatelessBackendResult<Self::Handle> {
        Ok(Handle {
            handle: Rc::new(RefCell::new(Default::default())),
        })
    }
}

impl StatelessDecoder<Vp8, Backend> {
    // Creates a new instance of the decoder using the dummy backend.
    pub fn new_dummy(blocking_mode: BlockingMode) -> Result<Self, NewStatelessDecoderError> {
        Self::new(Backend::new(), blocking_mode)
    }
}
