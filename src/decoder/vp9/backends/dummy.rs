// Copyright 2022 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

//! This file contains a dummy backend whose only purpose is to let the decoder
//! run so we can test it in isolation.

use std::cell::RefCell;
use std::rc::Rc;

use crate::decoder::vp9::backends::StatelessDecoderBackend;
use crate::decoder::vp9::decoder::Decoder;
use crate::decoder::vp9::decoder::Segmentation;
use crate::decoder::vp9::parser::Header;
use crate::decoder::vp9::parser::MAX_SEGMENTS;
use crate::decoder::vp9::parser::NUM_REF_FRAMES;
use crate::decoder::BlockingMode;
use crate::utils::dummy::*;

impl StatelessDecoderBackend for Backend {
    fn new_sequence(&mut self, _: &crate::decoder::vp9::parser::Header) -> super::Result<()> {
        Ok(())
    }

    fn submit_picture(
        &mut self,
        _: &Header,
        _: &[Option<Self::Handle>; NUM_REF_FRAMES],
        _: &[u8],
        _: u64,
        _: &[Segmentation; MAX_SEGMENTS],
    ) -> super::Result<Self::Handle> {
        Ok(Handle {
            handle: Rc::new(RefCell::new(BackendHandle)),
        })
    }
}

impl Decoder<Handle> {
    // Creates a new instance of the decoder using the dummy backend.
    pub fn new_dummy(blocking_mode: BlockingMode) -> anyhow::Result<Self> {
        Self::new(Box::new(Backend::new()), blocking_mode)
    }
}
