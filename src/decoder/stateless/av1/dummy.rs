// Copyright 2023 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

//! This file contains a dummy backend whose only purpose is to let the decoder
//! run so we can test it in isolation.

use std::cell::RefCell;
use std::rc::Rc;

use crate::backend::dummy::{Backend, Handle};

use crate::decoder::stateless::av1::Av1;
use crate::decoder::stateless::av1::StatelessAV1DecoderBackend;
use crate::decoder::stateless::StatelessDecoder;
use crate::decoder::BlockingMode;

impl StatelessAV1DecoderBackend for Backend {
    fn new_sequence(
        &mut self,
        _: &std::rc::Rc<crate::codec::av1::parser::SequenceHeaderObu>,
        _: Option<u32>,
    ) -> crate::decoder::stateless::StatelessBackendResult<()> {
        Ok(())
    }

    fn new_picture(
        &mut self,
        _: &crate::codec::av1::parser::SequenceHeaderObu,
        _: &crate::codec::av1::parser::FrameHeaderObu,
        _: u64,
        _: &[Option<Self::Handle>; crate::codec::av1::parser::NUM_REF_FRAMES],
    ) -> crate::decoder::stateless::StatelessBackendResult<Self::Picture> {
        Ok(())
    }

    fn decode_tile_group(
        &mut self,
        _: &mut Self::Picture,
        _: crate::codec::av1::parser::TileGroupObu,
    ) -> crate::decoder::stateless::StatelessBackendResult<()> {
        Ok(())
    }

    fn submit_picture(
        &mut self,
        _: Self::Picture,
    ) -> crate::decoder::stateless::StatelessBackendResult<Self::Handle> {
        Ok(Handle {
            handle: Rc::new(RefCell::new(Default::default())),
        })
    }
}

impl StatelessDecoder<Av1, Backend> {
    // Creates a new instance of the decoder using the dummy backend.
    pub fn new_dummy(blocking_mode: BlockingMode) -> Self {
        Self::new(Backend::new(), blocking_mode)
    }
}
