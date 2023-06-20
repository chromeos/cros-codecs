// Copyright 2023 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

//! This file contains a dummy backend whose only purpose is to let the decoder
//! run so we can test it in isolation.

use std::cell::RefCell;
use std::rc::Rc;

use crate::backend::dummy::Backend;
use crate::backend::dummy::BackendHandle;
use crate::backend::dummy::Handle;
use crate::decoder::stateless::h265::Decoder;
use crate::decoder::BlockingMode;

use crate::decoder::stateless::h265::StatelessH265DecoderBackend;

impl<M> StatelessH265DecoderBackend<M> for Backend {
    type Picture = ();

    fn new_sequence(
        &mut self,
        _: &crate::codec::h265::parser::Sps,
    ) -> crate::decoder::stateless::StatelessBackendResult<()> {
        Ok(())
    }

    fn new_picture(
        &mut self,
        _: &crate::codec::h265::picture::PictureData,
        _: u64,
    ) -> crate::decoder::stateless::StatelessBackendResult<Self::Picture> {
        Ok(())
    }

    fn handle_picture(
        &mut self,
        _: &mut Self::Picture,
        _: &crate::codec::h265::picture::PictureData,
        _: &crate::codec::h265::parser::Sps,
        _: &crate::codec::h265::parser::Pps,
        _: &crate::codec::h265::dpb::Dpb<Self::Handle>,
        _: &crate::decoder::stateless::h265::RefPicSet<Self::Handle>,
        _: &crate::codec::h265::parser::Slice<&[u8]>,
    ) -> crate::decoder::stateless::StatelessBackendResult<()> {
        Ok(())
    }

    fn decode_slice(
        &mut self,
        _: &mut Self::Picture,
        _: &crate::codec::h265::parser::Slice<&[u8]>,
        _: &crate::codec::h265::parser::Sps,
        _: &crate::codec::h265::parser::Pps,
        _: &crate::codec::h265::dpb::Dpb<Self::Handle>,
        _: &[Option<super::RefPicListEntry<Self::Handle>>; 16],
        _: &[Option<super::RefPicListEntry<Self::Handle>>; 16],
    ) -> crate::decoder::stateless::StatelessBackendResult<()> {
        Ok(())
    }

    fn submit_picture(
        &mut self,
        _: Self::Picture,
    ) -> crate::decoder::stateless::StatelessBackendResult<Self::Handle> {
        Ok(Handle {
            handle: Rc::new(RefCell::new(BackendHandle)),
        })
    }
}
impl<M> Decoder<Handle, (), M> {
    // Creates a new instance of the decoder using the dummy backend.
    pub fn new_dummy(blocking_mode: BlockingMode) -> anyhow::Result<Self> {
        Self::new(Box::new(Backend::new()), blocking_mode)
    }
}
