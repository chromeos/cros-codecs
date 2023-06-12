// Copyright 2022 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

//! This file contains a dummy backend whose only purpose is to let the decoder
//! run so we can test it in isolation.

use std::cell::RefCell;
use std::rc::Rc;

use crate::codec::h264::dpb::Dpb;
use crate::codec::h264::dpb::DpbEntry;
use crate::codec::h264::parser::Pps;
use crate::codec::h264::parser::Slice;
use crate::codec::h264::parser::Sps;
use crate::codec::h264::picture::PictureData;
use crate::decoder::stateless::h264::backends::Result as StatelessBackendResult;
use crate::decoder::stateless::h264::backends::StatelessH264DecoderBackend;
use crate::decoder::stateless::h264::Decoder;
use crate::decoder::BlockingMode;
use crate::utils::dummy::*;

impl StatelessH264DecoderBackend for Backend {
    type Picture = ();

    fn new_sequence(&mut self, _: &Sps) -> StatelessBackendResult<()> {
        Ok(())
    }

    fn start_picture(
        &mut self,
        _: &mut Self::Picture,
        _: &PictureData,
        _: &Sps,
        _: &Pps,
        _: &Dpb<Self::Handle>,
        _: &Slice<&[u8]>,
    ) -> StatelessBackendResult<()> {
        Ok(())
    }

    fn new_field_picture(
        &mut self,
        _: &PictureData,
        _: u64,
        _: &Self::Handle,
    ) -> StatelessBackendResult<()> {
        Ok(())
    }

    fn decode_slice(
        &mut self,
        _: &mut Self::Picture,
        _: &Slice<&[u8]>,
        _: &Sps,
        _: &Pps,
        _: &Dpb<Self::Handle>,
        _: &[DpbEntry<Self::Handle>],
        _: &[DpbEntry<Self::Handle>],
    ) -> StatelessBackendResult<()> {
        Ok(())
    }

    fn submit_picture(&mut self, _: Self::Picture) -> StatelessBackendResult<Self::Handle> {
        Ok(Handle {
            handle: Rc::new(RefCell::new(BackendHandle)),
        })
    }

    fn new_picture(&mut self, _: &PictureData, _: u64) -> StatelessBackendResult<()> {
        Ok(())
    }
}

impl Decoder<Handle, ()> {
    // Creates a new instance of the decoder using the dummy backend.
    pub fn new_dummy(blocking_mode: BlockingMode) -> anyhow::Result<Self> {
        Self::new(Box::new(Backend::new()), blocking_mode)
    }
}
