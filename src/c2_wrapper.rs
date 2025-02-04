// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::sync::Arc;
use std::vec::Vec;

use crate::Fourcc;

pub struct C2VideoFrame<'a> {
    // TODO: This will require us to memcpy frame data. This makes it easy to prototype, but is
    // inefficient. We will want to change this to DMA buffer FDs that we import directly.
    pub planes: Vec<&'a mut [u8]>,
    pub plane_strides: Vec<usize>,
    pub fourcc: Fourcc,
    pub visible_width: usize,
    pub visible_height: usize,
}

pub trait C2DecodeWorkObject {
    fn input(&mut self) -> &[u8];
    // C2 technically supports multiple output worklets, but in practice many decoders don't use
    // this feature. Note that we pass the width and height into this function because we are going
    // to allocate the frame lazily. This is required to support DRC.
    fn output(
        &mut self,
        fourcc: Fourcc,
        visible_width: usize,
        visible_height: usize,
    ) -> Result<C2VideoFrame, String>;
}

pub struct C2DecodeJob<T: C2DecodeWorkObject> {
    // Many of the callback functions in C2 take C2Work items as parameters, so we need to be able
    // to retrieve the original C++ object.
    pub work_object: T,
    // TODO: Add output delay and color aspect support as needed.
}

pub trait C2EncodeWorkObject {
    fn input(&mut self) -> C2VideoFrame;
    // C2 technically supports multiple output worklets, but in practice many encoders don't use
    // this feature.
    fn output(&mut self) -> Result<&mut [u8], String>;
}

pub struct C2EncodeJob<T: C2EncodeWorkObject> {
    // Many of the callback functions in C2 take C2Work items as parameters, so
    // we need to be able to retrieve the original C++ object.
    pub work_object: T,
    // In microseconds.
    pub timestamp: u64,
    // TODO: only support CBR right now, follow up with VBR support.
    pub bitrate: u32,
    // Framerate is actually negotiated, so the encoder can change this value
    // based on the timestamps of the frames it receives.
    pub framerate: Arc<u32>,
}

pub enum C2Status {
    C2Ok,
    C2BadState,
    C2BadValue,
    // TODO: Improve error handling by adding more statuses.
}

// T should be either C2DecodeJob or C2EncodeJob
pub trait C2CrosCodecsWrapper<T> {
    fn new(
        codec: Fourcc,
        work_done_cb: &mut dyn FnMut(Vec<T>),
        error_cb: &mut dyn FnMut(u32),
    ) -> Self;
    fn start(&mut self) -> C2Status;
    fn stop(&mut self) -> C2Status;
    fn reset(&mut self) -> C2Status;
    fn release(&mut self) -> C2Status;
    fn queue(&mut self, work_items: Vec<T>) -> C2Status;
    // TODO: Support different flush modes.
    fn flush(&mut self, flushed_work: &mut Vec<T>) -> C2Status;
    // TODO: Support different drain modes.
    fn drain(&mut self) -> C2Status;
}
