// Copyright 2022 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

pub mod stateless;

use std::cell::RefMut;
use std::collections::VecDeque;

use thiserror::Error;

use crate::decoder::stateless::StatelessBackendError;
use crate::decoder::stateless::StatelessBackendResult;
use crate::DecodedFormat;
use crate::Resolution;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Error, Debug)]
pub enum Error {
    #[error(transparent)]
    StatelessBackendError(#[from] StatelessBackendError),
    #[error(transparent)]
    Other(#[from] anyhow::Error),
}

/// Trait for objects allowing to negotiate the output format of a decoder.
///
/// A decoder always has a valid output format set, but that format can change if the stream
/// requests it. When this happens, the decoder stops accepting new input and a `FormatChanged`
/// event is emitted, carrying a negotiator trait object that allows the client to acknowledge that
/// the format change took place, and (in the future) negotiate its specifics.
///
/// When the object is dropped, the decoder can accept and process new input again.
pub trait DecoderFormatNegotiator<'a> {
    /// Gets the number of output resources allocated by the backend.
    fn num_resources_total(&self) -> usize;

    /// Returns the current coded resolution of the bitstream being processed.
    /// This may be None if we have not read the stream parameters yet.
    fn coded_resolution(&self) -> Resolution;

    fn format(&self) -> Option<DecodedFormat>;
    fn try_format(&mut self, format: DecodedFormat) -> Result<()>;
}

/// Events that can be retrieved using the `next_event` method of a decoder.
pub enum DecoderEvent<'a> {
    /// The next frame has been decoded.
    FrameReady(Box<dyn DecodedHandle>),
    /// The format of the stream has changed and action is required.
    FormatChanged(Box<dyn DecoderFormatNegotiator<'a> + 'a>),
}

pub trait DynHandle {
    /// Gets an exclusive reference to the backend handle of this picture.
    /// Assumes that this picture is backed by a handle and panics if not the case.
    fn dyn_mappable_handle_mut<'a>(&'a mut self) -> Box<dyn MappableHandle + 'a>;
}

/// A trait for types that can be mapped into the client's address space.
pub trait MappableHandle {
    /// Read the contents of `self` into `buffer`.
    ///
    /// The size of `buffer` must be equal to `image_size()`, or an error will be returned.
    fn read(&mut self, buffer: &mut [u8]) -> Result<()>;

    /// Returns the size of the `buffer` argument required to call `read` on this handle.
    fn image_size(&mut self) -> usize;
}

/// Instructs the decoder on whether it should block on the decode operations.
/// Nonblocking mode is conditional on backend support.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BlockingMode {
    Blocking,
    NonBlocking,
}

impl Default for BlockingMode {
    fn default() -> Self {
        Self::Blocking
    }
}

/// The handle type used by the stateless decoder backend. The only requirement
/// from implementors is that they give access to the underlying handle and
/// that they can be (cheaply) cloned.
pub trait DecodedHandle {
    fn dyn_picture_mut(&self) -> RefMut<dyn DynHandle>;

    /// Returns the timestamp of the picture.
    fn timestamp(&self) -> u64;

    /// Returns the coded resolution at the time this handle was decoded.
    fn coded_resolution(&self) -> Resolution;

    /// Returns the display resolution at the time this handle was decoded.
    fn display_resolution(&self) -> Resolution;

    /// Returns `true` if this handle has been completely decoded.
    fn is_ready(&self) -> bool;

    /// Wait until this handle has been completely rendered.
    fn sync(&self) -> StatelessBackendResult<()>;
}

/// A queue where decoding jobs wait until they are completed, at which point they can be
/// retrieved.
struct ReadyFramesQueue<T: DecodedHandle> {
    /// Queue of all the frames waiting to be sent to the client.
    queue: VecDeque<T>,
}

impl<T: DecodedHandle> Default for ReadyFramesQueue<T> {
    fn default() -> Self {
        Self {
            queue: Default::default(),
        }
    }
}

impl<T: DecodedHandle> ReadyFramesQueue<T> {
    /// Push `handle` to the back of the queue.
    fn push(&mut self, handle: T) {
        self.queue.push_back(handle)
    }
}

impl<T: DecodedHandle> Extend<T> for ReadyFramesQueue<T> {
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        self.queue.extend(iter)
    }
}

/// Allows us to manipulate the frames list like an iterator without consuming it and resetting its
/// display order counter.
impl<'a, T: DecodedHandle> Iterator for &'a mut ReadyFramesQueue<T> {
    type Item = T;

    /// Returns the next frame (if any) waiting to be dequeued.
    fn next(&mut self) -> Option<T> {
        self.queue.pop_front()
    }
}
