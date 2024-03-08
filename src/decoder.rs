// Copyright 2022 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

//! Encoded stream decoding.
//!
//! A decoder turns an encoded stream into its corresponding decoded frames. This module provides
//! several decoders for various codecs and backends.
//!
//! At the moment, only a [stateless] decoder interface is provided.

pub mod stateful;
pub mod stateless;

use std::collections::VecDeque;

pub use crate::BlockingMode;

use crate::decoder::stateless::PoolLayer;
use crate::DecodedFormat;
use crate::Resolution;

/// Trait for a pool of frames in a particular format.
///
/// This is mostly useful for the decoder where the user is expected to manage how the decoded
/// frames buffers are allocated and when.
pub trait FramePool {
    /// Type of descriptor for the memory backing the frames.
    type Descriptor;

    /// Returns the coded resolution of the pool.
    ///
    /// All the frames maintained by this pool are guaranteed to be able to contain the coded
    /// resolution.
    fn coded_resolution(&self) -> Resolution;
    /// Update the coded resolution of the pool.
    ///
    /// Frames managed by this pool that can not contain the new resolution are dropped.
    fn set_coded_resolution(&mut self, resolution: Resolution);
    /// Add new frames to the pool, using `descriptors` as backing memory.
    fn add_frames(&mut self, descriptors: Vec<Self::Descriptor>) -> Result<(), anyhow::Error>;
    /// Returns new number of frames currently available in this pool.
    fn num_free_frames(&self) -> usize;
    /// Returns the total number of managed frames in this pool.
    fn num_managed_frames(&self) -> usize;
    /// Remove all frames from this pool.
    fn clear(&mut self);
}

/// Information about the current stream.
///
/// This is static information obtained from the stream itself about its requirements. It does not
/// reflect the current settings of the decoder.
#[derive(Clone)]
pub struct StreamInfo {
    /// Pixel format for the output frames expected by the decoder.
    pub format: DecodedFormat,
    /// Coded resolution of the stream, i.e. minimum size of the frames to be decoded into.
    pub coded_resolution: Resolution,
    /// Display resolution of the stream, i.e. the part of the decoded frames we want to display.
    pub display_resolution: Resolution,
    /// Minimum number of output frames per layer required for decoding to proceed.
    ///
    /// Codecs keep some frames as references and cannot decode immediately into them again after
    /// they are returned. Allocating at least this number of frames guarantees that the decoder
    /// won't starve from output frames.
    pub min_num_frames: usize,
}

/// Trait for objects allowing to negotiate the output format of a decoder.
///
/// A decoder always has a valid output format set, but that format can change if the stream
/// requests it. When this happens, the decoder stops accepting new input and a `FormatChanged`
/// event is emitted, carrying a negotiator trait object that allows the client to acknowledge that
/// the format change took place, and (in the future) negotiate its specifics.
///
/// When the object is dropped, the decoder can accept and process new input again.
pub trait DecoderFormatNegotiator<'a, P>
where
    P: FramePool,
{
    /// Returns the current decoding parameters, as extracted from the stream.
    fn stream_info(&self) -> &StreamInfo;
    /// Returns the frame pool in use for the decoder for `layer` set up for the
    /// new format.
    fn frame_pool(&mut self, layer: PoolLayer) -> Vec<&mut P>;
    fn try_format(&mut self, format: DecodedFormat) -> anyhow::Result<()>;
}

/// Events that can be retrieved using the `next_event` method of a decoder.
pub enum DecoderEvent<'a, H: DecodedHandle, P: FramePool<Descriptor = H::Descriptor>> {
    /// The next frame has been decoded.
    FrameReady(H),
    /// The format of the stream has changed and action is required.
    FormatChanged(Box<dyn DecoderFormatNegotiator<'a, P> + 'a>),
}

pub trait DynHandle {
    /// Gets an CPU mapping to the memory backing the handle.
    /// Assumes that this picture is backed by a handle and panics if not the case.
    fn dyn_mappable_handle<'a>(&'a self) -> anyhow::Result<Box<dyn MappableHandle + 'a>>;
}

/// A trait for types that can be mapped into the client's address space.
pub trait MappableHandle {
    /// Read the contents of `self` into `buffer`.
    ///
    /// The size of `buffer` must be equal to `image_size()`, or an error will be returned.
    fn read(&mut self, buffer: &mut [u8]) -> anyhow::Result<()>;

    /// Returns the size of the `buffer` argument required to call `read` on this handle.
    fn image_size(&mut self) -> usize;
}

/// The handle type used by the decoder backend. The only requirement from implementors is that
/// they give access to the underlying handle and that they can be (cheaply) cloned.
pub trait DecodedHandle {
    /// Memory descriptor type - the type that provides the backend memory for the decoded frame.
    /// `()` is a special type meaning that the backend is responsible for allocating and managing
    /// the memory itself.
    type Descriptor;

    /// Returns a reference to an object allowing a CPU mapping of the decoded frame.
    fn dyn_picture<'a>(&'a self) -> Box<dyn DynHandle + 'a>;

    /// Returns the timestamp of the picture.
    fn timestamp(&self) -> u64;

    /// Returns the coded resolution at the time this handle was decoded.
    fn coded_resolution(&self) -> Resolution;

    /// Returns the display resolution at the time this handle was decoded.
    fn display_resolution(&self) -> Resolution;

    /// Returns `true` if this handle has been completely decoded.
    fn is_ready(&self) -> bool;

    /// Wait until this handle has been completely rendered.
    fn sync(&self) -> anyhow::Result<()>;

    fn resource(&self) -> std::cell::Ref<Self::Descriptor>;
}

/// A queue where decoding jobs wait until they are completed, at which point they can be
/// retrieved.
struct ReadyFramesQueue<T> {
    /// Queue of all the frames waiting to be sent to the client.
    queue: VecDeque<T>,
}

impl<T> Default for ReadyFramesQueue<T> {
    fn default() -> Self {
        Self {
            queue: Default::default(),
        }
    }
}

impl<T> ReadyFramesQueue<T> {
    /// Push `handle` to the back of the queue.
    fn push(&mut self, handle: T) {
        self.queue.push_back(handle)
    }
}

impl<T> Extend<T> for ReadyFramesQueue<T> {
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        self.queue.extend(iter)
    }
}

/// Allows us to manipulate the frames list like an iterator without consuming it and resetting its
/// display order counter.
impl<'a, T> Iterator for &'a mut ReadyFramesQueue<T> {
    type Item = T;

    /// Returns the next frame (if any) waiting to be dequeued.
    fn next(&mut self) -> Option<T> {
        self.queue.pop_front()
    }
}
