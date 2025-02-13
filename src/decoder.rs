// Copyright 2022 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

//! Encoded stream decoding.
//!
//! A decoder turns an encoded stream into its corresponding decoded frames. This module provides
//! several decoders for various codecs and backends.
//!
//! At the moment, only a [stateless] decoder interface is provided.

pub mod stateless;

use std::collections::VecDeque;
use std::os::fd::AsFd;
use std::os::fd::BorrowedFd;
use std::sync::Arc;

use nix::errno::Errno;
use nix::sys::eventfd::EventFd;

pub use crate::BlockingMode;

use crate::video_frame::VideoFrame;
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
#[derive(Clone, Debug)]
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

/// Events that can be retrieved using the `next_event` method of a decoder.
pub enum DecoderEvent<H: DecodedHandle> {
    /// The next frame has been decoded.
    FrameReady(H),
    /// The format of the stream has changed and action is required.
    FormatChanged,
}

/// The handle type used by the decoder backend. The only requirement from implementors is that
/// they give access to the underlying handle and that they can be (cheaply) cloned.
pub trait DecodedHandle {
    type Frame: VideoFrame;

    fn video_frame(&self) -> Arc<Self::Frame>;

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
}

/// Implementation for any boxed [`DecodedHandle`], including trait objects.
impl<H> DecodedHandle for Box<H>
where
    H: DecodedHandle + ?Sized,
{
    type Frame = H::Frame;

    fn video_frame(&self) -> Arc<Self::Frame> {
        self.as_ref().video_frame()
    }

    fn timestamp(&self) -> u64 {
        self.as_ref().timestamp()
    }

    fn coded_resolution(&self) -> Resolution {
        self.as_ref().coded_resolution()
    }

    fn display_resolution(&self) -> Resolution {
        self.as_ref().display_resolution()
    }

    fn is_ready(&self) -> bool {
        self.as_ref().is_ready()
    }

    fn sync(&self) -> anyhow::Result<()> {
        self.as_ref().sync()
    }
}

/// Trait object for [`DecodedHandle`]s using a specific `VideoFrame`.
pub type DynDecodedHandle<F> = Box<dyn DecodedHandle<Frame = F>>;

/// A queue where decoding jobs wait until they are completed, at which point they can be
/// retrieved.
struct ReadyFramesQueue<T> {
    /// Queue of all the frames waiting to be sent to the client.
    queue: VecDeque<T>,

    /// EventFd signaling `EPOLLIN` whenever the queue is not empty.
    poll_fd: EventFd,
}

impl<T> ReadyFramesQueue<T> {
    /// Create a nwe `ReadyFramesQueue`.
    ///
    /// This can only fail if the `EventFd` creation fails ; in this case the corresponding `Errno`
    /// is returned.
    fn new() -> Result<Self, Errno> {
        let poll_fd = EventFd::new()?;

        Ok(Self { queue: Default::default(), poll_fd })
    }

    /// Push `handle` to the back of the queue.
    fn push(&mut self, handle: T) {
        self.queue.push_back(handle);
        if let Err(e) = self.poll_fd.write(1) {
            log::error!("failed to write ready frames queue poll FD: {:#}", e);
        }
    }

    /// Returns a file descriptor that signals `POLLIN` whenever an event is available on this
    /// queue.
    pub fn poll_fd(&self) -> BorrowedFd {
        self.poll_fd.as_fd()
    }
}

impl<T> Extend<T> for ReadyFramesQueue<T> {
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        let len_before = self.queue.len();
        self.queue.extend(iter);
        if let Err(e) = self.poll_fd.write((self.queue.len() - len_before) as u64) {
            log::error!("failed to write ready frames queue poll FD: {:#}", e);
        }
    }
}

/// Allows us to manipulate the frames list like an iterator without consuming it and resetting its
/// display order counter.
impl<T> Iterator for ReadyFramesQueue<T> {
    type Item = T;

    /// Returns the next frame (if any) waiting to be dequeued.
    fn next(&mut self) -> Option<T> {
        let next = self.queue.pop_front();

        if next.is_some() && self.queue.is_empty() {
            if let Err(e) = self.poll_fd.read() {
                log::error!("failed to read ready frames queue poll FD: {:#}", e);
            }
        }

        next
    }
}

#[cfg(test)]
mod tests {
    use nix::sys::epoll::Epoll;
    use nix::sys::epoll::EpollCreateFlags;
    use nix::sys::epoll::EpollEvent;
    use nix::sys::epoll::EpollFlags;
    use nix::sys::epoll::EpollTimeout;

    use super::ReadyFramesQueue;

    #[test]
    fn test_ready_frame_queue_poll() {
        let mut queue = ReadyFramesQueue::<()>::new().unwrap();
        let epoll = Epoll::new(EpollCreateFlags::empty()).unwrap();
        epoll.add(queue.poll_fd(), EpollEvent::new(EpollFlags::EPOLLIN, 1)).unwrap();

        // Empty queue should not signal.
        let mut events = [EpollEvent::empty()];
        let nb_fds = epoll.wait(&mut events, EpollTimeout::ZERO).unwrap();
        assert_eq!(nb_fds, 0);

        // Events in the queue should signal.
        queue.push(());
        let mut events = [EpollEvent::empty()];
        let nb_fds = epoll.wait(&mut events, EpollTimeout::ZERO).unwrap();
        assert_eq!(nb_fds, 1);
        assert_eq!(events, [EpollEvent::new(EpollFlags::EPOLLIN, 1)]);

        // The queue is empty again and should not signal.
        queue.next().unwrap();
        let mut events = [EpollEvent::empty()];
        let nb_fds = epoll.wait(&mut events, EpollTimeout::ZERO).unwrap();
        assert_eq!(nb_fds, 0);
        assert_eq!(events, [EpollEvent::empty()]);

        // Add 3 elements to the queue, it should signal until we remove them all.
        queue.extend(std::iter::repeat(()).take(3));
        let mut events = [EpollEvent::empty()];
        let nb_fds = epoll.wait(&mut events, EpollTimeout::ZERO).unwrap();
        assert_eq!(nb_fds, 1);
        assert_eq!(events, [EpollEvent::new(EpollFlags::EPOLLIN, 1)]);

        queue.next().unwrap();
        let mut events = [EpollEvent::empty()];
        let nb_fds = epoll.wait(&mut events, EpollTimeout::ZERO).unwrap();
        assert_eq!(nb_fds, 1);
        assert_eq!(events, [EpollEvent::new(EpollFlags::EPOLLIN, 1)]);

        queue.next().unwrap();
        let mut events = [EpollEvent::empty()];
        let nb_fds = epoll.wait(&mut events, EpollTimeout::ZERO).unwrap();
        assert_eq!(nb_fds, 1);
        assert_eq!(events, [EpollEvent::new(EpollFlags::EPOLLIN, 1)]);

        queue.next().unwrap();
        let mut events = [EpollEvent::empty()];
        let nb_fds = epoll.wait(&mut events, EpollTimeout::ZERO).unwrap();
        assert_eq!(nb_fds, 0);
        assert_eq!(events, [EpollEvent::empty()]);
    }
}
