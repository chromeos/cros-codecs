// Copyright 2022 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::cell::RefMut;

use thiserror::Error;

use crate::DecodedFormat;
use crate::Resolution;

pub mod h264;
pub mod h265;
pub mod vp8;
pub mod vp9;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Error, Debug)]
pub enum Error {
    #[error(transparent)]
    StatelessBackendError(#[from] StatelessBackendError),
    #[error(transparent)]
    Other(#[from] anyhow::Error),
}

#[derive(Error, Debug)]
pub enum StatelessBackendError {
    #[error("not enough resources to proceed with the operation now")]
    OutOfResources,
    #[error("this resource is not ready")]
    ResourceNotReady,
    #[error("this format is not supported")]
    UnsupportedFormat,
    #[error("negotiation failed")]
    NegotiationFailed(anyhow::Error),
    #[error(transparent)]
    Other(#[from] anyhow::Error),
}

pub type StatelessBackendResult<T> = std::result::Result<T, StatelessBackendError>;

pub(crate) trait VideoDecoderBackend {
    /// The type that the backend returns as a result of a decode operation.
    /// This will usually be some backend-specific type with a resource and a
    /// resource pool so that said buffer can be reused for another decode
    /// operation when it goes out of scope.
    type Handle: DecodedHandle + Clone;

    /// Returns the current coded resolution of the bitstream being processed.
    /// This may be None if we have not read the stream parameters yet.
    fn coded_resolution(&self) -> Option<Resolution>;

    /// Returns the current display resolution of the bitstream being processed.
    /// This may be None if we have not read the stream parameters yet.
    fn display_resolution(&self) -> Option<Resolution>;

    /// Gets the number of output resources allocated by the backend.
    fn num_resources_total(&self) -> usize;

    /// Gets the number of output resources left in the backend.
    fn num_resources_left(&self) -> usize;

    /// Gets the chosen format. This is set to a default after the decoder reads
    /// enough stream metadata from the bitstream. Some buffers need to be
    /// processed first before the default format can be set.
    fn format(&self) -> Option<DecodedFormat>;

    /// Try altering the decoded format.
    fn try_format(&mut self, format: DecodedFormat) -> Result<()>;
}

pub trait VideoDecoder {
    /// Decode the `bitstream` represented by `timestamp`. Returns zero or more
    /// decoded handles representing the decoded data.
    fn decode(&mut self, timestamp: u64, bitstream: &[u8]) -> Result<Vec<Box<dyn DecodedHandle>>>;

    /// Flush the decoder i.e. finish processing all queued decode requests and
    /// emit frames for them.
    fn flush(&mut self) -> Result<Vec<Box<dyn DecodedHandle>>>;

    /// Whether negotiation of the decoded format is possible. In particular, a
    /// decoder will indicate that negotiation is possible after enough metadata
    /// is collected from parsing the bitstream through calls to the `decode()`
    /// method.
    ///
    /// The negotiation process will start as soon as `negotiation_possible()`
    /// returns true. At this moment, the client and the backend can settle on a
    /// format by using the `supported_formats_for_stream()`, `format()` and
    /// `try_format()` methods.
    ///
    /// When `negotiation_possible()` returns true, the client may also query
    /// the backend for new values for the coded resolution, display resolution
    /// and/or to the number of resources allocated.
    ///
    /// The negotiation process ends as soon as another call to `decode()` is
    /// made, at which point any queued data will be processed first in order to
    /// generate any frames that might have been pending while the negotiation
    /// process was under way and `negotiation_possible()` will from then on
    /// return false.
    ///
    /// If no action is undertaken by the client in the window of time where
    /// `negotiation_possible()` returns true, it is assumed that the default
    /// format chosen by the backend is acceptable.
    ///
    /// The negotiation process can happen more than once if new stream metadata
    /// indicate a change of the stream parameters such that the current decoded
    /// format becomes incompatible with the stream. In this case,
    /// `negotiation_possible()` will once again return true and the same
    /// process described above will take place.
    fn negotiation_possible(&self) -> bool;

    /// Gets the number of output resources left in the backend after accounting
    /// for any buffers that might be queued in the decoder.
    fn num_resources_left(&self) -> Option<usize>;

    /// Gets the number of output resources allocated by the backend.
    fn num_resources_total(&self) -> usize;
    ///
    /// Returns the current coded resolution of the bitstream being processed.
    /// This may be None if we have not read the stream parameters yet.
    fn coded_resolution(&self) -> Option<Resolution>;
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

    /// Returns the display order for the picture backed by this handle, if set by the decoder.
    fn display_order(&self) -> Option<u64>;
    /// Sets the display order for the picture backend by this handle
    fn set_display_order(&mut self, display_order: u64);

    /// Returns the timestamp of the picture.
    fn timestamp(&self) -> u64;

    /// Returns the display resolution at the time this handle was decoded.
    fn display_resolution(&self) -> Resolution;

    /// Returns `true` if this handle has been completely decoded.
    fn is_ready(&self) -> bool;

    /// Wait until this handle has been completely rendered.
    fn sync(&self) -> StatelessBackendResult<()>;
}

#[cfg(test)]
pub(crate) mod tests {
    use crate::decoders::DecodedHandle;
    use crate::decoders::VideoDecoder;

    /// Stream that can be used in tests, along with the CRC32 of all of its frames.
    pub struct TestStream {
        /// Bytestream to decode.
        pub stream: &'static [u8],
        /// Expected CRC for each frame, one per line.
        pub crcs: &'static str,
    }

    /// Run the codec-specific `decoding_loop` on a `decoder` with a given `test`, linearly
    /// decoding the stream until its end.
    ///
    /// If `check_crcs` is `true`, then the expected CRCs of the decoded images are compared
    /// against the existing result. We may want to set this to false when using a decoder backend
    /// that does not produce actual frames.
    ///
    /// `dump_yuv` will dump all the decoded frames into `/tmp/framexxx.yuv`. Set this to true in
    /// order to debug the output of the test.
    pub fn test_decode_stream<D, L>(
        decoding_loop: L,
        mut decoder: D,
        test: &TestStream,
        check_crcs: bool,
        dump_yuv: bool,
    ) where
        D: VideoDecoder,
        L: Fn(&mut D, &[u8], &mut dyn FnMut(Box<dyn DecodedHandle>)),
    {
        let mut crcs = test.crcs.lines().enumerate();

        decoding_loop(&mut decoder, test.stream, &mut |handle| {
            let (frame_num, expected_crc) = crcs.next().expect("decoded more frames than expected");

            if check_crcs || dump_yuv {
                let mut picture = handle.dyn_picture_mut();
                let mut backend_handle = picture.dyn_mappable_handle_mut();

                let buffer_size = backend_handle.image_size();
                let mut nv12 = vec![0; buffer_size];

                backend_handle.read(&mut nv12).unwrap();

                if dump_yuv {
                    std::fs::write(format!("/tmp/frame{:03}.yuv", frame_num), &nv12).unwrap();
                }

                if check_crcs {
                    let frame_crc = format!("{:08x}", crc32fast::hash(&nv12));
                    assert_eq!(frame_crc, expected_crc, "at frame {}", frame_num);
                }
            }
        });

        assert_eq!(crcs.next(), None, "decoded less frames than expected");
    }
}
