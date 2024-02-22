// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::collections::VecDeque;

use thiserror::Error;

use crate::codec::h264::synthesizer::SynthesizerError;
use crate::encoder::CodedBitstreamBuffer;
use crate::encoder::FrameMetadata;
use crate::BlockingMode;

pub mod h264;

#[derive(Error, Debug)]
pub enum StatelessBackendError {
    #[error("unsupported profile")]
    UnsupportedProfile,
    #[error("unsupported format")]
    UnsupportedFormat,
    #[error("not enough resources to proceed with the operation now")]
    OutOfResources,
    #[error(transparent)]
    Other(#[from] anyhow::Error),
}

pub type StatelessBackendResult<T> = Result<T, StatelessBackendError>;

#[derive(Error, Debug)]
pub enum EncodeError {
    #[error("invalid internal state. This is likely a bug.")]
    InvalidInternalState,
    #[error(transparent)]
    BackendError(#[from] StatelessBackendError),
    #[error(transparent)]
    H264SynthesizerError(#[from] SynthesizerError),
}

pub type EncodeResult<T> = Result<T, EncodeError>;

/// Trait for representing pending encoder output.
pub trait BackendPromise {
    type Output;

    /// Return coded result of the processing. Blocks if processing is not finished yet.
    fn sync(self) -> StatelessBackendResult<Self::Output>;

    /// Return true whenever the underlaying processing is done
    fn is_ready(&self) -> bool;
}

pub struct ReadyPromise<T>(T);

impl<T> From<T> for ReadyPromise<T> {
    fn from(value: T) -> Self {
        ReadyPromise(value)
    }
}

impl<T> BackendPromise for ReadyPromise<T> {
    type Output = T;

    fn sync(self) -> StatelessBackendResult<Self::Output> {
        Ok(self.0)
    }

    fn is_ready(&self) -> bool {
        true
    }
}

/// Internal structure representing all current processing represented using promises and allowing
/// polling for finished promises.
pub(crate) struct OutputQueue<O>
where
    O: BackendPromise,
{
    /// True if the every single polling call shall be blocking
    blocking: BlockingMode,

    /// Queue of currently pending [`BackendPromise`]
    promises: VecDeque<O>,
}

impl<O> OutputQueue<O>
where
    O: BackendPromise,
{
    pub(crate) fn new(blocking: BlockingMode) -> Self {
        Self {
            blocking,
            promises: Default::default(),
        }
    }

    /// Add new pending job to the queue. Which will be returned to client if it is done.
    pub(crate) fn add_promise(&mut self, pending: O) {
        self.promises.push_back(pending);
    }

    /// Returns the result of an oldest [`BackendPromise`] if it is done processing. If `force_block`
    /// is true, then the function will block till processing of the oldest [`BackendPromise`] is
    /// finished and return it's result.
    pub(crate) fn poll(&mut self, mode: BlockingMode) -> StatelessBackendResult<Option<O::Output>> {
        let block = self.blocking == BlockingMode::Blocking || mode == BlockingMode::Blocking;

        match self.promises.pop_front() {
            Some(o) if block || o.is_ready() => Ok(Some(o.sync()?)),
            Some(o) => {
                self.promises.push_front(o);
                Ok(None)
            }
            None => Ok(None),
        }
    }

    /// Returns true if queue is empty ie. no [`BackendPromise`] is pending.
    pub(crate) fn is_empty(&self) -> bool {
        self.promises.is_empty()
    }
}

/// Predictor is responsible for yielding stream parameter sets and creating requests to backend.
/// It accepts the frames and reconstructed frames and returns [`Request`]s for execution. For
/// example [`Predictor`] may hold frames from processing until enough is supplied to create a
/// specific prediction structure. [`Predictor::drain`] may be called to force predictor to
/// yield requests.
pub(super) trait Predictor<Picture, Reference, Request> {
    /// Called by encoder when there is new frame to encode. The predictor may return empty vector
    /// to postpone processing or a set of requests to process frames (it does not have to be a frame
    /// specified in parameters)
    fn new_frame(
        &mut self,
        backend_pic: Picture,
        meta: FrameMetadata,
    ) -> EncodeResult<Vec<Request>>;

    /// This function is called by the encoder, with reconstructed frame when backend finished
    /// processing the frame. the [`Predictor`] may choose to return [`Request`]s to submit to
    /// backend, if reconstructed was required for creating that request.
    fn reconstructed(&mut self, recon: Reference) -> EncodeResult<Vec<Request>>;

    /// Force [`Predictor`] to pop at least one frame from internal queue and return a [`Request`]s
    fn drain(&mut self) -> EncodeResult<Vec<Request>>;
}

/// Generic trait for stateless encoder backends
pub trait StatelessVideoEncoderBackend<Codec>
where
    Codec: StatelessCodec,
{
    /// Backend's specific representation of the input frame, transformed with [`import_picture`].
    /// Might be a wrapper of the input handle with additional backend specific data or a copy of
    /// an input frame in internal backend's representation.
    ///
    /// [`import_picture`]: StatelessEncoderBackendImport::import_picture
    type Picture;

    /// Backend's reconstructed frame handle.
    type Reconstructed: 'static;

    /// Backend's specific [`BackendPromise`] for bitstream, a result of [`Request`] submission.
    type CodedPromise: BackendPromise<Output = Vec<u8>>;

    /// Backend's specific [`BackendPromise`] for [`StatelessVideoEncoderBackend::Reconstructed`],
    /// a result of [`Request`] submission.
    type ReconPromise: BackendPromise<Output = Self::Reconstructed>;
}

pub trait StatelessEncoderBackendImport<Handle, Picture> {
    /// Imports the input [`Handle`] from client and transforms into [`Picture`]
    ///
    /// [`Picture`]: StatelessVideoEncoderBackend::Picture
    fn import_picture(
        &mut self,
        metadata: &FrameMetadata,
        handle: Handle,
    ) -> StatelessBackendResult<Picture>;
}

pub trait StatelessCodec {}

/// Stateless video encoder interface.
pub trait StatelessVideoEncoder<H> {
    /// Enqueues the frame for encoding. The implementation will drop the handle after it is no
    /// longer be needed. The encoder is not required to immediately start processing the frame
    /// and yield output bitstream. It is allowed to hold frames until certain conditions are met
    /// eg. for specified prediction structures or referencing in order to further optimize
    /// the compression rate of the bitstream.
    fn encode(&mut self, meta: FrameMetadata, handle: H) -> Result<(), EncodeError>;

    /// Drains the encoder. This means that encoder is required to finish processing of all the
    /// frames in the internal queue and yield output bitstream by the end of the call. The output
    /// bitstream then can be polled using [`poll`] function.
    ///
    /// Drain does not enforce the flush of the internal state, ie. the enqueued frame handles
    /// do not have to be returned to user (dropped) and key frame is not enforced on the next
    /// frame.
    ///
    /// [`poll`]: StatelessVideoEncoder::poll
    fn drain(&mut self) -> EncodeResult<()>;

    /// Polls on the encoder for the available output bitstream with compressed frames that where
    /// submitted with [`encode`].
    ///
    /// The call may also trigger a further processing aside of returning output. Therefore it
    /// *recommended* that this function is called frequently.
    ///
    /// [`encode`]: StatelessVideoEncoder::encode
    fn poll(&mut self) -> EncodeResult<Option<CodedBitstreamBuffer>>;
}

pub fn simple_encode_loop<E, H, P>(
    encoder: &mut E,
    frame_producer: &mut P,
    mut coded_consumer: impl FnMut(CodedBitstreamBuffer),
) -> EncodeResult<()>
where
    E: StatelessVideoEncoder<H>,
    P: Iterator<Item = (FrameMetadata, H)>,
{
    for (meta, handle) in frame_producer.by_ref() {
        encoder.encode(meta, handle)?;
        while let Some(coded) = encoder.poll()? {
            coded_consumer(coded);
        }
    }

    encoder.drain()?;
    while let Some(coded) = encoder.poll()? {
        coded_consumer(coded);
    }

    Ok(())
}
