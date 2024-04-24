// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::collections::VecDeque;

use thiserror::Error;

use crate::encoder::CodedBitstreamBuffer;
use crate::encoder::EncodeError;
use crate::encoder::EncodeResult;
use crate::encoder::FrameMetadata;
use crate::encoder::Tunings;
use crate::encoder::VideoEncoder;
use crate::BlockingMode;

pub mod av1;
pub mod h264;
pub(crate) mod predictor;
pub mod vp9;

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

/// Wrapper type for [`BackendPromise<Output = Vec<u8>>`], with additional
/// metadata.
pub struct BitstreamPromise<P>
where
    P: BackendPromise<Output = Vec<u8>>,
{
    /// Slice data and reconstructed surface promise
    bitstream: P,

    /// Input frame metadata, for [`CodedBitstreamBuffer`]
    meta: FrameMetadata,
}

impl<P> BackendPromise for BitstreamPromise<P>
where
    P: BackendPromise<Output = Vec<u8>>,
{
    type Output = CodedBitstreamBuffer;

    fn is_ready(&self) -> bool {
        self.bitstream.is_ready()
    }

    fn sync(self) -> StatelessBackendResult<Self::Output> {
        let coded_data = self.bitstream.sync()?;

        log::trace!("synced bitstream size={}", coded_data.len());

        Ok(CodedBitstreamBuffer::new(self.meta, coded_data))
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

    /// Requests the change of dynamic parameters (aka [`Tunings`]) for the stream. The predictor
    /// may choose to delay the change until entire or some part of the structure had been encoded.
    /// However in such case the predictor is responsible for ensuring the change will be
    /// successful.
    fn tune(&mut self, tunings: Tunings) -> EncodeResult<()>;

    /// Force [`Predictor`] to pop at least one frame from internal queue and return a [`Request`]s
    fn drain(&mut self) -> EncodeResult<Vec<Request>>;
}

/// Generic trait for stateless encoder backends
pub trait StatelessVideoEncoderBackend<Codec>: Sized
where
    Codec: StatelessCodec<Self>,
{
    /// Backend's specific representation of the input frame, transformed with [`import_picture`].
    /// Might be a wrapper of the input handle with additional backend specific data or a copy of
    /// an input frame in internal backend's representation.
    ///
    /// [`import_picture`]: StatelessEncoderBackendImport::import_picture
    type Picture: 'static;

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
    fn import_picture(
        &mut self,
        metadata: &FrameMetadata,
        handle: Handle,
    ) -> StatelessBackendResult<Picture>;
}

/// Trait helping contain all codec specific and backend specific types
pub trait StatelessCodec<Backend>: Sized
where
    Backend: StatelessVideoEncoderBackend<Self>,
{
    /// Codec specific representation of frame reference wrapping a backend reference type
    /// containing a codec specific frame metadata
    type Reference;

    /// A request type that will be delivered to codec specific stateless encoder backend
    type Request;

    /// Codec specific [`BackendPromise`] for [`CodedBitstreamBuffer`] wrapping a backend specific
    /// [`StatelessVideoEncoderBackend::CodedPromise`]
    type CodedPromise: BackendPromise<Output = CodedBitstreamBuffer>;

    /// Codec specific [`BackendPromise`] for [`StatelessCodecSpecific::Reference`] wrapping a
    /// backend speficic [`StatelessVideoEncoderBackend::ReconPromise`]
    type ReferencePromise: BackendPromise<Output = Self::Reference>;
}

/// Helper aliases for codec and backend specific types
type Picture<C, B> = <B as StatelessVideoEncoderBackend<C>>::Picture;

type Reference<C, B> = <C as StatelessCodec<B>>::Reference;

type Request<C, B> = <C as StatelessCodec<B>>::Request;

type CodedPromise<C, B> = <C as StatelessCodec<B>>::CodedPromise;

type ReferencePromise<C, B> = <C as StatelessCodec<B>>::ReferencePromise;

type BoxPredictor<C, B> = Box<dyn Predictor<Picture<C, B>, Reference<C, B>, Request<C, B>>>;

pub struct StatelessEncoder<Codec, Handle, Backend>
where
    Backend: StatelessVideoEncoderBackend<Codec>,
    Codec: StatelessCodec<Backend>,
{
    /// Pending frame output promise queue
    output_queue: OutputQueue<CodedPromise<Codec, Backend>>,

    /// Pending reconstructed pictures promise queue
    recon_queue: OutputQueue<ReferencePromise<Codec, Backend>>,

    /// [`Predictor`] instance responsible for the encoder decision making
    predictor: BoxPredictor<Codec, Backend>,

    // predictor: Box<dyn Predictor<B::Picture, B::Reference>>,
    coded_queue: VecDeque<CodedBitstreamBuffer>,

    /// Number of the currently held frames by the predictor
    predictor_frame_count: usize,

    /// [`StatelessVP9EncoderBackend`] instance to delegate [`BackendRequest`] to
    backend: Backend,

    _phantom: std::marker::PhantomData<Handle>,
}

/// A bridge trait between [`StatelessEncoder`] and codec specific backend trait (eg.
/// [`h264::StatelessH264EncoderBackend`] or [`vp9::StatelessVP9EncoderBackend`]).
/// Accepts [`Request`] and is responsible for adding resutling [`BackendPromise`] to
/// [`StatelessEncoder`] internal queues and  decrementing the internal predictor frame counter if
/// the backend moved the frame outside predictor ownership.
pub trait StatelessEncoderExecute<Codec, Handle, Backend>
where
    Backend: StatelessVideoEncoderBackend<Codec>,
    Codec: StatelessCodec<Backend>,
{
    fn execute(&mut self, request: Request<Codec, Backend>) -> EncodeResult<()>;
}

impl<Codec, Handle, Backend> StatelessEncoder<Codec, Handle, Backend>
where
    Codec: StatelessCodec<Backend>,
    Backend: StatelessVideoEncoderBackend<Codec>,
    Self: StatelessEncoderExecute<Codec, Handle, Backend>,
{
    fn new(
        backend: Backend,
        mode: BlockingMode,
        predictor: BoxPredictor<Codec, Backend>,
    ) -> EncodeResult<Self> {
        Ok(Self {
            backend,
            predictor,
            predictor_frame_count: 0,
            coded_queue: Default::default(),
            output_queue: OutputQueue::new(mode),
            recon_queue: OutputQueue::new(mode),
            _phantom: Default::default(),
        })
    }

    fn poll_pending(&mut self, mode: BlockingMode) -> EncodeResult<()> {
        // Poll the output queue once and then continue polling while new promise is submitted
        while let Some(coded) = self.output_queue.poll(mode)? {
            self.coded_queue.push_back(coded);
        }

        while let Some(recon) = self.recon_queue.poll(mode)? {
            let requests = self.predictor.reconstructed(recon)?;
            if requests.is_empty() {
                // No promise was submitted, therefore break
                break;
            }

            for request in requests {
                self.execute(request)?;
            }
        }

        Ok(())
    }
}

impl<Codec, Handle, Backend> VideoEncoder<Handle> for StatelessEncoder<Codec, Handle, Backend>
where
    Codec: StatelessCodec<Backend>,
    Backend: StatelessVideoEncoderBackend<Codec>,
    Backend: StatelessEncoderBackendImport<Handle, Backend::Picture>,
    Self: StatelessEncoderExecute<Codec, Handle, Backend>,
{
    fn tune(&mut self, tunings: Tunings) -> EncodeResult<()> {
        self.predictor.tune(tunings)
    }

    fn encode(&mut self, metadata: FrameMetadata, handle: Handle) -> EncodeResult<()> {
        log::trace!(
            "encode: timestamp={} layout={:?}",
            metadata.timestamp,
            metadata.layout
        );

        // Import `handle` to backends representation
        let backend_pic = self.backend.import_picture(&metadata, handle)?;

        // Increase the number of frames that predictor holds, before handing one to it
        self.predictor_frame_count += 1;

        // Ask predictor to decide on the next move and execute it
        let requests = self.predictor.new_frame(backend_pic, metadata)?;
        for request in requests {
            self.execute(request)?;
        }

        Ok(())
    }

    fn drain(&mut self) -> EncodeResult<()> {
        log::trace!("currently predictor holds {}", self.predictor_frame_count);

        // Drain the predictor
        while self.predictor_frame_count > 0 || !self.recon_queue.is_empty() {
            if self.output_queue.is_empty() && self.recon_queue.is_empty() {
                // The OutputQueue is empty and predictor holds frames, force it to yield a request
                // to empty it's internal queue.
                let requests = self.predictor.drain()?;
                if requests.is_empty() {
                    log::error!("failed to drain predictor, no request was returned");
                    return Err(EncodeError::InvalidInternalState);
                }

                for request in requests {
                    self.execute(request)?;
                }
            }

            self.poll_pending(BlockingMode::Blocking)?;
        }

        // There are still some requests being processed. Continue on polling them.
        while !self.output_queue.is_empty() {
            self.poll_pending(BlockingMode::Blocking)?;
        }

        Ok(())
    }

    fn poll(&mut self) -> EncodeResult<Option<CodedBitstreamBuffer>> {
        // Poll on output queue without blocking and try to dueue from coded queue
        self.poll_pending(BlockingMode::NonBlocking)?;
        Ok(self.coded_queue.pop_front())
    }
}
