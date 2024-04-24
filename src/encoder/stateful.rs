// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::collections::BTreeSet;
use std::collections::VecDeque;

use thiserror::Error;

use crate::encoder::CodedBitstreamBuffer;
use crate::encoder::EncodeError;
use crate::encoder::EncodeResult;
use crate::encoder::FrameMetadata;
use crate::encoder::Tunings;
use crate::encoder::VideoEncoder;

pub mod h264;

#[derive(Debug, Error)]
pub enum StatefulBackendError {
    #[error("invalid internal state. This is likely a bug.")]
    InvalidInternalState,
    #[error(transparent)]
    Other(#[from] anyhow::Error),
}

pub type StatefulBackendResult<T> = Result<T, StatefulBackendError>;

/// Unique identifier of the [`BackendRequest`]
#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct BackendRequestId(usize);

/// Request package that is offered to [`StatefulVideoEncoderBackend`] for processing
pub struct BackendRequest<Handle> {
    /// Request's unique identifier
    pub request_id: BackendRequestId,
    /// Frame's metadata
    pub meta: FrameMetadata,
    /// Frame's handle
    pub handle: Handle,
    /// Tunings set for the request
    pub tunings: Tunings,
}

pub struct BackendOutput {
    /// Request's unique identifier corresponding to [`BackendRequest`]
    pub request_id: BackendRequestId,
    /// Result of the request. [`CodedBitstreamBuffer`] containing encoded frame
    pub buffer: CodedBitstreamBuffer,
}

/// Generic trait for stateful encoder backends
pub trait StatefulVideoEncoderBackend<Handle> {
    /// Try to submit encode request to the backend. The backend may not be able to accept the
    /// request eg. if there are not enough available resources or backend desires to finish
    /// previous request first. The function shall not be blocking.
    /// If backend accepts the request for processing it shall take the `request` (take ownership of
    /// [`BackendRequest`] and set ref mut to [`None`].
    fn consume_request(
        &mut self,
        request: &mut Option<BackendRequest<Handle>>,
    ) -> StatefulBackendResult<()>;

    /// Function shall block, until the backend can accept request with [`consume_request`] or
    /// will finished processing of some [`BackendRequest`] and [`poll`] can be used to
    /// fetch is result.
    ///
    /// [`consume_request`]: StatefulVideoEncoderBackend::consume_request
    /// [`poll`]: StatefulVideoEncoderBackend::poll
    fn sync(&mut self) -> StatefulBackendResult<()>;

    /// Blocking function, until the backend finishes processing all [`BackendRequest`], that the
    /// backend has accepted and all outputs of those requests are returned.
    fn drain(&mut self) -> StatefulBackendResult<Vec<BackendOutput>>;

    /// If the processing of any [`BackendRequest`] is finished then the function should yield it's
    /// corresponding [`BackendOutput`].
    ///
    /// [`consume_request`]: StatefulVideoEncoderBackend::consume_request
    fn poll(&mut self) -> StatefulBackendResult<Option<BackendOutput>>;
}

pub struct StatefulEncoder<Handle, Backend>
where
    Backend: StatefulVideoEncoderBackend<Handle>,
{
    /// Pending queue of frames to encoded by the backend
    queue: VecDeque<BackendRequest<Handle>>,

    /// Unique request identifier continue
    request_counter: usize,

    /// Latest [`Tunings`], that will be cloned in to request
    tunings: Tunings,

    /// Processed encoded bitstream queue for client to poll
    coded_queue: VecDeque<CodedBitstreamBuffer>,

    /// Currently processed requests by the backend
    processing: BTreeSet<BackendRequestId>,

    // [`StatefulVideoEncoderBackend`] instance to delegate [`BackendRequest`] to
    backend: Backend,
}

impl<Handle, Backend> StatefulEncoder<Handle, Backend>
where
    Backend: StatefulVideoEncoderBackend<Handle>,
{
    /// Utility function that creates an new [`StatefulEncoder`] with [`Tunings`] and
    /// [`StatefulVideoEncoderBackend`] instance.
    #[allow(dead_code)]
    fn create(tunings: Tunings, backend: Backend) -> Self {
        Self {
            queue: Default::default(),
            request_counter: 0,
            tunings,
            coded_queue: Default::default(),
            processing: Default::default(),
            backend,
        }
    }

    /// Handles the [`BackendOutput`] from the backend, ie add to the queue for client to poll.
    fn handle_output(&mut self, output: BackendOutput) -> EncodeResult<()> {
        log::debug!(
            "Backend yieled output buffer for request id={:?} timestamp={} bytes={}",
            output.request_id,
            output.buffer.metadata.timestamp,
            output.buffer.bitstream.len()
        );
        if !self.processing.remove(&output.request_id) {
            log::warn!("Coded buffer returned for non existing or already processed request id={:?} timestamp={}",
                output.request_id,
                output.buffer.metadata.timestamp,
            );
        }
        self.coded_queue.push_back(output.buffer);
        Ok(())
    }

    /// Poll the backend for outputs and handles them
    fn poll_backend(&mut self) -> EncodeResult<()> {
        while let Some(output) = self.backend.poll()? {
            self.handle_output(output)?;
        }

        Ok(())
    }

    /// Performs essential processing. Poll the backend for outputs and tries to submit requests to
    /// backends.
    fn process(&mut self) -> EncodeResult<()> {
        log::debug!(
            "Pending requests: {}, currently processed: {:?}, pending coded buffer: {}",
            self.queue.len(),
            self.processing,
            self.coded_queue.len()
        );

        if !self.processing.is_empty() {
            self.poll_backend()?;
        }

        while let Some(request) = self.queue.pop_front() {
            let request_id = request.request_id;
            let timestamp = request.meta.timestamp;
            let mut request = Some(request);

            log::trace!("Passing request to backend id={request_id:?} timestamp={timestamp}");
            self.backend.consume_request(&mut request)?;

            if let Some(request) = request {
                log::trace!("Backend stalled request id={request_id:?} timestamp={timestamp}");
                self.queue.push_front(request);
                break;
            } else {
                log::debug!("Backend consumed request id={request_id:?} timestamp={timestamp}");
                self.processing.insert(request_id);
            }
        }

        Ok(())
    }

    /// [`StatefulVideoEncoderBackend`]'s instance
    pub fn backend(&mut self) -> &Backend {
        &self.backend
    }
}

impl<Handle, Backend> VideoEncoder<Handle> for StatefulEncoder<Handle, Backend>
where
    Backend: StatefulVideoEncoderBackend<Handle>,
{
    fn tune(&mut self, tunings: Tunings) -> EncodeResult<()> {
        self.tunings = tunings;
        Ok(())
    }

    fn encode(&mut self, meta: FrameMetadata, handle: Handle) -> Result<(), EncodeError> {
        let request_id = BackendRequestId(self.request_counter);
        self.request_counter = self.request_counter.wrapping_add(1);

        log::trace!(
            "Got new request id={request_id:?} timestamp={}",
            meta.timestamp
        );

        let request = BackendRequest {
            request_id,
            meta,
            handle,
            tunings: self.tunings.clone(),
        };

        self.queue.push_back(request);
        self.process()?;

        Ok(())
    }

    fn poll(&mut self) -> EncodeResult<Option<CodedBitstreamBuffer>> {
        if !self.queue.is_empty() || !self.processing.is_empty() {
            self.process()?;
        }

        if let Some(buffer) = self.coded_queue.pop_front() {
            log::debug!(
                "Returning coded buffer timestamp={}",
                buffer.metadata.timestamp
            );
            return Ok(Some(buffer));
        }
        Ok(None)
    }

    fn drain(&mut self) -> EncodeResult<()> {
        log::debug!(
            "Got drain request. Pending in queue: {}. Currently processed: {:?}",
            self.queue.len(),
            self.processing
        );

        while !self.queue.is_empty() {
            self.process()?;

            if !self.queue.is_empty() {
                self.backend.sync()?;
            }
        }

        if self.processing.is_empty() {
            log::debug!("Skipping drain request to backend, everything is drained");
        }

        log::debug!("Sending drain request to backend");
        for output in self.backend.drain()? {
            self.handle_output(output)?;
        }

        Ok(())
    }
}
