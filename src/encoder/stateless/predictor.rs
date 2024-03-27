// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::collections::VecDeque;
use std::rc::Rc;

use crate::encoder::stateless::EncodeError;
use crate::encoder::stateless::EncodeResult;
use crate::encoder::stateless::Predictor;
use crate::encoder::FrameMetadata;
use crate::encoder::RateControl;
use crate::encoder::Tunings;

#[derive(Clone)]
pub enum PredictionStructure {
    /// Simplest prediction structure, suitable eg. for RTC. Interframe is produced at the start of
    /// the stream and every time when [`limit`] frames are reached. Following interframe frames
    /// are frames relying solely on the last frame.
    LowDelay { limit: u16 },
}

/// Implementation of [`LowDelay`] prediction structure. See [`LowDelay`] for details.
///
/// [`LowDelay`]: PredictionStructure::LowDelay
pub(crate) struct LowDelay<Picture, Reference, Delegate, Request> {
    /// Pending frames for encoding
    pub(super) queue: VecDeque<(Picture, FrameMetadata)>,

    /// Availabe frames for references
    pub(super) references: VecDeque<Rc<Reference>>,

    /// Current frame counter
    pub(super) counter: usize,

    /// The number of frames between intra frames
    pub(super) limit: u16,

    /// Codec specific delegate. Holds codec specific state. Is also used to differentiate
    /// [`LowDelay`] implementations between codecs.
    pub(super) delegate: Delegate,

    /// Currently set tunings for the stream
    pub(super) tunings: Tunings,

    /// Pending [`Tunings`] to be applied with `counter` value when to set them.
    /// In this way we ensure that the requested frames prior tuning request are encoded using
    /// previous tunings. This is especially important for bitrate, this way we ensure the bitrate
    /// changes when requested.
    pub(super) tunings_queue: VecDeque<(usize, Tunings)>,

    pub(super) _phantom: std::marker::PhantomData<Request>,
}

/// Helper trait enabling forcing [`LowDelay`] to implement codec specific functions.
pub(crate) trait LowDelayDelegate<Picture, Reference, Request> {
    /// Creates keyframe or IDR request for the codec backend
    fn request_keyframe(
        &mut self,
        input: Picture,
        input_meta: FrameMetadata,
        idr: bool,
    ) -> EncodeResult<Request>;

    /// Creates interframe request for the codec backend
    fn request_interframe(
        &mut self,
        input: Picture,
        input_meta: FrameMetadata,
    ) -> EncodeResult<Request>;

    /// Checks if the [`_tunings`] can be applied
    fn try_tunings(&self, _tunings: &Tunings) -> EncodeResult<()> {
        Err(EncodeError::Unsupported)
    }

    /// Applies [`_tunings`]
    fn apply_tunings(&mut self, _tunings: &Tunings) -> EncodeResult<()> {
        Err(EncodeError::Unsupported)
    }
}

impl<Picture, Reference, Delegate, Request> LowDelay<Picture, Reference, Delegate, Request>
where
    Self: LowDelayDelegate<Picture, Reference, Request>,
{
    fn pop_tunings(&mut self) -> EncodeResult<()> {
        while let Some((when_counter, _)) = self.tunings_queue.front() {
            if self.counter < *when_counter {
                log::trace!(
                    "Pending tuning skipped counter={} scheduled={}",
                    self.counter,
                    when_counter
                );
                break;
            }

            // SAFETY: checked in loop condition
            let (_, tunings) = self.tunings_queue.pop_front().unwrap();
            log::info!("Applying tuning {tunings:?}");
            self.apply_tunings(&tunings)?;
            self.tunings = tunings;
        }

        Ok(())
    }

    fn next_request(&mut self) -> EncodeResult<Vec<Request>> {
        log::trace!("Pending frames in the queue: {}", self.queue.len());

        let mut requests = Vec::new();
        while let Some((input, meta)) = self.queue.pop_front() {
            self.pop_tunings()?;

            if self.counter == 0 || meta.force_keyframe {
                log::trace!("Requesting keyframe/IDR for timestamp={}", meta.timestamp);
                // If first frame in the sequence or forced IDR then clear references and create
                // keyframe request.
                // TODO: Maybe don't clear references on just keyframe (!= IDR)
                self.references.clear();

                let request = self.request_keyframe(input, meta, self.counter == 0)?;

                requests.push(request);
                self.counter = self.counter.wrapping_add(1) % (self.limit as usize);
            } else if self.references.is_empty() {
                log::trace!("Awaiting more reconstructed frames");
                // There is no enough frames reconstructed
                self.queue.push_front((input, meta));
                break;
            } else {
                log::trace!("Requesting interframe for timestamp={}", meta.timestamp);
                let request = self.request_interframe(input, meta)?;

                requests.push(request);
                self.counter = self.counter.wrapping_add(1) % (self.limit as usize);

                break;
            }
        }

        Ok(requests)
    }
}

impl<Picture, Reference, Delegate, Request> Predictor<Picture, Reference, Request>
    for LowDelay<Picture, Reference, Delegate, Request>
where
    Self: LowDelayDelegate<Picture, Reference, Request>,
{
    fn new_frame(
        &mut self,
        input: Picture,
        frame_metadata: FrameMetadata,
    ) -> EncodeResult<Vec<Request>> {
        log::trace!(
            "New frame added to queue timestamp={}",
            frame_metadata.timestamp
        );
        // Add new frame in the request queue and request new encoding if possible
        self.queue.push_back((input, frame_metadata));
        self.next_request()
    }

    fn reconstructed(&mut self, reference: Reference) -> EncodeResult<Vec<Request>> {
        log::trace!("A frame was reconstructed");
        // Add new reconstructed surface and request next encoding if possible
        self.references.push_back(Rc::new(reference));
        self.next_request()
    }

    fn tune(&mut self, tunings: Tunings) -> EncodeResult<()> {
        log::trace!("Tuning requested with {tunings:?}");
        if !RateControl::is_same_variant(&self.tunings.rate_control, &tunings.rate_control) {
            // TODO(bgrzesik): consider enabling switching between RateControl variants
            log::error!("Changing RateControl variant is not supported at the moment");
            return Err(EncodeError::Unsupported);
        }

        // Check if the tunings are or will be the same, in such case we skip.
        let skip = match self.tunings_queue.front() {
            Some((_, preceeding_tunnings)) if preceeding_tunnings == &tunings => true,
            None if self.tunings == tunings => true,
            _ => false,
        };

        if skip {
            log::debug!("Tuning skipped, the requested values are the same.");
            return Ok(());
        }

        // Check if applying tunings will succeed.
        self.try_tunings(&tunings)?;

        let when_counter = self.counter + self.queue.len();
        self.tunings_queue.push_back((when_counter, tunings));

        Ok(())
    }

    fn drain(&mut self) -> EncodeResult<Vec<Request>> {
        // [`LowDelay`] will not hold any frames, therefore the drain function shall never be called.
        Err(EncodeError::InvalidInternalState)
    }
}

#[cfg(test)]
mod tests {
    use crate::Fourcc;

    use super::*;

    #[derive(Debug, PartialEq, Eq)]
    enum MockRequest {
        KeyFrameRequest(u32, Tunings),
        InterframerRequest(u32, Tunings),
    }

    struct MockDelegate;

    impl LowDelayDelegate<u32, u32, MockRequest> for LowDelay<u32, u32, MockDelegate, MockRequest> {
        fn request_interframe(
            &mut self,
            input: u32,
            _input_meta: FrameMetadata,
        ) -> EncodeResult<MockRequest> {
            Ok(MockRequest::InterframerRequest(input, self.tunings.clone()))
        }

        fn request_keyframe(
            &mut self,
            input: u32,
            _input_meta: FrameMetadata,
            _idr: bool,
        ) -> EncodeResult<MockRequest> {
            Ok(MockRequest::KeyFrameRequest(input, self.tunings.clone()))
        }

        fn try_tunings(&self, _tunings: &Tunings) -> EncodeResult<()> {
            Ok(())
        }

        fn apply_tunings(&mut self, _tunings: &Tunings) -> EncodeResult<()> {
            Ok(())
        }
    }

    fn dummy_frame_meta(timestamp: u64, force_keyframe: bool) -> FrameMetadata {
        FrameMetadata {
            timestamp,
            display_resolution: crate::Resolution {
                width: 0,
                height: 0,
            },
            layout: crate::FrameLayout {
                format: (Fourcc::from(b"NV12"), 0),
                size: crate::Resolution {
                    width: 0,
                    height: 0,
                },
                planes: vec![],
            },
            force_keyframe,
        }
    }

    /// This test ensures that Tunings change applies to only frames following the change
    #[test]
    fn test_tuning_delay() {
        let _ = env_logger::try_init();

        let tunings_prev = Tunings {
            framerate: 1,
            ..Default::default()
        };

        let mut predictor: LowDelay<u32, u32, MockDelegate, MockRequest> = LowDelay {
            queue: Default::default(),
            references: Default::default(),
            counter: 0,
            limit: 1028,
            delegate: MockDelegate,
            tunings: tunings_prev.clone(),
            tunings_queue: Default::default(),
            _phantom: Default::default(),
        };

        let mut requests = Vec::new();

        requests.extend(predictor.new_frame(0, dummy_frame_meta(0, false)).unwrap());
        requests.extend(predictor.new_frame(1, dummy_frame_meta(1, false)).unwrap());
        requests.extend(predictor.new_frame(2, dummy_frame_meta(2, false)).unwrap());
        requests.extend(predictor.new_frame(3, dummy_frame_meta(3, false)).unwrap());

        let tunings_next = Tunings {
            framerate: 2,
            ..Default::default()
        };

        predictor.tune(tunings_next.clone()).unwrap();

        assert_eq!(predictor.tunings, tunings_prev);

        requests.extend(predictor.new_frame(4, dummy_frame_meta(4, false)).unwrap());

        requests.extend(predictor.reconstructed(0).unwrap());
        requests.extend(predictor.reconstructed(1).unwrap());
        requests.extend(predictor.reconstructed(2).unwrap());

        assert_eq!(predictor.tunings, tunings_prev);
        requests.extend(predictor.reconstructed(3).unwrap());

        assert_eq!(predictor.tunings, tunings_next);
        requests.extend(predictor.reconstructed(4).unwrap());

        assert_eq!(
            requests,
            vec![
                MockRequest::KeyFrameRequest(0, tunings_prev.clone()),
                MockRequest::InterframerRequest(1, tunings_prev.clone()),
                MockRequest::InterframerRequest(2, tunings_prev.clone()),
                MockRequest::InterframerRequest(3, tunings_prev.clone()),
                MockRequest::InterframerRequest(4, tunings_next.clone()),
            ]
        );
    }
}
