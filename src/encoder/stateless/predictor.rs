// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::collections::VecDeque;
use std::rc::Rc;

use crate::encoder::stateless::EncodeError;
use crate::encoder::stateless::EncodeResult;
use crate::encoder::stateless::Predictor;
use crate::encoder::FrameMetadata;

#[derive(Clone)]
pub enum PredictionStructure {
    /// Simplest prediction structure, suitable eg. for RTC. Interframe is produced at the start of
    /// the stream and every time when [`limit`] frames are reached. Following interframe frames
    /// are frames relying solely on the last frame.
    LowDelay { limit: u16 },
}

pub(crate) struct LowDelay<Picture, Reference, Delegate, Request> {
    /// Pending frames for encoding
    pub(super) queue: VecDeque<(Picture, FrameMetadata)>,

    /// Availabe frames for references
    pub(super) references: VecDeque<Rc<Reference>>,

    /// Current frame counter
    pub(super) counter: usize,

    /// The number of frames between intra frames
    pub(super) limit: u16,

    pub(super) delegate: Delegate,

    pub(super) _phantom: std::marker::PhantomData<Request>,
}

pub(crate) trait LowDelayDelegate<Picture, References, Request> {
    fn request_keyframe(
        &mut self,
        input: Picture,
        input_meta: FrameMetadata,
        idr: bool,
    ) -> EncodeResult<Request>;

    fn request_interframe(
        &mut self,
        input: Picture,
        input_meta: FrameMetadata,
    ) -> EncodeResult<Request>;
}

impl<Picture, Reference, Delegate, Request> LowDelay<Picture, Reference, Delegate, Request>
where
    Self: LowDelayDelegate<Picture, Reference, Request>,
{
    fn next_request(&mut self) -> EncodeResult<Vec<Request>> {
        let mut requests = Vec::new();

        loop {
            match self.queue.pop_front() {
                // Nothing to do. Quit.
                None => break,

                // If first frame in the sequence or forced IDR then create IDR request.
                Some((input, meta)) if self.counter == 0 || meta.force_keyframe => {
                    self.references.clear();

                    let request =
                        self.request_keyframe(input, meta, self.counter == 0)?;

                    requests.push(request);
                    self.counter = self.counter.wrapping_add(1) % (self.limit as usize);
                }

                // There is no enough frames reconstructed
                Some((input, meta)) if self.references.is_empty() => {
                    self.queue.push_front((input, meta));
                    break;
                }

                Some((input, meta)) => {
                    let request = self.request_interframe(input, meta)?;

                    requests.push(request);
                    self.counter = self.counter.wrapping_add(1) % (self.limit as usize);

                    break;
                }
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
        // Add new frame in the request queue and request new encoding if possible
        self.queue.push_back((input, frame_metadata));
        self.next_request()
    }

    fn reconstructed(&mut self, reference: Reference) -> EncodeResult<Vec<Request>> {
        // Add new reconstructed surface and request next encoding if possible
        self.references.push_back(Rc::new(reference));
        self.next_request()
    }

    fn drain(&mut self) -> EncodeResult<Vec<Request>> {
        // [`LowDelay`] will not hold any frames, therefore the drain function shall never be called.
        Err(EncodeError::InvalidInternalState)
    }
}
