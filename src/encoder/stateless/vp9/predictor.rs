// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::collections::VecDeque;
use std::rc::Rc;

use super::BackendRequest;
use super::EncoderConfig;
use crate::codec::vp9::parser::FrameType;
use crate::codec::vp9::parser::Header;
use crate::encoder::stateless::vp9::ReferenceUse;
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

/// See [`PredictionStructure::LowDelay`]
pub(super) struct LowDelay<P, R> {
    queue: VecDeque<(P, FrameMetadata)>,

    references: VecDeque<Rc<R>>,

    counter: usize,

    /// Encoder config
    config: EncoderConfig,
}

impl<P, R> LowDelay<P, R> {
    pub(super) fn new(config: EncoderConfig) -> Self {
        Self {
            queue: Default::default(),
            references: Default::default(),
            counter: 0,
            config,
        }
    }

    fn request_keyframe(
        &mut self,
        input: P,
        input_meta: FrameMetadata,
    ) -> EncodeResult<Vec<BackendRequest<P, R>>> {
        log::trace!("Requested keyframe timestamp={}", input_meta.timestamp);

        let header = Header {
            frame_type: FrameType::KeyFrame,
            show_frame: true,
            error_resilient_mode: true,
            width: self.config.resolution.width,
            height: self.config.resolution.height,
            render_and_frame_size_different: false,
            render_width: self.config.resolution.width,
            render_height: self.config.resolution.height,
            intra_only: true,
            refresh_frame_flags: 0x01,
            ref_frame_idx: [0, 0, 0],

            ..Default::default()
        };

        let request = BackendRequest {
            header,
            input,
            input_meta,
            last_frame_ref: None,
            golden_frame_ref: None,
            altref_frame_ref: None,
            bitrate: self.config.bitrate.clone(),
            coded_output: Vec::new(),
        };

        self.counter += 1;

        Ok(vec![request])
    }

    fn request_interframe(
        &mut self,
        input: P,
        input_meta: FrameMetadata,
    ) -> EncodeResult<Vec<BackendRequest<P, R>>> {
        log::trace!("Requested interframe timestamp={}", input_meta.timestamp);

        let header = Header {
            frame_type: FrameType::InterFrame,
            show_frame: true,
            error_resilient_mode: true,
            width: self.config.resolution.width,
            height: self.config.resolution.height,
            render_and_frame_size_different: false,
            render_width: self.config.resolution.width,
            render_height: self.config.resolution.height,
            intra_only: false,
            refresh_frame_flags: 0x01,
            ref_frame_idx: [0; 3],

            ..Default::default()
        };

        let ref_frame = self.references.pop_front().unwrap();

        let request = BackendRequest {
            header,
            input,
            input_meta,
            last_frame_ref: Some((ref_frame, ReferenceUse::Single)),
            golden_frame_ref: None,
            altref_frame_ref: None,
            bitrate: self.config.bitrate.clone(),
            coded_output: Vec::new(),
        };

        self.counter += 1;
        self.references.clear();

        Ok(vec![request])
    }

    fn next_request(&mut self) -> EncodeResult<Vec<BackendRequest<P, R>>> {
        match self.queue.pop_front() {
            // Nothing to do. Quit.
            None => Ok(Vec::new()),
            // If first frame in the sequence or forced IDR then create IDR request.
            Some((input, meta)) if self.counter == 0 || meta.force_keyframe => {
                self.request_keyframe(input, meta)
            }
            // There is no enough frames reconstructed
            Some((input, meta)) if self.references.is_empty() => {
                self.queue.push_front((input, meta));
                Ok(Vec::new())
            }

            Some((input, meta)) => self.request_interframe(input, meta),
        }
    }
}

impl<P, R> Predictor<P, R, BackendRequest<P, R>> for LowDelay<P, R> {
    fn new_frame(
        &mut self,
        input: P,
        frame_metadata: FrameMetadata,
    ) -> EncodeResult<Vec<BackendRequest<P, R>>> {
        // Add new frame in the request queue and request new encoding if possible
        self.queue.push_back((input, frame_metadata));
        self.next_request()
    }

    fn reconstructed(&mut self, recon: R) -> EncodeResult<Vec<BackendRequest<P, R>>> {
        // Add new reconstructed surface and request next encoding if possible
        self.references.push_back(Rc::new(recon));
        self.next_request()
    }

    fn drain(&mut self) -> EncodeResult<Vec<BackendRequest<P, R>>> {
        // [`LowDelay`] will not hold any frames, therefore the drain function shall never be called.
        Err(EncodeError::InvalidInternalState)
    }
}
