// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use super::BackendRequest;
use super::EncoderConfig;
use crate::codec::vp9::parser::BitDepth;
use crate::codec::vp9::parser::FrameType;
use crate::codec::vp9::parser::Header;
use crate::codec::vp9::parser::Profile;
use crate::codec::vp9::parser::QuantizationParams;
use crate::encoder::stateless::predictor::LowDelay;
use crate::encoder::stateless::predictor::LowDelayDelegate;
use crate::encoder::stateless::vp9::ReferenceUse;
use crate::encoder::stateless::EncodeResult;
use crate::encoder::FrameMetadata;
use crate::encoder::RateControl;
use crate::encoder::Tunings;

pub(crate) struct LowDelayVP9Delegate {
    config: EncoderConfig,
}

pub(crate) type LowDelayVP9<Picture, Reference> =
    LowDelay<Picture, Reference, LowDelayVP9Delegate, BackendRequest<Picture, Reference>>;

impl<Picture, Reference> LowDelayVP9<Picture, Reference> {
    pub(super) fn new(config: EncoderConfig, limit: u16) -> Self {
        Self {
            queue: Default::default(),
            references: Default::default(),
            counter: 0,
            limit,
            tunings: config.initial_tunings.clone(),
            delegate: LowDelayVP9Delegate { config },
            tunings_queue: Default::default(),
            _phantom: Default::default(),
        }
    }

    fn create_frame_header(&mut self, frame_type: FrameType) -> Header {
        let width = self.delegate.config.resolution.width;
        let height = self.delegate.config.resolution.height;

        let profile = match self.delegate.config.bit_depth {
            BitDepth::Depth8 => Profile::Profile0,
            BitDepth::Depth10 | BitDepth::Depth12 => Profile::Profile2,
        };

        const MIN_Q_IDX: u8 = 0;
        const MAX_Q_IDX: u8 = 255;

        let base_q_idx = if let RateControl::ConstantQuality(base_q_idx) = self.tunings.rate_control
        {
            // Limit Q index to valid values
            base_q_idx.clamp(MIN_Q_IDX as u32, MAX_Q_IDX as u32) as u8
        } else {
            // Pick middle Q index
            (MAX_Q_IDX + MIN_Q_IDX) / 2
        };

        Header {
            profile,
            bit_depth: BitDepth::Depth10,
            frame_type,
            show_frame: true,
            error_resilient_mode: true,
            width,
            height,
            render_and_frame_size_different: false,
            intra_only: matches!(frame_type, FrameType::KeyFrame),
            refresh_frame_flags: 0x01,
            ref_frame_idx: [0, 0, 0],
            quant: QuantizationParams {
                base_q_idx,
                ..Default::default()
            },

            ..Default::default()
        }
    }
}

impl<Picture, Reference> LowDelayDelegate<Picture, Reference, BackendRequest<Picture, Reference>>
    for LowDelayVP9<Picture, Reference>
{
    fn request_keyframe(
        &mut self,
        input: Picture,
        input_meta: FrameMetadata,
        _idr: bool,
    ) -> EncodeResult<BackendRequest<Picture, Reference>> {
        log::trace!("Requested keyframe timestamp={}", input_meta.timestamp);

        let request = BackendRequest {
            header: self.create_frame_header(FrameType::KeyFrame),
            input,
            input_meta,
            last_frame_ref: None,
            golden_frame_ref: None,
            altref_frame_ref: None,
            rate_control: self.tunings.rate_control.clone(),
            coded_output: Vec::new(),
        };

        Ok(request)
    }

    fn request_interframe(
        &mut self,
        input: Picture,
        input_meta: FrameMetadata,
    ) -> EncodeResult<BackendRequest<Picture, Reference>> {
        log::trace!("Requested interframe timestamp={}", input_meta.timestamp);

        let ref_frame = self.references.pop_front().unwrap();

        let request = BackendRequest {
            header: self.create_frame_header(FrameType::InterFrame),
            input,
            input_meta,
            last_frame_ref: Some((ref_frame, ReferenceUse::Single)),
            golden_frame_ref: None,
            altref_frame_ref: None,
            rate_control: self.tunings.rate_control.clone(),
            coded_output: Vec::new(),
        };

        self.references.clear();

        Ok(request)
    }

    fn try_tunings(&self, _tunings: &Tunings) -> EncodeResult<()> {
        Ok(())
    }

    fn apply_tunings(&mut self, _tunings: &Tunings) -> EncodeResult<()> {
        Ok(())
    }
}
