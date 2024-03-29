// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::rc::Rc;

use crate::codec::av1::parser::BitDepth;
use crate::codec::av1::parser::FrameHeaderObu;
use crate::codec::av1::parser::Profile;
use crate::codec::av1::parser::ReferenceFrameType;
use crate::codec::av1::parser::SequenceHeaderObu;
use crate::codec::av1::parser::REFS_PER_FRAME;
use crate::encoder::stateless::av1::predictor::LowDelayAV1;
use crate::encoder::stateless::BitstreamPromise;
use crate::encoder::stateless::EncodeResult;
use crate::encoder::stateless::PredictionStructure;
use crate::encoder::stateless::Predictor;
use crate::encoder::stateless::StatelessBackendResult;
use crate::encoder::stateless::StatelessCodec;
use crate::encoder::stateless::StatelessEncoderExecute;
use crate::encoder::stateless::StatelessVideoEncoderBackend;
use crate::encoder::FrameMetadata;
use crate::encoder::Tunings;
use crate::BlockingMode;
use crate::Resolution;

mod predictor;

#[cfg(feature = "vaapi")]
pub mod vaapi;

#[derive(Clone)]
pub struct EncoderConfig {
    pub profile: Profile,
    pub bit_depth: BitDepth,
    pub resolution: Resolution,
    pub pred_structure: PredictionStructure,
    /// Initial tunings values
    pub initial_tunings: Tunings,
}

impl Default for EncoderConfig {
    fn default() -> Self {
        // Artificially encoder configuration with intent to be widely supported.
        Self {
            profile: Profile::Profile0,
            bit_depth: BitDepth::Depth8,
            resolution: Resolution {
                width: 320,
                height: 240,
            },
            pred_structure: PredictionStructure::LowDelay { limit: 1024 },
            initial_tunings: Default::default(),
        }
    }
}

pub struct BackendRequest<P, R> {
    /// Current sequence's header OBU
    sequence: SequenceHeaderObu,

    /// Current frame OBU contains
    frame: FrameHeaderObu,

    /// Input frame to be encoded
    input: P,

    /// Input frame metadata
    input_meta: FrameMetadata,

    /// References for the frame to be encoded
    /// Use `ReferenceFrameType::Golden - ReferenceFrameType::Last` for indexing
    references: [Option<Rc<R>>; REFS_PER_FRAME],

    /// The reference frame search priority list. From highest to lowest
    /// Use [`ReferenceFrameType::Intra`] for invalid
    ref_frame_ctrl_l0: [ReferenceFrameType; REFS_PER_FRAME],
    ref_frame_ctrl_l1: [ReferenceFrameType; REFS_PER_FRAME],

    /// Period between intra frames
    intra_period: u32,

    /// Period between intra frame and P frame
    ip_period: u32,

    /// Container for the request output. [`StatelessAV1EncoderBackend`] impl shall move it and
    /// append the slice data to it. This prevents unnecessary copying of bitstream around.
    coded_output: Vec<u8>,
}

pub struct AV1;

impl<Backend> StatelessCodec<Backend> for AV1
where
    Backend: StatelessVideoEncoderBackend<AV1>,
{
    type Reference = Backend::Reconstructed;

    type Request = BackendRequest<Backend::Picture, Backend::Reconstructed>;

    type CodedPromise = BitstreamPromise<Backend::CodedPromise>;

    type ReferencePromise = Backend::ReconPromise;
}

/// Trait for stateless encoder backend for H.264
pub trait StatelessAV1EncoderBackend: StatelessVideoEncoderBackend<AV1> {
    /// Submit a [`BackendRequest`] to the backend. This operation returns both a
    /// [`Self::CodedPromise`] and a [`Self::ReconPromise`] with resulting slice data.
    fn encode_tile_group(
        &mut self,
        request: BackendRequest<Self::Picture, Self::Reconstructed>,
    ) -> StatelessBackendResult<(Self::ReconPromise, Self::CodedPromise)>;
}

pub type StatelessEncoder<Handle, Backend> =
    crate::encoder::stateless::StatelessEncoder<AV1, Handle, Backend>;

impl<Handle, Backend> StatelessEncoderExecute<AV1, Handle, Backend>
    for StatelessEncoder<Handle, Backend>
where
    Backend: StatelessAV1EncoderBackend,
{
    fn execute(
        &mut self,
        request: BackendRequest<Backend::Picture, Backend::Reconstructed>,
    ) -> EncodeResult<()> {
        let meta = request.input_meta.clone();

        // The [`BackendRequest`] has a frame from predictor. Decresing internal counter.
        self.predictor_frame_count -= 1;

        log::trace!("submitting new request");
        let (recon, bitstream) = self.backend.encode_tile_group(request)?;

        // Wrap promise from backend with headers and metadata
        let tilegroup_promise = BitstreamPromise { bitstream, meta };

        self.output_queue.add_promise(tilegroup_promise);

        self.recon_queue.add_promise(recon);

        Ok(())
    }
}

impl<Handle, Backend> StatelessEncoder<Handle, Backend>
where
    Backend: StatelessAV1EncoderBackend,
{
    fn new_av1(backend: Backend, config: EncoderConfig, mode: BlockingMode) -> EncodeResult<Self> {
        let predictor: Box<dyn Predictor<_, _, _>> = match config.pred_structure {
            PredictionStructure::LowDelay { limit } => Box::new(LowDelayAV1::new(config, limit)),
        };

        Self::new(backend, mode, predictor)
    }
}
