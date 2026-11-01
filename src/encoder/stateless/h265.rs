// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::rc::Rc;

use crate::codec::h265::parser::Pps;
use crate::codec::h265::parser::SliceType;
use crate::codec::h265::parser::Sps;
use crate::encoder::h265::EncoderConfig;
use crate::encoder::h265::H265;
use crate::encoder::stateless::h265::predictor::LowDelayH265;
use crate::encoder::stateless::BackendPromise;
use crate::encoder::stateless::BitstreamPromise;
use crate::encoder::stateless::FrameMetadata;
use crate::encoder::stateless::Predictor;
use crate::encoder::stateless::StatelessBackendResult;
use crate::encoder::stateless::StatelessCodec;
use crate::encoder::stateless::StatelessEncoderBackendImport;
use crate::encoder::stateless::StatelessEncoderExecute;
use crate::encoder::stateless::StatelessVideoEncoderBackend;
use crate::encoder::EncodeResult;
use crate::encoder::PredictionStructure;
use crate::encoder::Tunings;
use crate::BlockingMode;

mod predictor;

#[cfg(feature = "vaapi")]
pub mod vaapi;

/// `nuh_temporal_id_plus1` of every NALU the encoder emits. The encoder uses a single temporal
/// sub-layer, hence `TemporalId` is always zero. See H.265 7.4.2.2.
pub(crate) const NUH_TEMPORAL_ID_PLUS1: u8 = 1;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum IsReference {
    No,
    ShortTerm,
    LongTerm,
}

#[derive(Clone, Debug)]
pub struct DpbEntryMeta {
    /// Picture order count. Unlike H.264 this is a full PicOrderCntVal, the encoder never uses
    /// more than one temporal sub-layer, therefore the value is never negative.
    poc: i32,
    is_reference: IsReference,
}

/// Frame structure used in the backend representing currently encoded frame or references used
/// for its encoding.
pub struct DpbEntry<R> {
    /// Reconstructed picture
    recon_pic: R,
    /// Decoded picture buffer entry metadata
    meta: DpbEntryMeta,
}

/// Stateless H.265 encoder backend input.
pub struct BackendRequest<P, R> {
    sps: Rc<Sps>,
    pps: Rc<Pps>,

    /// Type of the single slice segment covering the whole picture. H.265 has no
    /// `SliceHeaderBuilder` counterpart yet, therefore the backend derives the remaining slice
    /// segment header syntax elements from `sps`, `pps` and the reference lists.
    slice_type: SliceType,

    /// Input frame to be encoded
    input: P,

    /// Input frame metadata
    input_meta: FrameMetadata,

    /// DPB entry metadata
    dpb_meta: DpbEntryMeta,

    /// Reference lists
    ref_list_0: Vec<Rc<DpbEntry<R>>>,
    ref_list_1: Vec<Rc<DpbEntry<R>>>,

    /// Period between intra frames
    intra_period: u32,

    /// Period between intra frame and P frame
    ip_period: u32,

    /// Number of coding tree units to be encoded in slice
    num_ctus: usize,

    /// True whenever the result is IDR
    is_idr: bool,

    /// [`Tunings`] for the frame
    tunings: Tunings,

    /// Container for the request output. [`StatelessH265EncoderBackend`] impl shall move it and
    /// append the slice data to it. This prevents unnecessary copying of bitstream around.
    coded_output: Vec<u8>,
}

/// Wrapper type for [`BackendPromise<Output = R>`], with additional
/// metadata.
pub struct ReferencePromise<P>
where
    P: BackendPromise,
{
    /// Slice data and reconstructed surface promise
    recon: P,

    /// [`DpbEntryMeta`] of reconstructed surface
    dpb_meta: DpbEntryMeta,
}

impl<P> BackendPromise for ReferencePromise<P>
where
    P: BackendPromise,
{
    type Output = DpbEntry<P::Output>;

    fn is_ready(&self) -> bool {
        self.recon.is_ready()
    }

    fn sync(self) -> StatelessBackendResult<Self::Output> {
        let recon_pic = self.recon.sync()?;

        log::trace!("synced recon picture poc={}", self.dpb_meta.poc);

        Ok(DpbEntry { recon_pic, meta: self.dpb_meta })
    }
}

impl<Backend> StatelessCodec<Backend> for H265
where
    Backend: StatelessVideoEncoderBackend<H265>,
{
    type Reference = DpbEntry<Backend::Reconstructed>;

    type Request = BackendRequest<Backend::Picture, Backend::Reconstructed>;

    type CodedPromise = BitstreamPromise<Backend::CodedPromise>;

    type ReferencePromise = ReferencePromise<Backend::ReconPromise>;
}

/// Trait for stateless encoder backend for H.265
pub trait StatelessH265EncoderBackend: StatelessVideoEncoderBackend<H265> {
    /// Submit a [`BackendRequest`] to the backend. This operation returns both a
    /// [`StatelessVideoEncoderBackend::CodedPromise`] and a
    /// [`StatelessVideoEncoderBackend::ReconPromise`] with resulting slice data.
    fn encode_slice(
        &mut self,
        request: BackendRequest<Self::Picture, Self::Reconstructed>,
    ) -> StatelessBackendResult<(Self::ReconPromise, Self::CodedPromise)>;
}

pub type StatelessEncoder<Handle, Backend> =
    crate::encoder::stateless::StatelessEncoder<H265, Handle, Backend>;

impl<Handle, Backend> StatelessEncoderExecute<H265, Handle, Backend>
    for StatelessEncoder<Handle, Backend>
where
    Backend: StatelessH265EncoderBackend,
{
    fn execute(
        &mut self,
        request: BackendRequest<Backend::Picture, Backend::Reconstructed>,
    ) -> EncodeResult<()> {
        let meta = request.input_meta.clone();
        let dpb_meta = request.dpb_meta.clone();

        // The [`BackendRequest`] has a frame from predictor. Decreasing internal counter.
        self.predictor_frame_count -= 1;

        log::trace!("submitting new request");
        let (recon, bitstream) = self.backend.encode_slice(request)?;

        // Wrap promise from backend with headers and metadata
        let slice_promise = BitstreamPromise { bitstream, meta };

        self.output_queue.add_promise(slice_promise);

        let ref_promise = ReferencePromise { recon, dpb_meta };

        self.recon_queue.add_promise(ref_promise);

        Ok(())
    }
}

impl<Handle, Backend> StatelessEncoder<Handle, Backend>
where
    Backend: StatelessH265EncoderBackend,
    Backend: StatelessEncoderBackendImport<Handle, Backend::Picture>,
{
    fn new_h265(backend: Backend, config: EncoderConfig, mode: BlockingMode) -> EncodeResult<Self> {
        let predictor: Box<dyn Predictor<_, _, _>> = match config.pred_structure {
            PredictionStructure::LowDelay { limit } => Box::new(LowDelayH265::new(config, limit)),
        };

        Self::new(backend, mode, predictor)
    }
}
