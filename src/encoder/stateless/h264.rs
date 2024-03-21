// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::rc::Rc;

use crate::codec::h264::parser::Level;
use crate::codec::h264::parser::Pps;
use crate::codec::h264::parser::Profile;
use crate::codec::h264::parser::SliceHeader;
use crate::codec::h264::parser::Sps;
use crate::encoder::stateless::h264::predictor::LowDelay;
use crate::encoder::stateless::h264::predictor::PredictionStructure;
use crate::encoder::stateless::BackendPromise;
use crate::encoder::stateless::BitstreamPromise;
use crate::encoder::stateless::EncodeResult;
use crate::encoder::stateless::FrameMetadata;
use crate::encoder::stateless::Predictor;
use crate::encoder::stateless::StatelessBackendResult;
use crate::encoder::stateless::StatelessCodec;
use crate::encoder::stateless::StatelessEncoder;
use crate::encoder::stateless::StatelessEncoderBackendImport;
use crate::encoder::stateless::StatelessEncoderExecute;
use crate::encoder::stateless::StatelessVideoEncoderBackend;
use crate::encoder::Bitrate;
use crate::BlockingMode;
use crate::Resolution;

mod predictor;

#[cfg(feature = "vaapi")]
pub mod vaapi;

#[derive(Clone)]
pub struct EncoderConfig {
    pub bitrate: Bitrate,
    pub framerate: u32,
    pub resolution: Resolution,
    pub profile: Profile,
    pub level: Level,
    pub pred_structure: PredictionStructure,
    pub default_qp: u8,
}

impl Default for EncoderConfig {
    fn default() -> Self {
        // Artificially encoder configuration with intent to be widely supported.
        Self {
            bitrate: Bitrate::Constant(30_000_000),
            framerate: 30,
            resolution: Resolution {
                width: 320,
                height: 240,
            },
            profile: Profile::Baseline,
            level: Level::L4,
            pred_structure: PredictionStructure::LowDelay {
                tail: 1,
                limit: 2048,
            },
            default_qp: 26,
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum IsReference {
    No,
    ShortTerm,
    LongTerm,
}

#[derive(Clone, Debug)]
pub struct DpbEntryMeta {
    /// Picture order count
    poc: u16,
    frame_num: u32,
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

/// Stateless H.264 encoder backend input.
pub struct BackendRequest<P, R> {
    sps: Rc<Sps>,
    pps: Rc<Pps>,
    header: SliceHeader,

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

    /// Number of macroblock to be encoded in slice
    num_macroblocks: usize,

    /// True whenever the result is IDR
    is_idr: bool,

    /// Current expected bitrate
    bitrate: Bitrate,

    /// Container for the request output. [`StatelessH264EncoderBackend`] impl shall move it and
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

        log::trace!("synced recon picture frame_num={}", self.dpb_meta.frame_num);

        Ok(DpbEntry {
            recon_pic,
            meta: self.dpb_meta,
        })
    }
}

pub struct H264;

impl<Backend> StatelessCodec<Backend> for H264
where
    Backend: StatelessVideoEncoderBackend<H264>,
{
    type Reference = DpbEntry<Backend::Reconstructed>;

    type Request = BackendRequest<Backend::Picture, Backend::Reconstructed>;

    type CodedPromise = BitstreamPromise<Backend::CodedPromise>;

    type ReferencePromise = ReferencePromise<Backend::ReconPromise>;
}

/// Trait for stateless encoder backend for H.264
pub trait StatelessH264EncoderBackend: StatelessVideoEncoderBackend<H264> {
    /// Submit a [`BackendRequest`] to the backend. This operation returns both a
    /// [`Self::CodedPromise`] and a [`Self::ReconPromise`] with resulting slice data.
    fn encode_slice(
        &mut self,
        request: BackendRequest<Self::Picture, Self::Reconstructed>,
    ) -> StatelessBackendResult<(Self::ReconPromise, Self::CodedPromise)>;
}

impl<Handle, Backend> StatelessEncoderExecute<H264, Handle, Backend>
    for StatelessEncoder<H264, Handle, Backend>
where
    Backend: StatelessH264EncoderBackend,
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

impl<Handle, Backend> StatelessEncoder<H264, Handle, Backend>
where
    Backend: StatelessH264EncoderBackend,
    Backend: StatelessEncoderBackendImport<Handle, Backend::Picture>,
{
    fn new_h264(backend: Backend, config: EncoderConfig, mode: BlockingMode) -> EncodeResult<Self> {
        let predictor: Box<dyn Predictor<_, _, _>> = match config.pred_structure {
            PredictionStructure::LowDelay { .. } => Box::new(LowDelay::new(config)),
        };

        Self::new(backend, mode, predictor)
    }
}
