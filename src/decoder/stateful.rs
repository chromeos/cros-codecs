// Copyright 2023 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use thiserror::Error;
use zerocopy::AsBytes;

use crate::decoder::DecodedHandle;
use crate::decoder::StreamInfo;
use crate::DecodedFormat;
use crate::Resolution;

use std::sync::Arc;

pub enum DecodeError {
    InvalidState,
    InvalidData,
}

pub enum ConfigError {
    InvalidState,
    NotSupported,
    InvalidConfig,
}

/// Error returned by stateful backend methods.
#[derive(Error, Debug)]
pub enum StatefulBackendError {
    #[error("not enough resources to proceed with the operation now")]
    OutOfResources,
    #[error("this format is not supported")]
    UnsupportedFormat,
    #[error(transparent)]
    Other(#[from] anyhow::Error),
}

#[derive(Copy, Clone, Debug)]
pub enum EncodedFormat {
    AV1,
    H264,
    H265,
    VP8,
    VP9,
}

/// Common trait shared by all stateful video decoder backends, providing codec-independent
/// methods.
pub trait StatefulDecoderBackend<Codec: StatefulCodec> {
    /// The type that the backend returns as a result of a decode operation.
    /// This will usually be some backend-specific type with a resource and a
    /// resource pool so that said buffer can be reused for another decode
    /// operation when it goes out of scope.
    type Handle: DecodedHandle;

    /// Returns the current decoding parameters, as parsed from the stream.
    fn stream_info(&self) -> Option<&StreamInfo>;
}

pub trait StatefulCodec {
    /// State that needs to be kept during a decoding operation, typed by backend.
    type DecoderState<B: StatefulDecoderBackend<Self>>;
}

#[derive(Copy, Clone, Debug)]

pub enum ColorGamut {
    Bt709,
    Bt470bg,
    Smpte170m,
}

#[derive(Copy, Clone, Debug)]
pub enum ColorTransfer {
    Bt709,
    Smpte170m,
    Iec61966_2_1,
}

#[derive(Copy, Clone, Debug)]
pub enum ColorMatrix {
    Rgb,
    Bt709,
    Bt470bg,
    Smpte170m,
}

#[derive(Copy, Clone, Debug)]
pub struct VideoColorSpace {
    primaries: ColorGamut,
    transfer: ColorTransfer,
    matrix: ColorMatrix,
}

#[derive(Clone, Debug)]
pub struct EncodedVideoChunk {
    data: Arc<Vec<u8>>,
}

pub struct VideoFrame {
    format: DecodedFormat,
    coded_resolution: Resolution,
    display_resolution: Resolution,
    duration: usize,
    timestamp: usize,
    color_space: VideoColorSpace,
    buffer: Option<Box<dyn AsBytes>>,
}

pub enum StatefulDecoderEvent {
    /// The next frame has been decoded.
    FrameReady(VideoFrame),
    /// The format of the stream has changed and action is required. A VideoFrame
    /// with no buffer is returned.
    FormatChanged(VideoFrame),
}

pub trait StatefulVideoDecoder<M> {
    /// Add a new chunk of video to decoding queue, this is a non-blocking method
    fn decode(&mut self, chunk: EncodedVideoChunk) -> Result<(), DecodeError>;

    /// Return information about the currently decoded stream
    fn stream_info(&self) -> Option<&StreamInfo>;

    /// Close the decoder and prevent it from future use
    fn close(&mut self);

    /// Finish processing all pending decode requests
    fn flush(&mut self) -> Result<(), DecodeError>;

    /// Returns the next event, if there is any pending.
    fn next_event(&mut self) -> Option<StatefulDecoderEvent>;
}

#[derive(Copy, Clone, Debug, Default)]
pub enum StatefulDecoderState {
    #[default]
    Unconfigured,
    Configured,
    Closed,
}

pub struct StatefulDecoder<C, B>
where
    C: StatefulCodec,
    B: StatefulDecoderBackend<C>,
{
    decoding_state: StatefulDecoderState,

    /// The backend used for hardware acceleration.
    backend: B,

    /// Codec-specific state.
    codec_state: C::DecoderState<B>,
}

impl<C, B> StatefulDecoder<C, B>
where
    C: StatefulCodec,
    B: StatefulDecoderBackend<C>,
    C::DecoderState<B>: Default,
{
    pub fn new(backend: B) -> Self {
        Self {
            backend,
            decoding_state: Default::default(),
            codec_state: Default::default(),
        }
    }
}
