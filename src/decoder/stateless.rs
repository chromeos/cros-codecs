// Copyright 2023 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

//! Stateless decoders.
//!
//! Stateless here refers to the backend API targeted by these decoders. The decoders themselves do
//! hold the decoding state so the backend doesn't need to.
//!
//! The [`StatelessDecoder`] struct is the basis of all stateless decoders. It is created by
//! combining a codec codec to a [backend](crate::backend), after which bitstream units can be
//! submitted through the [`StatelessDecoder::decode`] method.

pub mod av1;
pub mod h264;
pub mod h265;
pub mod vp8;
pub mod vp9;

use thiserror::Error;

use crate::decoder::BlockingMode;
use crate::decoder::DecodedHandle;
use crate::decoder::DecoderEvent;
use crate::decoder::DecoderFormatNegotiator;
use crate::decoder::FramePool;
use crate::decoder::ReadyFramesQueue;
use crate::decoder::StreamInfo;
use crate::DecodedFormat;
use crate::Resolution;

/// Error returned by stateless backend methods.
#[derive(Error, Debug)]
pub enum StatelessBackendError {
    #[error("not enough resources to proceed with the operation now")]
    OutOfResources,
    #[error(transparent)]
    Other(#[from] anyhow::Error),
}

/// Result type returned by stateless backend methods.
pub type StatelessBackendResult<T> = Result<T, StatelessBackendError>;

/// Decoder implementations can use this struct to represent their decoding state.
///
/// `F` is a type containing the parsed stream format, that the decoder will use for format
/// negotiation with the client.
#[derive(Default)]
enum DecodingState<F> {
    /// Decoder will ignore all input until format and resolution information passes by.
    #[default]
    AwaitingStreamInfo,
    /// Decoder is stopped until the client has confirmed the output format.
    AwaitingFormat(F),
    /// Decoder is currently decoding input.
    Decoding,
    /// Decoder has been reset after a flush, and can resume with the current parameters after
    /// seeing a key frame.
    Reset,
}

/// Error returned by the [`StatelessVideoDecoder::decode`] method.
#[derive(Debug, Error)]
pub enum DecodeError {
    #[error("not enough output buffers available to continue, need {0} more")]
    NotEnoughOutputBuffers(usize),
    #[error("cannot accept more input until pending events are processed")]
    CheckEvents,
    #[error(transparent)]
    DecoderError(#[from] anyhow::Error),
    #[error(transparent)]
    BackendError(#[from] StatelessBackendError),
}

mod private {
    use super::*;

    /// Private trait for methods we need to expose for crate types (e.g.
    /// [`DecoderFormatNegotiator`]s), but don't want to be directly used by the client.
    pub(super) trait StatelessVideoDecoder {
        /// Try to apply `format` to output frames. If successful, all frames emitted after the
        /// call will be in the new format.
        fn try_format(&mut self, format: DecodedFormat) -> anyhow::Result<()>;
    }
}

/// Specifies the type of picture that a backend will create for a given codec.
///
/// The picture type is state that is preserved from the start of a given frame to its submission
/// to the backend. Some codecs don't need it, in this case they can just set `Picture` to `()`.
pub trait StatelessDecoderBackendPicture<Codec: StatelessCodec> {
    /// Backend-specific type representing a frame being decoded. Useful for decoders that need
    /// to render a frame in several steps and to preserve its state in between.
    ///
    /// Backends that don't use this can simply set it to `()`.
    type Picture;
}

pub trait TryFormat<Codec: StatelessCodec> {
    /// Try to alter the decoded format.
    fn try_format(
        &mut self,
        format_info: &Codec::FormatInfo,
        format: DecodedFormat,
    ) -> anyhow::Result<()>;
}

/// Common trait shared by all stateless video decoder backends, providing codec-independent
/// methods.
pub trait StatelessDecoderBackend {
    /// The type that the backend returns as a result of a decode operation.
    /// This will usually be some backend-specific type with a resource and a
    /// resource pool so that said buffer can be reused for another decode
    /// operation when it goes out of scope.
    type Handle: DecodedHandle;

    /// Returns the current decoding parameters, as parsed from the stream.
    fn stream_info(&self) -> Option<&StreamInfo>;

    /// Returns the frame pool currently in use by the backend for `layer`.
    fn frame_pool(
        &mut self,
        layer: PoolLayer,
    ) -> Vec<&mut dyn FramePool<<Self::Handle as DecodedHandle>::Descriptor>>;
}

/// Helper to implement [`DecoderFormatNegotiator`] for stateless decoders.
struct StatelessDecoderFormatNegotiator<'a, D, H, FH, F>
where
    H: DecodedHandle,
    D: StatelessVideoDecoder<H>,
    F: Fn(&mut D, &FH),
{
    decoder: &'a mut D,
    format_hint: FH,
    apply_format: F,
    _mem_desc: std::marker::PhantomData<H::Descriptor>,
}

impl<'a, D, H, FH, F> StatelessDecoderFormatNegotiator<'a, D, H, FH, F>
where
    H: DecodedHandle,
    D: StatelessVideoDecoder<H>,
    F: Fn(&mut D, &FH),
{
    /// Creates a new format negotiator.
    ///
    /// `decoder` is the decoder negotiation is done for. The decoder is exclusively borrowed as
    /// long as this object exists.
    ///
    /// `format_hint` is a codec-specific structure describing the properties of the format.
    ///
    /// `apply_format` is a closure called when the object is dropped, and is responsible for
    /// applying the format and allowing decoding to resume.
    fn new(decoder: &'a mut D, format_hint: FH, apply_format: F) -> Self {
        Self {
            decoder,
            format_hint,
            apply_format,
            _mem_desc: std::marker::PhantomData,
        }
    }
}

impl<'a, D, H, FH, F> DecoderFormatNegotiator<'a, H::Descriptor>
    for StatelessDecoderFormatNegotiator<'a, D, H, FH, F>
where
    H: DecodedHandle,
    D: StatelessVideoDecoder<H> + private::StatelessVideoDecoder,
    F: Fn(&mut D, &FH),
{
    /// Try to apply `format` to output frames. If successful, all frames emitted after the
    /// call will be in the new format.
    fn try_format(&mut self, format: DecodedFormat) -> anyhow::Result<()> {
        self.decoder.try_format(format)
    }

    fn frame_pool(&mut self, layer: PoolLayer) -> Vec<&mut dyn FramePool<H::Descriptor>> {
        self.decoder.frame_pool(layer)
    }

    fn stream_info(&self) -> &StreamInfo {
        self.decoder.stream_info().unwrap()
    }
}

impl<'a, D, H, FH, F> Drop for StatelessDecoderFormatNegotiator<'a, D, H, FH, F>
where
    H: DecodedHandle,
    D: StatelessVideoDecoder<H>,
    F: Fn(&mut D, &FH),
{
    fn drop(&mut self) {
        (self.apply_format)(self.decoder, &self.format_hint)
    }
}

/// Controls the pool returned by [`StatelessVideoDecoder::frame_pool`].
#[derive(Debug, Clone, Copy)]
pub enum PoolLayer {
    /// The pool for the highest spatial layer.
    Highest,
    /// The pool for the given resolution.
    Layer(Resolution),
    /// All pools.
    All,
}

/// Stateless video decoder interface.
///
/// A stateless decoder differs from a stateful one in that its input and output queues are not
/// operating independently: a new decode unit can only be processed if there is already an output
/// resource available to receive its decoded content.
///
/// Therefore [`decode`] can refuse work if there is no output resource
/// available at the time of calling, in which case the caller is responsible for calling
/// [`decode`] again with the same parameters after processing at least one
/// pending output frame and returning it to the decoder.
///
/// The `M` generic parameter is the type of the memory descriptor backing the output frames.
///
/// [`decode`]: StatelessVideoDecoder::decode
pub trait StatelessVideoDecoder<H: DecodedHandle> {
    /// Attempts to decode `bitstream` if the current conditions allow it.
    ///
    /// This method will return [`DecodeError::CheckEvents`] if processing cannot take place until
    /// pending events are handled. This could either be because a change of output format has
    /// been detected that the client should acknowledge, or because there are no available output
    /// resources and dequeueing and returning pending frames will fix that. After the cause has
    /// been addressed, the client is responsible for calling this method again with the same data.
    ///
    /// The return value is the number of bytes in `bitstream` that have been processed. Usually
    /// this will be equal to the length of `bitstream`, but some codecs may only do partial
    /// processing if e.g. several units are sent at the same time. It is the responsibility of the
    /// caller to check that all submitted input has been processed, and to resubmit the
    /// unprocessed part if it hasn't. See the documentation of each codec for their expectations.
    fn decode(&mut self, timestamp: u64, bitstream: &[u8]) -> Result<usize, DecodeError>;

    /// Flush the decoder i.e. finish processing all pending decode requests and make sure the
    /// resulting frames are ready to be retrieved via [`next_event`].
    ///
    /// Note that after flushing, a key frame must be submitted before decoding can resume.
    ///
    /// [`next_event`]: StatelessVideoDecoder::next_event
    fn flush(&mut self) -> Result<(), DecodeError>;

    /// Returns the frame pool for `resolution` in use with the decoder. If
    /// `resolution` is None, the pool of the highest resolution is returned.
    ///
    /// Multiple pools may be in use for SVC streams, since each spatial layer
    /// will receive its frames from a separate pool.
    ///
    /// Useful to add new frames as decode targets.
    fn frame_pool(&mut self, layer: PoolLayer) -> Vec<&mut dyn FramePool<H::Descriptor>>;

    fn stream_info(&self) -> Option<&StreamInfo>;

    /// Returns the next event, if there is any pending.
    fn next_event(&mut self) -> Option<DecoderEvent<H>>;
}

pub trait StatelessCodec {
    /// Type providing current format information for the codec: resolution, color format, etc.
    ///
    /// For H.264 this would be the Sps, for VP8 or VP9 the frame header.
    type FormatInfo;
    /// State that needs to be kept during a decoding operation, typed by backend.
    type DecoderState<B: StatelessDecoderBackend + StatelessDecoderBackendPicture<Self>>;
}

/// A struct that serves as a basis to implement a stateless decoder.
///
/// A stateless decoder is defined by three generic parameters:
///
/// * A codec, represented by a type that implements [`StatelessCodec`]. This type defines the
/// codec-specific decoder state and other codec properties.
/// * A backend, i.e. an interface to talk to the hardware that accelerates decoding. An example is
/// the VAAPI backend that uses VAAPI for acceleration. The backend will typically itself be typed
/// against a memory decriptor, defining how memory is provided for decoded frames.
///
/// So for instance, a decoder for the H264 codec, using VAAPI for acceleration with self-managed
/// memory, will have the following type:
///
/// ```text
/// let decoder: StatelessDecoder<H264, VaapiBackend<()>>;
/// ```
///
/// This struct just manages the high-level decoder state as well as the queue of decoded frames.
/// All the rest is left to codec-specific code.
pub struct StatelessDecoder<C, B>
where
    C: StatelessCodec,
    B: StatelessDecoderBackend + StatelessDecoderBackendPicture<C>,
{
    /// The current coded resolution
    coded_resolution: Resolution,

    /// Whether the decoder should block on decode operations.
    blocking_mode: BlockingMode,

    ready_queue: ReadyFramesQueue<B::Handle>,

    decoding_state: DecodingState<C::FormatInfo>,

    /// The backend used for hardware acceleration.
    backend: B,

    /// Codec-specific state.
    codec: C::DecoderState<B>,
}

impl<C, B> StatelessDecoder<C, B>
where
    C: StatelessCodec,
    B: StatelessDecoderBackend + StatelessDecoderBackendPicture<C>,
    C::DecoderState<B>: Default,
{
    pub fn new(backend: B, blocking_mode: BlockingMode) -> Self {
        Self {
            backend,
            blocking_mode,
            coded_resolution: Default::default(),
            decoding_state: Default::default(),
            ready_queue: Default::default(),
            codec: Default::default(),
        }
    }
}

impl<C, B> StatelessDecoder<C, B>
where
    C: StatelessCodec,
    B: StatelessDecoderBackend + StatelessDecoderBackendPicture<C>,
{
    fn frame_pool(
        &mut self,
        layer: PoolLayer,
    ) -> Vec<&mut dyn FramePool<<B::Handle as DecodedHandle>::Descriptor>> {
        self.backend.frame_pool(layer)
    }

    fn stream_info(&self) -> Option<&StreamInfo> {
        self.backend.stream_info()
    }
}

impl<C, B> private::StatelessVideoDecoder for StatelessDecoder<C, B>
where
    C: StatelessCodec,
    B: StatelessDecoderBackend + StatelessDecoderBackendPicture<C> + TryFormat<C>,
{
    fn try_format(&mut self, format: crate::DecodedFormat) -> anyhow::Result<()> {
        match &self.decoding_state {
            DecodingState::AwaitingFormat(sps) => self.backend.try_format(sps, format),
            _ => Err(anyhow::anyhow!(
                "current decoder state does not allow format change"
            )),
        }
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use crate::decoder::stateless::StatelessVideoDecoder;
    use crate::decoder::DecodedHandle;

    /// Stream that can be used in tests, along with the CRC32 of all of its frames.
    pub struct TestStream {
        /// Bytestream to decode.
        pub stream: &'static [u8],
        /// Expected CRC for each frame, one per line.
        pub crcs: &'static str,
    }

    /// Run the codec-specific `decoding_loop` on a `decoder` with a given `test`, linearly
    /// decoding the stream until its end.
    ///
    /// If `check_crcs` is `true`, then the expected CRCs of the decoded images are compared
    /// against the existing result. We may want to set this to false when using a decoder backend
    /// that does not produce actual frames.
    ///
    /// `dump_yuv` will dump all the decoded frames into `/tmp/framexxx.yuv`. Set this to true in
    /// order to debug the output of the test.
    pub fn test_decode_stream<D, H, L>(
        decoding_loop: L,
        mut decoder: D,
        test: &TestStream,
        check_crcs: bool,
        dump_yuv: bool,
    ) where
        H: DecodedHandle,
        D: StatelessVideoDecoder<H>,
        L: Fn(&mut D, &[u8], &mut dyn FnMut(H)) -> anyhow::Result<()>,
    {
        let mut crcs = test.crcs.lines().enumerate();

        decoding_loop(&mut decoder, test.stream, &mut |handle| {
            let (frame_num, expected_crc) = crcs.next().expect("decoded more frames than expected");

            if check_crcs || dump_yuv {
                handle.sync().unwrap();
                let picture = handle.dyn_picture();
                let mut backend_handle = picture.dyn_mappable_handle().unwrap();

                let buffer_size = backend_handle.image_size();
                let mut nv12 = vec![0; buffer_size];

                backend_handle.read(&mut nv12).unwrap();

                if dump_yuv {
                    std::fs::write(format!("/tmp/frame{:03}.yuv", frame_num), &nv12).unwrap();
                }

                if check_crcs {
                    let frame_crc = format!("{:08x}", crc32fast::hash(&nv12));
                    assert_eq!(frame_crc, expected_crc, "at frame {}", frame_num);
                }
            }
        })
        .unwrap();

        assert_eq!(crcs.next(), None, "decoded less frames than expected");
    }
}
