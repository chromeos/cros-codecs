// Copyright 2022 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#[cfg(test)]
mod dummy;
#[cfg(feature = "vaapi")]
mod vaapi;

use crate::codec::vp8::parser::Frame;
use crate::codec::vp8::parser::Header;
use crate::codec::vp8::parser::MbLfAdjustments;
use crate::codec::vp8::parser::Parser;
use crate::codec::vp8::parser::Segmentation;
use crate::decoder::stateless::private;
use crate::decoder::stateless::DecodeError;
use crate::decoder::stateless::DecodingState;
use crate::decoder::stateless::StatelessBackendResult;
use crate::decoder::stateless::StatelessDecoderBackend;
use crate::decoder::stateless::StatelessDecoderFormatNegotiator;
use crate::decoder::stateless::StatelessVideoDecoder;
use crate::decoder::BlockingMode;
use crate::decoder::DecodedHandle;
use crate::decoder::DecoderEvent;
use crate::decoder::ReadyFramesQueue;
use crate::decoder::StreamInfo;
use crate::decoder::SurfacePool;
use crate::Resolution;

/// Stateless backend methods specific to VP8.
trait StatelessVp8DecoderBackend<M>: StatelessDecoderBackend<Header, M> {
    /// Called when new stream parameters are found.
    fn new_sequence(&mut self, header: &Header) -> StatelessBackendResult<()>;

    /// Called when the decoder wants the backend to finish the decoding
    /// operations for `picture`.
    ///
    /// This call will assign the ownership of the BackendHandle to the Picture
    /// and then assign the ownership of the Picture to the Handle.
    #[allow(clippy::too_many_arguments)]
    fn submit_picture(
        &mut self,
        picture: &Header,
        last_ref: Option<&Self::Handle>,
        golden_ref: Option<&Self::Handle>,
        alt_ref: Option<&Self::Handle>,
        bitstream: &[u8],
        segmentation: &Segmentation,
        mb_lf_adjust: &MbLfAdjustments,
        timestamp: u64,
    ) -> StatelessBackendResult<Self::Handle>;
}

pub struct Decoder<T: DecodedHandle<M>, M> {
    /// A parser to extract bitstream data and build frame data in turn
    parser: Parser,

    /// Whether the decoder should block on decode operations.
    blocking_mode: BlockingMode,

    /// The backend used for hardware acceleration.
    backend: Box<dyn StatelessVp8DecoderBackend<M, Handle = T>>,

    decoding_state: DecodingState<Header>,

    /// The current resolution
    coded_resolution: Resolution,

    ready_queue: ReadyFramesQueue<T>,

    /// The picture used as the last reference picture.
    last_picture: Option<T>,
    /// The picture used as the golden reference picture.
    golden_ref_picture: Option<T>,
    /// The picture used as the alternate reference picture.
    alt_ref_picture: Option<T>,
}

impl<T: DecodedHandle<M> + Clone + 'static, M> Decoder<T, M> {
    /// Create a new decoder using the given `backend`.
    #[cfg(any(feature = "vaapi", test))]
    fn new(
        backend: Box<dyn StatelessVp8DecoderBackend<M, Handle = T>>,
        blocking_mode: BlockingMode,
    ) -> anyhow::Result<Self> {
        Ok(Self {
            backend,
            blocking_mode,
            // wait_keyframe: true,
            parser: Default::default(),
            decoding_state: Default::default(),
            last_picture: Default::default(),
            golden_ref_picture: Default::default(),
            alt_ref_picture: Default::default(),
            coded_resolution: Default::default(),
            ready_queue: Default::default(),
        })
    }

    /// Replace a reference frame with `handle`.
    fn replace_reference(reference: &mut Option<T>, handle: &T) {
        *reference = Some(handle.clone());
    }

    pub(crate) fn update_references(
        header: &Header,
        decoded_handle: &T,
        last_picture: &mut Option<T>,
        golden_ref_picture: &mut Option<T>,
        alt_ref_picture: &mut Option<T>,
    ) -> anyhow::Result<()> {
        if header.key_frame() {
            Self::replace_reference(last_picture, decoded_handle);
            Self::replace_reference(golden_ref_picture, decoded_handle);
            Self::replace_reference(alt_ref_picture, decoded_handle);
        } else {
            if header.refresh_alternate_frame() {
                Self::replace_reference(alt_ref_picture, decoded_handle);
            } else {
                match header.copy_buffer_to_alternate() {
                    0 => { /* do nothing */ }

                    1 => {
                        if let Some(last_picture) = last_picture {
                            Self::replace_reference(alt_ref_picture, last_picture);
                        }
                    }

                    2 => {
                        if let Some(golden_ref) = golden_ref_picture {
                            Self::replace_reference(alt_ref_picture, golden_ref);
                        }
                    }

                    other => panic!("Invalid value: {}", other),
                }
            }

            if header.refresh_golden_frame() {
                Self::replace_reference(golden_ref_picture, decoded_handle);
            } else {
                match header.copy_buffer_to_golden() {
                    0 => { /* do nothing */ }

                    1 => {
                        if let Some(last_picture) = last_picture {
                            Self::replace_reference(golden_ref_picture, last_picture);
                        }
                    }

                    2 => {
                        if let Some(alt_ref) = alt_ref_picture {
                            Self::replace_reference(golden_ref_picture, alt_ref);
                        }
                    }

                    other => panic!("Invalid value: {}", other),
                }
            }

            if header.refresh_last() {
                Self::replace_reference(last_picture, decoded_handle);
            }
        }

        Ok(())
    }

    /// Handle a single frame.
    fn handle_frame(&mut self, frame: Frame<&[u8]>, timestamp: u64) -> Result<(), DecodeError> {
        if self.backend.surface_pool().num_free_surfaces() == 0 {
            return Err(DecodeError::CheckEvents);
        }

        let show_frame = frame.header.show_frame();

        let decoded_handle = self.backend.submit_picture(
            &frame.header,
            self.last_picture.as_ref(),
            self.golden_ref_picture.as_ref(),
            self.alt_ref_picture.as_ref(),
            frame.bitstream,
            self.parser.segmentation(),
            self.parser.mb_lf_adjust(),
            timestamp,
        )?;

        if self.blocking_mode == BlockingMode::Blocking {
            decoded_handle.sync()?;
        }

        // Do DPB management
        Self::update_references(
            &frame.header,
            &decoded_handle,
            &mut self.last_picture,
            &mut self.golden_ref_picture,
            &mut self.alt_ref_picture,
        )?;

        if show_frame {
            self.ready_queue.push(decoded_handle);
        }

        Ok(())
    }

    fn negotiation_possible(&self, frame: &Frame<impl AsRef<[u8]>>) -> bool {
        let coded_resolution = self.coded_resolution;
        let hdr = &frame.header;
        let width = u32::from(hdr.width());
        let height = u32::from(hdr.height());

        width != coded_resolution.width || height != coded_resolution.height
    }
}

impl<T: DecodedHandle<M> + Clone + 'static, M> StatelessVideoDecoder<M> for Decoder<T, M> {
    fn decode(&mut self, timestamp: u64, bitstream: &[u8]) -> Result<(), DecodeError> {
        let frame = self.parser.parse_frame(bitstream)?;

        if frame.header.key_frame() && self.negotiation_possible(&frame) {
            self.backend.new_sequence(&frame.header)?;
            self.decoding_state = DecodingState::AwaitingFormat(frame.header.clone());
        }

        match &mut self.decoding_state {
            // Skip input until we get information from the stream.
            DecodingState::AwaitingStreamInfo => Ok(()),
            // Ask the client to confirm the format before we can process this.
            DecodingState::AwaitingFormat(_) => Err(DecodeError::CheckEvents),
            DecodingState::Decoding => self.handle_frame(frame, timestamp),
        }
    }

    fn flush(&mut self) {
        // Note: all the submitted frames are already in the ready queue.
        self.last_picture = Default::default();
        self.golden_ref_picture = Default::default();
        self.alt_ref_picture = Default::default();
        self.decoding_state = DecodingState::AwaitingStreamInfo;
    }

    fn next_event(&mut self) -> Option<DecoderEvent<M>> {
        // The next event is either the next frame, or, if we are awaiting negotiation, the format
        // change event that will allow us to keep going.
        (&mut self.ready_queue)
            .next()
            .map(|handle| DecoderEvent::FrameReady(Box::new(handle)))
            .or_else(|| {
                if let DecodingState::AwaitingFormat(hdr) = &self.decoding_state {
                    Some(DecoderEvent::FormatChanged(Box::new(
                        StatelessDecoderFormatNegotiator::new(self, hdr.clone(), |decoder, hdr| {
                            decoder.coded_resolution = Resolution {
                                width: hdr.width() as u32,
                                height: hdr.height() as u32,
                            };
                            decoder.decoding_state = DecodingState::Decoding;
                        }),
                    )))
                } else {
                    None
                }
            })
    }

    fn surface_pool(&mut self) -> &mut dyn SurfacePool<M> {
        self.backend.surface_pool()
    }

    fn stream_info(&self) -> Option<&StreamInfo> {
        self.backend.stream_info()
    }
}

impl<T: DecodedHandle<M> + Clone + 'static, M> private::StatelessVideoDecoder for Decoder<T, M> {
    fn try_format(&mut self, format: crate::DecodedFormat) -> anyhow::Result<()> {
        match &self.decoding_state {
            DecodingState::AwaitingFormat(header) => self.backend.try_format(header, format),
            _ => Err(anyhow::anyhow!(
                "current decoder state does not allow format change"
            )),
        }
    }
}

#[cfg(test)]
pub mod tests {
    use crate::decoder::stateless::tests::test_decode_stream;
    use crate::decoder::stateless::tests::TestStream;
    use crate::decoder::stateless::vp8::Decoder;
    use crate::decoder::BlockingMode;
    use crate::utils::simple_playback_loop;
    use crate::utils::simple_playback_loop_owned_surfaces;
    use crate::utils::IvfIterator;
    use crate::DecodedFormat;

    /// Run `test` using the dummy decoder, in both blocking and non-blocking modes.
    fn test_decoder_dummy(test: &TestStream, blocking_mode: BlockingMode) {
        let decoder = Decoder::new_dummy(blocking_mode).unwrap();

        test_decode_stream(
            |d, s, c| {
                simple_playback_loop(
                    d,
                    IvfIterator::new(s),
                    c,
                    &mut simple_playback_loop_owned_surfaces,
                    DecodedFormat::NV12,
                    blocking_mode,
                )
            },
            decoder,
            test,
            false,
            false,
        );
    }

    /// Same as Chromium's test-25fps.vp8
    pub const DECODE_TEST_25FPS: TestStream = TestStream {
        stream: include_bytes!("../../codec/vp8/test_data/test-25fps.vp8"),
        crcs: include_str!("../../codec/vp8/test_data/test-25fps.vp8.crc"),
    };

    #[test]
    fn test_25fps_block() {
        test_decoder_dummy(&DECODE_TEST_25FPS, BlockingMode::Blocking);
    }

    #[test]
    fn test_25fps_nonblock() {
        test_decoder_dummy(&DECODE_TEST_25FPS, BlockingMode::NonBlocking);
    }
}
