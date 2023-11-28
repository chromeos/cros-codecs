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
use crate::decoder::stateless::DecodeError;
use crate::decoder::stateless::DecodingState;
use crate::decoder::stateless::StatelessBackendResult;
use crate::decoder::stateless::StatelessCodec;
use crate::decoder::stateless::StatelessDecoder;
use crate::decoder::stateless::StatelessDecoderBackend;
use crate::decoder::stateless::StatelessDecoderFormatNegotiator;
use crate::decoder::stateless::StatelessVideoDecoder;
use crate::decoder::BlockingMode;
use crate::decoder::DecodedHandle;
use crate::decoder::DecoderEvent;
use crate::decoder::FramePool;
use crate::decoder::StreamInfo;
use crate::Resolution;

/// Stateless backend methods specific to VP8.
pub trait StatelessVp8DecoderBackend: StatelessDecoderBackend<Vp8> {
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

pub struct Vp8DecoderState<B: StatelessDecoderBackend<Vp8>> {
    /// VP8 bitstream parser.
    parser: Parser,

    /// The picture used as the last reference picture.
    last_picture: Option<B::Handle>,
    /// The picture used as the golden reference picture.
    golden_ref_picture: Option<B::Handle>,
    /// The picture used as the alternate reference picture.
    alt_ref_picture: Option<B::Handle>,
}

impl<B: StatelessDecoderBackend<Vp8>> Default for Vp8DecoderState<B> {
    fn default() -> Self {
        Self {
            parser: Default::default(),
            last_picture: Default::default(),
            golden_ref_picture: Default::default(),
            alt_ref_picture: Default::default(),
        }
    }
}

/// [`StatelessCodec`] structure to use in order to create a VP8 stateless decoder.
///
/// # Accepted input
///
/// A decoder using this codec processes exactly one encoded frame per call to
/// [`StatelessDecoder::decode`], and returns the number of bytes actually taken by the frame data.
/// If the frame was properly encapsulated in its container, the returned value should be equal to
/// the length of the submitted input.
pub struct Vp8;

impl StatelessCodec for Vp8 {
    type FormatInfo = Header;
    type DecoderState<B: StatelessDecoderBackend<Self>> = Vp8DecoderState<B>;
}

impl<B> Vp8DecoderState<B>
where
    B: StatelessDecoderBackend<Vp8>,
    B::Handle: Clone,
{
    /// Replace a reference frame with `handle`.
    fn replace_reference(reference: &mut Option<B::Handle>, handle: &B::Handle) {
        *reference = Some(handle.clone());
    }

    pub(crate) fn update_references(
        &mut self,
        header: &Header,
        decoded_handle: &B::Handle,
    ) -> anyhow::Result<()> {
        if header.key_frame {
            Self::replace_reference(&mut self.last_picture, decoded_handle);
            Self::replace_reference(&mut self.golden_ref_picture, decoded_handle);
            Self::replace_reference(&mut self.alt_ref_picture, decoded_handle);
        } else {
            if header.refresh_alternate_frame {
                Self::replace_reference(&mut self.alt_ref_picture, decoded_handle);
            } else {
                match header.copy_buffer_to_alternate {
                    0 => { /* do nothing */ }

                    1 => {
                        if let Some(last_picture) = &self.last_picture {
                            Self::replace_reference(&mut self.alt_ref_picture, last_picture);
                        }
                    }

                    2 => {
                        if let Some(golden_ref) = &self.golden_ref_picture {
                            Self::replace_reference(&mut self.alt_ref_picture, golden_ref);
                        }
                    }

                    other => panic!("Invalid value: {}", other),
                }
            }

            if header.refresh_golden_frame {
                Self::replace_reference(&mut self.golden_ref_picture, decoded_handle);
            } else {
                match header.copy_buffer_to_golden {
                    0 => { /* do nothing */ }

                    1 => {
                        if let Some(last_picture) = &self.last_picture {
                            Self::replace_reference(&mut self.golden_ref_picture, last_picture);
                        }
                    }

                    2 => {
                        if let Some(alt_ref) = &self.alt_ref_picture {
                            Self::replace_reference(&mut self.golden_ref_picture, alt_ref);
                        }
                    }

                    other => panic!("Invalid value: {}", other),
                }
            }

            if header.refresh_last {
                Self::replace_reference(&mut self.last_picture, decoded_handle);
            }
        }

        Ok(())
    }
}

impl<B> StatelessDecoder<Vp8, B>
where
    B: StatelessVp8DecoderBackend,
    B::Handle: Clone,
{
    /// Handle a single frame.
    fn handle_frame(&mut self, frame: Frame, timestamp: u64) -> Result<(), DecodeError> {
        if self.backend.frame_pool().num_free_frames() == 0 {
            return Err(DecodeError::NotEnoughOutputBuffers(1));
        }

        let show_frame = frame.header.show_frame;

        let decoded_handle = self.backend.submit_picture(
            &frame.header,
            self.codec.last_picture.as_ref(),
            self.codec.golden_ref_picture.as_ref(),
            self.codec.alt_ref_picture.as_ref(),
            frame.as_ref(),
            self.codec.parser.segmentation(),
            self.codec.parser.mb_lf_adjust(),
            timestamp,
        )?;

        if self.blocking_mode == BlockingMode::Blocking {
            decoded_handle.sync()?;
        }

        // Do DPB management
        self.codec
            .update_references(&frame.header, &decoded_handle)?;

        if show_frame {
            self.ready_queue.push(decoded_handle);
        }

        Ok(())
    }

    fn negotiation_possible(&self, frame: &Frame) -> bool {
        let coded_resolution = self.coded_resolution;
        let hdr = &frame.header;
        let width = u32::from(hdr.width);
        let height = u32::from(hdr.height);

        width != coded_resolution.width || height != coded_resolution.height
    }
}

impl<B> StatelessVideoDecoder<<B::Handle as DecodedHandle>::Descriptor> for StatelessDecoder<Vp8, B>
where
    B: StatelessVp8DecoderBackend,
    B::Handle: Clone + 'static,
{
    fn decode(&mut self, timestamp: u64, bitstream: &[u8]) -> Result<usize, DecodeError> {
        let frame = self.codec.parser.parse_frame(bitstream)?;

        if frame.header.key_frame {
            if self.negotiation_possible(&frame) {
                self.backend.new_sequence(&frame.header)?;
                self.decoding_state = DecodingState::AwaitingFormat(frame.header.clone());
            } else if matches!(self.decoding_state, DecodingState::Reset) {
                // We can resume decoding since the decoding parameters have not changed.
                self.decoding_state = DecodingState::Decoding;
            }
        }

        match &mut self.decoding_state {
            // Skip input until we get information from the stream.
            DecodingState::AwaitingStreamInfo | DecodingState::Reset => Ok(bitstream.len()),
            // Ask the client to confirm the format before we can process this.
            DecodingState::AwaitingFormat(_) => Err(DecodeError::CheckEvents),
            DecodingState::Decoding => {
                let len = frame.header.frame_len();
                self.handle_frame(frame, timestamp)?;
                Ok(len)
            }
        }
    }

    fn flush(&mut self) -> Result<(), DecodeError> {
        // Note: all the submitted frames are already in the ready queue.
        self.codec.last_picture = Default::default();
        self.codec.golden_ref_picture = Default::default();
        self.codec.alt_ref_picture = Default::default();
        self.decoding_state = DecodingState::Reset;

        Ok(())
    }

    fn next_event(&mut self) -> Option<DecoderEvent<<B::Handle as DecodedHandle>::Descriptor>> {
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
                                width: hdr.width as u32,
                                height: hdr.height as u32,
                            };
                            decoder.decoding_state = DecodingState::Decoding;
                        }),
                    )))
                } else {
                    None
                }
            })
    }

    fn frame_pool(&mut self) -> &mut dyn FramePool<<B::Handle as DecodedHandle>::Descriptor> {
        self.backend.frame_pool()
    }

    fn stream_info(&self) -> Option<&StreamInfo> {
        self.backend.stream_info()
    }
}

#[cfg(test)]
pub mod tests {
    use crate::decoder::stateless::tests::test_decode_stream;
    use crate::decoder::stateless::tests::TestStream;
    use crate::decoder::stateless::vp8::Vp8;
    use crate::decoder::stateless::StatelessDecoder;
    use crate::decoder::BlockingMode;
    use crate::utils::simple_playback_loop;
    use crate::utils::simple_playback_loop_owned_frames;
    use crate::utils::IvfIterator;
    use crate::DecodedFormat;

    /// Run `test` using the dummy decoder, in both blocking and non-blocking modes.
    fn test_decoder_dummy(test: &TestStream, blocking_mode: BlockingMode) {
        let decoder = StatelessDecoder::<Vp8, _>::new_dummy(blocking_mode);

        test_decode_stream(
            |d, s, c| {
                simple_playback_loop(
                    d,
                    IvfIterator::new(s),
                    c,
                    &mut simple_playback_loop_owned_frames,
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
