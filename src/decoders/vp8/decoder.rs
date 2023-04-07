// Copyright 2022 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use crate::decoders::vp8::backends::StatelessDecoderBackend;
use crate::decoders::vp8::parser::Frame;
use crate::decoders::vp8::parser::Header;
use crate::decoders::vp8::parser::Parser;
use crate::decoders::BlockingMode;
use crate::decoders::DecodeError;
use crate::decoders::DecodedHandle;
use crate::decoders::DecoderEvent;
use crate::decoders::DecodingState;
use crate::decoders::ReadyFramesQueue;
use crate::decoders::StatelessDecoderFormatNegotiator;
use crate::decoders::VideoDecoder;
use crate::Resolution;

pub struct Decoder<T: DecodedHandle> {
    /// A parser to extract bitstream data and build frame data in turn
    parser: Parser,

    /// Whether the decoder should block on decode operations.
    blocking_mode: BlockingMode,

    /// The backend used for hardware acceleration.
    backend: Box<dyn StatelessDecoderBackend<Handle = T>>,

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

impl<T: DecodedHandle + Clone + 'static> Decoder<T> {
    /// Create a new codec backend for VP8.
    #[cfg(any(feature = "vaapi", test))]
    pub(crate) fn new(
        backend: Box<dyn StatelessDecoderBackend<Handle = T>>,
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
            Decoder::replace_reference(last_picture, decoded_handle);
            Decoder::replace_reference(golden_ref_picture, decoded_handle);
            Decoder::replace_reference(alt_ref_picture, decoded_handle);
        } else {
            if header.refresh_alternate_frame() {
                Decoder::replace_reference(alt_ref_picture, decoded_handle);
            } else {
                match header.copy_buffer_to_alternate() {
                    0 => { /* do nothing */ }

                    1 => {
                        if let Some(last_picture) = last_picture {
                            Decoder::replace_reference(alt_ref_picture, last_picture);
                        }
                    }

                    2 => {
                        if let Some(golden_ref) = golden_ref_picture {
                            Decoder::replace_reference(alt_ref_picture, golden_ref);
                        }
                    }

                    other => panic!("Invalid value: {}", other),
                }
            }

            if header.refresh_golden_frame() {
                Decoder::replace_reference(golden_ref_picture, decoded_handle);
            } else {
                match header.copy_buffer_to_golden() {
                    0 => { /* do nothing */ }

                    1 => {
                        if let Some(last_picture) = last_picture {
                            Decoder::replace_reference(golden_ref_picture, last_picture);
                        }
                    }

                    2 => {
                        if let Some(alt_ref) = alt_ref_picture {
                            Decoder::replace_reference(golden_ref_picture, alt_ref);
                        }
                    }

                    other => panic!("Invalid value: {}", other),
                }
            }

            if header.refresh_last() {
                Decoder::replace_reference(last_picture, decoded_handle);
            }
        }

        Ok(())
    }

    /// Handle a single frame.
    fn handle_frame(&mut self, frame: Frame<&[u8]>, timestamp: u64) -> Result<(), DecodeError> {
        if self.backend.num_resources_left() == 0 {
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

impl<T: DecodedHandle + Clone + 'static> VideoDecoder for Decoder<T> {
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

    // All the submitted frames are already in the ready queue.
    fn flush(&mut self) {}

    fn num_resources_left(&self) -> usize {
        self.backend.num_resources_left()
    }

    fn num_resources_total(&self) -> usize {
        self.backend.num_resources_total()
    }

    fn coded_resolution(&self) -> Option<Resolution> {
        self.backend.coded_resolution()
    }

    fn next_event(&mut self) -> Option<DecoderEvent> {
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
}

#[cfg(test)]
pub mod tests {
    use crate::decoders::tests::test_decode_stream;
    use crate::decoders::tests::TestStream;
    use crate::decoders::vp8::decoder::Decoder;
    use crate::decoders::BlockingMode;
    use crate::decoders::DecodeError;
    use crate::decoders::DecodedHandle;
    use crate::decoders::DecoderEvent;
    use crate::decoders::VideoDecoder;
    use crate::utils::IvfIterator;

    pub fn vp8_decoding_loop<D>(
        decoder: &mut D,
        test_stream: &[u8],
        on_new_frame: &mut dyn FnMut(Box<dyn DecodedHandle>),
        blocking_mode: BlockingMode,
    ) where
        D: VideoDecoder,
    {
        let ivf_iter = IvfIterator::new(test_stream);
        let mut frame_num = 0;

        for packet in ivf_iter {
            loop {
                let res = decoder.decode(frame_num, packet);
                match &res {
                    Ok(()) => frame_num += 1,
                    Err(DecodeError::CheckEvents) => (),
                    Err(e) => panic!("{:#}", e),
                }

                if matches!(res, Err(DecodeError::CheckEvents))
                    || blocking_mode == BlockingMode::Blocking
                {
                    while let Some(event) = decoder.next_event() {
                        match event {
                            DecoderEvent::FrameReady(frame) => {
                                on_new_frame(frame);
                            }
                            DecoderEvent::FormatChanged(_) => {}
                        }
                    }
                }

                // Break the loop so we can process the next NAL if we sent the current one
                // successfully.
                if res.is_ok() {
                    break;
                }
            }
        }

        decoder.flush();
        while let Some(event) = decoder.next_event() {
            match event {
                DecoderEvent::FrameReady(frame) => {
                    on_new_frame(frame);
                }
                DecoderEvent::FormatChanged(_) => (),
            }
        }
    }

    /// Run `test` using the dummy decoder, in both blocking and non-blocking modes.
    fn test_decoder_dummy(test: &TestStream, blocking_mode: BlockingMode) {
        let decoder = Decoder::new_dummy(blocking_mode).unwrap();

        test_decode_stream(
            |d, s, c| vp8_decoding_loop(d, s, c, blocking_mode),
            decoder,
            test,
            false,
            false,
        );
    }

    /// Same as Chromium's test-25fps.vp8
    pub const DECODE_TEST_25FPS: TestStream = TestStream {
        stream: include_bytes!("test_data/test-25fps.vp8"),
        crcs: include_str!("test_data/test-25fps.vp8.crc"),
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
