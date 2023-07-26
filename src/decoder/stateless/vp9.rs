// Copyright 2022 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#[cfg(test)]
mod dummy;
#[cfg(feature = "vaapi")]
mod vaapi;

use log::debug;

use crate::codec::vp9::parser::BitDepth;
use crate::codec::vp9::parser::Frame;
use crate::codec::vp9::parser::Header;
use crate::codec::vp9::parser::Parser;
use crate::codec::vp9::parser::Profile;
use crate::codec::vp9::parser::Segmentation;
use crate::codec::vp9::parser::MAX_SEGMENTS;
use crate::codec::vp9::parser::NUM_REF_FRAMES;
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
use crate::decoder::StreamInfo;
use crate::decoder::SurfacePool;
use crate::Resolution;

/// Stateless backend methods specific to VP9.
pub trait StatelessVp9DecoderBackend: StatelessDecoderBackend<Header> {
    /// Called when new stream parameters are found.
    fn new_sequence(&mut self, header: &Header) -> StatelessBackendResult<()>;

    /// Called when the decoder wants the backend to finish the decoding
    /// operations for `picture`.
    ///
    /// This call will assign the ownership of the BackendHandle to the Picture
    /// and then assign the ownership of the Picture to the Handle.
    fn submit_picture(
        &mut self,
        picture: &Header,
        reference_frames: &[Option<Self::Handle>; NUM_REF_FRAMES],
        bitstream: &[u8],
        timestamp: u64,
        segmentation: &[Segmentation; MAX_SEGMENTS],
    ) -> StatelessBackendResult<Self::Handle>;
}

pub struct Vp9DecoderState<B: StatelessDecoderBackend<Header>> {
    /// VP9 bitstream parser.
    parser: Parser,

    /// The reference frames in use.
    reference_frames: [Option<B::Handle>; NUM_REF_FRAMES],

    /// Per-segment data.
    segmentation: [Segmentation; MAX_SEGMENTS],

    /// Cached value for bit depth
    bit_depth: BitDepth,
    /// Cached value for profile
    profile: Profile,
}

impl<B: StatelessDecoderBackend<Header>> Default for Vp9DecoderState<B> {
    fn default() -> Self {
        Self {
            parser: Default::default(),
            reference_frames: Default::default(),
            segmentation: Default::default(),
            bit_depth: Default::default(),
            profile: Default::default(),
        }
    }
}

/// [`StatelessCodec`] structure to use in order to create a VP9 stateless decoder.
///
/// # Accepted input
///
/// the VP9 specification requires the last byte of the chunk to contain the superframe marker.
/// Thus, a decoder using this codec processes exactly one encoded chunk per call to
/// [`StatelessDecoder::decode`], and always returns the size of the passed input if successful.
pub struct Vp9;

impl StatelessCodec for Vp9 {
    type FormatInfo = Header;
    type DecoderState<B: StatelessDecoderBackend<Header>> = Vp9DecoderState<B>;
}

impl<B> StatelessDecoder<Vp9, B>
where
    B: StatelessVp9DecoderBackend,
    B::Handle: Clone,
{
    fn update_references(
        reference_frames: &mut [Option<B::Handle>; NUM_REF_FRAMES],
        picture: &B::Handle,
        mut refresh_frame_flags: u8,
    ) -> anyhow::Result<()> {
        #[allow(clippy::needless_range_loop)]
        for i in 0..NUM_REF_FRAMES {
            if (refresh_frame_flags & 1) == 1 {
                debug!("Replacing reference frame {}", i);
                reference_frames[i] = Some(picture.clone());
            }

            refresh_frame_flags >>= 1;
        }

        Ok(())
    }

    /// Handle a single frame.
    fn handle_frame(&mut self, frame: &Frame<&[u8]>, timestamp: u64) -> Result<(), DecodeError> {
        let decoded_handle = if frame.header.show_existing_frame {
            // Frame to be shown. Unwrapping must produce a Picture, because the
            // spec mandates frame_to_show_map_idx references a valid entry in
            // the DPB
            let idx = usize::from(frame.header.frame_to_show_map_idx);
            let ref_frame = self.codec.reference_frames[idx].as_ref().unwrap();

            // We are done, no further processing needed.
            ref_frame.clone()
        } else {
            // Otherwise, we must actually arrange to decode a frame
            let refresh_frame_flags = frame.header.refresh_frame_flags;

            Segmentation::update_segmentation(&mut self.codec.segmentation, &frame.header)?;
            let decoded_handle = self.backend.submit_picture(
                &frame.header,
                &self.codec.reference_frames,
                frame.as_ref(),
                timestamp,
                &self.codec.segmentation,
            )?;

            if self.blocking_mode == BlockingMode::Blocking {
                decoded_handle.sync()?;
            }

            // Do DPB management
            Self::update_references(
                &mut self.codec.reference_frames,
                &decoded_handle,
                refresh_frame_flags,
            )?;

            decoded_handle
        };

        let show_existing_frame = frame.header.show_existing_frame;
        if frame.header.show_frame || show_existing_frame {
            self.ready_queue.push(decoded_handle);
        }

        Ok(())
    }

    fn negotiation_possible(&self, hdr: &Header) -> bool {
        let coded_resolution = self.coded_resolution;
        let width = hdr.width;
        let height = hdr.height;
        let bit_depth = hdr.bit_depth;
        let profile = hdr.profile;

        if width == 0 || height == 0 {
            return false;
        }

        width != coded_resolution.width
            || height != coded_resolution.height
            || bit_depth != self.codec.bit_depth
            || profile != self.codec.profile
    }
}

impl<B> StatelessVideoDecoder<<B::Handle as DecodedHandle>::Descriptor> for StatelessDecoder<Vp9, B>
where
    B: StatelessVp9DecoderBackend,
    B::Handle: Clone + 'static,
{
    fn decode(&mut self, timestamp: u64, bitstream: &[u8]) -> Result<usize, DecodeError> {
        let frames = self.codec.parser.parse_chunk(bitstream)?;

        let num_free_surfaces = self.backend.surface_pool().num_free_surfaces();
        if matches!(self.decoding_state, DecodingState::Decoding)
            && num_free_surfaces < frames.len()
        {
            return Err(DecodeError::NotEnoughOutputBuffers(
                frames.len() - num_free_surfaces,
            ));
        }

        // With SVC, the first frame will usually be a key-frame, with
        // inter-frames carrying the other layers.
        //
        // We do not want each of those to be considered as a separate DRC
        // event. Not only that, allowing them to be will cause an infinite
        // loop.
        //
        // Instead, negotiate based on the largest spatial layer. That will be
        // enough to house the other layers in between.
        let largest_in_superframe = frames.iter().max_by(|&a, &b| {
            let a_res = Resolution::from((a.header.width, a.header.height));
            let b_res = Resolution::from((b.header.width, b.header.height));
            if a_res == b_res {
                std::cmp::Ordering::Equal
            } else if a_res.can_contain(b_res) {
                std::cmp::Ordering::Greater
            } else {
                std::cmp::Ordering::Less
            }
        });

        if let Some(frame) = largest_in_superframe {
            if self.negotiation_possible(&frame.header) {
                self.backend.new_sequence(&frame.header)?;
                self.decoding_state = DecodingState::AwaitingFormat(frame.header.clone());
            } else if matches!(self.decoding_state, DecodingState::Reset) {
                // We can resume decoding since the decoding parameters have not changed.
                self.decoding_state = DecodingState::Decoding;
            }
        }

        for frame in frames {
            match &mut self.decoding_state {
                // Skip input until we get information from the stream.
                DecodingState::AwaitingStreamInfo | DecodingState::Reset => (),
                // Ask the client to confirm the format before we can process this.
                DecodingState::AwaitingFormat(_) => return Err(DecodeError::CheckEvents),
                DecodingState::Decoding => self.handle_frame(&frame, timestamp)?,
            }
        }

        Ok(bitstream.len())
    }

    fn flush(&mut self) -> Result<(), DecodeError> {
        // Note: all the submitted frames are already in the ready queue.
        self.codec.reference_frames = Default::default();
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
                            decoder.codec.profile = hdr.profile;
                            decoder.codec.bit_depth = hdr.bit_depth;
                            decoder.coded_resolution = Resolution {
                                width: hdr.width,
                                height: hdr.height,
                            };
                            decoder.decoding_state = DecodingState::Decoding;
                        }),
                    )))
                } else {
                    None
                }
            })
    }

    fn surface_pool(&mut self) -> &mut dyn SurfacePool<<B::Handle as DecodedHandle>::Descriptor> {
        self.backend.surface_pool()
    }

    fn stream_info(&self) -> Option<&StreamInfo> {
        self.backend.stream_info()
    }
}

#[cfg(test)]
pub mod tests {
    use crate::decoder::stateless::tests::test_decode_stream;
    use crate::decoder::stateless::tests::TestStream;
    use crate::decoder::stateless::vp9::Vp9;
    use crate::decoder::stateless::StatelessDecoder;
    use crate::decoder::BlockingMode;
    use crate::utils::simple_playback_loop;
    use crate::utils::simple_playback_loop_owned_surfaces;
    use crate::utils::IvfIterator;
    use crate::DecodedFormat;

    /// Run `test` using the dummy decoder, in both blocking and non-blocking modes.
    fn test_decoder_dummy(test: &TestStream, blocking_mode: BlockingMode) {
        let decoder = StatelessDecoder::<Vp9, _>::new_dummy(blocking_mode);

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
        stream: include_bytes!("../../codec/vp9/test_data/test-25fps.vp9"),
        crcs: include_str!("../../codec/vp9/test_data/test-25fps.vp9.crc"),
    };

    #[test]
    fn test_25fps_block() {
        test_decoder_dummy(&DECODE_TEST_25FPS, BlockingMode::Blocking);
    }

    #[test]
    fn test_25fps_nonblock() {
        test_decoder_dummy(&DECODE_TEST_25FPS, BlockingMode::NonBlocking);
    }

    // Remuxed from the original matroska source in libvpx using ffmpeg:
    // ffmpeg -i vp90-2-10-show-existing-frame.webm/vp90-2-10-show-existing-frame.webm -c:v copy /tmp/vp90-2-10-show-existing-frame.vp9.ivf
    pub const DECODE_TEST_25FPS_SHOW_EXISTING_FRAME: TestStream = TestStream {
        stream: include_bytes!("../../codec/vp9/test_data/vp90-2-10-show-existing-frame.vp9.ivf"),
        crcs: include_str!("../../codec/vp9/test_data/vp90-2-10-show-existing-frame.vp9.ivf.crc"),
    };

    #[test]
    fn show_existing_frame_block() {
        test_decoder_dummy(
            &DECODE_TEST_25FPS_SHOW_EXISTING_FRAME,
            BlockingMode::Blocking,
        );
    }

    #[test]
    fn show_existing_frame_nonblock() {
        test_decoder_dummy(
            &DECODE_TEST_25FPS_SHOW_EXISTING_FRAME,
            BlockingMode::NonBlocking,
        );
    }

    pub const DECODE_TEST_25FPS_SHOW_EXISTING_FRAME2: TestStream = TestStream {
        stream: include_bytes!("../../codec/vp9/test_data/vp90-2-10-show-existing-frame2.vp9.ivf"),
        crcs: include_str!("../../codec/vp9/test_data/vp90-2-10-show-existing-frame2.vp9.ivf.crc"),
    };

    #[test]
    fn show_existing_frame2_block() {
        test_decoder_dummy(
            &DECODE_TEST_25FPS_SHOW_EXISTING_FRAME2,
            BlockingMode::Blocking,
        );
    }

    #[test]
    fn show_existing_frame2_nonblock() {
        test_decoder_dummy(
            &DECODE_TEST_25FPS_SHOW_EXISTING_FRAME2,
            BlockingMode::NonBlocking,
        );
    }

    // Remuxed from the original matroska source in libvpx using ffmpeg:
    // ffmpeg -i vp90-2-10-show-existing-frame.webm/vp90-2-10-show-existing-frame.webm -c:v copy /tmp/vp90-2-10-show-existing-frame.vp9.ivf
    // There are some weird padding issues introduced by GStreamer for
    // resolutions that are not multiple of 4, so we're ignoring CRCs for
    // this one.
    pub const DECODE_RESOLUTION_CHANGE_500FRAMES: TestStream = TestStream {
        stream: include_bytes!("../../codec/vp9/test_data/resolution_change_500frames-vp9.ivf"),
        crcs: include_str!("../../codec/vp9/test_data/resolution_change_500frames-vp9.ivf.crc"),
    };

    #[test]
    fn test_resolution_change_500frames_block() {
        test_decoder_dummy(&DECODE_RESOLUTION_CHANGE_500FRAMES, BlockingMode::Blocking);
    }

    #[test]
    fn test_resolution_change_500frames_nonblock() {
        test_decoder_dummy(&DECODE_RESOLUTION_CHANGE_500FRAMES, BlockingMode::Blocking);
    }
}
