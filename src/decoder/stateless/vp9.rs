// Copyright 2022 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#[cfg(test)]
mod dummy;
#[cfg(feature = "vaapi")]
mod vaapi;

use std::os::fd::AsFd;
use std::os::fd::BorrowedFd;

use anyhow::anyhow;
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
use crate::decoder::stateless::PoolLayer;
use crate::decoder::stateless::StatelessBackendResult;
use crate::decoder::stateless::StatelessCodec;
use crate::decoder::stateless::StatelessDecoder;
use crate::decoder::stateless::StatelessDecoderBackend;
use crate::decoder::stateless::StatelessVideoDecoder;
use crate::decoder::stateless::TryFormat;
use crate::decoder::BlockingMode;
use crate::decoder::DecodedHandle;
use crate::decoder::DecoderEvent;
use crate::decoder::FramePool;
use crate::decoder::StreamInfo;
use crate::Resolution;

use super::StatelessDecoderBackendPicture;

/// Stateless backend methods specific to VP9.
pub trait StatelessVp9DecoderBackend:
    StatelessDecoderBackend + StatelessDecoderBackendPicture<Vp9>
{
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

pub struct Vp9DecoderState<H: DecodedHandle> {
    /// VP9 bitstream parser.
    parser: Parser,

    /// The reference frames in use.
    reference_frames: [Option<H>; NUM_REF_FRAMES],

    /// Per-segment data.
    segmentation: [Segmentation; MAX_SEGMENTS],

    /// Keeps track of the last values seen for negotiation purposes.
    negotiation_info: NegotiationInfo,
}

impl<H> Default for Vp9DecoderState<H>
where
    H: DecodedHandle,
{
    fn default() -> Self {
        Self {
            parser: Default::default(),
            reference_frames: Default::default(),
            segmentation: Default::default(),
            negotiation_info: Default::default(),
        }
    }
}

/// Keeps track of the last values seen for negotiation purposes.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
struct NegotiationInfo {
    /// The current coded resolution
    coded_resolution: Resolution,
    /// Cached value for bit depth
    bit_depth: BitDepth,
    /// Cached value for profile
    profile: Profile,
}

impl From<&Header> for NegotiationInfo {
    fn from(hdr: &Header) -> Self {
        NegotiationInfo {
            coded_resolution: Resolution {
                width: hdr.width,
                height: hdr.height,
            },
            bit_depth: hdr.bit_depth,
            profile: hdr.profile,
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
    type DecoderState<H: DecodedHandle, P> = Vp9DecoderState<H>;
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
    fn handle_frame(&mut self, frame: &Frame, timestamp: u64) -> Result<(), DecodeError> {
        let decoded_handle = if frame.header.show_existing_frame {
            // Frame to be shown. Because the spec mandates that frame_to_show_map_idx references a
            // valid entry in the DPB, an non-existing index means that the stream is invalid.
            let idx = usize::from(frame.header.frame_to_show_map_idx);
            let ref_frame = self
                .codec
                .reference_frames
                .get(idx)
                .ok_or_else(|| anyhow::anyhow!("invalid reference frame index in header"))?
                .as_ref()
                .ok_or_else(|| {
                    anyhow::anyhow!("empty reference frame referenced in frame header")
                })?;

            // We are done, no further processing needed.
            ref_frame.clone()
        } else {
            // Otherwise, we must actually arrange to decode a frame
            let refresh_frame_flags = frame.header.refresh_frame_flags;

            Segmentation::update_segmentation(&mut self.codec.segmentation, &frame.header);
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

    fn negotiation_possible(&self, hdr: &Header, old_negotiation_info: &NegotiationInfo) -> bool {
        let negotiation_info = NegotiationInfo::from(hdr);

        if negotiation_info.coded_resolution.width == 0
            || negotiation_info.coded_resolution.height == 0
        {
            false
        } else {
            *old_negotiation_info != negotiation_info
        }
    }
}

impl<B> StatelessVideoDecoder for StatelessDecoder<Vp9, B>
where
    B: StatelessVp9DecoderBackend + TryFormat<Vp9>,
    B::Handle: Clone + 'static,
{
    type Handle = B::Handle;
    type FramePool = B::FramePool;

    fn decode(&mut self, timestamp: u64, bitstream: &[u8]) -> Result<usize, DecodeError> {
        let frames = self.codec.parser.parse_chunk(bitstream)?;

        let num_free_frames = self
            .backend
            .frame_pool(PoolLayer::Highest)
            .pop()
            .ok_or(DecodeError::DecoderError(anyhow!("No pool found")))?
            .num_free_frames();

        if matches!(self.decoding_state, DecodingState::Decoding) && num_free_frames < frames.len()
        {
            return Err(DecodeError::NotEnoughOutputBuffers(
                frames.len() - num_free_frames,
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
            if self.negotiation_possible(&frame.header, &self.codec.negotiation_info) {
                self.backend.new_sequence(&frame.header)?;
                self.await_format_change(frame.header.clone());
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

    fn next_event(&mut self) -> Option<DecoderEvent<B::Handle>> {
        self.query_next_event(|decoder, hdr| {
            decoder.codec.negotiation_info = hdr.into();
        })
    }

    fn frame_pool(&mut self, layer: PoolLayer) -> Vec<&mut B::FramePool> {
        self.backend.frame_pool(layer)
    }

    fn stream_info(&self) -> Option<&StreamInfo> {
        self.backend.stream_info()
    }

    fn poll_fd(&self) -> BorrowedFd {
        self.epoll_fd.0.as_fd()
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
    use crate::utils::simple_playback_loop_owned_frames;
    use crate::utils::IvfIterator;
    use crate::DecodedFormat;

    /// Run `test` using the dummy decoder, in both blocking and non-blocking modes.
    fn test_decoder_dummy(test: &TestStream, blocking_mode: BlockingMode) {
        let decoder = StatelessDecoder::<Vp9, _>::new_dummy(blocking_mode).unwrap();

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
