// Copyright 2023 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::os::fd::AsFd;
use std::os::fd::BorrowedFd;

use anyhow::anyhow;

use crate::codec::av1::parser::FrameHeaderObu;
use crate::codec::av1::parser::FrameObu;
use crate::codec::av1::parser::FrameType;
use crate::codec::av1::parser::ObuAction;
use crate::codec::av1::parser::ObuType;
use crate::codec::av1::parser::ParsedObu;
use crate::codec::av1::parser::Parser;
use crate::codec::av1::parser::StreamInfo;
use crate::codec::av1::parser::TileGroupObu;
use crate::codec::av1::parser::NUM_REF_FRAMES;
use crate::decoder::stateless::DecodeError;
use crate::decoder::stateless::DecodingState;
use crate::decoder::stateless::NewPictureResult;
use crate::decoder::stateless::StatelessBackendResult;
use crate::decoder::stateless::StatelessCodec;
use crate::decoder::stateless::StatelessDecoder;
use crate::decoder::stateless::StatelessDecoderBackend;
use crate::decoder::stateless::StatelessDecoderBackendPicture;
use crate::decoder::stateless::StatelessVideoDecoder;
use crate::decoder::BlockingMode;
use crate::decoder::DecodedHandle;
use crate::Resolution;

#[cfg(test)]
mod dummy;
#[cfg(feature = "v4l2")]
mod v4l2;
#[cfg(feature = "vaapi")]
mod vaapi;

/// Stateless backend methods specific to AV1.
pub trait StatelessAV1DecoderBackend:
    StatelessDecoderBackend + StatelessDecoderBackendPicture<Av1>
{
    /// Called when a new Sequence Header OBU is parsed. The
    /// `highest_spatial_layer` argument refers to the maximum layer selected by
    /// the client through `set_operating_point()` and the scalability
    /// information present in the stream, if any.
    fn change_stream_info(&mut self, stream_info: &StreamInfo) -> StatelessBackendResult<()>;

    /// Called when the decoder determines that a new picture was found. The backend allocates all
    /// the resources it needs to process that picture.
    fn new_picture(
        &mut self,
        hdr: &FrameHeaderObu,
        timestamp: u64,
        alloc_cb: &mut dyn FnMut() -> Option<
            <<Self as StatelessDecoderBackend>::Handle as DecodedHandle>::Frame,
        >,
    ) -> NewPictureResult<Self::Picture>;

    /// Called to set the global parameters of a picture.
    fn begin_picture(
        &mut self,
        picture: &mut Self::Picture,
        stream_info: &StreamInfo,
        hdr: &FrameHeaderObu,
        reference_frames: &[Option<Self::Handle>; NUM_REF_FRAMES],
    ) -> StatelessBackendResult<()>;

    /// Called to dispatch a decode operation to the backend.
    #[allow(clippy::too_many_arguments)]
    fn decode_tile_group(
        &mut self,
        picture: &mut Self::Picture,
        tile_group: TileGroupObu,
    ) -> StatelessBackendResult<()>;

    /// Called when the decoder wants the backend to finish the decoding
    /// operations for `picture`. At this point, `decode_tile` has been called
    /// for all tiles.
    fn submit_picture(&mut self, picture: Self::Picture) -> StatelessBackendResult<Self::Handle>;
}

/// State of the picture being currently decoded.
///
/// Stored between calls to [`StatelessDecoder::decode_tile_group`] that belong to the same
/// picture.
enum CurrentPicState<H: DecodedHandle, P> {
    /// A regular frame
    RegularFrame {
        /// Data for the current picture as extracted from the stream.
        header: FrameHeaderObu,
        /// Backend-specific data for that picture.
        backend_picture: P,
    },

    /// A frame that has 'show_existing_frame' set.
    ShowExistingFrame {
        /// Data for the current picture as extracted from the stream.
        header: FrameHeaderObu,
        /// The handle of the reference frame that this frame points to.
        handle: H,
    },
}

pub struct AV1DecoderState<H: DecodedHandle, P> {
    /// AV1 bitstream parser.
    parser: Parser,

    /// The reference frames in use.
    reference_frames: [Option<H>; NUM_REF_FRAMES],

    /// Keeps track of the last values seen for negotiation purposes.
    stream_info: Option<StreamInfo>,

    /// The picture currently being decoded. We need to preserve it between
    /// calls to `decode` because multiple tiles will be processed in different
    /// calls to `decode`.
    current_pic: Option<CurrentPicState<H, P>>,

    /// Keep track of the number of frames we've processed for logging purposes.
    frame_count: u32,

    /// For SVC streams, we only want to output the highest layer possible given
    /// the choice of operating point.
    highest_spatial_layer: Option<u32>,
}

impl<H, P> Default for AV1DecoderState<H, P>
where
    H: DecodedHandle,
{
    fn default() -> Self {
        Self {
            parser: Default::default(),
            reference_frames: Default::default(),
            stream_info: Default::default(),
            current_pic: Default::default(),
            frame_count: Default::default(),
            highest_spatial_layer: Default::default(),
        }
    }
}

/// [`StatelessCodec`] structure to use in order to create a AV1 stateless decoder.
///
/// # Accepted input
///
/// the VP9 specification requires the last byte of the chunk to contain the superframe marker.
/// Thus, a decoder using this codec processes exactly one encoded chunk per call to
/// [`StatelessDecoder::decode`], and always returns the size of the passed input if successful.
pub struct Av1;

impl StatelessCodec for Av1 {
    type FormatInfo = StreamInfo;
    type DecoderState<H: DecodedHandle, P> = AV1DecoderState<H, P>;
}

impl<B> StatelessDecoder<Av1, B>
where
    B: StatelessAV1DecoderBackend,
    B::Handle: Clone,
{
    fn decode_frame_header(
        &mut self,
        frame_header: FrameHeaderObu,
        timestamp: u64,
        alloc_cb: &mut dyn FnMut() -> Option<
            <<B as StatelessDecoderBackend>::Handle as DecodedHandle>::Frame,
        >,
    ) -> Result<(), DecodeError> {
        log::debug!("Processing frame {} with timestamp {}", self.codec.frame_count, timestamp);

        if frame_header.show_existing_frame {
            let ref_frame = self.codec.reference_frames
                [frame_header.frame_to_show_map_idx as usize]
                .as_ref()
                .ok_or(anyhow!("Broken stream: no reference picture to display"))?;
            self.codec.current_pic = Some(CurrentPicState::ShowExistingFrame {
                header: frame_header,
                handle: ref_frame.clone(),
            });
        } else if let Some(stream_info) = &self.codec.stream_info {
            let mut backend_picture =
                self.backend.new_picture(&frame_header, timestamp, alloc_cb)?;

            self.backend.begin_picture(
                &mut backend_picture,
                stream_info,
                &frame_header,
                &self.codec.reference_frames,
            )?;

            self.codec.current_pic = Some(CurrentPicState::RegularFrame {
                header: frame_header.clone(),
                backend_picture,
            });
        } else {
            log::warn!("invalid stream: frame header received while no valid sequence ongoing");
        }

        Ok(())
    }

    fn decode_tile_group(&mut self, tile_group: TileGroupObu) -> anyhow::Result<()> {
        let picture = match self.codec.current_pic.as_mut() {
            Some(CurrentPicState::RegularFrame { backend_picture, .. }) => backend_picture,
            Some(CurrentPicState::ShowExistingFrame { .. }) => {
                return Err(anyhow!("Broken stream: cannot decode a tile group for a frame with show_existing_frame set"));
            }
            None => {
                return Err(anyhow!(
                "Broken stream: cannot decode a tile group without first decoding a frame header"
            ))
            }
        };

        self.backend.decode_tile_group(picture, tile_group)?;
        Ok(())
    }

    fn decode_frame(
        &mut self,
        frame: FrameObu,
        timestamp: u64,
        alloc_cb: &mut dyn FnMut() -> Option<
            <<B as StatelessDecoderBackend>::Handle as DecodedHandle>::Frame,
        >,
    ) -> Result<(), DecodeError> {
        let FrameObu { header, tile_group } = frame;
        self.decode_frame_header(header, timestamp, alloc_cb)?;
        self.decode_tile_group(tile_group)?;
        Ok(())
    }

    fn submit_frame(&mut self, timestamp: u64) -> anyhow::Result<()> {
        log::debug!("Finishing frame {} with timestamp: {}", self.codec.frame_count, timestamp);

        let picture = self.codec.current_pic.take();

        let (handle, header) = match picture {
            Some(CurrentPicState::RegularFrame { header, backend_picture }) => {
                let handle = self.backend.submit_picture(backend_picture)?;

                if self.blocking_mode == BlockingMode::Blocking {
                    handle.sync()?;
                }
                (handle, header)
            }
            Some(CurrentPicState::ShowExistingFrame { header, handle }) => (handle, header),
            None => return Err(anyhow!("Broken stream: no picture to submit")),
        };

        let update_refs = if header.show_existing_frame {
            header.frame_type == FrameType::KeyFrame
        } else {
            true
        };

        if update_refs {
            let mut refresh_frame_flags = header.refresh_frame_flags;

            #[allow(clippy::needless_range_loop)]
            for i in 0..NUM_REF_FRAMES {
                if (refresh_frame_flags & 1) == 1 {
                    log::debug!(
                        "Replacing reference frame {} to new timestamp {} on frame count: {}",
                        i,
                        timestamp,
                        self.codec.frame_count
                    );
                    self.codec.reference_frames[i] = Some(handle.clone());
                }

                refresh_frame_flags >>= 1;
            }
        }

        let show_existing_frame = header.show_existing_frame;
        if header.show_frame || show_existing_frame {
            match self.codec.highest_spatial_layer {
                None => self.ready_queue.push(handle),
                Some(highest_spatial_layer) => {
                    if header.obu_header.spatial_id >= highest_spatial_layer {
                        self.ready_queue.push(handle);
                    } else {
                        log::debug!(
                            "Dropping frame with spatial_id {}",
                            header.obu_header.spatial_id
                        );
                    }
                }
            }
        }

        self.codec.parser.ref_frame_update(&header).map_err(|err| anyhow!(err))?;
        self.codec.frame_count += 1;
        Ok(())
    }
}

impl<B> StatelessVideoDecoder for StatelessDecoder<Av1, B>
where
    B: StatelessAV1DecoderBackend,
    B::Handle: Clone + 'static,
{
    type Handle = B::Handle;

    /// Decode an AV1 stream.
    ///
    /// `bitstream` should initially be submitted as a whole temporal unit, however a call to this
    /// method will only consume a single OBU. The caller must be careful to check the return value
    /// and resubmit the remainder if the whole bitstream has not been consumed.
    fn decode(
        &mut self,
        timestamp: u64,
        bitstream: &[u8],
        alloc_cb: &mut dyn FnMut() -> Option<
            <<B as StatelessDecoderBackend>::Handle as DecodedHandle>::Frame,
        >,
    ) -> Result<usize, DecodeError> {
        let obu = match self
            .codec
            .parser
            .read_obu(bitstream)
            .map_err(|err| DecodeError::ParseFrameError(err))?
        {
            ObuAction::Process(obu) => obu,
            // This OBU should be dropped.
            ObuAction::Drop(length) => return Ok(length as usize),
        };
        let obu_length = obu.bytes_used;

        let is_decode_op = matches!(
            obu.header.obu_type,
            ObuType::Frame | ObuType::FrameHeader | ObuType::TileGroup
        );

        if is_decode_op {
            match self.decoding_state {
                /* we want to be here */
                DecodingState::Decoding => (),

                /* otherwise... */
                DecodingState::AwaitingStreamInfo => {
                    /* Skip input until we get information from the stream. */
                    return Ok(obu_length);
                }
                /* Ask the client to confirm the format before we can process this. */
                DecodingState::FlushingForDRC | DecodingState::AwaitingFormat(_) => {
                    // Start signaling the awaiting format event to process a format change.
                    self.awaiting_format_event.write(1).unwrap();
                    return Err(DecodeError::CheckEvents);
                }

                DecodingState::Reset => {
                    let mut parser = self.codec.parser.clone();

                    let is_key_frame = match obu.header.obu_type {
                        ObuType::Frame | ObuType::FrameHeader => {
                            let fh = parser
                                .parse_frame_header_obu(&obu)
                                .map_err(|err| DecodeError::ParseFrameError(err))?;
                            fh.frame_type == FrameType::KeyFrame
                        }
                        _ => false,
                    };

                    /* we can only resume from key frames */
                    if !is_key_frame {
                        return Ok(obu_length);
                    } else {
                        self.decoding_state = DecodingState::Decoding;
                    }
                }
            }
        }

        /* We are in `Decoding` state if we reached here */

        match self.codec.parser.parse_obu(obu).map_err(|err| DecodeError::ParseFrameError(err))? {
            ParsedObu::SequenceHeader(sequence) => {
                let sequence_differs = match &self.codec.stream_info {
                    Some(old_stream_info) => *old_stream_info.seq_header != *sequence,
                    None => true,
                };

                if matches!(self.decoding_state, DecodingState::AwaitingStreamInfo)
                    || sequence_differs
                {
                    if self.codec.current_pic.is_some() {
                        return Err(DecodeError::DecoderError(anyhow!(
                                "broken stream: a picture is being decoded while a new sequence header is encountered"
                            )));
                    }

                    /* make sure we sync *before* we clear any state in the backend */
                    for f in &mut self.ready_queue.queue {
                        /* TODO: this fixes av1-1-b8-03-sizeup on Intel
                         * gen12, but we apparently do not do the same in
                         * VP9. How is it that we do not get similar crashes there?
                         *
                         * TODO: syncing before calling new_sequence() in VP9 may fix some tests
                         */
                        f.sync()?;
                    }

                    log::debug!(
                        "found new sequence, resolution: {:?}, profile: {:?}, bit depth: {:?}",
                        Resolution::from((
                            sequence.max_frame_width_minus_1 as u32 + 1,
                            sequence.max_frame_height_minus_1 as u32 + 1
                        )),
                        sequence.seq_profile,
                        sequence.bit_depth
                    );
                    /* there is nothing to drain, much like vp8 and vp9 */
                    self.codec.highest_spatial_layer = self.codec.parser.highest_operating_point();

                    let stream_info = match &self.codec.parser.last_frame_header {
                        Some(fh) => StreamInfo {
                            seq_header: sequence.clone(),
                            render_width: fh.render_width,
                            render_height: fh.render_height,
                        },
                        None => StreamInfo {
                            seq_header: sequence.clone(),
                            render_width: sequence.max_frame_width_minus_1 as u32 + 1,
                            render_height: sequence.max_frame_height_minus_1 as u32 + 1,
                        },
                    };
                    self.backend.change_stream_info(&stream_info)?;
                    self.await_format_change(stream_info);
                }
            }
            ParsedObu::FrameHeader(frame_header) => {
                if self.codec.current_pic.is_some() {
                    /* submit this frame immediately, as we need to update the
                     * DPB and the reference info state *before* processing the
                     * next frame */
                    self.submit_frame(timestamp)?;
                }
                self.decode_frame_header(frame_header, timestamp, alloc_cb)?;
            }
            ParsedObu::TileGroup(tile_group) => {
                self.decode_tile_group(tile_group)?;
            }
            ParsedObu::Frame(frame) => {
                let stream_info =
                    self.codec.stream_info.as_ref().ok_or(DecodeError::DecoderError(anyhow!(
                        "broken stream: a picture is being decoded without a sequence header"
                    )))?;
                if stream_info.render_width != frame.header.render_width
                    || stream_info.render_height != frame.header.render_height
                {
                    let new_stream_info = StreamInfo {
                        seq_header: stream_info.seq_header.clone(),
                        render_width: frame.header.render_width,
                        render_height: frame.header.render_height,
                    };
                    self.backend.change_stream_info(&new_stream_info)?;
                }
                if self.codec.current_pic.is_some() {
                    /* submit this frame immediately, as we need to update the
                     * DPB and the reference info state *before* processing the
                     * next frame */
                    self.submit_frame(timestamp)?;
                }
                self.decode_frame(frame, timestamp, alloc_cb)?;
                /* submit this frame immediately, as we need to update the
                 * DPB and the reference info state *before* processing the
                 * next frame */
                self.submit_frame(timestamp)?;
            }
            ParsedObu::TileList => {
                return Err(DecodeError::DecoderError(anyhow!(
                    "large tile scale mode is not supported"
                )));
            }
            other => {
                log::debug!("skipping OBU of type {:?}", other.obu_type());
            }
        }

        /* Submit the last frame if we have reached the end of the temporal unit. */
        if bitstream.len() == obu_length && self.codec.current_pic.is_some() {
            self.submit_frame(timestamp)?;
        }

        Ok(obu_length)
    }

    fn flush(&mut self) -> Result<(), super::DecodeError> {
        // Note: all the submitted frames are already in the ready queue.
        self.codec.reference_frames = Default::default();
        self.decoding_state = DecodingState::Reset;

        Ok(())
    }

    fn stream_info(&self) -> Option<&crate::decoder::StreamInfo> {
        self.backend.stream_info()
    }

    fn next_event(&mut self) -> Option<crate::decoder::DecoderEvent<B::Handle>> {
        self.query_next_event(|decoder, stream_info| {
            decoder.codec.stream_info = Some(stream_info.clone());
        })
    }

    fn poll_fd(&self) -> BorrowedFd {
        self.epoll_fd.0.as_fd()
    }
}

#[cfg(test)]
pub mod tests {
    use crate::bitstream_utils::IvfIterator;
    use crate::decoder::stateless::av1::Av1;
    use crate::decoder::stateless::tests::test_decode_stream;
    use crate::decoder::stateless::tests::TestStream;
    use crate::decoder::stateless::StatelessDecoder;
    use crate::decoder::BlockingMode;
    use crate::utils::simple_playback_loop;
    use crate::utils::simple_playback_loop_owned_frames;
    use crate::DecodedFormat;

    /// Run `test` using the dummy decoder, in both blocking and non-blocking modes.
    fn test_decoder_dummy(test: &TestStream, blocking_mode: BlockingMode) {
        let decoder = StatelessDecoder::<Av1, _>::new_dummy(blocking_mode).unwrap();

        test_decode_stream(
            |d, s, f| {
                simple_playback_loop(
                    d,
                    IvfIterator::new(s),
                    f,
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

    /// Same as Chromium's test-25fps.av1.ivf
    pub const DECODE_TEST_25FPS: TestStream = TestStream {
        stream: include_bytes!("../../codec/av1/test_data/test-25fps.ivf.av1"),
        crcs: include_str!("../../codec/av1/test_data/test-25fps.ivf.av1.crc"),
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
