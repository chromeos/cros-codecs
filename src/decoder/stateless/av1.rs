// Copyright 2023 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::rc::Rc;

use anyhow::anyhow;
use anyhow::Context;

use crate::codec::av1::parser::FrameHeaderObu;
use crate::codec::av1::parser::FrameObu;
use crate::codec::av1::parser::FrameType;
use crate::codec::av1::parser::ObuType;
use crate::codec::av1::parser::ParsedObu;
use crate::codec::av1::parser::Parser;
use crate::codec::av1::parser::SequenceHeaderObu;
use crate::codec::av1::parser::NUM_REF_FRAMES;
use crate::Resolution;

use crate::codec::av1::parser::TileGroupObu;
use crate::decoder::stateless::DecoderEvent;
use crate::decoder::stateless::DecodingState;
use crate::decoder::stateless::StatelessBackendResult;
use crate::decoder::stateless::StatelessDecoderBackend;
use crate::decoder::stateless::StatelessDecoderFormatNegotiator;
use crate::decoder::stateless::StatelessVideoDecoder;
use crate::decoder::BlockingMode;
use crate::decoder::DecodedHandle;
use crate::decoder::FramePool;
use crate::decoder::PoolLayer;

use crate::decoder::stateless::DecodeError;
use crate::decoder::stateless::StatelessCodec;
use crate::decoder::stateless::StatelessDecoder;

#[cfg(test)]
mod dummy;
#[cfg(feature = "vaapi")]
mod vaapi;

/// Stateless backend methods specific to AV1.
pub trait StatelessAV1DecoderBackend: StatelessDecoderBackend<Av1> {
    /// Called when a new Sequence Header OBU is parsed. The
    /// `highest_spatial_layer` argument refers to the maximum layer selected by
    /// the client through `set_operating_point()` and the scalability
    /// information present in the stream, if any.
    fn new_sequence(
        &mut self,
        sequence: &Rc<SequenceHeaderObu>,
        highest_spatial_layer: Option<u32>,
    ) -> StatelessBackendResult<()>;

    /// Called when the decoder determines that a new picture was found.
    fn new_picture(
        &mut self,
        sequence: &SequenceHeaderObu,
        picture: &FrameHeaderObu,
        timestamp: u64,
        reference_frames: &[Option<Self::Handle>; NUM_REF_FRAMES],
    ) -> StatelessBackendResult<Self::Picture>;

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
/// Stored between calls to [`StatelessDecoder::handle_tile`] that belong to the
/// same picture.
enum CurrentPicState<B: StatelessDecoderBackend<Av1>> {
    /// A regular frame
    RegularFrame {
        /// Data for the current picture as extracted from the stream.
        header: FrameHeaderObu,
        /// Backend-specific data for that picture.
        backend_picture: B::Picture,
    },

    /// A frame that has 'show_existing_frame' set.
    ShowExistingFrame {
        /// Data for the current picture as extracted from the stream.
        header: FrameHeaderObu,
        /// The handle of the reference frame that this frame points to.
        handle: B::Handle,
    },
}

pub struct AV1DecoderState<B: StatelessDecoderBackend<Av1>> {
    /// AV1 bitstream parser.
    parser: Parser,

    /// The reference frames in use.
    reference_frames: [Option<B::Handle>; NUM_REF_FRAMES],

    /// Keeps track of the last values seen for negotiation purposes.
    sequence: Option<Rc<SequenceHeaderObu>>,

    /// The picture currently being decoded. We need to preserve it between
    /// calls to `decode` because multiple tiles will be processed in different
    /// calls to `decode`.
    current_pic: Option<CurrentPicState<B>>,

    /// Keep track of the number of frames we've processed for logging purposes.
    frame_count: u32,

    /// For SVC streams, we only want to output the highest layer possible given
    /// the choice of operating point.
    highest_spatial_layer: Option<u32>,
}

impl<B> Default for AV1DecoderState<B>
where
    B: StatelessAV1DecoderBackend,
    B::Handle: Clone,
{
    fn default() -> Self {
        Self {
            parser: Default::default(),
            reference_frames: Default::default(),
            sequence: Default::default(),
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
    type FormatInfo = Rc<SequenceHeaderObu>;
    type DecoderState<B: StatelessDecoderBackend<Self>> = AV1DecoderState<B>;
}

impl<B> StatelessDecoder<Av1, B>
where
    B: StatelessAV1DecoderBackend,
    B::Handle: Clone,
{
    fn count_frames(&mut self, bitstream: &[u8]) -> usize {
        let mut nframes = 0;
        let mut consumed = 0;

        while let Ok(obu) = self.codec.parser.parse_obu(&bitstream[consumed..]) {
            let obu = match obu {
                ParsedObu::Process(obu) => obu,
                ParsedObu::Drop(length) => {
                    consumed += usize::try_from(length).unwrap();
                    continue;
                }
            };

            if matches!(obu.header.obu_type, ObuType::Frame | ObuType::FrameHeader) {
                nframes += 1;
            }

            consumed += obu.data.len();
        }

        nframes
    }

    fn decode_frame_header(
        &mut self,
        frame_header: FrameHeaderObu,
        timestamp: u64,
    ) -> anyhow::Result<()> {
        log::debug!(
            "Processing frame {} with timestamp {}",
            self.codec.frame_count,
            timestamp
        );

        if frame_header.show_existing_frame {
            let idx = usize::try_from(frame_header.frame_to_show_map_idx)
                .context("Broken stream: invalid frame_to_show_map_idx")?;
            let ref_frame = self.codec.reference_frames[idx]
                .as_ref()
                .ok_or(anyhow!("Broken stream: no reference picture to display"))?;
            self.codec.current_pic = Some(CurrentPicState::ShowExistingFrame {
                header: frame_header,
                handle: ref_frame.clone(),
            });
        } else if let Some(sequence) = &self.codec.sequence {
            let backend_picture = self.backend.new_picture(
                sequence,
                &frame_header,
                timestamp,
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
            Some(CurrentPicState::RegularFrame {
                backend_picture, ..
            }) => backend_picture,
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

    fn decode_frame(&mut self, frame: FrameObu, timestamp: u64) -> anyhow::Result<()> {
        let FrameObu { header, tile_group } = frame;
        self.decode_frame_header(header, timestamp)?;
        self.decode_tile_group(tile_group)?;
        Ok(())
    }

    fn submit_frame(&mut self, timestamp: u64) -> anyhow::Result<()> {
        log::debug!(
            "Finishing frame {} with timestamp: {}",
            self.codec.frame_count,
            timestamp
        );

        let picture = self.codec.current_pic.take();

        let (handle, header) = match picture {
            Some(CurrentPicState::RegularFrame {
                header,
                backend_picture,
            }) => {
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

        self.codec.parser.ref_frame_update(&header)?;
        self.codec.frame_count += 1;
        Ok(())
    }
}

impl<B> StatelessVideoDecoder<<B::Handle as DecodedHandle>::Descriptor> for StatelessDecoder<Av1, B>
where
    B: StatelessAV1DecoderBackend,
    B::Handle: Clone + 'static,
{
    fn decode(&mut self, timestamp: u64, bitstream: &[u8]) -> Result<usize, super::DecodeError> {
        let mut consumed = 0;

        let nframes = self.count_frames(bitstream);
        /* we do not know the resolution at this point, as we haven't parsed the
         * frames yet. Be conservative and check whether we have enough frames
         * across all layers */
        let num_free_frames = self
            .backend
            .frame_pool(PoolLayer::All)
            .iter()
            .map(|x| x.num_free_frames())
            .min()
            .ok_or(anyhow!("No pool found"))?;

        if matches!(self.decoding_state, DecodingState::Decoding) && num_free_frames < nframes {
            return Err(DecodeError::NotEnoughOutputBuffers(
                nframes - num_free_frames,
            ));
        }

        while let Ok(obu) = self.codec.parser.parse_obu(&bitstream[consumed..]) {
            let obu = match obu {
                ParsedObu::Process(obu) => obu,
                // This OBU should be dropped.
                ParsedObu::Drop(length) => {
                    consumed += usize::try_from(length).context("OBU length too large")?;
                    continue;
                }
            };

            let obu_length = obu.data.len();

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
                        consumed += obu_length;
                        continue;
                    }
                    /* Ask the client to confirm the format before we can process this. */
                    DecodingState::AwaitingFormat(_) => return Err(DecodeError::CheckEvents),
                    DecodingState::Reset => {
                        let mut parser = self.codec.parser.clone();

                        let is_key_frame = match obu.header.obu_type {
                            ObuType::Frame => {
                                let frame = parser.parse_frame_obu(obu.clone())?;
                                frame.header.frame_type == FrameType::KeyFrame
                            }
                            ObuType::FrameHeader => {
                                let fh = parser.parse_frame_header_obu(&obu)?;
                                fh.frame_type == FrameType::KeyFrame
                            }
                            _ => false,
                        };

                        /* we can only resume from key frames */
                        if !is_key_frame {
                            consumed += obu_length;
                            continue;
                        } else {
                            self.decoding_state = DecodingState::Decoding;
                        }
                    }
                }
            }

            match obu.header.obu_type {
                ObuType::SequenceHeader => {
                    let sequence = self.codec.parser.parse_sequence_header_obu(&obu)?;
                    let sequence_differs = match &self.codec.sequence {
                        Some(old_sequence) => **old_sequence != *sequence,
                        None => true,
                    };

                    if matches!(self.decoding_state, DecodingState::AwaitingStreamInfo)
                        || sequence_differs
                    {
                        if self.codec.current_pic.is_some() {
                            return Err(DecodeError::DecoderError(anyhow!(
                                "Broken stream: a picture is being decoded while a new sequence header is encountered"
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
                            "Found new sequence, resolution: {:?}, profile: {:?}, bit depth: {:?}",
                            Resolution::from((
                                sequence.max_frame_width_minus_1 + 1,
                                sequence.max_frame_height_minus_1 + 1
                            )),
                            sequence.seq_profile,
                            sequence.bit_depth
                        );
                        /* there is nothing to drain, much like vp8 and vp9 */
                        self.codec.highest_spatial_layer =
                            self.codec.parser.highest_operating_point();
                        self.backend
                            .new_sequence(&sequence, self.codec.highest_spatial_layer)?;
                        self.decoding_state = DecodingState::AwaitingFormat(sequence);
                    }
                }
                ObuType::TemporalDelimiter => {
                    self.codec.parser.parse_temporal_delimiter_obu(&obu)?
                }
                ObuType::FrameHeader => {
                    if self.codec.current_pic.is_some() {
                        /* submit this frame immediately, as we need to update the
                         * DPB and the reference info state *before* processing the
                         * next frame */
                        self.submit_frame(timestamp)?;
                    }
                    let frame_header = self.codec.parser.parse_frame_header_obu(&obu)?;
                    self.decode_frame_header(frame_header, timestamp)?;
                }
                ObuType::TileGroup => {
                    let tile_group = self.codec.parser.parse_tile_group_obu(obu)?;
                    self.decode_tile_group(tile_group)?;
                }
                ObuType::Frame => {
                    let frame = self.codec.parser.parse_frame_obu(obu)?;
                    self.decode_frame(frame, timestamp)?;
                    /* submit this frame immediately, as we need to update the
                     * DPB and the reference info state *before* processing the
                     * next frame */
                    self.submit_frame(timestamp)?;
                }
                ObuType::TileList => {
                    return Err(DecodeError::DecoderError(anyhow!(
                        "Large tile scale mode is not supported"
                    )));
                }
                other => {
                    log::debug!("Skipping OBU of type {:?}", other);
                }
            }

            consumed += obu_length;
        }

        /* we may already have dispatched work if we got ObuType::Frame */
        if self.codec.current_pic.is_some() {
            /* dispatch work to the backend */
            self.submit_frame(timestamp)?;
        }

        Ok(consumed)
    }

    fn flush(&mut self) -> Result<(), super::DecodeError> {
        // Note: all the submitted frames are already in the ready queue.
        self.codec.reference_frames = Default::default();
        self.decoding_state = DecodingState::Reset;

        Ok(())
    }

    fn frame_pool(
        &mut self,
        layer: PoolLayer,
    ) -> Vec<&mut dyn FramePool<<B::Handle as DecodedHandle>::Descriptor>> {
        self.backend.frame_pool(layer)
    }

    fn stream_info(&self) -> Option<&crate::decoder::StreamInfo> {
        self.backend.stream_info()
    }

    fn next_event(
        &mut self,
    ) -> Option<crate::decoder::DecoderEvent<<B::Handle as DecodedHandle>::Descriptor>> {
        // The next event is either the next frame, or, if we are awaiting negotiation, the format
        // change event that will allow us to keep going.
        (&mut self.ready_queue)
            .next()
            .map(|handle| DecoderEvent::FrameReady(Box::new(handle)))
            .or_else(|| {
                if let DecodingState::AwaitingFormat(sequence) = &self.decoding_state {
                    Some(DecoderEvent::FormatChanged(Box::new(
                        StatelessDecoderFormatNegotiator::new(
                            self,
                            sequence.clone(),
                            |decoder, sequence| {
                                decoder.codec.sequence = Some(Rc::clone(sequence));
                                decoder.decoding_state = DecodingState::Decoding;
                            },
                        ),
                    )))
                } else {
                    None
                }
            })
    }
}

#[cfg(test)]
pub mod tests {
    use crate::decoder::stateless::av1::Av1;
    use crate::decoder::stateless::tests::test_decode_stream;
    use crate::decoder::stateless::tests::TestStream;
    use crate::decoder::stateless::StatelessDecoder;
    use crate::decoder::BlockingMode;
    use crate::utils::simple_playback_loop;
    use crate::utils::simple_playback_loop_owned_frames;
    use crate::utils::IvfIterator;
    use crate::DecodedFormat;

    /// Run `test` using the dummy decoder, in both blocking and non-blocking modes.
    fn test_decoder_dummy(test: &TestStream, blocking_mode: BlockingMode) {
        let decoder = StatelessDecoder::<Av1, _>::new_dummy(blocking_mode);

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
