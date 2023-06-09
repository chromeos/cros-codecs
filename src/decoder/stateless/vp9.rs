// Copyright 2022 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

pub mod backends;
pub mod decoder;
mod lookups;
pub mod parser;

use log::debug;

use crate::decoder::stateless::private::VideoDecoderPrivate;
use crate::decoder::stateless::vp9::backends::StatelessDecoderBackend;
use crate::decoder::stateless::vp9::lookups::AC_QLOOKUP;
use crate::decoder::stateless::vp9::lookups::AC_QLOOKUP_10;
use crate::decoder::stateless::vp9::lookups::AC_QLOOKUP_12;
use crate::decoder::stateless::vp9::lookups::DC_QLOOKUP;
use crate::decoder::stateless::vp9::lookups::DC_QLOOKUP_10;
use crate::decoder::stateless::vp9::lookups::DC_QLOOKUP_12;
use crate::decoder::stateless::vp9::parser::BitDepth;
use crate::decoder::stateless::vp9::parser::Frame;
use crate::decoder::stateless::vp9::parser::Header;
use crate::decoder::stateless::vp9::parser::Parser;
use crate::decoder::stateless::vp9::parser::Profile;
use crate::decoder::stateless::vp9::parser::INTRA_FRAME;
use crate::decoder::stateless::vp9::parser::LAST_FRAME;
use crate::decoder::stateless::vp9::parser::MAX_LOOP_FILTER;
use crate::decoder::stateless::vp9::parser::MAX_MODE_LF_DELTAS;
use crate::decoder::stateless::vp9::parser::MAX_REF_FRAMES;
use crate::decoder::stateless::vp9::parser::MAX_SEGMENTS;
use crate::decoder::stateless::vp9::parser::NUM_REF_FRAMES;
use crate::decoder::stateless::vp9::parser::SEG_LVL_ALT_L;
use crate::decoder::stateless::vp9::parser::SEG_LVL_REF_FRAME;
use crate::decoder::stateless::vp9::parser::SEG_LVL_SKIP;
use crate::decoder::stateless::DecodeError;
use crate::decoder::stateless::DecodingState;
use crate::decoder::stateless::StatelessDecoderFormatNegotiator;
use crate::decoder::stateless::VideoDecoder;
use crate::decoder::BlockingMode;
use crate::decoder::DecodedHandle;
use crate::decoder::DecoderEvent;
use crate::decoder::ReadyFramesQueue;
use crate::Resolution;

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Segmentation {
    /// Loop filter level
    pub lvl_lookup: [[u8; MAX_MODE_LF_DELTAS]; MAX_REF_FRAMES],

    /// AC quant scale for luma component
    pub luma_ac_quant_scale: i16,
    /// DC quant scale for luma component
    pub luma_dc_quant_scale: i16,
    /// AC quant scale for chroma component
    pub chroma_ac_quant_scale: i16,
    /// DC quant scale for chroma component
    pub chroma_dc_quant_scale: i16,

    /// Whether the alternate reference frame segment feature is enabled (SEG_LVL_REF_FRAME)
    pub reference_frame_enabled: bool,
    /// The feature data for the reference frame featire
    pub reference_frame: i16,
    /// Whether the skip segment feature is enabled (SEG_LVL_SKIP)
    pub reference_skip_enabled: bool,
}

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

    /// The reference frames in use.
    reference_frames: [Option<T>; NUM_REF_FRAMES],

    /// Per-segment data.
    segmentation: [Segmentation; MAX_SEGMENTS],

    /// Cached value for bit depth
    bit_depth: BitDepth,
    /// Cached value for profile
    profile: Profile,
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
            parser: Default::default(),
            decoding_state: Default::default(),
            reference_frames: Default::default(),
            segmentation: Default::default(),
            coded_resolution: Default::default(),
            ready_queue: Default::default(),
            bit_depth: Default::default(),
            profile: Default::default(),
        })
    }

    /// Replace a reference frame with `handle`.
    fn replace_reference(reference: &mut Option<T>, handle: &T) {
        *reference = Some(handle.clone());
    }

    fn update_references(
        reference_frames: &mut [Option<T>; NUM_REF_FRAMES],
        picture: &T,
        mut refresh_frame_flags: u8,
    ) -> anyhow::Result<()> {
        #[allow(clippy::needless_range_loop)]
        for i in 0..NUM_REF_FRAMES {
            if (refresh_frame_flags & 1) == 1 {
                debug!("Replacing reference frame {}", i);
                Self::replace_reference(&mut reference_frames[i], picture)
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
            let ref_frame = self.reference_frames[idx].as_ref().unwrap();

            // We are done, no further processing needed.
            ref_frame.clone()
        } else {
            // Otherwise, we must actually arrange to decode a frame
            let refresh_frame_flags = frame.header.refresh_frame_flags;

            Self::update_segmentation(&frame.header, &mut self.segmentation)?;
            let decoded_handle = self.backend.submit_picture(
                &frame.header,
                &self.reference_frames,
                frame.as_ref(),
                timestamp,
                &self.segmentation,
            )?;

            if self.blocking_mode == BlockingMode::Blocking {
                decoded_handle.sync()?;
            }

            // Do DPB management
            Self::update_references(
                &mut self.reference_frames,
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
            || bit_depth != self.bit_depth
            || profile != self.profile
    }

    /// A clamp such that min <= x <= max
    fn clamp<U: PartialOrd>(x: U, low: U, high: U) -> U {
        if x > high {
            high
        } else if x < low {
            low
        } else {
            x
        }
    }

    /// An implementation of seg_feature_active as per "6.4.9 Segmentation feature active syntax"
    fn seg_feature_active(hdr: &Header, segment_id: u8, feature: u8) -> bool {
        let feature_enabled = hdr.seg.feature_enabled;
        hdr.seg.enabled && feature_enabled[usize::from(segment_id)][usize::from(feature)]
    }

    /// An implementation of get_qindex as per "8.6.1 Dequantization functions"
    fn get_qindex(hdr: &Header, segment_id: u8) -> i32 {
        let base_q_idx = hdr.quant.base_q_idx;

        if Self::seg_feature_active(hdr, segment_id, 0) {
            let mut data = hdr.seg.feature_data[usize::from(segment_id)][0] as i32;

            if !hdr.seg.abs_or_delta_update {
                data += i32::from(base_q_idx);
            }

            Self::clamp(data, 0, 255)
        } else {
            i32::from(base_q_idx)
        }
    }

    /// An implementation of get_dc_quant as per "8.6.1 Dequantization functions"
    fn get_dc_quant(hdr: &Header, segment_id: u8, luma: bool) -> anyhow::Result<i32> {
        let delta_q_dc = if luma {
            hdr.quant.delta_q_y_dc
        } else {
            hdr.quant.delta_q_uv_dc
        };
        let qindex = Self::get_qindex(hdr, segment_id);
        let q_table_idx = Self::clamp(qindex + i32::from(delta_q_dc), 0, 255) as usize;
        match hdr.bit_depth {
            BitDepth::Depth8 => Ok(i32::from(DC_QLOOKUP[q_table_idx])),
            BitDepth::Depth10 => Ok(i32::from(DC_QLOOKUP_10[q_table_idx])),
            BitDepth::Depth12 => Ok(i32::from(DC_QLOOKUP_12[q_table_idx])),
        }
    }

    /// An implementation of get_ac_quant as per "8.6.1 Dequantization functions"
    fn get_ac_quant(hdr: &Header, segment_id: u8, luma: bool) -> anyhow::Result<i32> {
        let delta_q_ac = if luma { 0 } else { hdr.quant.delta_q_uv_ac };
        let qindex = Self::get_qindex(hdr, segment_id);
        let q_table_idx = usize::try_from(Self::clamp(qindex + i32::from(delta_q_ac), 0, 255))?;

        match hdr.bit_depth {
            BitDepth::Depth8 => Ok(i32::from(AC_QLOOKUP[q_table_idx])),
            BitDepth::Depth10 => Ok(i32::from(AC_QLOOKUP_10[q_table_idx])),
            BitDepth::Depth12 => Ok(i32::from(AC_QLOOKUP_12[q_table_idx])),
        }
    }

    /// Update the state of the segmentation parameters after seeing a frame
    pub(crate) fn update_segmentation(
        hdr: &Header,
        segmentation: &mut [Segmentation; MAX_SEGMENTS],
    ) -> anyhow::Result<()> {
        let lf = &hdr.lf;
        let seg = &hdr.seg;

        let n_shift = lf.level >> 5;

        for segment_id in 0..MAX_SEGMENTS as u8 {
            let luma_dc_quant_scale = i16::try_from(Self::get_dc_quant(hdr, segment_id, true)?)?;
            let luma_ac_quant_scale = i16::try_from(Self::get_ac_quant(hdr, segment_id, true)?)?;
            let chroma_dc_quant_scale = i16::try_from(Self::get_dc_quant(hdr, segment_id, false)?)?;
            let chroma_ac_quant_scale = i16::try_from(Self::get_ac_quant(hdr, segment_id, false)?)?;

            let mut lvl_seg = i32::from(lf.level);
            let mut lvl_lookup: [[u8; MAX_MODE_LF_DELTAS]; MAX_REF_FRAMES];

            if lvl_seg == 0 {
                lvl_lookup = Default::default()
            } else {
                // 8.8.1 Loop filter frame init process
                if Self::seg_feature_active(hdr, segment_id, SEG_LVL_ALT_L as u8) {
                    if seg.abs_or_delta_update {
                        lvl_seg =
                            i32::from(seg.feature_data[usize::from(segment_id)][SEG_LVL_ALT_L]);
                    } else {
                        lvl_seg +=
                            i32::from(seg.feature_data[usize::from(segment_id)][SEG_LVL_ALT_L]);
                    }

                    lvl_seg = Self::clamp(lvl_seg, 0, MAX_LOOP_FILTER as i32);
                }

                if !lf.delta_enabled {
                    lvl_lookup = [[u8::try_from(lvl_seg)?; MAX_MODE_LF_DELTAS]; MAX_REF_FRAMES]
                } else {
                    let intra_delta = i32::from(lf.ref_deltas[INTRA_FRAME]);
                    let mut intra_lvl = lvl_seg + (intra_delta << n_shift);

                    lvl_lookup = segmentation[usize::from(segment_id)].lvl_lookup;
                    lvl_lookup[INTRA_FRAME][0] =
                        u8::try_from(Self::clamp(intra_lvl, 0, MAX_LOOP_FILTER as i32))?;

                    // Note, this array has the [0] element unspecified/unused in
                    // VP9. Confusing, but we do start to index from 1.
                    #[allow(clippy::needless_range_loop)]
                    for ref_ in LAST_FRAME..MAX_REF_FRAMES {
                        for mode in 0..MAX_MODE_LF_DELTAS {
                            let ref_delta = i32::from(lf.ref_deltas[ref_]);
                            let mode_delta = i32::from(lf.mode_deltas[mode]);

                            intra_lvl = lvl_seg + (ref_delta << n_shift) + (mode_delta << n_shift);

                            lvl_lookup[ref_][mode] =
                                u8::try_from(Self::clamp(intra_lvl, 0, MAX_LOOP_FILTER as i32))?;
                        }
                    }
                }
            }

            segmentation[usize::from(segment_id)] = Segmentation {
                lvl_lookup,
                luma_ac_quant_scale,
                luma_dc_quant_scale,
                chroma_ac_quant_scale,
                chroma_dc_quant_scale,
                reference_frame_enabled: seg.feature_enabled[usize::from(segment_id)]
                    [SEG_LVL_REF_FRAME],
                reference_frame: seg.feature_data[usize::from(segment_id)][SEG_LVL_REF_FRAME],
                reference_skip_enabled: seg.feature_enabled[usize::from(segment_id)][SEG_LVL_SKIP],
            }
        }

        Ok(())
    }

    #[cfg(test)]
    #[allow(dead_code)]
    pub(crate) fn backend(&self) -> &dyn StatelessDecoderBackend<Handle = T> {
        self.backend.as_ref()
    }
}

impl<T: DecodedHandle + Clone + 'static> VideoDecoder for Decoder<T> {
    fn decode(&mut self, timestamp: u64, bitstream: &[u8]) -> Result<(), DecodeError> {
        let frames = self.parser.parse_chunk(bitstream)?;

        if matches!(self.decoding_state, DecodingState::Decoding)
            && self.num_resources_left() < frames.len()
        {
            return Err(DecodeError::CheckEvents);
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
            }
        }

        for frame in frames {
            match &mut self.decoding_state {
                // Skip input until we get information from the stream.
                DecodingState::AwaitingStreamInfo => (),
                // Ask the client to confirm the format before we can process this.
                DecodingState::AwaitingFormat(_) => return Err(DecodeError::CheckEvents),
                DecodingState::Decoding => self.handle_frame(&frame, timestamp)?,
            }
        }

        Ok(())
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
                            decoder.profile = hdr.profile;
                            decoder.bit_depth = hdr.bit_depth;
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

    fn format(&self) -> Option<crate::DecodedFormat> {
        self.backend.format()
    }
}

impl<T: DecodedHandle + Clone + 'static> VideoDecoderPrivate for Decoder<T> {
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
    use crate::decoder::stateless::vp8::tests::vpx_decoding_loop;
    use crate::decoder::stateless::vp9::Decoder;
    use crate::decoder::BlockingMode;
    use crate::DecodedFormat;

    /// Run `test` using the dummy decoder, in both blocking and non-blocking modes.
    fn test_decoder_dummy(test: &TestStream, blocking_mode: BlockingMode) {
        let decoder = Decoder::new_dummy(blocking_mode).unwrap();

        test_decode_stream(
            |d, s, c| vpx_decoding_loop(d, s, c, DecodedFormat::NV12, blocking_mode),
            decoder,
            test,
            false,
            false,
        );
    }

    /// Same as Chromium's test-25fps.vp8
    pub const DECODE_TEST_25FPS: TestStream = TestStream {
        stream: include_bytes!("vp9/test_data/test-25fps.vp9"),
        crcs: include_str!("vp9/test_data/test-25fps.vp9.crc"),
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
        stream: include_bytes!("vp9/test_data/vp90-2-10-show-existing-frame.vp9.ivf"),
        crcs: include_str!("vp9/test_data/vp90-2-10-show-existing-frame.vp9.ivf.crc"),
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
        stream: include_bytes!("vp9/test_data/vp90-2-10-show-existing-frame2.vp9.ivf"),
        crcs: include_str!("vp9/test_data/vp90-2-10-show-existing-frame2.vp9.ivf.crc"),
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
        stream: include_bytes!("vp9/test_data/resolution_change_500frames-vp9.ivf"),
        crcs: include_str!("vp9/test_data/resolution_change_500frames-vp9.ivf.crc"),
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
