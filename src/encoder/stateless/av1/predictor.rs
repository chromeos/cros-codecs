// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::collections::VecDeque;
use std::rc::Rc;

use crate::codec::av1::parser::BitDepth;
use crate::codec::av1::parser::CdefParams;
use crate::codec::av1::parser::ColorConfig;
use crate::codec::av1::parser::FrameHeaderObu;
use crate::codec::av1::parser::FrameType;
use crate::codec::av1::parser::ObuHeader;
use crate::codec::av1::parser::ObuType;
use crate::codec::av1::parser::OperatingPoint;
use crate::codec::av1::parser::Profile;
use crate::codec::av1::parser::QuantizationParams;
use crate::codec::av1::parser::ReferenceFrameType;
use crate::codec::av1::parser::SequenceHeaderObu;
use crate::codec::av1::parser::TemporalDelimiterObu;
use crate::codec::av1::parser::TileInfo;
use crate::codec::av1::parser::TxMode;
use crate::codec::av1::parser::MAX_NUM_OPERATING_POINTS;
use crate::codec::av1::parser::MAX_TILE_COLS;
use crate::codec::av1::parser::MAX_TILE_ROWS;
use crate::codec::av1::parser::PRIMARY_REF_NONE;
use crate::codec::av1::parser::REFS_PER_FRAME;
use crate::codec::av1::parser::SELECT_INTEGER_MV;
use crate::codec::av1::parser::SUPERRES_NUM;
use crate::codec::av1::synthesizer::Synthesizer;
use crate::encoder::stateless::av1::BackendRequest;
use crate::encoder::stateless::av1::EncoderConfig;
use crate::encoder::stateless::EncodeError;
use crate::encoder::stateless::EncodeResult;
use crate::encoder::stateless::Predictor;
use crate::encoder::FrameMetadata;

#[derive(Clone)]
pub enum PredictionStructure {
    /// Simplest prediction structure, suitable eg. for RTC. Interframe is produced at the start of
    /// the stream and every time when [`limit`] frames are reached. Following interframe frames
    /// are frames relying solely on the last frame.
    LowDelay { limit: u16 },
}

pub(super) struct LowDelay<Picture, Reference> {
    /// Pending frames for encoding
    queue: VecDeque<(Picture, FrameMetadata)>,

    /// Availabe frames for references
    references: VecDeque<Rc<Reference>>,

    /// Current sequence header obu
    sequence: SequenceHeaderObu,

    /// Current frame counter
    counter: usize,

    /// The number of frames between intra frames
    limit: u16,

    /// Encoder config
    config: EncoderConfig,
}

impl<P, R> LowDelay<P, R> {
    pub(super) fn new(config: EncoderConfig, limit: u16) -> Self {
        Self {
            queue: Default::default(),
            references: Default::default(),
            sequence: Self::create_sequence_header(&config),
            counter: 0,
            limit,
            config,
        }
    }

    fn create_sequence_header(config: &EncoderConfig) -> SequenceHeaderObu {
        let width = config.resolution.width;
        let height = config.resolution.height;

        SequenceHeaderObu {
            obu_header: ObuHeader {
                obu_type: ObuType::SequenceHeader,
                extension_flag: false,
                has_size_field: true,
                temporal_id: 0,
                spatial_id: 0,
            },

            seq_profile: Profile::Profile0,
            num_planes: 3,

            enable_order_hint: true,
            order_hint_bits: 8,
            order_hint_bits_minus_1: 8 - 1,

            // Use maximum size (16 bits)
            frame_width_bits_minus_1: (1 << 4) - 1,
            frame_height_bits_minus_1: (1 << 4) - 1,

            // Current resolution is the maximum resolution
            max_frame_width_minus_1: width - 1,
            max_frame_height_minus_1: height - 1,

            seq_force_integer_mv: SELECT_INTEGER_MV as u32,

            operating_points: {
                let mut ops: [OperatingPoint; MAX_NUM_OPERATING_POINTS] = Default::default();
                ops[0].idc = 0;
                // Use highest level 23 for now.
                // TODO(bgrzesik): approximate level base on resolution and framerate
                ops[0].seq_level_idx = 23;
                ops
            },

            bit_depth: BitDepth::Depth8,
            color_config: ColorConfig {
                // YUV 4:2:0 8-bit
                high_bitdepth: false,
                mono_chrome: false,
                subsampling_x: true,
                subsampling_y: true,
                ..Default::default()
            },

            ..Default::default()
        }
    }

    fn create_temporal_delimiter() -> TemporalDelimiterObu {
        TemporalDelimiterObu {
            obu_header: ObuHeader {
                obu_type: ObuType::TemporalDelimiter,
                extension_flag: false,
                has_size_field: true,
                temporal_id: 0,
                spatial_id: 0,
            },
        }
    }

    fn create_frame_header(&self, frame_type: FrameType) -> FrameHeaderObu {
        let width = self.config.resolution.width;
        let height = self.config.resolution.height;

        // Superblock size
        let sb_size = if self.sequence.use_128x128_superblock {
            128
        } else {
            64
        };

        // Use frame counter for order hinting
        let order_hint_mask = (1 << self.sequence.order_hint_bits) - 1;
        let order_hint = (self.counter & order_hint_mask) as u32;

        // Set the frame size in superblocks for the only tile
        let mut width_in_sbs_minus_1 = [0u32; MAX_TILE_COLS];
        width_in_sbs_minus_1[0] = ((width + sb_size - 1) / sb_size) - 1;

        let mut height_in_sbs_minus_1 = [0u32; MAX_TILE_ROWS];
        height_in_sbs_minus_1[0] = ((height + sb_size - 1) / sb_size) - 1;

        FrameHeaderObu {
            obu_header: ObuHeader {
                obu_type: ObuType::FrameHeader,
                extension_flag: false,
                has_size_field: true,
                temporal_id: 0,
                spatial_id: 0,
            },
            show_frame: true,
            showable_frame: !matches!(frame_type, FrameType::KeyFrame),
            frame_type,
            frame_is_intra: matches!(frame_type, FrameType::KeyFrame | FrameType::IntraOnlyFrame),
            primary_ref_frame: PRIMARY_REF_NONE,
            refresh_frame_flags: if matches!(frame_type, FrameType::KeyFrame) {
                0xff
            } else {
                0x01
            },

            // Use error resilient mode, and provide the order hints for referencing frame ie. just
            // previous frame
            error_resilient_mode: true,
            order_hint,
            ref_order_hint: [0, 0, 0, 0, 0, 0, 0, 0],

            reduced_tx_set: true,
            tx_mode_select: 1,
            tx_mode: TxMode::Select,

            // Provide the Q index from config
            quantization_params: QuantizationParams {
                base_q_idx: self.config.base_qindex as u32,
                ..Default::default()
            },

            // Use single tile for now
            tile_info: TileInfo {
                uniform_tile_spacing_flag: true,
                tile_cols: 1,
                tile_rows: 1,
                tile_cols_log2: 0,
                tile_rows_log2: 0,
                width_in_sbs_minus_1,
                height_in_sbs_minus_1,
                ..Default::default()
            },

            // CDEF is not used currently, use default value to keep Synthesizer happy
            cdef_params: CdefParams {
                cdef_damping: 3,
                ..Default::default()
            },

            // No superres
            superres_denom: SUPERRES_NUM as u32,
            upscaled_width: width,
            frame_width: width,
            frame_height: height,
            render_width: width,
            render_height: height,

            ..Default::default()
        }
    }

    fn request_keyframe(
        &mut self,
        input: P,
        input_meta: FrameMetadata,
    ) -> EncodeResult<Vec<BackendRequest<P, R>>> {
        log::trace!("Requested keyframe timestamp={}", input_meta.timestamp);

        let temporal_delim = Self::create_temporal_delimiter();
        let sequence = self.sequence.clone();
        let frame = self.create_frame_header(FrameType::KeyFrame);

        // This is intra frame, so there is no references
        let references = [None, None, None, None, None, None, None];
        let ref_frame_ctrl_l0 = [ReferenceFrameType::Intra; REFS_PER_FRAME];
        let ref_frame_ctrl_l1 = [ReferenceFrameType::Intra; REFS_PER_FRAME];

        let mut coded_output = Vec::new();

        // Output Temporal Delimiter, Sequence Header and Frame Header OBUs to bitstream
        Synthesizer::<'_, TemporalDelimiterObu, _>::synthesize(&temporal_delim, &mut coded_output)?;
        Synthesizer::<'_, SequenceHeaderObu, _>::synthesize(&sequence, &mut coded_output)?;
        Synthesizer::<'_, FrameHeaderObu, _>::synthesize(&frame, &sequence, &mut coded_output)?;

        let request = BackendRequest {
            sequence,
            frame,
            input,
            input_meta,
            references,
            ref_frame_ctrl_l0,
            ref_frame_ctrl_l1,
            intra_period: self.limit as u32,
            ip_period: 1,
            coded_output,
        };

        self.counter += 1;

        Ok(vec![request])
    }

    fn request_interframe(
        &mut self,
        input: P,
        input_meta: FrameMetadata,
    ) -> EncodeResult<Vec<BackendRequest<P, R>>> {
        log::trace!("Requested interframe timestamp={}", input_meta.timestamp);

        let temporal_delim = Self::create_temporal_delimiter();
        let sequence = self.sequence.clone();
        let mut frame = self.create_frame_header(FrameType::InterFrame);

        // Use previous frame as last frame reference
        let references = [
            self.references.front().cloned(),
            None,
            None,
            None,
            None,
            None,
            None,
        ];

        let order_hint_mask = (1 << self.sequence.order_hint_bits) - 1;
        let mut ref_frame_ctrl_l0 = [ReferenceFrameType::Intra; REFS_PER_FRAME];
        let ref_frame_ctrl_l1 = [ReferenceFrameType::Intra; REFS_PER_FRAME];

        // Enable previous frame as reference
        ref_frame_ctrl_l0[0] = ReferenceFrameType::Last;
        frame.ref_frame_idx[0] = 0;
        frame.last_frame_idx = 0;
        frame.ref_order_hint[0] = ((self.counter - 1) & order_hint_mask) as u32;

        let mut coded_output = Vec::new();

        // Output Temporal Delimiter and Frame Header OBUs to bitstream, marking next frame
        Synthesizer::<'_, TemporalDelimiterObu, _>::synthesize(&temporal_delim, &mut coded_output)?;
        Synthesizer::<'_, FrameHeaderObu, _>::synthesize(&frame, &sequence, &mut coded_output)?;

        let request = BackendRequest {
            sequence,
            frame,
            input,
            input_meta,
            references,
            ref_frame_ctrl_l0,
            ref_frame_ctrl_l1,
            intra_period: self.limit as u32,
            ip_period: 1,
            coded_output,
        };

        self.counter = self.counter.wrapping_add(1) % (self.limit as usize);
        self.references.clear();

        Ok(vec![request])
    }

    fn next_request(&mut self) -> EncodeResult<Vec<BackendRequest<P, R>>> {
        match self.queue.pop_front() {
            // Nothing to do. Quit.
            None => Ok(Vec::new()),
            // If first frame in the sequence or forced IDR then create IDR request.
            Some((input, meta)) if self.counter == 0 || meta.force_keyframe => {
                self.request_keyframe(input, meta)
            }
            // There is no enough frames reconstructed
            Some((input, meta)) if self.references.is_empty() => {
                self.queue.push_front((input, meta));
                Ok(Vec::new())
            }

            Some((input, meta)) => self.request_interframe(input, meta),
        }
    }
}

impl<P, R> Predictor<P, R, BackendRequest<P, R>> for LowDelay<P, R> {
    fn new_frame(
        &mut self,
        input: P,
        frame_metadata: FrameMetadata,
    ) -> EncodeResult<Vec<BackendRequest<P, R>>> {
        // Add new frame in the request queue and request new encoding if possible
        self.queue.push_back((input, frame_metadata));
        self.next_request()
    }

    fn reconstructed(&mut self, recon: R) -> EncodeResult<Vec<BackendRequest<P, R>>> {
        // Add new reconstructed surface and request next encoding if possible
        self.references.push_back(Rc::new(recon));
        self.next_request()
    }

    fn drain(&mut self) -> EncodeResult<Vec<BackendRequest<P, R>>> {
        // [`LowDelay`] will not hold any frames, therefore the drain function shall never be called.
        Err(EncodeError::InvalidInternalState)
    }
}
