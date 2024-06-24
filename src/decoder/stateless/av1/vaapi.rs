// Copyright 2023 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::rc::Rc;

use anyhow::anyhow;
use anyhow::Context;
use libva::Display;
use libva::Picture as VaPicture;
use libva::SurfaceMemoryDescriptor;

use crate::backend::vaapi::decoder::va_surface_id;
use crate::backend::vaapi::decoder::DecodedHandle as VADecodedHandle;
use crate::backend::vaapi::decoder::PoolCreationMode;
use crate::backend::vaapi::decoder::VaStreamInfo;
use crate::backend::vaapi::decoder::VaapiBackend;
use crate::backend::vaapi::decoder::VaapiPicture;
use crate::codec::av1::parser::BitDepth;
use crate::codec::av1::parser::FrameHeaderObu;
use crate::codec::av1::parser::Profile;
use crate::codec::av1::parser::SequenceHeaderObu;
use crate::codec::av1::parser::TileGroupObu;
use crate::codec::av1::parser::WarpModelType;
use crate::codec::av1::parser::MAX_SEGMENTS;
use crate::codec::av1::parser::MAX_TILE_COLS;
use crate::codec::av1::parser::MAX_TILE_ROWS;
use crate::codec::av1::parser::NUM_REF_FRAMES;
use crate::codec::av1::parser::SEG_LVL_MAX;
use crate::codec::av1::parser::TOTAL_REFS_PER_FRAME;
use crate::decoder::stateless::av1::Av1;
use crate::decoder::stateless::av1::StatelessAV1DecoderBackend;
use crate::decoder::stateless::NewStatelessDecoderError;
use crate::decoder::stateless::StatelessBackendError;
use crate::decoder::stateless::StatelessDecoder;
use crate::decoder::stateless::StatelessDecoderBackendPicture;
use crate::decoder::BlockingMode;
use crate::Resolution;

/// The number of surfaces to allocate for this codec.
const NUM_SURFACES: usize = 16;

impl VaStreamInfo for &Rc<SequenceHeaderObu> {
    fn va_profile(&self) -> anyhow::Result<i32> {
        match self.seq_profile {
            Profile::Profile0 => Ok(libva::VAProfile::VAProfileAV1Profile0),
            Profile::Profile1 => Ok(libva::VAProfile::VAProfileAV1Profile1),
            Profile::Profile2 => Err(anyhow!(
                "Profile {:?} is not supported by VAAPI",
                self.seq_profile
            )),
        }
    }

    fn rt_format(&self) -> anyhow::Result<u32> {
        // See table 6.4.1.
        match self.seq_profile {
            Profile::Profile0 => {
                if self.bit_depth == BitDepth::Depth8 {
                    Ok(libva::constants::VA_RT_FORMAT_YUV420)
                } else if self.bit_depth == BitDepth::Depth10 {
                    Ok(libva::constants::VA_RT_FORMAT_YUV420_10)
                } else {
                    Err(anyhow!(
                        "Unsupported bit depth {:?} for profile {:?}",
                        self.bit_depth,
                        self.seq_profile
                    ))
                }
            }
            Profile::Profile1 => {
                if self.bit_depth == BitDepth::Depth8 {
                    Ok(libva::constants::VA_RT_FORMAT_YUV444)
                } else if self.bit_depth == BitDepth::Depth10 {
                    Ok(libva::constants::VA_RT_FORMAT_YUV444_10)
                } else {
                    Err(anyhow!(
                        "Unsupported bit depth {:?} for profile {:?}",
                        self.bit_depth,
                        self.seq_profile
                    ))
                }
            }
            Profile::Profile2 => Err(anyhow!(
                "Profile {:?} is not supported by VAAPI",
                self.seq_profile
            )),
        }
    }

    fn min_num_surfaces(&self) -> usize {
        NUM_SURFACES
    }

    fn coded_size(&self) -> (u32, u32) {
        (
            self.max_frame_width_minus_1 + 1,
            self.max_frame_height_minus_1 + 1,
        )
    }

    fn visible_rect(&self) -> ((u32, u32), (u32, u32)) {
        ((0, 0), self.coded_size())
    }
}

impl From<&FrameHeaderObu> for libva::AV1FilmGrain {
    fn from(hdr: &FrameHeaderObu) -> Self {
        let fg = &hdr.film_grain_params;

        if fg.apply_grain {
            log::warn!("Film grain is not officially supported yet.")
        }

        let film_grain_fields = libva::AV1FilmGrainFields::new(
            u32::from(fg.apply_grain),
            u32::from(fg.chroma_scaling_from_luma),
            u32::from(fg.grain_scaling_minus_8),
            fg.ar_coeff_lag,
            fg.ar_coeff_shift_minus_6 as u32,
            fg.grain_scale_shift as u32,
            u32::from(fg.overlap_flag),
            u32::from(fg.clip_to_restricted_range),
        );

        const NUM_POINT_Y: usize = 14;
        let fg_point_y_value = {
            let mut fg_point_y_value = [0u8; NUM_POINT_Y];
            fg_point_y_value.copy_from_slice(&fg.point_y_value[0..NUM_POINT_Y]);
            fg_point_y_value
        };
        let fg_point_y_scaling = {
            let mut fg_point_y_scaling = [0u8; NUM_POINT_Y];
            fg_point_y_scaling.copy_from_slice(&fg.point_y_scaling[0..NUM_POINT_Y]);
            fg_point_y_scaling
        };

        const NUM_POINT_CB: usize = 10;
        let fg_point_cb_value = {
            let mut fg_point_cb_value = [0u8; NUM_POINT_CB];
            fg_point_cb_value.copy_from_slice(&fg.point_cb_value[0..NUM_POINT_CB]);
            fg_point_cb_value
        };
        let fg_point_cb_scaling = {
            let mut fg_point_cb_scaling = [0u8; NUM_POINT_CB];
            fg_point_cb_scaling.copy_from_slice(&fg.point_cb_scaling[0..NUM_POINT_CB]);
            fg_point_cb_scaling
        };

        const NUM_POINT_CR: usize = 10;
        let fg_point_cr_value = {
            let mut fg_point_cr_value = [0u8; NUM_POINT_CR];
            fg_point_cr_value.copy_from_slice(&fg.point_cr_value[0..NUM_POINT_CR]);
            fg_point_cr_value
        };
        let fg_point_cr_scaling = {
            let mut fg_point_cr_scaling = [0u8; NUM_POINT_CR];
            fg_point_cr_scaling.copy_from_slice(&fg.point_cr_scaling[0..NUM_POINT_CR]);
            fg_point_cr_scaling
        };

        let fg_ar_coeffs_y = {
            let mut fg_ar_coeffs_y = [0i8; 24];
            fg_ar_coeffs_y
                .iter_mut()
                .zip(fg.ar_coeffs_y_plus_128.iter().copied())
                .for_each(|(dest, src)| *dest = ((src as i16) - 128) as i8);
            fg_ar_coeffs_y
        };
        let fg_ar_coeffs_cb = {
            let mut fg_ar_coeffs_cb = [0i8; 25];
            fg_ar_coeffs_cb
                .iter_mut()
                .zip(fg.ar_coeffs_cb_plus_128.iter().copied())
                .for_each(|(dest, src)| *dest = ((src as i16) - 128) as i8);
            fg_ar_coeffs_cb
        };
        let fg_ar_coeffs_cr = {
            let mut fg_ar_coeffs_cr = [0i8; 25];
            fg_ar_coeffs_cr
                .iter_mut()
                .zip(fg.ar_coeffs_cr_plus_128.iter().copied())
                .for_each(|(dest, src)| *dest = ((src as i16) - 128) as i8);
            fg_ar_coeffs_cr
        };

        libva::AV1FilmGrain::new(
            &film_grain_fields,
            fg.grain_seed,
            fg.num_y_points,
            fg_point_y_value,
            fg_point_y_scaling,
            fg.num_cb_points,
            fg_point_cb_value,
            fg_point_cb_scaling,
            fg.num_cr_points,
            fg_point_cr_value,
            fg_point_cr_scaling,
            fg_ar_coeffs_y,
            fg_ar_coeffs_cb,
            fg_ar_coeffs_cr,
            fg.cb_mult,
            fg.cb_luma_mult,
            fg.cb_offset,
            fg.cr_mult,
            fg.cr_luma_mult,
            fg.cr_offset,
        )
    }
}

fn build_wm_info(hdr: &FrameHeaderObu) -> [libva::AV1WarpedMotionParams; 7] {
    let mut wm = vec![];
    let gm = &hdr.global_motion_params;
    for i in 1..=7 {
        let wm_type = match gm.gm_type[i] {
            /* TODO: these were not exported in cros-libva */
            WarpModelType::Identity => 0,
            WarpModelType::Translation => 1,
            WarpModelType::RotZoom => 2,
            WarpModelType::Affine => 3,
        };

        let params = {
            let mut params = [0; 8];
            params[0..6].copy_from_slice(&gm.gm_params[i][0..6]);
            params
        };

        wm.push(libva::AV1WarpedMotionParams::new(
            wm_type,
            params,
            u8::from(!gm.warp_valid[i]),
        ));
    }

    match wm.try_into() {
        Ok(wm) => wm,
        Err(_) => unreachable!("The Vec should have the right size"),
    }
}

fn build_pic_param<M: SurfaceMemoryDescriptor>(
    hdr: &FrameHeaderObu,
    seq: &SequenceHeaderObu,
    current_frame: libva::VASurfaceID,
    reference_frames: &[Option<VADecodedHandle<M>>; NUM_REF_FRAMES],
) -> anyhow::Result<libva::BufferType> {
    let seq_info_fields = libva::AV1SeqFields::new(
        u32::from(seq.still_picture),
        u32::from(seq.use_128x128_superblock),
        u32::from(seq.enable_filter_intra),
        u32::from(seq.enable_intra_edge_filter),
        u32::from(seq.enable_interintra_compound),
        u32::from(seq.enable_masked_compound),
        u32::from(seq.enable_dual_filter),
        u32::from(seq.enable_order_hint),
        u32::from(seq.enable_jnt_comp),
        u32::from(seq.enable_cdef),
        u32::from(seq.color_config.mono_chrome),
        u32::from(seq.color_config.color_range),
        u32::from(seq.color_config.subsampling_x),
        u32::from(seq.color_config.subsampling_y),
        seq.color_config.chroma_sample_position as u32,
        u32::from(seq.film_grain_params_present),
    );

    let seg = &hdr.segmentation_params;
    let seg_info_fields = libva::AV1SegmentInfoFields::new(
        u32::from(seg.segmentation_enabled),
        u32::from(seg.segmentation_update_map),
        u32::from(seg.segmentation_temporal_update),
        u32::from(seg.segmentation_update_data),
    );

    let seg_feature_mask = {
        let mut seg_feature_mask = [0u8; MAX_SEGMENTS];
        #[allow(clippy::needless_range_loop)]
        for i in 0..MAX_SEGMENTS {
            let mut mask = 0;
            for j in 0..SEG_LVL_MAX {
                if seg.feature_enabled[i][j] {
                    mask |= 1 << j;
                }
            }
            seg_feature_mask[i] = mask;
        }
        seg_feature_mask
    };

    let seg_info =
        libva::AV1Segmentation::new(&seg_info_fields, seg.feature_data, seg_feature_mask);

    let pic_info_fields = libva::AV1PicInfoFields::new(
        hdr.frame_type as u32,
        u32::from(hdr.show_frame),
        u32::from(hdr.showable_frame),
        u32::from(hdr.error_resilient_mode),
        u32::from(hdr.disable_cdf_update),
        hdr.allow_screen_content_tools,
        hdr.force_integer_mv,
        u32::from(hdr.allow_intrabc),
        u32::from(hdr.use_superres),
        u32::from(hdr.allow_high_precision_mv),
        u32::from(hdr.is_motion_mode_switchable),
        u32::from(hdr.use_ref_frame_mvs),
        u32::from(hdr.disable_frame_end_update_cdf),
        u32::from(hdr.tile_info.uniform_tile_spacing_flag),
        u32::from(hdr.allow_warped_motion),
        0, /* large_scale_tile */
    );

    let bit_depth_idx = match seq.bit_depth {
        BitDepth::Depth8 => 0,
        BitDepth::Depth10 => 1,
        BitDepth::Depth12 => 2,
    };

    let ref_frame_map: [libva::VASurfaceID; NUM_REF_FRAMES] = reference_frames
        .iter()
        .map(va_surface_id)
        .collect::<Vec<_>>()
        .try_into()
        .unwrap();

    let width_in_sbs_minus_1 = {
        let mut width_in_sbs_minus_1 = [0; MAX_TILE_COLS - 1];
        #[allow(clippy::needless_range_loop)]
        for i in 0..width_in_sbs_minus_1.len() {
            width_in_sbs_minus_1[i] = u16::try_from(hdr.tile_info.width_in_sbs_minus_1[i])
                .context("Invalid width_in_sbs_minus_1")?;
        }
        width_in_sbs_minus_1
    };

    let height_in_sbs_minus_1 = {
        let mut height_in_sbs_minus_1 = [0; MAX_TILE_ROWS - 1];
        #[allow(clippy::needless_range_loop)]
        for i in 0..height_in_sbs_minus_1.len() {
            height_in_sbs_minus_1[i] = u16::try_from(hdr.tile_info.height_in_sbs_minus_1[i])
                .context("Invalid height_in_sbs_minus_1")?;
        }
        height_in_sbs_minus_1
    };

    let lf = &hdr.loop_filter_params;
    let filter_level = {
        let mut filter_level = [0u8; 2];
        filter_level[0] =
            u8::try_from(lf.loop_filter_level[0]).context("Invalid loop_filter_level")?;
        filter_level[1] =
            u8::try_from(lf.loop_filter_level[1]).context("Invalid loop_filter_level")?;
        filter_level
    };

    let lf_fields = libva::AV1LoopFilterFields::new(
        u8::try_from(lf.loop_filter_sharpness).context("Invalid loop_filter_sharpness")?,
        u8::from(lf.loop_filter_delta_enabled),
        u8::from(lf.loop_filter_delta_update),
    );

    let (lf_ref_deltas, lf_mode_deltas) = {
        let mut ref_deltas = [0i8; TOTAL_REFS_PER_FRAME];
        #[allow(clippy::needless_range_loop)]
        for i in 0..ref_deltas.len() {
            ref_deltas[i] = i8::try_from(lf.loop_filter_ref_deltas[i])
                .context("Invalid loop_filter_ref_deltas")?;
        }
        let mut mode_deltas = [0i8; 2];
        #[allow(clippy::needless_range_loop)]
        for i in 0..mode_deltas.len() {
            mode_deltas[i] = i8::try_from(lf.loop_filter_mode_deltas[i])
                .context("Invalid loop_filter_mode_deltas")?;
        }

        (ref_deltas, mode_deltas)
    };

    let quant = &hdr.quantization_params;
    let qmatrix_fields = libva::AV1QMatrixFields::new(
        u16::from(quant.using_qmatrix),
        u16::try_from(quant.qm_y).context("Invalid qm_y")?,
        u16::try_from(quant.qm_u).context("Invalid qm_u")?,
        u16::try_from(quant.qm_v).context("Invalid qm_v")?,
    );

    let mode_control_fields = libva::AV1ModeControlFields::new(
        u32::from(quant.delta_q_present),
        quant.delta_q_res,
        u32::from(lf.delta_lf_present),
        lf.delta_lf_res,
        lf.delta_lf_multi,
        hdr.tx_mode as u32,
        u32::from(hdr.reference_select),
        u32::from(hdr.reduced_tx_set),
        u32::from(hdr.skip_mode_present),
    );

    let cdef = &hdr.cdef_params;
    let (cdef_y_strengths, cdef_uv_strengths) = {
        let num_cdef_strenghts = 1 << cdef.cdef_bits;
        let mut cdef_y_strengths = [0; 8];
        let mut cdef_uv_strengths = [0; 8];

        #[allow(clippy::needless_range_loop)]
        for i in 0..num_cdef_strenghts {
            let mut sec_strength = cdef.cdef_y_sec_strength[i];
            if sec_strength == 4 {
                sec_strength -= 1;
            }
            cdef_y_strengths[i] =
                u8::try_from(((cdef.cdef_y_pri_strength[i] & 0xf) << 2) | (sec_strength & 0x3))
                    .context("Failed to merge primary and secondary strengths")?;
        }

        #[allow(clippy::needless_range_loop)]
        for i in 0..num_cdef_strenghts {
            let mut sec_strength = cdef.cdef_uv_sec_strength[i];
            if sec_strength == 4 {
                sec_strength -= 1;
            }
            cdef_uv_strengths[i] =
                u8::try_from(((cdef.cdef_uv_pri_strength[i] & 0xf) << 2) | (sec_strength & 0x3))
                    .context("Failed to merge primary and secondary strengths")?;
        }

        (cdef_y_strengths, cdef_uv_strengths)
    };

    let lr = &hdr.loop_restoration_params;
    let loop_restoration_fields = libva::AV1LoopRestorationFields::new(
        lr.frame_restoration_type[0] as u16,
        lr.frame_restoration_type[1] as u16,
        lr.frame_restoration_type[2] as u16,
        u16::from(lr.lr_unit_shift),
        u16::from(lr.lr_uv_shift),
    );

    let wm = build_wm_info(hdr);

    let pic_param = libva::PictureParameterBufferAV1::new(
        u8::try_from(seq.seq_profile as u32).context("Invalid profile")?,
        u8::try_from(seq.order_hint_bits_minus_1).context("Invalid order hint bits")?,
        bit_depth_idx,
        u8::try_from(seq.color_config.matrix_coefficients as u32)
            .context("Invalid matrix_coefficients")?,
        &seq_info_fields,
        current_frame,
        libva::constants::VA_INVALID_SURFACE, /* film grain is unsupported for now */
        vec![],                               /* anchor_frames_list */
        u16::try_from(hdr.upscaled_width - 1).context("Invalid frame width")?,
        u16::try_from(hdr.frame_height - 1).context("Invalid frame height")?,
        0, /* output_frame_width_in_tiles_minus_1 */
        0, /* output_frame_height_in_tiles_minus_1 */
        ref_frame_map,
        hdr.ref_frame_idx,
        u8::try_from(hdr.primary_ref_frame).context("Invalid primary_ref_frame")?,
        u8::try_from(hdr.order_hint).context("Invalid order_hint")?,
        &seg_info,
        &libva::AV1FilmGrain::from(hdr),
        u8::try_from(hdr.tile_info.tile_cols).context("Invalid tile_cols")?,
        u8::try_from(hdr.tile_info.tile_rows).context("Invalid tile_rows")?,
        width_in_sbs_minus_1,
        height_in_sbs_minus_1,
        0, /* large-scale tile not supported */
        u16::try_from(hdr.tile_info.context_update_tile_id)
            .context("Invalid context_update_tile_id")?,
        &pic_info_fields,
        u8::try_from(hdr.superres_denom).context("Invalid superres_denom")?,
        u8::try_from(hdr.interpolation_filter as u32).context("Invalid interpolation_filter")?,
        filter_level,
        u8::try_from(lf.loop_filter_level[2]).context("Invalid loop_filter_level")?,
        u8::try_from(lf.loop_filter_level[3]).context("Invalid loop_filter_level")?,
        &lf_fields,
        lf_ref_deltas,
        lf_mode_deltas,
        u8::try_from(quant.base_q_idx).context("Invalid base_q_idx")?,
        i8::try_from(quant.delta_q_y_dc).context("Invalid delta_q_y_dc")?,
        i8::try_from(quant.delta_q_u_dc).context("Invalid delta_q_u_dc")?,
        i8::try_from(quant.delta_q_u_ac).context("Invalid delta_q_u_ac")?,
        i8::try_from(quant.delta_q_v_dc).context("Invalid delta_q_v_dc")?,
        i8::try_from(quant.delta_q_v_ac).context("Invalid delta_q_v_ac")?,
        &qmatrix_fields,
        &mode_control_fields,
        u8::try_from(hdr.cdef_params.cdef_damping - 3).context("Invalid cdef_damping")?,
        u8::try_from(hdr.cdef_params.cdef_bits).context("Invalid cdef_bits")?,
        cdef_y_strengths,
        cdef_uv_strengths,
        &loop_restoration_fields,
        &wm,
    );

    Ok(libva::BufferType::PictureParameter(
        libva::PictureParameter::AV1(pic_param),
    ))
}

fn build_slice_params_for_tg(tg: &TileGroupObu) -> anyhow::Result<libva::BufferType> {
    let mut slice_params = libva::SliceParameterBufferAV1::new();

    for tile in &tg.tiles {
        /* all tiles must be submitted in the same slice parameter array */
        slice_params.add_slice_parameter(
            tile.tile_size,
            tile.tile_offset,
            0,
            u16::try_from(tile.tile_row).context("Invalid tile_row")?,
            u16::try_from(tile.tile_col).context("Invalid tile_col")?,
            u16::try_from(tg.tg_start).context("Invalid tg_start")?,
            u16::try_from(tg.tg_end).context("Invalid tg_end")?,
            0,
            0,
        );
    }

    Ok(libva::BufferType::SliceParameter(
        libva::SliceParameter::AV1(slice_params),
    ))
}

fn build_slice_data_for_tg(tg: TileGroupObu) -> libva::BufferType {
    let TileGroupObu { obu, .. } = tg;
    libva::BufferType::SliceData(Vec::from(obu.as_ref()))
}

impl<M: SurfaceMemoryDescriptor + 'static> StatelessDecoderBackendPicture<Av1> for VaapiBackend<M> {
    type Picture = VaapiPicture<M>;
}

impl<M: SurfaceMemoryDescriptor + 'static> StatelessAV1DecoderBackend for VaapiBackend<M> {
    fn new_sequence(
        &mut self,
        sequence: &Rc<SequenceHeaderObu>,
        highest_spatial_layer: Option<u32>,
    ) -> crate::decoder::stateless::StatelessBackendResult<()> {
        let pool_creation_mode = match highest_spatial_layer {
            Some(highest_layer) => {
                /* The spec mandates a 2:1 or 1.5:1 ratio, let's go with 2:1 to
                 * accomodate the other case. See 6.7.5 in the spec */
                let layers = (0..=highest_layer).map(|layer| Resolution {
                    width: (sequence.max_frame_width_minus_1 + 1) / (layer + 1),
                    height: (sequence.max_frame_height_minus_1 + 1) / (layer + 1),
                });

                PoolCreationMode::Layers(layers.collect())
            }
            None => PoolCreationMode::Highest,
        };
        self.new_sequence(sequence, pool_creation_mode)
    }

    fn new_picture(
        &mut self,
        sequence: &SequenceHeaderObu,
        hdr: &FrameHeaderObu,
        timestamp: u64,
        reference_frames: &[Option<Self::Handle>; NUM_REF_FRAMES],
        highest_spatial_layer: Option<u32>,
    ) -> crate::decoder::stateless::StatelessBackendResult<Self::Picture> {
        let surface = match highest_spatial_layer {
            Some(_) => {
                let layer = Resolution {
                    width: hdr.upscaled_width,
                    height: hdr.frame_height,
                };

                let pool = self
                    .pool(layer)
                    .ok_or(StatelessBackendError::Other(anyhow!(
                        "No pool available for this layer"
                    )))?;

                pool.get_surface()
                    .ok_or(StatelessBackendError::OutOfResources)?
            }
            None => {
                let highest_pool = self.highest_pool();
                highest_pool
                    .get_surface()
                    .ok_or(StatelessBackendError::OutOfResources)?
            }
        };

        let metadata = self.metadata_state.get_parsed()?;
        let mut picture = VaPicture::new(timestamp, Rc::clone(&metadata.context), surface);

        let surface_id = picture.surface().id();

        let pic_param = build_pic_param(hdr, sequence, surface_id, reference_frames)
            .context("Failed to build picture parameter")?;
        let pic_param = metadata
            .context
            .create_buffer(pic_param)
            .context("Failed to create picture parameter buffer")?;
        picture.add_buffer(pic_param);

        Ok(picture)
    }

    fn decode_tile_group(
        &mut self,
        picture: &mut Self::Picture,
        tile_group: TileGroupObu,
    ) -> crate::decoder::stateless::StatelessBackendResult<()> {
        let slice_params = build_slice_params_for_tg(&tile_group)?;
        let slice_data = build_slice_data_for_tg(tile_group);

        let metadata = self.metadata_state.get_parsed()?;
        let context = &metadata.context;

        let buffer = context
            .create_buffer(slice_params)
            .context("Failed to create slice parameter buffer")?;

        picture.add_buffer(buffer);

        let buffer = context
            .create_buffer(slice_data)
            .context("Failed to create slice data buffer")?;

        picture.add_buffer(buffer);

        Ok(())
    }

    fn submit_picture(
        &mut self,
        picture: Self::Picture,
    ) -> crate::decoder::stateless::StatelessBackendResult<Self::Handle> {
        self.process_picture::<Av1>(picture)
    }
}

impl<M: SurfaceMemoryDescriptor + 'static> StatelessDecoder<Av1, VaapiBackend<M>> {
    // Creates a new instance of the decoder using the VAAPI backend.
    pub fn new_vaapi<S>(
        display: Rc<Display>,
        blocking_mode: BlockingMode,
    ) -> Result<Self, NewStatelessDecoderError>
    where
        M: From<S>,
        S: From<M>,
    {
        Self::new(VaapiBackend::new(display, true), blocking_mode)
    }
}

#[cfg(test)]
mod tests {
    use libva::Display;

    use crate::decoder::stateless::av1::Av1;
    use crate::decoder::stateless::tests::test_decode_stream;
    use crate::decoder::stateless::tests::TestStream;
    use crate::decoder::stateless::StatelessDecoder;
    use crate::decoder::BlockingMode;
    use crate::utils::simple_playback_loop;
    use crate::utils::simple_playback_loop_owned_frames;
    use crate::utils::IvfIterator;
    use crate::DecodedFormat;

    /// Run `test` using the vaapi decoder, in both blocking and non-blocking modes.
    fn test_decoder_vaapi(
        test: &TestStream,
        output_format: DecodedFormat,
        blocking_mode: BlockingMode,
    ) {
        let display = Display::open().unwrap();
        let decoder = StatelessDecoder::<Av1, _>::new_vaapi::<()>(display, blocking_mode).unwrap();

        test_decode_stream(
            |d, s, f| {
                simple_playback_loop(
                    d,
                    IvfIterator::new(s),
                    f,
                    &mut simple_playback_loop_owned_frames,
                    output_format,
                    blocking_mode,
                )
            },
            decoder,
            test,
            true,
            false,
        );
    }

    #[test]
    // Ignore this test by default as it requires libva-compatible hardware.
    #[ignore]
    fn test_25fps_av1_blocking() {
        use crate::decoder::stateless::av1::tests::DECODE_TEST_25FPS;
        test_decoder_vaapi(
            &DECODE_TEST_25FPS,
            DecodedFormat::NV12,
            BlockingMode::Blocking,
        );
    }

    #[test]
    // Ignore this test by default as it requires libva-compatible hardware.
    #[ignore]
    fn test_25fps_av1_non_blocking() {
        use crate::decoder::stateless::av1::tests::DECODE_TEST_25FPS;
        test_decoder_vaapi(
            &DECODE_TEST_25FPS,
            DecodedFormat::NV12,
            BlockingMode::NonBlocking,
        );
    }
}
