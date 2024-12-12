// Copyright 2023 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::rc::Rc;

use anyhow::anyhow;
use anyhow::Context;
use libva::BufferType;
use libva::Display;
use libva::HevcSliceExtFlags;
use libva::IQMatrix;
use libva::IQMatrixBufferHEVC;
use libva::Picture as VaPicture;
use libva::PictureHEVC;
use libva::PictureParameterBufferHEVC;
use libva::SliceParameter;
use libva::SliceParameterBufferHEVC;
use libva::SliceParameterBufferHEVCRext;
use libva::SurfaceMemoryDescriptor;

use crate::backend::vaapi::decoder::DecodedHandle as VADecodedHandle;
use crate::backend::vaapi::decoder::PoolCreationMode;
use crate::backend::vaapi::decoder::VaStreamInfo;
use crate::backend::vaapi::decoder::VaapiBackend;
use crate::backend::vaapi::decoder::VaapiPicture;
use crate::codec::h265::dpb::Dpb;
use crate::codec::h265::parser::NaluType;
use crate::codec::h265::parser::Pps;
use crate::codec::h265::parser::Profile;
use crate::codec::h265::parser::Slice;
use crate::codec::h265::parser::Sps;
use crate::codec::h265::picture::PictureData;
use crate::codec::h265::picture::Reference;
use crate::decoder::stateless::h265::clip3;
use crate::decoder::stateless::h265::RefPicListEntry;
use crate::decoder::stateless::h265::RefPicSet;
use crate::decoder::stateless::h265::StatelessH265DecoderBackend;
use crate::decoder::stateless::h265::H265;
use crate::decoder::stateless::NewPictureError;
use crate::decoder::stateless::NewPictureResult;
use crate::decoder::stateless::NewStatelessDecoderError;
use crate::decoder::stateless::PoolLayer;
use crate::decoder::stateless::StatelessBackendResult;
use crate::decoder::stateless::StatelessDecoder;
use crate::decoder::stateless::StatelessDecoderBackend;
use crate::decoder::stateless::StatelessDecoderBackendPicture;
use crate::decoder::BlockingMode;
use crate::Rect;
use crate::Resolution;

enum ScalingListType {
    Sps,
    Pps,
    None,
}

impl VaStreamInfo for &Sps {
    fn va_profile(&self) -> anyhow::Result<i32> {
        let profile_idc = self.profile_tier_level.general_profile_idc;
        let profile = Profile::try_from(profile_idc).map_err(|err| anyhow!(err))?;

        let bit_depth = std::cmp::max(
            self.bit_depth_luma_minus8 + 8,
            self.bit_depth_chroma_minus8 + 8,
        );

        let chroma_format_idc = self.chroma_format_idc;
        let err = Err(anyhow!(
            "Invalid combination of profile, bit depth an chroma_format_idc: ({:?}, {}, {}",
            profile,
            bit_depth,
            chroma_format_idc
        ));

        // TODO: This can still be much improved in light of table A.2.
        match profile {
            Profile::Main | Profile::MainStill | Profile::Main10 => {
                match (bit_depth, chroma_format_idc) {
                    (8, 0) | (8, 1) => Ok(libva::VAProfile::VAProfileHEVCMain),
                    (8, 3) => Ok(libva::VAProfile::VAProfileHEVCMain444),
                    (10, 0) | (10, 1) => Ok(libva::VAProfile::VAProfileHEVCMain10),
                    (10, 2) => Ok(libva::VAProfile::VAProfileHEVCMain422_10),
                    (12, 1) => Ok(libva::VAProfile::VAProfileHEVCMain12),
                    (12, 2) => Ok(libva::VAProfile::VAProfileHEVCMain422_12),
                    (12, 3) => Ok(libva::VAProfile::VAProfileHEVCMain444_12),
                    _ => err,
                }
            }

            // See table A.4.
            Profile::ScalableMain => match (bit_depth, chroma_format_idc) {
                (8, 1) => Ok(libva::VAProfile::VAProfileHEVCSccMain),
                (8, 3) => Ok(libva::VAProfile::VAProfileHEVCSccMain444),
                (10, 1) => Ok(libva::VAProfile::VAProfileHEVCSccMain10),
                (10, 3) => Ok(libva::VAProfile::VAProfileHEVCSccMain444_10),
                _ => err,
            },

            _ => unimplemented!("Adding more profile support based on A.3. is still TODO"),
        }
    }

    fn rt_format(&self) -> anyhow::Result<u32> {
        let bit_depth = std::cmp::max(
            self.bit_depth_luma_minus8 + 8,
            self.bit_depth_chroma_minus8 + 8,
        );

        let chroma_format_idc = self.chroma_format_idc;

        match (bit_depth, chroma_format_idc) {
            (8, 0) | (8, 1) => Ok(libva::VA_RT_FORMAT_YUV420),
            (8, 2) => Ok(libva::VA_RT_FORMAT_YUV422),
            (8, 3) => Ok(libva::VA_RT_FORMAT_YUV444),
            (9, 0) | (9, 1) | (10, 0) | (10, 1) => Ok(libva::VA_RT_FORMAT_YUV420_10),
            (9, 2) | (10, 2) => Ok(libva::VA_RT_FORMAT_YUV422_10),
            (9, 3) | (10, 3) => Ok(libva::VA_RT_FORMAT_YUV444_10),
            (11, 0) | (11, 1) | (12, 0) | (12, 1) => Ok(libva::VA_RT_FORMAT_YUV420_12),
            (11, 2) | (12, 2) => Ok(libva::VA_RT_FORMAT_YUV422_12),
            (11, 3) | (12, 3) => Ok(libva::VA_RT_FORMAT_YUV444_12),
            _ => Err(anyhow!(
                "unsupported bit depth/chroma format pair {}, {}",
                bit_depth,
                chroma_format_idc
            )),
        }
    }

    fn min_num_surfaces(&self) -> usize {
        self.max_dpb_size() + 4
    }

    fn coded_size(&self) -> Resolution {
        Resolution::from((self.width().into(), self.height().into()))
    }

    fn visible_rect(&self) -> Rect {
        let rect = self.visible_rectangle();

        Rect {
            x: rect.min.x,
            y: rect.min.y,
            width: rect.max.x,
            height: rect.max.y,
        }
    }
}

fn build_slice_ref_pic_list<M: SurfaceMemoryDescriptor>(
    ref_pic_list: &[Option<RefPicListEntry<VADecodedHandle<M>>>; 16],
    va_references: &[PictureHEVC; 15],
) -> [u8; 15] {
    let mut va_refs = [0xff; 15];

    for (ref_pic_list_idx, ref_pic_list_entry) in ref_pic_list.iter().enumerate() {
        if ref_pic_list_idx == 15 {
            break;
        }

        if let Some(ref_pic_list_entry) = ref_pic_list_entry {
            for (va_ref_idx, va_ref) in va_references.iter().enumerate() {
                if va_ref.picture_id() == libva::VA_INVALID_ID {
                    break;
                }

                let pic_order_cnt = match ref_pic_list_entry {
                    RefPicListEntry::CurrentPicture(p) => p.pic_order_cnt_val,
                    RefPicListEntry::DpbEntry(p) => p.0.borrow().pic_order_cnt_val,
                };

                if va_ref.pic_order_cnt() == pic_order_cnt {
                    va_refs[ref_pic_list_idx] = va_ref_idx as u8;
                }
            }
        }
    }

    va_refs
}

fn va_rps_flag<M: SurfaceMemoryDescriptor>(
    hevc_pic: &PictureData,
    rps: &RefPicSet<VADecodedHandle<M>>,
) -> u32 {
    if rps
        .ref_pic_set_st_curr_before
        .iter()
        .flatten()
        .any(|dpb_entry| *dpb_entry.0.borrow() == *hevc_pic)
    {
        libva::VA_PICTURE_HEVC_RPS_ST_CURR_BEFORE
    } else if rps
        .ref_pic_set_st_curr_after
        .iter()
        .flatten()
        .any(|dpb_entry| *dpb_entry.0.borrow() == *hevc_pic)
    {
        libva::VA_PICTURE_HEVC_RPS_ST_CURR_AFTER
    } else if rps
        .ref_pic_set_lt_curr
        .iter()
        .flatten()
        .any(|dpb_entry| *dpb_entry.0.borrow() == *hevc_pic)
    {
        libva::VA_PICTURE_HEVC_RPS_LT_CURR
    } else {
        0
    }
}

/// Builds an invalid VaPictureHEVC. These pictures are used to fill empty
/// array slots there is no data to fill them with.
fn build_invalid_va_hevc_pic() -> libva::PictureHEVC {
    libva::PictureHEVC::new(libva::VA_INVALID_ID, 0, libva::VA_PICTURE_HEVC_INVALID)
}

fn fill_va_hevc_pic<M: SurfaceMemoryDescriptor>(
    hevc_pic: &PictureData,
    surface_id: libva::VASurfaceID,
    rps: &RefPicSet<VADecodedHandle<M>>,
) -> libva::PictureHEVC {
    let mut flags = 0;

    if matches!(hevc_pic.reference(), Reference::LongTerm) {
        flags |= libva::VA_PICTURE_HEVC_LONG_TERM_REFERENCE;
    }

    flags |= va_rps_flag(hevc_pic, rps);

    libva::PictureHEVC::new(surface_id, hevc_pic.pic_order_cnt_val, flags)
}

fn is_range_extension_profile(va_profile: libva::VAProfile::Type) -> bool {
    matches!(
        va_profile,
        libva::VAProfile::VAProfileHEVCMain422_10
            | libva::VAProfile::VAProfileHEVCMain444
            | libva::VAProfile::VAProfileHEVCMain444_10
            | libva::VAProfile::VAProfileHEVCMain12
            | libva::VAProfile::VAProfileHEVCMain422_12
            | libva::VAProfile::VAProfileHEVCMain444_12
    )
}

fn is_scc_ext_profile(va_profile: libva::VAProfile::Type) -> bool {
    matches!(
        va_profile,
        libva::VAProfile::VAProfileHEVCSccMain
            | libva::VAProfile::VAProfileHEVCSccMain10
            | libva::VAProfile::VAProfileHEVCSccMain444
            | libva::VAProfile::VAProfileHEVCMain444_10,
    )
}

fn build_picture_rext(sps: &Sps, pps: &Pps) -> anyhow::Result<BufferType> {
    let sps_rext = &sps.range_extension;
    let pps_rext = &pps.range_extension;

    let range_extension_pic_fields = libva::HevcRangeExtensionPicFields::new(
        sps_rext.transform_skip_rotation_enabled_flag as u32,
        sps_rext.transform_skip_context_enabled_flag as u32,
        sps_rext.implicit_rdpcm_enabled_flag as u32,
        sps_rext.explicit_rdpcm_enabled_flag as u32,
        sps_rext.extended_precision_processing_flag as u32,
        sps_rext.intra_smoothing_disabled_flag as u32,
        sps_rext.high_precision_offsets_enabled_flag as u32,
        sps_rext.persistent_rice_adaptation_enabled_flag as u32,
        sps_rext.cabac_bypass_alignment_enabled_flag as u32,
        pps_rext.cross_component_prediction_enabled_flag as u32,
        pps_rext.chroma_qp_offset_list_enabled_flag as u32,
    );

    let rext = libva::PictureParameterBufferHEVCRext::new(
        &range_extension_pic_fields,
        pps_rext.diff_cu_chroma_qp_offset_depth as u8,
        pps_rext.chroma_qp_offset_list_len_minus1 as u8,
        pps_rext.log2_sao_offset_scale_luma as u8,
        pps_rext.log2_sao_offset_scale_chroma as u8,
        pps_rext.log2_max_transform_skip_block_size_minus2 as u8,
        pps_rext.cb_qp_offset_list.map(|x| x as i8),
        pps_rext.cr_qp_offset_list.map(|x| x as i8),
    );

    Ok(BufferType::PictureParameter(
        libva::PictureParameter::HEVCRext(rext),
    ))
}

fn build_picture_scc(sps: &Sps, pps: &Pps) -> anyhow::Result<BufferType> {
    let sps_scc = &sps.scc_extension;
    let pps_scc = &pps.scc_extension;

    let scc_pic_fields = libva::HevcScreenContentPicFields::new(
        pps_scc.curr_pic_ref_enabled_flag as u32,
        sps_scc.palette_mode_enabled_flag as u32,
        sps_scc.motion_vector_resolution_control_idc as u32,
        sps_scc.intra_boundary_filtering_disabled_flag as u32,
        pps_scc.residual_adaptive_colour_transform_enabled_flag as u32,
        pps_scc.slice_act_qp_offsets_present_flag as u32,
    );

    let (predictor_palette_entries, predictor_palette_size) =
        if pps_scc.palette_predictor_initializers_present_flag {
            (
                pps_scc
                    .palette_predictor_initializer
                    .map(|outer| outer.map(u16::from)),
                pps_scc.num_palette_predictor_initializers,
            )
        } else if sps_scc.palette_predictor_initializers_present_flag {
            (
                sps_scc
                    .palette_predictor_initializer
                    .map(|outer| outer.map(|inner| inner as u16)),
                sps_scc.num_palette_predictor_initializer_minus1 + 1,
            )
        } else {
            ([[0; 128]; 3], 0)
        };

    let scc = libva::PictureParameterBufferHEVCScc::new(
        &scc_pic_fields,
        sps_scc.palette_max_size,
        sps_scc.delta_palette_max_predictor_size,
        predictor_palette_size,
        predictor_palette_entries,
        pps_scc.act_y_qp_offset_plus5,
        pps_scc.act_cb_qp_offset_plus5,
        pps_scc.act_cr_qp_offset_plus3,
    );

    Ok(BufferType::PictureParameter(
        libva::PictureParameter::HEVCScc(scc),
    ))
}

fn build_pic_param<M: SurfaceMemoryDescriptor>(
    _: &Slice,
    current_picture: &PictureData,
    current_surface_id: libva::VASurfaceID,
    dpb: &Dpb<VADecodedHandle<M>>,
    rps: &RefPicSet<VADecodedHandle<M>>,
    sps: &Sps,
    pps: &Pps,
) -> anyhow::Result<(BufferType, [PictureHEVC; 15])> {
    let curr_pic = fill_va_hevc_pic(current_picture, current_surface_id, rps);

    let mut reference_frames = vec![];

    for ref_pic in dpb.get_all_references() {
        let surface_id = ref_pic.1.borrow().surface().id();
        let ref_pic = fill_va_hevc_pic(&ref_pic.0.borrow(), surface_id, rps);
        reference_frames.push(ref_pic);
    }

    // RefPicListL0 and RefPicListL1 may signal that they want to refer to
    // the current picture. We must tell VA that it is a reference as it is
    // not in the DPB at this point.
    if pps.scc_extension.curr_pic_ref_enabled_flag {
        if reference_frames.len() >= 15 {
            log::warn!(
                "Bug: Trying to set the current picture as a VA reference, but the VA DPB is full."
            )
        } else {
            reference_frames.push(curr_pic);
        }
    }

    for _ in reference_frames.len()..15 {
        reference_frames.push(build_invalid_va_hevc_pic());
    }

    let reference_frames = reference_frames.try_into();
    let reference_frames = match reference_frames {
        Ok(va_refs) => va_refs,
        Err(_) => {
            // Can't panic, we guarantee len() == 15.
            panic!("Bug: wrong number of references, expected 15");
        }
    };

    let pic_fields = libva::HevcPicFields::new(
        sps.chroma_format_idc as u32,
        sps.separate_colour_plane_flag as u32,
        sps.pcm_enabled_flag as u32,
        sps.scaling_list_enabled_flag as u32,
        pps.transform_skip_enabled_flag as u32,
        sps.amp_enabled_flag as u32,
        sps.strong_intra_smoothing_enabled_flag as u32,
        pps.sign_data_hiding_enabled_flag as u32,
        pps.constrained_intra_pred_flag as u32,
        pps.cu_qp_delta_enabled_flag as u32,
        pps.weighted_pred_flag as u32,
        pps.weighted_bipred_flag as u32,
        pps.transquant_bypass_enabled_flag as u32,
        pps.tiles_enabled_flag as u32,
        pps.entropy_coding_sync_enabled_flag as u32,
        pps.loop_filter_across_slices_enabled_flag as u32,
        pps.loop_filter_across_tiles_enabled_flag as u32,
        sps.pcm_loop_filter_disabled_flag as u32,
        /* lets follow the FFMPEG and GStreamer train and set these to false */
        0,
        0,
    );

    let rap_pic_flag = current_picture.nalu_type as u32 >= NaluType::BlaWLp as u32
        && current_picture.nalu_type as u32 <= NaluType::CraNut as u32;

    let slice_parsing_fields = libva::HevcSliceParsingFields::new(
        pps.lists_modification_present_flag as u32,
        sps.long_term_ref_pics_present_flag as u32,
        sps.temporal_mvp_enabled_flag as u32,
        pps.cabac_init_present_flag as u32,
        pps.output_flag_present_flag as u32,
        pps.dependent_slice_segments_enabled_flag as u32,
        pps.slice_chroma_qp_offsets_present_flag as u32,
        sps.sample_adaptive_offset_enabled_flag as u32,
        pps.deblocking_filter_override_enabled_flag as u32,
        pps.deblocking_filter_disabled_flag as u32,
        pps.slice_segment_header_extension_present_flag as u32,
        rap_pic_flag as u32,
        current_picture.nalu_type.is_idr() as u32,
        current_picture.nalu_type.is_irap() as u32,
    );

    let pic_param = PictureParameterBufferHEVC::new(
        curr_pic,
        reference_frames,
        sps.pic_width_in_luma_samples,
        sps.pic_height_in_luma_samples,
        &pic_fields,
        sps.max_dec_pic_buffering_minus1[usize::from(sps.max_sub_layers_minus1)],
        sps.bit_depth_luma_minus8,
        sps.bit_depth_chroma_minus8,
        sps.pcm_sample_bit_depth_luma_minus1,
        sps.pcm_sample_bit_depth_chroma_minus1,
        sps.log2_min_luma_coding_block_size_minus3,
        sps.log2_diff_max_min_luma_coding_block_size,
        sps.log2_min_luma_transform_block_size_minus2,
        sps.log2_diff_max_min_luma_transform_block_size,
        sps.log2_min_pcm_luma_coding_block_size_minus3,
        sps.log2_diff_max_min_pcm_luma_coding_block_size,
        sps.max_transform_hierarchy_depth_intra,
        sps.max_transform_hierarchy_depth_inter,
        pps.init_qp_minus26,
        pps.diff_cu_qp_delta_depth,
        pps.cb_qp_offset,
        pps.cr_qp_offset,
        pps.log2_parallel_merge_level_minus2,
        pps.num_tile_columns_minus1,
        pps.num_tile_rows_minus1,
        pps.column_width_minus1.map(|x| x as u16),
        pps.row_height_minus1.map(|x| x as u16),
        &slice_parsing_fields,
        sps.log2_max_pic_order_cnt_lsb_minus4,
        sps.num_short_term_ref_pic_sets,
        sps.num_long_term_ref_pics_sps,
        pps.num_ref_idx_l0_default_active_minus1,
        pps.num_ref_idx_l1_default_active_minus1,
        pps.beta_offset_div2,
        pps.tc_offset_div2,
        pps.num_extra_slice_header_bits,
        current_picture.short_term_ref_pic_set_size_bits,
    );

    Ok((
        BufferType::PictureParameter(libva::PictureParameter::HEVC(pic_param)),
        reference_frames,
    ))
}

fn find_scaling_list(sps: &Sps, pps: &Pps) -> ScalingListType {
    if pps.scaling_list_data_present_flag
        || (sps.scaling_list_enabled_flag && !sps.scaling_list_data_present_flag)
    {
        ScalingListType::Pps
    } else if sps.scaling_list_enabled_flag && sps.scaling_list_data_present_flag {
        ScalingListType::Sps
    } else {
        ScalingListType::None
    }
}

fn build_iq_matrix(sps: &Sps, pps: &Pps) -> BufferType {
    let scaling_lists = match find_scaling_list(sps, pps) {
        ScalingListType::Sps => &sps.scaling_list,
        ScalingListType::Pps => &pps.scaling_list,
        ScalingListType::None => panic!("No scaling list data available"),
    };

    let mut scaling_list_32x32 = [[0; 64]; 2];

    for i in (0..6).step_by(3) {
        for j in 0..64 {
            scaling_list_32x32[i / 3][j] = scaling_lists.scaling_list_32x32[i][j];
        }
    }

    let mut scaling_list_dc_32x32 = [0; 2];
    for i in (0..6).step_by(3) {
        scaling_list_dc_32x32[i / 3] =
            (scaling_lists.scaling_list_dc_coef_minus8_32x32[i] + 8) as u8;
    }

    let mut scaling_list_4x4 = [[0; 16]; 6];
    let mut scaling_list_8x8 = [[0; 64]; 6];
    let mut scaling_list_16x16 = [[0; 64]; 6];
    let mut scaling_list_32x32_r = [[0; 64]; 2];

    (0..6).for_each(|i| {
        super::get_raster_from_up_right_diagonal_4x4(
            scaling_lists.scaling_list_4x4[i],
            &mut scaling_list_4x4[i],
        );

        super::get_raster_from_up_right_diagonal_8x8(
            scaling_lists.scaling_list_8x8[i],
            &mut scaling_list_8x8[i],
        );

        super::get_raster_from_up_right_diagonal_8x8(
            scaling_lists.scaling_list_16x16[i],
            &mut scaling_list_16x16[i],
        );
    });

    (0..2).for_each(|i| {
        super::get_raster_from_up_right_diagonal_8x8(
            scaling_list_32x32[i],
            &mut scaling_list_32x32_r[i],
        );
    });

    BufferType::IQMatrix(IQMatrix::HEVC(IQMatrixBufferHEVC::new(
        scaling_list_4x4,
        scaling_list_8x8,
        scaling_list_16x16,
        scaling_list_32x32_r,
        scaling_lists
            .scaling_list_dc_coef_minus8_16x16
            .map(|x| (x + 8) as u8),
        scaling_list_dc_32x32,
    )))
}

impl<M: SurfaceMemoryDescriptor + 'static> VaapiBackend<M> {
    fn submit_last_slice(
        &mut self,
        picture: &mut <Self as StatelessDecoderBackendPicture<H265>>::Picture,
    ) -> anyhow::Result<()> {
        if let Some(last_slice) = picture.last_slice.take() {
            let metadata = self.metadata_state.get_parsed()?;
            let context = &metadata.context;
            let picture = &mut picture.picture;

            let slice_param = BufferType::SliceParameter(SliceParameter::HEVC(last_slice.0));
            let slice_param = context.create_buffer(slice_param)?;
            picture.add_buffer(slice_param);

            if let Some(slice_param_rext) = last_slice.1 {
                let slice_param_rext =
                    BufferType::SliceParameter(SliceParameter::HEVCRext(slice_param_rext));
                let slice_param_rext = context.create_buffer(slice_param_rext)?;
                picture.add_buffer(slice_param_rext);
            }

            let slice_data = BufferType::SliceData(last_slice.2);
            let slice_data = context.create_buffer(slice_data)?;
            picture.add_buffer(slice_data);
        }

        Ok(())
    }
}

pub struct VaapiH265Picture<Picture> {
    picture: Picture,

    // We are always one slice behind, so that we can mark the last one in
    // submit_picture()
    last_slice: Option<(
        SliceParameterBufferHEVC,
        Option<SliceParameterBufferHEVCRext>,
        Vec<u8>,
    )>,

    va_references: [PictureHEVC; 15],
}

impl<M: SurfaceMemoryDescriptor + 'static> StatelessDecoderBackendPicture<H265>
    for VaapiBackend<M>
{
    type Picture = VaapiH265Picture<VaapiPicture<M>>;
}

impl<M: SurfaceMemoryDescriptor + 'static> StatelessH265DecoderBackend for VaapiBackend<M> {
    fn new_sequence(&mut self, sps: &Sps) -> StatelessBackendResult<()> {
        self.new_sequence(sps, PoolCreationMode::Highest)
    }

    fn new_picture(
        &mut self,
        coded_resolution: Resolution,
        timestamp: u64,
    ) -> NewPictureResult<Self::Picture> {
        let layer = PoolLayer::Layer(coded_resolution);
        let pool = self
            .frame_pool(layer)
            .pop()
            .ok_or(NewPictureError::NoFramePool(coded_resolution))?;
        let surface = pool
            .get_surface()
            .ok_or(NewPictureError::OutOfOutputBuffers)?;
        let metadata = self.metadata_state.get_parsed()?;

        Ok(VaapiH265Picture {
            picture: VaPicture::new(timestamp, Rc::clone(&metadata.context), surface),
            last_slice: Default::default(),
            va_references: Default::default(),
        })
    }

    fn begin_picture(
        &mut self,
        picture: &mut Self::Picture,
        picture_data: &PictureData,
        sps: &Sps,
        pps: &Pps,
        dpb: &Dpb<Self::Handle>,
        rps: &RefPicSet<Self::Handle>,
        slice: &Slice,
    ) -> crate::decoder::stateless::StatelessBackendResult<()> {
        let metadata = self.metadata_state.get_parsed()?;
        let context = &metadata.context;

        let surface_id = picture.picture.surface().id();

        let (pic_param, reference_frames) =
            build_pic_param(slice, picture_data, surface_id, dpb, rps, sps, pps)?;

        picture.va_references = reference_frames;

        let picture = &mut picture.picture;

        let pic_param = context
            .create_buffer(pic_param)
            .context("while creating picture parameter buffer")?;

        picture.add_buffer(pic_param);

        if !matches!(find_scaling_list(sps, pps), ScalingListType::None) {
            let iq_matrix = build_iq_matrix(sps, pps);
            let iq_matrix = context
                .create_buffer(iq_matrix)
                .context("while creating IQ matrix buffer")?;

            picture.add_buffer(iq_matrix);
        }

        let va_profile = sps.va_profile()?;
        if is_range_extension_profile(va_profile) || is_scc_ext_profile(va_profile) {
            let rext = build_picture_rext(sps, pps)?;
            let rext = context
                .create_buffer(rext)
                .context("while creating picture parameter range extension buffer")?;

            picture.add_buffer(rext);

            if is_scc_ext_profile(va_profile) {
                let scc = build_picture_scc(sps, pps)?;
                let scc = context
                    .create_buffer(scc)
                    .context("while creating picture screen content coding buffer")?;

                picture.add_buffer(scc);
            }
        }

        Ok(())
    }

    fn decode_slice(
        &mut self,
        picture: &mut Self::Picture,
        slice: &Slice,
        sps: &Sps,
        _: &Pps,
        ref_pic_list0: &[Option<RefPicListEntry<Self::Handle>>; 16],
        ref_pic_list1: &[Option<RefPicListEntry<Self::Handle>>; 16],
    ) -> crate::decoder::stateless::StatelessBackendResult<()> {
        self.submit_last_slice(picture)?;
        let hdr = &slice.header;

        let va_references = &picture.va_references;
        let ref_pic_list0 = build_slice_ref_pic_list(ref_pic_list0, va_references);
        let ref_pic_list1 = build_slice_ref_pic_list(ref_pic_list1, va_references);

        let long_slice_flags = libva::HevcLongSliceFlags::new(
            0,
            hdr.dependent_slice_segment_flag as u32,
            hdr.type_ as u32,
            hdr.colour_plane_id as u32,
            hdr.sao_luma_flag as u32,
            hdr.sao_chroma_flag as u32,
            hdr.mvd_l1_zero_flag as u32,
            hdr.cabac_init_flag as u32,
            hdr.temporal_mvp_enabled_flag as u32,
            hdr.deblocking_filter_disabled_flag as u32,
            hdr.collocated_from_l0_flag as u32,
            hdr.loop_filter_across_slices_enabled_flag as u32,
        );

        let collocated_ref_idx = if hdr.temporal_mvp_enabled_flag {
            hdr.collocated_ref_idx
        } else {
            0xff
        };

        let pwt = &hdr.pred_weight_table;

        let mut delta_luma_weight_l0: [i8; 15usize] = Default::default();
        let mut luma_offset_l0: [i8; 15usize] = Default::default();
        let mut delta_chroma_weight_l0: [[i8; 2usize]; 15usize] = Default::default();
        let mut chroma_offset_l0: [[i8; 2usize]; 15usize] = Default::default();
        let mut delta_luma_weight_l1: [i8; 15usize] = Default::default();
        let mut luma_offset_l1: [i8; 15usize] = Default::default();
        let mut delta_chroma_weight_l1: [[i8; 2usize]; 15usize] = Default::default();
        let mut chroma_offset_l1: [[i8; 2usize]; 15usize] = Default::default();

        for i in 0..15 {
            delta_luma_weight_l0[i] = pwt.delta_luma_weight_l0[i];
            luma_offset_l0[i] = pwt.luma_offset_l0[i];

            if hdr.type_.is_b() {
                delta_luma_weight_l1[i] = pwt.delta_luma_weight_l1[i];
                luma_offset_l1[i] = pwt.luma_offset_l1[i];
            }

            for j in 0..2 {
                delta_chroma_weight_l0[i][j] = pwt.delta_chroma_weight_l0[i][j];
                let delta_chroma_offset = pwt.delta_chroma_offset_l0[i][j];

                let chroma_weight_l0 = (1 << pwt.chroma_log2_weight_denom)
                    + i32::from(pwt.delta_chroma_weight_l0[i][j]);

                let offset = sps.wp_offset_half_range_c as i32 + delta_chroma_offset as i32
                    - ((sps.wp_offset_half_range_c as i32 * chroma_weight_l0)
                        >> pwt.chroma_log2_weight_denom);

                chroma_offset_l0[i][j] = clip3(
                    -(sps.wp_offset_half_range_c as i32),
                    (sps.wp_offset_half_range_c - 1) as i32,
                    offset,
                ) as _;

                if hdr.type_.is_b() {
                    delta_chroma_weight_l1[i][j] = pwt.delta_chroma_weight_l1[i][j];
                    let delta_chroma_offset = pwt.delta_chroma_offset_l1[i][j];

                    let chroma_weight_l1 = (1 << pwt.chroma_log2_weight_denom)
                        + i32::from(pwt.delta_chroma_weight_l1[i][j]);

                    let offset = sps.wp_offset_half_range_c as i32 + delta_chroma_offset as i32
                        - ((sps.wp_offset_half_range_c as i32 * chroma_weight_l1)
                            >> pwt.chroma_log2_weight_denom);

                    chroma_offset_l1[i][j] = clip3(
                        -(sps.wp_offset_half_range_c as i32),
                        (sps.wp_offset_half_range_c - 1) as i32,
                        offset,
                    ) as _;
                }
            }
        }

        let slice_param = SliceParameterBufferHEVC::new(
            slice.nalu.size as u32,
            0,
            libva::VA_SLICE_DATA_FLAG_ALL,
            (hdr.header_bit_size / 8) as _,
            hdr.segment_address,
            [ref_pic_list0, ref_pic_list1],
            &long_slice_flags,
            collocated_ref_idx,
            hdr.num_ref_idx_l0_active_minus1,
            hdr.num_ref_idx_l1_active_minus1,
            hdr.qp_delta,
            hdr.cb_qp_offset,
            hdr.cr_qp_offset,
            hdr.beta_offset_div2,
            hdr.tc_offset_div2,
            pwt.luma_log2_weight_denom,
            pwt.delta_chroma_log2_weight_denom,
            delta_luma_weight_l0,
            luma_offset_l0,
            delta_chroma_weight_l0,
            chroma_offset_l0,
            delta_luma_weight_l1,
            luma_offset_l1,
            delta_chroma_weight_l1,
            chroma_offset_l1,
            hdr.five_minus_max_num_merge_cand,
            hdr.num_entry_point_offsets as _,
            0,
            hdr.n_emulation_prevention_bytes as _,
        );

        let va_profile = sps.va_profile()?;

        let slice_param_ext =
            if is_range_extension_profile(va_profile) || is_scc_ext_profile(va_profile) {
                let slice_ext_flags = HevcSliceExtFlags::new(
                    hdr.cu_chroma_qp_offset_enabled_flag as u32,
                    hdr.use_integer_mv_flag as u32,
                );

                let slice_param_ext = SliceParameterBufferHEVCRext::new(
                    luma_offset_l0.map(i16::from),
                    chroma_offset_l0.map(|outer| outer.map(i16::from)),
                    luma_offset_l1.map(i16::from),
                    chroma_offset_l1.map(|outer| outer.map(i16::from)),
                    &slice_ext_flags,
                    hdr.slice_act_y_qp_offset,
                    hdr.slice_act_cb_qp_offset,
                    hdr.slice_act_cr_qp_offset,
                );

                Some(slice_param_ext)
            } else {
                None
            };

        let slice_data = Vec::from(slice.nalu.as_ref());

        picture.last_slice = Some((slice_param, slice_param_ext, slice_data));

        Ok(())
    }

    fn submit_picture(
        &mut self,
        mut picture: Self::Picture,
    ) -> StatelessBackendResult<Self::Handle> {
        if let Some(last_slice) = &mut picture.last_slice {
            last_slice.0.set_as_last();
        }
        self.submit_last_slice(&mut picture)?;
        self.process_picture::<H265>(picture.picture)
    }
}

impl<M: SurfaceMemoryDescriptor + 'static> StatelessDecoder<H265, VaapiBackend<M>> {
    // Creates a new instance of the decoder using the VAAPI backend.
    pub fn new_vaapi<S>(
        display: Rc<Display>,
        blocking_mode: BlockingMode,
    ) -> Result<Self, NewStatelessDecoderError>
    where
        M: From<S>,
        S: From<M>,
    {
        Self::new(VaapiBackend::new(display, false), blocking_mode)
    }
}

#[cfg(test)]
mod tests {
    use libva::Display;

    use crate::bitstream_utils::NalIterator;
    use crate::codec::h265::parser::Nalu;
    use crate::decoder::stateless::h265::H265;
    use crate::decoder::stateless::tests::test_decode_stream;
    use crate::decoder::stateless::tests::TestStream;
    use crate::decoder::stateless::StatelessDecoder;
    use crate::decoder::BlockingMode;
    use crate::utils::simple_playback_loop;
    use crate::utils::simple_playback_loop_owned_frames;
    use crate::DecodedFormat;

    /// Run `test` using the vaapi decoder, in both blocking and non-blocking modes.
    fn test_decoder_vaapi(
        test: &TestStream,
        output_format: DecodedFormat,
        blocking_mode: BlockingMode,
    ) {
        let display = Display::open().unwrap();
        let decoder = StatelessDecoder::<H265, _>::new_vaapi::<()>(display, blocking_mode).unwrap();

        test_decode_stream(
            |d, s, f| {
                simple_playback_loop(
                    d,
                    NalIterator::<Nalu>::new(s),
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
    fn test_64x64_progressive_i_block() {
        use crate::decoder::stateless::h265::tests::DECODE_64X64_PROGRESSIVE_I;
        test_decoder_vaapi(
            &DECODE_64X64_PROGRESSIVE_I,
            DecodedFormat::NV12,
            BlockingMode::Blocking,
        );
    }

    #[test]
    // Ignore this test by default as it requires libva-compatible hardware.
    #[ignore]
    fn test_64x64_progressive_i_nonblock() {
        use crate::decoder::stateless::h265::tests::DECODE_64X64_PROGRESSIVE_I;
        test_decoder_vaapi(
            &DECODE_64X64_PROGRESSIVE_I,
            DecodedFormat::NV12,
            BlockingMode::NonBlocking,
        );
    }

    #[test]
    // Ignore this test by default as it requires libva-compatible hardware.
    #[ignore]
    fn test_64x64_progressive_i_p_block() {
        use crate::decoder::stateless::h265::tests::DECODE_64X64_PROGRESSIVE_I_P;
        test_decoder_vaapi(
            &DECODE_64X64_PROGRESSIVE_I_P,
            DecodedFormat::NV12,
            BlockingMode::Blocking,
        );
    }

    #[test]
    // Ignore this test by default as it requires libva-compatible hardware.
    #[ignore]
    fn test_64x64_progressive_i_p_nonblock() {
        use crate::decoder::stateless::h265::tests::DECODE_64X64_PROGRESSIVE_I_P;
        test_decoder_vaapi(
            &DECODE_64X64_PROGRESSIVE_I_P,
            DecodedFormat::NV12,
            BlockingMode::NonBlocking,
        );
    }

    #[test]
    // Ignore this test by default as it requires libva-compatible hardware.
    #[ignore]
    fn test_64x64_progressive_i_p_b_p_block() {
        use crate::decoder::stateless::h265::tests::DECODE_64X64_PROGRESSIVE_I_P_B_P;
        test_decoder_vaapi(
            &DECODE_64X64_PROGRESSIVE_I_P_B_P,
            DecodedFormat::NV12,
            BlockingMode::Blocking,
        );
    }

    #[test]
    // Ignore this test by default as it requires libva-compatible hardware.
    #[ignore]
    fn test_64x64_progressive_i_p_b_p_nonblock() {
        use crate::decoder::stateless::h265::tests::DECODE_64X64_PROGRESSIVE_I_P_B_P;
        test_decoder_vaapi(
            &DECODE_64X64_PROGRESSIVE_I_P_B_P,
            DecodedFormat::NV12,
            BlockingMode::NonBlocking,
        );
    }
}
