// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::any::Any;
use std::borrow::Borrow;
use std::rc::Rc;

use anyhow::Context;
use libva::BufferType;
use libva::Display;
use libva::EncCodedBuffer;
use libva::EncPictureParameter;
use libva::EncPictureParameterBufferHEVC;
use libva::EncSequenceParameter;
use libva::EncSequenceParameterBufferHEVC;
use libva::EncSliceParameter;
use libva::EncSliceParameterBufferHEVC;
use libva::HEVCEncPicFields;
use libva::HEVCEncSeqFields;
use libva::HevcEncPicSccFields;
use libva::HevcEncSeqSccFields;
use libva::HevcEncSliceFields;
use libva::HevcEncVuiFields;
use libva::Picture;
use libva::PictureHEVC;
use libva::Surface;
use libva::SurfaceMemoryDescriptor;
use libva::VAProfile;
use libva::VA_INVALID_ID;
use libva::VA_PICTURE_HEVC_INVALID;
use libva::VA_PICTURE_HEVC_LONG_TERM_REFERENCE;

use crate::backend::vaapi::encoder::tunings_to_libva_rc;
use crate::backend::vaapi::encoder::CodedOutputPromise;
use crate::backend::vaapi::encoder::Reconstructed;
use crate::backend::vaapi::encoder::VaapiBackend;
use crate::codec::h265::parser::NaluType;
use crate::codec::h265::parser::Pps;
use crate::codec::h265::parser::Profile;
use crate::codec::h265::parser::SliceType;
use crate::codec::h265::parser::Sps;
use crate::encoder::h265::EncoderConfig;
use crate::encoder::h265::H265;
use crate::encoder::stateless::h265::predictor::MAX_QP;
use crate::encoder::stateless::h265::predictor::MIN_QP;
use crate::encoder::stateless::h265::BackendRequest;
use crate::encoder::stateless::h265::DpbEntry;
use crate::encoder::stateless::h265::DpbEntryMeta;
use crate::encoder::stateless::h265::IsReference;
use crate::encoder::stateless::h265::StatelessEncoder;
use crate::encoder::stateless::h265::StatelessH265EncoderBackend;
use crate::encoder::stateless::ReadyPromise;
use crate::encoder::stateless::StatelessBackendError;
use crate::encoder::stateless::StatelessBackendResult;
use crate::encoder::stateless::StatelessVideoEncoderBackend;
use crate::encoder::EncodeResult;
use crate::encoder::RateControl;
use crate::video_frame::VideoFrame;
use crate::BlockingMode;
use crate::Fourcc;
use crate::Resolution;

/// Size of `VAEncPictureParameterBufferHEVC::reference_frames` and of the slice parameter
/// reference picture lists.
const MAX_REFERENCE_FRAMES: usize = 15;

/// `VAEncPictureParameterBufferHEVC::coding_type` for an I picture. See va_enc_hevc.h.
const CODING_TYPE_I: u32 = 1;
/// `VAEncPictureParameterBufferHEVC::coding_type` for a P picture. See va_enc_hevc.h.
const CODING_TYPE_P: u32 = 2;

/// `VAEncPictureParameterBufferHEVC::collocated_ref_pic_index` value meaning that no collocated
/// reference picture is used, ie. `slice_temporal_mvp_enabled_flag` is zero. See va_enc_hevc.h.
const NO_COLLOCATED_REF_PIC: u8 = 0xff;

/// Value of `five_minus_max_num_merge_cand` subtracted from five, ie. the maximum number of
/// merging motion vector prediction candidates. See H.265 7.4.7.1.
const MAX_NUM_MERGE_CAND: u8 = 5;

type Request<'l, H> = BackendRequest<H, Reconstructed>;

impl<M, H> StatelessVideoEncoderBackend<H265> for VaapiBackend<M, H>
where
    M: SurfaceMemoryDescriptor,
    H: std::borrow::Borrow<Surface<M>> + 'static,
{
    type Picture = H;
    type Reconstructed = Reconstructed;
    type CodedPromise = CodedOutputPromise<M, H>;
    type ReconPromise = ReadyPromise<Self::Reconstructed>;
}

/// Builds an invalid [`libva::PictureHEVC`]. This is usually a place
/// holder to fill staticly sized array.
fn build_invalid_va_h265_pic_enc() -> PictureHEVC {
    PictureHEVC::new(VA_INVALID_ID, 0, VA_PICTURE_HEVC_INVALID)
}

/// Builds [`libva::PictureHEVC`] from `surface` and its `meta`.
fn build_h265_pic(surface: &Reconstructed, meta: &DpbEntryMeta) -> PictureHEVC {
    let flags = match meta.is_reference {
        IsReference::No | IsReference::ShortTerm => 0,
        IsReference::LongTerm => VA_PICTURE_HEVC_LONG_TERM_REFERENCE,
    };

    PictureHEVC::new(surface.surface_id(), meta.poc, flags)
}

/// Fills a reference picture list of [`MAX_REFERENCE_FRAMES`] entries with `refs`, padding
/// the remaining slots with invalid pictures.
fn build_va_ref_pic_list<'e>(
    refs: impl Iterator<Item = &'e Rc<DpbEntry<Reconstructed>>>,
) -> [PictureHEVC; MAX_REFERENCE_FRAMES] {
    let mut list: [PictureHEVC; MAX_REFERENCE_FRAMES] = (0..MAX_REFERENCE_FRAMES)
        .map(|_| build_invalid_va_h265_pic_enc())
        .collect::<Vec<_>>()
        .try_into()
        .unwrap_or_else(|_| panic!());

    for (idx, ref_frame) in refs.enumerate().take(MAX_REFERENCE_FRAMES) {
        list[idx] = build_h265_pic(&ref_frame.recon_pic, &ref_frame.meta);
    }

    list
}

/// Builds [`BufferType::EncSequenceParameter`] from `sps`
fn build_enc_seq_param(
    sps: &Sps,
    bits_per_second: u32,
    intra_period: u32,
    ip_period: u32,
) -> BufferType {
    let intra_idr_period = intra_period;

    let seq_fields = HEVCEncSeqFields::new(
        sps.chroma_format_idc as u32,
        sps.separate_colour_plane_flag as u32,
        sps.bit_depth_luma_minus8 as u32,
        sps.bit_depth_chroma_minus8 as u32,
        sps.scaling_list_enabled_flag as u32,
        sps.strong_intra_smoothing_enabled_flag as u32,
        sps.amp_enabled_flag as u32,
        sps.sample_adaptive_offset_enabled_flag as u32,
        sps.pcm_enabled_flag as u32,
        sps.pcm_loop_filter_disabled_flag as u32,
        sps.temporal_mvp_enabled_flag as u32,
        // The prediction structure never references a picture that follows in output order.
        true as u32,
        // No hierarchical prediction structure is used.
        false as u32,
    );

    let vui = &sps.vui_parameters;
    let vui_fields = if sps.vui_parameters_present_flag {
        Some(HevcEncVuiFields::new(
            vui.aspect_ratio_info_present_flag as u32,
            vui.neutral_chroma_indication_flag as u32,
            vui.field_seq_flag as u32,
            vui.timing_info_present_flag as u32,
            vui.bitstream_restriction_flag as u32,
            vui.tiles_fixed_structure_flag as u32,
            vui.motion_vectors_over_pic_boundaries_flag as u32,
            vui.restricted_ref_pic_lists_flag as u32,
            vui.log2_max_mv_length_horizontal,
            vui.log2_max_mv_length_vertical,
        ))
    } else {
        None
    };

    // The encoder never enables the palette mode of the screen content coding extension.
    let scc_fields = HevcEncSeqSccFields::new(false as u32);

    BufferType::EncSequenceParameter(EncSequenceParameter::HEVC(
        EncSequenceParameterBufferHEVC::new(
            sps.profile_tier_level.general_profile_idc,
            sps.profile_tier_level.general_level_idc as u8,
            sps.profile_tier_level.general_tier_flag as u8,
            intra_period,
            intra_idr_period,
            ip_period,
            bits_per_second,
            sps.pic_width_in_luma_samples,
            sps.pic_height_in_luma_samples,
            &seq_fields,
            sps.log2_min_luma_coding_block_size_minus3,
            sps.log2_diff_max_min_luma_coding_block_size,
            sps.log2_min_luma_transform_block_size_minus2,
            sps.log2_diff_max_min_luma_transform_block_size,
            sps.max_transform_hierarchy_depth_inter,
            sps.max_transform_hierarchy_depth_intra,
            sps.pcm_sample_bit_depth_luma_minus1 as u32,
            sps.pcm_sample_bit_depth_chroma_minus1 as u32,
            sps.log2_min_pcm_luma_coding_block_size_minus3 as u32,
            (sps.log2_min_pcm_luma_coding_block_size_minus3
                + sps.log2_diff_max_min_pcm_luma_coding_block_size) as u32,
            vui_fields,
            vui.aspect_ratio_idc as u8,
            vui.sar_width,
            vui.sar_height,
            vui.num_units_in_tick,
            vui.time_scale,
            vui.min_spatial_segmentation_idc as u16,
            vui.max_bytes_per_pic_denom as u8,
            vui.max_bits_per_min_cu_denom as u8,
            &scc_fields,
        ),
    ))
}

/// H.265 Table 7-1. A non IDR key frame is coded as a trailing picture, because the prediction
/// structure never emits leading pictures.
fn nal_unit_type(is_idr: bool) -> NaluType {
    if is_idr {
        NaluType::IdrWRadl
    } else {
        NaluType::TrailR
    }
}

/// Builds [`BufferType::EncPictureParameter`] from [`Request`] and sets bitstream
/// output to `coded_buf`.
fn build_enc_pic_param<H>(
    request: &Request<'_, H>,
    coded_buf: &EncCodedBuffer,
    recon: &Reconstructed,
) -> BufferType {
    let pps = &request.pps;

    let coding_type = match request.slice_type {
        SliceType::I => CODING_TYPE_I,
        _ => CODING_TYPE_P,
    };

    let pic_fields = HEVCEncPicFields::new(
        request.is_idr as u32,
        coding_type,
        (request.dpb_meta.is_reference != IsReference::No) as u32,
        pps.dependent_slice_segments_enabled_flag as u32,
        pps.sign_data_hiding_enabled_flag as u32,
        pps.constrained_intra_pred_flag as u32,
        pps.transform_skip_enabled_flag as u32,
        pps.cu_qp_delta_enabled_flag as u32,
        pps.weighted_pred_flag as u32,
        pps.weighted_bipred_flag as u32,
        pps.transquant_bypass_enabled_flag as u32,
        pps.tiles_enabled_flag as u32,
        pps.entropy_coding_sync_enabled_flag as u32,
        pps.loop_filter_across_tiles_enabled_flag as u32,
        pps.loop_filter_across_slices_enabled_flag as u32,
        pps.scaling_list_data_present_flag as u32,
        // Screen content tools and GPU weighted prediction are not used.
        false as u32,
        false as u32,
        // H.265 7.4.7.1: only meaningful for an IRAP picture, and the encoder never needs
        // the decoder to discard already decoded pictures.
        false as u32,
    );

    let curr_pic = build_h265_pic(recon, &request.dpb_meta);

    assert!(request.ref_list_0.len() + request.ref_list_1.len() <= MAX_REFERENCE_FRAMES);

    let reference_frames =
        build_va_ref_pic_list(request.ref_list_0.iter().chain(request.ref_list_1.iter()));

    let nal_unit_type = nal_unit_type(request.is_idr);

    // The encoder never enables the palette mode of the screen content coding extension.
    let scc_fields = HevcEncPicSccFields::new(false as u16);

    BufferType::EncPictureParameter(EncPictureParameter::HEVC(EncPictureParameterBufferHEVC::new(
        curr_pic,
        reference_frames,
        coded_buf.id(),
        NO_COLLOCATED_REF_PIC,
        0, // last_picture, don't append EOS
        (pps.init_qp_minus26 + 26) as u8,
        pps.diff_cu_qp_delta_depth,
        pps.cb_qp_offset,
        pps.cr_qp_offset,
        pps.num_tile_columns_minus1,
        pps.num_tile_rows_minus1,
        // A single tile spans the whole picture, the tile sizes are not signalled.
        [0; 19],
        [0; 21],
        pps.log2_parallel_merge_level_minus2,
        // No limit on the size of a coded coding tree unit.
        0,
        pps.num_ref_idx_l0_default_active_minus1,
        pps.num_ref_idx_l1_default_active_minus1,
        pps.pic_parameter_set_id,
        nal_unit_type as u8,
        &pic_fields,
        // No hierarchical prediction structure is used.
        0,
        0,
        &scc_fields,
    )))
}

/// Builds [`BufferType::EncSliceParameter`] for the single slice segment covering the whole
/// picture.
fn build_enc_slice_param(
    pps: &Pps,
    slice_type: SliceType,
    ref_list_0: &[Rc<DpbEntry<Reconstructed>>],
    ref_list_1: &[Rc<DpbEntry<Reconstructed>>],
    num_ctus: u32,
) -> BufferType {
    let ref_pic_list0 = build_va_ref_pic_list(ref_list_0.iter());
    let ref_pic_list1 = build_va_ref_pic_list(ref_list_1.iter());

    // The reference lists hold every available reference, therefore their length may differ
    // from the PPS defaults and has to be overridden in the slice segment header.
    let num_ref_idx_l0_active_minus1 = ref_list_0.len().saturating_sub(1) as u8;
    let num_ref_idx_l1_active_minus1 = ref_list_1.len().saturating_sub(1) as u8;

    let slice_fields = HevcEncSliceFields::new(
        // The single slice segment covers the whole picture.
        true as u32,
        false as u32,
        0,
        // Temporal motion vector prediction and SAO are disabled in the SPS.
        false as u32,
        false as u32,
        false as u32,
        true as u32,
        // No B slices are produced, list1 is never used.
        false as u32,
        false as u32,
        pps.deblocking_filter_disabled_flag as u32,
        pps.loop_filter_across_slices_enabled_flag as u32,
        // Only meaningful with temporal motion vector prediction enabled.
        true as u32,
    );

    BufferType::EncSliceParameter(EncSliceParameter::HEVC(EncSliceParameterBufferHEVC::new(
        // The single slice segment starts at the first coding tree block.
        0,
        num_ctus,
        slice_type as u8,
        pps.pic_parameter_set_id,
        num_ref_idx_l0_active_minus1,
        num_ref_idx_l1_active_minus1,
        ref_pic_list0,
        ref_pic_list1,
        // Weighted prediction is disabled in the PPS, the tables are not signalled.
        0,
        0,
        [0; MAX_REFERENCE_FRAMES],
        [0; MAX_REFERENCE_FRAMES],
        [[0; 2]; MAX_REFERENCE_FRAMES],
        [[0; 2]; MAX_REFERENCE_FRAMES],
        [0; MAX_REFERENCE_FRAMES],
        [0; MAX_REFERENCE_FRAMES],
        [[0; 2]; MAX_REFERENCE_FRAMES],
        [[0; 2]; MAX_REFERENCE_FRAMES],
        MAX_NUM_MERGE_CAND,
        // SliceQpY is the PPS initial QP, the rate controller adjusts it on its own.
        0,
        pps.cb_qp_offset,
        pps.cr_qp_offset,
        pps.beta_offset_div2,
        pps.tc_offset_div2,
        &slice_fields,
        0,
        0,
    )))
}

impl<M, H> StatelessH265EncoderBackend for VaapiBackend<M, H>
where
    M: SurfaceMemoryDescriptor,
    H: Borrow<Surface<M>> + 'static,
{
    /// The slice segment headers are left to the driver, as the H.264 backend does. Note that
    /// H.265 cannot fully express a slice segment header through VA-API:
    /// `VAEncSequenceParameterBufferHEVC` carries neither `log2_max_pic_order_cnt_lsb_minus4`
    /// nor the short term reference picture sets, and `VAEncPictureParameterBufferHEVC` has no
    /// picture order count of its own, so a driver's slice segment headers may disagree with the
    /// parameter sets synthesized here. On AMD VCN the driver writes a constant
    /// `slice_pic_order_cnt_lsb`, which leaves intra only streams decodable but inter pictures
    /// not. Supplying a packed slice segment header instead would remove that dependency on
    /// driver behaviour - `VAConfigAttribEncPackedHeaders` advertises
    /// `VA_ENC_PACKED_HEADER_SLICE` there - but a first attempt at it hung the encode ring, so
    /// it needs its own investigation.
    fn encode_slice(
        &mut self,
        request: Request<'_, H>,
    ) -> StatelessBackendResult<(Self::ReconPromise, Self::CodedPromise)> {
        let coded_buf = self.new_coded_buffer(&request.tunings.rate_control)?;
        let recon = self.new_scratch_picture()?;

        // Use bitrate from RateControl or ask driver to ignore
        let bits_per_second = request.tunings.rate_control.bitrate_target().unwrap_or(0) as u32;
        let seq_param = build_enc_seq_param(
            &request.sps,
            bits_per_second,
            request.intra_period,
            request.ip_period,
        );

        let pic_param = build_enc_pic_param(&request, &coded_buf, &recon);
        let slice_param = build_enc_slice_param(
            &request.pps,
            request.slice_type,
            &request.ref_list_0,
            &request.ref_list_1,
            request.num_ctus as u32,
        );

        // Clone reference frames
        let references: Vec<Rc<dyn Any>> = request
            .ref_list_0
            .iter()
            .cloned()
            .chain(request.ref_list_1.iter().cloned())
            .map(|entry| entry as Rc<dyn Any>)
            .collect();

        // Clone picture using [`Picture::new_from_same_surface`] to avoid
        // creatig a shared cell picture between its references and processed
        // picture.
        let mut picture =
            Picture::new(request.input_meta.timestamp, Rc::clone(self.context()), request.input);

        let rc_param =
            tunings_to_libva_rc::<{ MIN_QP as u32 }, { MAX_QP as u32 }>(&request.tunings)?;
        let rc_param = BufferType::EncMiscParameter(libva::EncMiscParameter::RateControl(rc_param));

        let framerate_param = BufferType::EncMiscParameter(libva::EncMiscParameter::FrameRate(
            libva::EncMiscParameterFrameRate::new(request.tunings.framerate, 0),
        ));

        picture.add_buffer(self.context().create_buffer(seq_param)?);
        picture.add_buffer(self.context().create_buffer(pic_param)?);
        picture.add_buffer(self.context().create_buffer(slice_param)?);
        picture.add_buffer(self.context().create_buffer(rc_param)?);
        picture.add_buffer(self.context().create_buffer(framerate_param)?);

        // Start processing the picture encoding
        let picture = picture.begin().context("picture begin")?;
        let picture = picture.render().context("picture render")?;
        let picture = picture.end().context("picture end")?;

        // libva will handle the synchronization of reconstructed surface with implicit fences.
        // Therefore return the reconstructed frame immediately.
        let reference_promise = ReadyPromise::from(recon);

        let bitstream_promise =
            CodedOutputPromise::new(picture, references, coded_buf, request.coded_output);

        Ok((reference_promise, bitstream_promise))
    }
}

impl<V: VideoFrame> StatelessEncoder<V, VaapiBackend<V::MemDescriptor, Surface<V::MemDescriptor>>> {
    pub fn new_vaapi(
        display: Rc<Display>,
        config: EncoderConfig,
        fourcc: Fourcc,
        coded_size: Resolution,
        low_power: bool,
        blocking_mode: BlockingMode,
    ) -> EncodeResult<Self> {
        let va_profile = match config.profile {
            Profile::Main => VAProfile::VAProfileHEVCMain,
            Profile::Main10 => VAProfile::VAProfileHEVCMain10,
            _ => return Err(StatelessBackendError::UnsupportedProfile.into()),
        };

        let bitrate_control = match config.initial_tunings.rate_control {
            RateControl::ConstantBitrate(_) => libva::VA_RC_CBR,
            RateControl::ConstantQuality(_) => libva::VA_RC_CQP,
        };

        let backend =
            VaapiBackend::new(display, va_profile, fourcc, coded_size, bitrate_control, low_power)?;

        Self::new_h265(backend, config, blocking_mode)
    }
}

#[cfg(test)]
pub(super) mod tests {
    use libva::Display;
    use libva::UsageHint;
    use libva::VAEntrypoint::VAEntrypointEncSliceLP;
    use libva::VAProfile::VAProfileHEVCMain;
    use libva::VA_RT_FORMAT_YUV420;

    use super::*;
    use crate::backend::vaapi::encoder::tests::upload_test_frame_nv12;
    use crate::backend::vaapi::encoder::tests::TestFrameGenerator;
    use crate::backend::vaapi::surface_pool::PooledVaSurface;
    use crate::backend::vaapi::surface_pool::VaSurfacePool;
    use crate::codec::h265::parser::Level;
    use crate::codec::h265::parser::PpsBuilder;
    use crate::codec::h265::parser::Profile;
    use crate::codec::h265::parser::SpsBuilder;
    use crate::codec::h265::parser::Vps;
    use crate::codec::h265::parser::VpsBuilder;
    use crate::codec::h265::synthesizer::Synthesizer;
    use crate::decoder::FramePool;
    use crate::encoder::simple_encode_loop;
    use crate::encoder::stateless::h265::BackendRequest;
    use crate::encoder::stateless::h265::EncoderConfig;
    use crate::encoder::stateless::h265::StatelessEncoder;
    use crate::encoder::stateless::BackendPromise;
    use crate::encoder::stateless::StatelessEncoderBackendImport;
    use crate::encoder::FrameMetadata;
    use crate::encoder::Tunings;
    use crate::FrameLayout;
    use crate::PlaneLayout;
    use crate::Resolution;

    #[test]
    // Ignore this test by default as it requires libva-compatible hardware.
    #[ignore]
    fn test_simple_encode_slice() {
        type Descriptor = ();
        type Surface = libva::Surface<Descriptor>;
        // Some drivers refuse smaller HEVC encode contexts, eg. AMD VCN requires at least
        // 384x384.
        const WIDTH: u32 = 512;
        const HEIGHT: u32 = 512;
        let fourcc = b"NV12".into();

        let frame_layout = FrameLayout {
            format: (fourcc, 0),
            size: Resolution { width: WIDTH, height: HEIGHT },
            planes: vec![
                PlaneLayout { buffer_index: 0, offset: 0, stride: WIDTH as usize },
                PlaneLayout {
                    buffer_index: 0,
                    offset: (WIDTH * HEIGHT) as usize,
                    stride: WIDTH as usize,
                },
            ],
        };

        let display = Display::open().unwrap();
        let entrypoints = display.query_config_entrypoints(VAProfileHEVCMain).unwrap();
        let low_power = entrypoints.contains(&VAEntrypointEncSliceLP);

        let mut backend = VaapiBackend::<Descriptor, Surface>::new(
            Rc::clone(&display),
            VAProfileHEVCMain,
            fourcc,
            Resolution { width: WIDTH, height: HEIGHT },
            libva::VA_RC_CBR,
            low_power,
        )
        .unwrap();

        let mut surfaces = display
            .create_surfaces(
                VA_RT_FORMAT_YUV420,
                Some(frame_layout.format.0 .0),
                WIDTH,
                HEIGHT,
                Some(UsageHint::USAGE_HINT_ENCODER),
                vec![()],
            )
            .unwrap();

        let surface = surfaces.pop().unwrap();

        upload_test_frame_nv12(&display, &surface, 0.0);

        let input_meta =
            FrameMetadata { layout: frame_layout, force_keyframe: false, timestamp: 0 };

        let pic = backend.import_picture(&input_meta, surface).unwrap();

        let vps = VpsBuilder::new()
            .video_parameter_set_id(0)
            .general_profile(Profile::Main)
            .general_level_idc(Level::L4)
            .max_dec_pic_buffering_minus1(1)
            .build();

        let sps = SpsBuilder::new(Rc::clone(&vps))
            .seq_parameter_set_id(0)
            .chroma_format_idc(1)
            .resolution(WIDTH, HEIGHT)
            .bit_depth_luma(8)
            .bit_depth_chroma(8)
            .max_pic_order_cnt_lsb(256)
            .max_dec_pic_buffering_minus1(1)
            .build();

        let pps = PpsBuilder::new(Rc::clone(&sps))
            .pic_parameter_set_id(0)
            .init_qp(26)
            .deblocking_filter_control_present_flag(true)
            .build();

        let dpb_entry_meta = DpbEntryMeta { poc: 0, is_reference: IsReference::ShortTerm };

        let request = BackendRequest {
            sps: Rc::clone(&sps),
            pps: Rc::clone(&pps),
            slice_type: SliceType::I,
            dpb_meta: dpb_entry_meta,
            input: pic,
            input_meta,
            ref_list_0: vec![],
            ref_list_1: vec![],
            intra_period: 1,
            ip_period: 0,
            num_ctus: sps.pic_size_in_ctbs_y as usize,
            is_idr: true,
            tunings: Tunings {
                rate_control: RateControl::ConstantBitrate(30_000),
                ..Default::default()
            },
            coded_output: vec![],
        };

        let (_, output) = backend.encode_slice(request).unwrap();
        let output = output.sync().unwrap();

        assert!(!output.is_empty());

        let write_to_file = std::option_env!("CROS_CODECS_TEST_WRITE_TO_FILE") == Some("true");
        if write_to_file {
            use std::io::Write;

            let mut out = std::fs::File::create("test_simple_encode_slice.h265").unwrap();

            Synthesizer::<'_, Vps, &mut std::fs::File>::synthesize(1, &vps, &mut out, true)
                .unwrap();
            Synthesizer::<'_, Sps, &mut std::fs::File>::synthesize(1, &sps, &mut out, true)
                .unwrap();
            Synthesizer::<'_, Pps, &mut std::fs::File>::synthesize(1, &pps, &mut out, true)
                .unwrap();
            out.write_all(&output).unwrap();
            out.flush().unwrap();
        }
    }

    #[test]
    // Ignore this test by default as it requires libva-compatible hardware.
    #[ignore]
    fn test_vaapi_encoder() {
        type VaapiH265Encoder<'l> =
            StatelessEncoder<PooledVaSurface<()>, VaapiBackend<(), PooledVaSurface<()>>>;

        const WIDTH: usize = 512;
        const HEIGHT: usize = 512;

        let _ = env_logger::try_init();

        let display = libva::Display::open().unwrap();
        let entrypoints = display.query_config_entrypoints(VAProfileHEVCMain).unwrap();
        let low_power = entrypoints.contains(&VAEntrypointEncSliceLP);

        let config = EncoderConfig {
            profile: Profile::Main,
            resolution: Resolution { width: WIDTH as u32, height: HEIGHT as u32 },
            initial_tunings: Tunings {
                rate_control: RateControl::ConstantBitrate(1_200_000),
                framerate: 30,
                ..Default::default()
            },
            ..Default::default()
        };

        let frame_layout = FrameLayout {
            format: (b"NV12".into(), 0),
            size: Resolution { width: WIDTH as u32, height: HEIGHT as u32 },
            planes: vec![
                PlaneLayout { buffer_index: 0, offset: 0, stride: WIDTH },
                PlaneLayout { buffer_index: 0, offset: WIDTH * HEIGHT, stride: WIDTH },
            ],
        };

        // [`StatelessEncoder::new_vaapi`] only accepts [`VideoFrame`] handles, whereas the test
        // frame generator hands out pooled VA surfaces, hence the backend is built by hand.
        let backend = VaapiBackend::new(
            Rc::clone(&display),
            VAProfileHEVCMain,
            frame_layout.format.0,
            frame_layout.size,
            libva::VA_RC_CBR,
            low_power,
        )
        .unwrap();

        let mut encoder =
            VaapiH265Encoder::new_h265(backend, config, BlockingMode::Blocking).unwrap();

        let mut pool = VaSurfacePool::new(
            Rc::clone(&display),
            VA_RT_FORMAT_YUV420,
            Some(UsageHint::USAGE_HINT_ENCODER),
            Resolution { width: WIDTH as u32, height: HEIGHT as u32 },
        );

        pool.add_frames(vec![(); 16]).unwrap();

        let mut frame_producer = TestFrameGenerator::new(100, display, pool, frame_layout);

        let mut bitstream = Vec::new();

        simple_encode_loop(&mut encoder, &mut frame_producer, |coded| {
            bitstream.extend(coded.bitstream)
        })
        .unwrap();

        assert!(!bitstream.is_empty());

        let write_to_file = std::option_env!("CROS_CODECS_TEST_WRITE_TO_FILE") == Some("true");
        if write_to_file {
            use std::io::Write;
            let mut out = std::fs::File::create("test_vaapi_encoder.h265").unwrap();
            out.write_all(&bitstream).unwrap();
            out.flush().unwrap();
        }
    }
}
