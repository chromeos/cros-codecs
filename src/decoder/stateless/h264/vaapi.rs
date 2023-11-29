// Copyright 2022 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::rc::Rc;

use anyhow::anyhow;
use anyhow::Context as AnyhowContext;
use libva::BufferType;
use libva::Display;
use libva::IQMatrix;
use libva::IQMatrixBufferH264;
use libva::Picture as VaPicture;
use libva::PictureParameter;
use libva::PictureParameterBufferH264;
use libva::SliceParameter;
use libva::SurfaceMemoryDescriptor;

use crate::backend::vaapi::DecodedHandle as VADecodedHandle;
use crate::backend::vaapi::VaStreamInfo;
use crate::backend::vaapi::VaapiBackend;
use crate::backend::vaapi::VaapiPicture;
use crate::codec::h264::dpb::Dpb;
use crate::codec::h264::dpb::DpbEntry;
use crate::codec::h264::parser::Level;
use crate::codec::h264::parser::Pps;
use crate::codec::h264::parser::Profile;
use crate::codec::h264::parser::Slice;
use crate::codec::h264::parser::SliceHeader;
use crate::codec::h264::parser::Sps;
use crate::codec::h264::picture::Field;
use crate::codec::h264::picture::PictureData;
use crate::codec::h264::picture::Reference;
use crate::decoder::stateless::h264::StatelessH264DecoderBackend;
use crate::decoder::stateless::h264::H264;
use crate::decoder::stateless::StatelessBackendError;
use crate::decoder::stateless::StatelessBackendResult;
use crate::decoder::stateless::StatelessDecoder;
use crate::decoder::stateless::StatelessDecoderBackendPicture;
use crate::decoder::BlockingMode;
use crate::decoder::DecodedHandle;

impl VaStreamInfo for &Rc<Sps> {
    fn va_profile(&self) -> anyhow::Result<i32> {
        let profile_idc = self.profile_idc;
        let profile = Profile::n(profile_idc)
            .with_context(|| format!("Invalid profile_idc {:?}", profile_idc))?;

        match profile {
            Profile::Baseline => {
                if self.constraint_set0_flag {
                    Ok(libva::VAProfile::VAProfileH264ConstrainedBaseline)
                } else {
                    Err(anyhow!(
                        "Unsupported stream: profile_idc=66, but constraint_set0_flag is unset"
                    ))
                }
            }
            Profile::Main => Ok(libva::VAProfile::VAProfileH264Main),
            Profile::Extended => {
                if self.constraint_set1_flag {
                    Ok(libva::VAProfile::VAProfileH264Main)
                } else {
                    Err(anyhow!(
                        "Unsupported stream: profile_idc=88, but constraint_set1_flag is unset"
                    ))
                }
            }
            Profile::High | Profile::High422P | Profile::High10 => {
                Ok(libva::VAProfile::VAProfileH264High)
            }
        }
    }

    fn rt_format(&self) -> anyhow::Result<u32> {
        let bit_depth_luma = self.bit_depth_chroma_minus8 + 8;
        let chroma_format_idc = self.chroma_format_idc;

        match (bit_depth_luma, chroma_format_idc) {
            (8, 0) | (8, 1) => Ok(libva::constants::VA_RT_FORMAT_YUV420),
            (8, 2) => Ok(libva::constants::VA_RT_FORMAT_YUV422),
            (8, 3) => Ok(libva::constants::VA_RT_FORMAT_YUV444),
            (10, 0) | (10, 1) => Ok(libva::constants::VA_RT_FORMAT_YUV420_10),
            (10, 2) => Ok(libva::constants::VA_RT_FORMAT_YUV422_10),
            (10, 3) => Ok(libva::constants::VA_RT_FORMAT_YUV444_10),
            (12, 0) | (12, 1) => Ok(libva::constants::VA_RT_FORMAT_YUV420_12),
            (12, 2) => Ok(libva::constants::VA_RT_FORMAT_YUV422_12),
            (12, 3) => Ok(libva::constants::VA_RT_FORMAT_YUV444_12),
            _ => Err(anyhow!(
                "unsupported bit depth/chroma format pair {}, {}",
                bit_depth_luma,
                chroma_format_idc
            )),
        }
    }

    fn min_num_surfaces(&self) -> usize {
        self.max_dpb_frames() + 4
    }

    fn coded_size(&self) -> (u32, u32) {
        (self.width, self.height)
    }

    fn visible_rect(&self) -> ((u32, u32), (u32, u32)) {
        let rect = self.visible_rectangle();

        ((rect.min.x, rect.min.y), (rect.max.x, rect.max.y))
    }
}

/// Gets the VASurfaceID for the given `picture`.
fn surface_id<M: SurfaceMemoryDescriptor>(
    handle: &Option<VADecodedHandle<M>>,
) -> libva::VASurfaceID {
    match handle {
        None => libva::constants::VA_INVALID_SURFACE,
        Some(handle) => handle.borrow().surface_id(),
    }
}

/// Fills the internal `va_pic` picture parameter with data from `h264_pic`
fn fill_va_h264_pic(
    h264_pic: &PictureData,
    surface_id: libva::VASurfaceID,
    merge_other_field: bool,
) -> libva::PictureH264 {
    let mut flags = 0;
    let frame_idx = if matches!(h264_pic.reference(), Reference::LongTerm) {
        flags |= libva::constants::VA_PICTURE_H264_LONG_TERM_REFERENCE;
        h264_pic.long_term_frame_idx
    } else {
        if matches!(h264_pic.reference(), Reference::ShortTerm { .. }) {
            flags |= libva::constants::VA_PICTURE_H264_SHORT_TERM_REFERENCE;
        }

        h264_pic.frame_num
    };

    let top_field_order_cnt;
    let bottom_field_order_cnt;

    match h264_pic.field {
        Field::Frame => {
            top_field_order_cnt = h264_pic.top_field_order_cnt;
            bottom_field_order_cnt = h264_pic.bottom_field_order_cnt;
        }
        Field::Top => {
            match (merge_other_field, h264_pic.other_field()) {
                (true, Some(other_field)) => {
                    bottom_field_order_cnt = other_field.borrow().bottom_field_order_cnt
                }
                (_, _) => {
                    flags |= libva::constants::VA_PICTURE_H264_TOP_FIELD;
                    bottom_field_order_cnt = 0;
                }
            }

            top_field_order_cnt = h264_pic.top_field_order_cnt;
        }
        Field::Bottom => {
            match (merge_other_field, h264_pic.other_field()) {
                (true, Some(other_field)) => {
                    top_field_order_cnt = other_field.borrow().top_field_order_cnt
                }
                (_, _) => {
                    flags |= libva::constants::VA_PICTURE_H264_BOTTOM_FIELD;
                    top_field_order_cnt = 0;
                }
            }

            bottom_field_order_cnt = h264_pic.bottom_field_order_cnt;
        }
    }

    libva::PictureH264::new(
        surface_id,
        frame_idx as u32,
        flags,
        top_field_order_cnt,
        bottom_field_order_cnt,
    )
}

/// Builds an invalid VaPictureH264. These pictures are used to fill empty
/// array slots there is no data to fill them with.
fn build_invalid_va_h264_pic() -> libva::PictureH264 {
    libva::PictureH264::new(
        libva::constants::VA_INVALID_ID,
        0,
        libva::constants::VA_PICTURE_H264_INVALID,
        0,
        0,
    )
}

fn build_iq_matrix(pps: &Pps) -> BufferType {
    let mut scaling_list4x4 = [[0; 16]; 6];
    let mut scaling_list8x8 = [[0; 64]; 2];

    (0..6).for_each(|i| {
        super::get_raster_from_zigzag_4x4(pps.scaling_lists_4x4()[i], &mut scaling_list4x4[i]);
    });

    (0..2).for_each(|i| {
        super::get_raster_from_zigzag_8x8(pps.scaling_lists_8x8()[i], &mut scaling_list8x8[i]);
    });

    BufferType::IQMatrix(IQMatrix::H264(IQMatrixBufferH264::new(
        scaling_list4x4,
        scaling_list8x8,
    )))
}

fn build_pic_param<M: SurfaceMemoryDescriptor>(
    hdr: &SliceHeader,
    current_picture: &PictureData,
    current_surface_id: libva::VASurfaceID,
    dpb: &Dpb<VADecodedHandle<M>>,
    sps: &Sps,
    pps: &Pps,
) -> anyhow::Result<BufferType> {
    let curr_pic = fill_va_h264_pic(current_picture, current_surface_id, false);

    let mut refs = vec![];
    let mut va_refs = vec![];

    dpb.get_short_term_refs(&mut refs);
    refs.retain(|handle| {
        let pic = handle.0.borrow();
        !pic.nonexisting && !pic.is_second_field()
    });

    for handle in &refs {
        let ref_pic = handle.0.borrow();
        let surface_id = surface_id(&handle.1);
        let pic = fill_va_h264_pic(&ref_pic, surface_id, true);
        va_refs.push(pic);
    }

    refs.clear();

    dpb.get_long_term_refs(&mut refs);
    refs.retain(|handle| {
        let pic = handle.0.borrow();
        !pic.is_second_field()
    });

    for handle in &refs {
        let ref_pic = handle.0.borrow();
        let surface_id = surface_id(&handle.1);
        let pic = fill_va_h264_pic(&ref_pic, surface_id, true);
        va_refs.push(pic);
    }

    for _ in va_refs.len()..16 {
        va_refs.push(build_invalid_va_h264_pic());
    }

    refs.clear();

    let seq_fields = libva::H264SeqFields::new(
        sps.chroma_format_idc as u32,
        sps.separate_colour_plane_flag as u32,
        sps.gaps_in_frame_num_value_allowed_flag as u32,
        sps.frame_mbs_only_flag as u32,
        sps.mb_adaptive_frame_field_flag as u32,
        sps.direct_8x8_inference_flag as u32,
        (sps.level_idc >= Level::L3_1) as u32, /* see A.3.3.2 */
        sps.log2_max_frame_num_minus4 as u32,
        sps.pic_order_cnt_type as u32,
        sps.log2_max_pic_order_cnt_lsb_minus4 as u32,
        sps.delta_pic_order_always_zero_flag as u32,
    );
    let interlaced = !sps.frame_mbs_only_flag as u32;
    let picture_height_in_mbs_minus1 = ((sps.pic_height_in_map_units_minus1 + 1) << interlaced) - 1;

    let pic_fields = libva::H264PicFields::new(
        pps.entropy_coding_mode_flag() as u32,
        pps.weighted_pred_flag() as u32,
        pps.weighted_bipred_idc() as u32,
        pps.transform_8x8_mode_flag() as u32,
        hdr.field_pic_flag as u32,
        pps.constrained_intra_pred_flag() as u32,
        pps.bottom_field_pic_order_in_frame_present_flag() as u32,
        pps.deblocking_filter_control_present_flag() as u32,
        pps.redundant_pic_cnt_present_flag() as u32,
        (current_picture.nal_ref_idc != 0) as u32,
    );

    let va_refs = va_refs.try_into();
    let va_refs = match va_refs {
        Ok(va_refs) => va_refs,
        Err(_) => {
            panic!("Bug: wrong number of references, expected 16");
        }
    };

    let pic_param = PictureParameterBufferH264::new(
        curr_pic,
        va_refs,
        u16::try_from(sps.pic_width_in_mbs_minus1)?,
        u16::try_from(picture_height_in_mbs_minus1)?,
        sps.bit_depth_luma_minus8,
        sps.bit_depth_chroma_minus8,
        u8::try_from(sps.max_num_ref_frames)?,
        &seq_fields,
        0, /* FMO not supported by VA */
        0, /* FMO not supported by VA */
        0, /* FMO not supported by VA */
        pps.pic_init_qp_minus26(),
        pps.pic_init_qs_minus26(),
        pps.chroma_qp_index_offset(),
        pps.second_chroma_qp_index_offset(),
        &pic_fields,
        hdr.frame_num,
    );

    Ok(BufferType::PictureParameter(PictureParameter::H264(
        pic_param,
    )))
}

fn fill_ref_pic_list<M: SurfaceMemoryDescriptor>(
    ref_list_x: &[DpbEntry<VADecodedHandle<M>>],
) -> [libva::PictureH264; 32] {
    let mut va_pics = vec![];

    for handle in ref_list_x {
        let pic = handle.0.borrow();
        let surface_id = surface_id(&handle.1);
        let merge = matches!(pic.field, Field::Frame);
        let va_pic = fill_va_h264_pic(&pic, surface_id, merge);

        va_pics.push(va_pic);
    }

    for _ in va_pics.len()..32 {
        va_pics.push(build_invalid_va_h264_pic());
    }

    let va_pics: [libva::PictureH264; 32] = match va_pics.try_into() {
        Ok(va_pics) => va_pics,
        Err(e) => panic!(
            "Bug: wrong number of references, expected 32, got {:?}",
            e.len()
        ),
    };

    va_pics
}

fn build_slice_param<M: SurfaceMemoryDescriptor>(
    hdr: &SliceHeader,
    slice_size: usize,
    ref_list_0: &[DpbEntry<VADecodedHandle<M>>],
    ref_list_1: &[DpbEntry<VADecodedHandle<M>>],
    sps: &Sps,
    pps: &Pps,
) -> anyhow::Result<BufferType> {
    let ref_list_0 = fill_ref_pic_list(ref_list_0);
    let ref_list_1 = fill_ref_pic_list(ref_list_1);
    let pwt = &hdr.pred_weight_table;

    let mut luma_weight_l0_flag = false;
    let mut chroma_weight_l0_flag = false;
    let mut luma_weight_l0 = [0i16; 32];
    let mut luma_offset_l0 = [0i16; 32];
    let mut chroma_weight_l0: [[i16; 2]; 32] = [[0i16; 2]; 32];
    let mut chroma_offset_l0: [[i16; 2]; 32] = [[0i16; 2]; 32];

    let mut luma_weight_l1_flag = false;
    let mut chroma_weight_l1_flag = false;
    let mut luma_weight_l1 = [0i16; 32];
    let mut luma_offset_l1 = [0i16; 32];
    let mut chroma_weight_l1: [[i16; 2]; 32] = [[0i16; 2]; 32];
    let mut chroma_offset_l1: [[i16; 2]; 32] = [[0i16; 2]; 32];

    let mut fill_l0 = false;
    let mut fill_l1 = false;

    if pps.weighted_pred_flag() && (hdr.slice_type.is_p() || hdr.slice_type.is_sp()) {
        fill_l0 = true;
    } else if pps.weighted_bipred_idc() == 1 && hdr.slice_type.is_b() {
        fill_l0 = true;
        fill_l1 = true;
    }

    if fill_l0 {
        luma_weight_l0_flag = true;

        for i in 0..=hdr.num_ref_idx_l0_active_minus1 as usize {
            luma_weight_l0[i] = pwt.luma_weight_l0()[i];
            luma_offset_l0[i] = i16::from(pwt.luma_offset_l0()[i]);
        }

        chroma_weight_l0_flag = sps.chroma_array_type != 0;
        if chroma_weight_l0_flag {
            for i in 0..=hdr.num_ref_idx_l0_active_minus1 as usize {
                for j in 0..2 {
                    chroma_weight_l0[i][j] = pwt.chroma_weight_l0()[i][j];
                    chroma_offset_l0[i][j] = i16::from(pwt.chroma_offset_l0()[i][j]);
                }
            }
        }
    }

    if fill_l1 {
        luma_weight_l1_flag = true;

        luma_weight_l1[..(hdr.num_ref_idx_l1_active_minus1 as usize + 1)].clone_from_slice(
            &pwt.luma_weight_l1()[..(hdr.num_ref_idx_l1_active_minus1 as usize + 1)],
        );
        luma_offset_l1[..(hdr.num_ref_idx_l1_active_minus1 as usize + 1)].clone_from_slice(
            &pwt.luma_offset_l1()[..(hdr.num_ref_idx_l1_active_minus1 as usize + 1)],
        );

        chroma_weight_l1_flag = sps.chroma_array_type != 0;
        if chroma_weight_l1_flag {
            for i in 0..=hdr.num_ref_idx_l1_active_minus1 as usize {
                for j in 0..2 {
                    chroma_weight_l1[i][j] = pwt.chroma_weight_l1()[i][j];
                    chroma_offset_l1[i][j] = i16::from(pwt.chroma_offset_l1()[i][j]);
                }
            }
        }
    }

    let slice_param = libva::SliceParameterBufferH264::new(
        slice_size as u32,
        0,
        libva::constants::VA_SLICE_DATA_FLAG_ALL,
        hdr.header_bit_size as u16,
        hdr.first_mb_in_slice as u16,
        hdr.slice_type as u8,
        hdr.direct_spatial_mv_pred_flag as u8,
        hdr.num_ref_idx_l0_active_minus1,
        hdr.num_ref_idx_l1_active_minus1,
        hdr.cabac_init_idc,
        hdr.slice_qp_delta,
        hdr.disable_deblocking_filter_idc,
        hdr.slice_alpha_c0_offset_div2,
        hdr.slice_beta_offset_div2,
        ref_list_0,
        ref_list_1,
        pwt.luma_log2_weight_denom(),
        pwt.chroma_log2_weight_denom(),
        luma_weight_l0_flag as u8,
        luma_weight_l0,
        luma_offset_l0,
        chroma_weight_l0_flag as u8,
        chroma_weight_l0,
        chroma_offset_l0,
        luma_weight_l1_flag as u8,
        luma_weight_l1,
        luma_offset_l1,
        chroma_weight_l1_flag as u8,
        chroma_weight_l1,
        chroma_offset_l1,
    );

    Ok(BufferType::SliceParameter(SliceParameter::H264(
        slice_param,
    )))
}

impl<M: SurfaceMemoryDescriptor + 'static> StatelessDecoderBackendPicture<H264>
    for VaapiBackend<M>
{
    type Picture = VaapiPicture<M>;
}

impl<M: SurfaceMemoryDescriptor + 'static> StatelessH264DecoderBackend for VaapiBackend<M> {
    fn new_sequence(&mut self, sps: &Rc<Sps>) -> StatelessBackendResult<()> {
        self.new_sequence(sps)
    }

    fn start_picture(
        &mut self,
        picture: &mut Self::Picture,
        picture_data: &PictureData,
        sps: &Sps,
        pps: &Pps,
        dpb: &Dpb<Self::Handle>,
        hdr: &SliceHeader,
    ) -> StatelessBackendResult<()> {
        let metadata = self.metadata_state.get_parsed()?;
        let context = &metadata.context;

        let surface_id = picture.surface().id();

        let pic_param = build_pic_param(hdr, picture_data, surface_id, dpb, sps, pps)?;
        let pic_param = context
            .create_buffer(pic_param)
            .context("while creating picture parameter buffer")?;

        let iq_matrix = build_iq_matrix(pps);
        let iq_matrix = context
            .create_buffer(iq_matrix)
            .context("while creating IQ matrix buffer")?;

        picture.add_buffer(pic_param);
        picture.add_buffer(iq_matrix);

        Ok(())
    }

    fn decode_slice(
        &mut self,
        picture: &mut Self::Picture,
        slice: &Slice,
        sps: &Sps,
        pps: &Pps,
        _: &Dpb<Self::Handle>,
        ref_pic_list0: &[DpbEntry<Self::Handle>],
        ref_pic_list1: &[DpbEntry<Self::Handle>],
    ) -> StatelessBackendResult<()> {
        let metadata = self.metadata_state.get_parsed()?;
        let context = &metadata.context;

        let slice_param = context
            .create_buffer(build_slice_param(
                slice.header(),
                slice.nalu().size(),
                ref_pic_list0,
                ref_pic_list1,
                sps,
                pps,
            )?)
            .context("while creating slice params buffer")?;

        picture.add_buffer(slice_param);

        let slice_data = context
            .create_buffer(BufferType::SliceData(Vec::from(slice.nalu().as_ref())))
            .context("while creating slice data buffer")?;

        picture.add_buffer(slice_data);

        Ok(())
    }

    fn submit_picture(&mut self, picture: Self::Picture) -> StatelessBackendResult<Self::Handle> {
        self.process_picture::<H264>(picture)
    }

    fn new_picture(
        &mut self,
        _: &PictureData,
        timestamp: u64,
    ) -> StatelessBackendResult<Self::Picture> {
        let metadata = self.metadata_state.get_parsed()?;
        let surface = self
            .surface_pool
            .borrow_mut()
            .get_surface(&self.surface_pool)
            .ok_or(StatelessBackendError::OutOfResources)?;

        Ok(VaPicture::new(
            timestamp,
            Rc::clone(&metadata.context),
            surface,
        ))
    }

    fn new_field_picture(
        &mut self,
        _: &PictureData,
        timestamp: u64,
        first_field: &Self::Handle,
    ) -> StatelessBackendResult<Self::Picture> {
        // Block on the first field if it is not ready yet.
        first_field.sync()?;

        // Decode to the same surface as the first field picture.
        let first_va_handle = first_field.borrow();
        let va_picture = first_va_handle
            .picture()
            .expect("no valid backend handle after blocking on it");

        Ok(VaPicture::new_from_same_surface(timestamp, va_picture))
    }
}

impl<M: SurfaceMemoryDescriptor + 'static> StatelessDecoder<H264, VaapiBackend<M>> {
    // Creates a new instance of the decoder using the VAAPI backend.
    pub fn new_vaapi<S>(display: Rc<Display>, blocking_mode: BlockingMode) -> Self
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

    use crate::codec::h264::parser::Nalu;
    use crate::decoder::stateless::h264::H264;
    use crate::decoder::stateless::tests::test_decode_stream;
    use crate::decoder::stateless::tests::TestStream;
    use crate::decoder::stateless::StatelessDecoder;
    use crate::decoder::BlockingMode;
    use crate::utils::simple_playback_loop;
    use crate::utils::simple_playback_loop_owned_frames;
    use crate::utils::NalIterator;
    use crate::DecodedFormat;

    /// Run `test` using the vaapi decoder, in both blocking and non-blocking modes.
    fn test_decoder_vaapi(
        test: &TestStream,
        output_format: DecodedFormat,
        blocking_mode: BlockingMode,
    ) {
        let display = Display::open().unwrap();
        let decoder = StatelessDecoder::<H264, _>::new_vaapi::<()>(display, blocking_mode);

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
        use crate::decoder::stateless::h264::tests::DECODE_64X64_PROGRESSIVE_I;
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
        use crate::decoder::stateless::h264::tests::DECODE_64X64_PROGRESSIVE_I;
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
        use crate::decoder::stateless::h264::tests::DECODE_64X64_PROGRESSIVE_I_P;
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
        use crate::decoder::stateless::h264::tests::DECODE_64X64_PROGRESSIVE_I_P;
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
        use crate::decoder::stateless::h264::tests::DECODE_64X64_PROGRESSIVE_I_P_B_P;
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
        use crate::decoder::stateless::h264::tests::DECODE_64X64_PROGRESSIVE_I_P_B_P;
        test_decoder_vaapi(
            &DECODE_64X64_PROGRESSIVE_I_P_B_P,
            DecodedFormat::NV12,
            BlockingMode::NonBlocking,
        );
    }

    #[test]
    // Ignore this test by default as it requires libva-compatible hardware.
    #[ignore]
    fn test_64x64_progressive_i_p_b_p_high_block() {
        use crate::decoder::stateless::h264::tests::DECODE_64X64_PROGRESSIVE_I_P_B_P_HIGH;
        test_decoder_vaapi(
            &DECODE_64X64_PROGRESSIVE_I_P_B_P_HIGH,
            DecodedFormat::NV12,
            BlockingMode::Blocking,
        );
    }

    #[test]
    // Ignore this test by default as it requires libva-compatible hardware.
    #[ignore]
    fn test_64x64_progressive_i_p_b_p_high_nonblock() {
        use crate::decoder::stateless::h264::tests::DECODE_64X64_PROGRESSIVE_I_P_B_P_HIGH;
        test_decoder_vaapi(
            &DECODE_64X64_PROGRESSIVE_I_P_B_P_HIGH,
            DecodedFormat::NV12,
            BlockingMode::NonBlocking,
        );
    }

    #[test]
    // Ignore this test by default as it requires libva-compatible hardware.
    #[ignore]
    fn test_25fps_block() {
        use crate::decoder::stateless::h264::tests::DECODE_TEST_25FPS;
        test_decoder_vaapi(
            &DECODE_TEST_25FPS,
            DecodedFormat::NV12,
            BlockingMode::Blocking,
        );
    }

    #[test]
    // Ignore this test by default as it requires libva-compatible hardware.
    #[ignore]
    fn test_25fps_nonblock() {
        use crate::decoder::stateless::h264::tests::DECODE_TEST_25FPS;
        test_decoder_vaapi(
            &DECODE_TEST_25FPS,
            DecodedFormat::NV12,
            BlockingMode::NonBlocking,
        );
    }

    #[test]
    // Ignore this test by default as it requires libva-compatible hardware.
    #[ignore]
    fn test_25fps_interlaced_block() {
        use crate::decoder::stateless::h264::tests::DECODE_TEST_25FPS_INTERLACED;
        test_decoder_vaapi(
            &DECODE_TEST_25FPS_INTERLACED,
            DecodedFormat::NV12,
            BlockingMode::Blocking,
        );
    }

    #[test]
    // Ignore this test by default as it requires libva-compatible hardware.
    #[ignore]
    fn test_25fps_interlaced_nonblock() {
        use crate::decoder::stateless::h264::tests::DECODE_TEST_25FPS_INTERLACED;
        test_decoder_vaapi(
            &DECODE_TEST_25FPS_INTERLACED,
            DecodedFormat::NV12,
            BlockingMode::NonBlocking,
        );
    }
}
