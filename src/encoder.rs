// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::fmt;

pub mod av1;
pub mod h264;
pub mod h265;
pub mod vp8;
pub mod vp9;

pub mod stateful;
pub mod stateless;

use crate::codec::av1::synthesizer::SynthesizerError as AV1SynthesizerError;
use crate::codec::h264::synthesizer::SynthesizerError as H264SynthesizerError;
use crate::encoder::stateful::StatefulBackendError;
use crate::encoder::stateless::StatelessBackendError;
use crate::FrameLayout;

/// Specifies the encoder operation
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RateControl {
    /// The encoder shall maintain the constant bitrate
    ConstantBitrate(u64),

    /// The encoder shall maintain codec specific quality parameter constant (eg. QP for H.264)
    /// disregarding bitrate.
    ConstantQuality(u32),
}

impl RateControl {
    #[allow(dead_code)]
    pub(crate) fn is_same_variant(left: &Self, right: &Self) -> bool {
        std::mem::discriminant(left) == std::mem::discriminant(right)
    }

    pub(crate) fn bitrate_target(&self) -> Option<u64> {
        match self {
            RateControl::ConstantBitrate(target) => Some(*target),
            RateControl::ConstantQuality(_) => None,
        }
    }
}

#[derive(Clone)]
pub enum PredictionStructure {
    /// Simplest prediction structure, suitable eg. for RTC. Interframe is produced at the start of
    /// the stream and every time when `limit` frames are reached. Following interframe frames
    /// are frames relying solely on the last frame.
    LowDelay { limit: u16 },
}

/// Dynamic parameters of the encoded stream that client may choose to change during the encoding
/// session without recreating the entire encoder instance.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Tunings {
    /// The stream's [`RateControl`]
    pub rate_control: RateControl,
    /// Stream framerate in frames per second
    pub framerate: u32,
    /// Minimum value of codec specific quality parameter constant (eg. QP for H.264)
    pub min_quality: u32,
    /// Maximum value of codec specific quality parameter constant (eg. QP for H.264)
    pub max_quality: u32,
}

impl Default for Tunings {
    fn default() -> Self {
        Self {
            rate_control: RateControl::ConstantBitrate(200_000),
            framerate: 30,
            min_quality: 0,
            max_quality: u32::MAX,
        }
    }
}

/// Encoder's input metadata
#[derive(Debug, Clone)]
pub struct FrameMetadata {
    pub timestamp: u64,
    pub layout: FrameLayout,
    pub force_keyframe: bool,
}

/// Encoder's coded output with contained frame.
pub struct CodedBitstreamBuffer {
    /// [`FrameMetadata`] of the frame that is compressed in [`Self::bitstream`]
    pub metadata: FrameMetadata,

    /// Bitstream with compressed frame together with optionally other compressed control messages
    pub bitstream: Vec<u8>,
}

impl CodedBitstreamBuffer {
    pub fn new(metadata: FrameMetadata, bitstream: Vec<u8>) -> Self {
        Self { metadata, bitstream }
    }
}

impl From<CodedBitstreamBuffer> for Vec<u8> {
    fn from(value: CodedBitstreamBuffer) -> Self {
        value.bitstream
    }
}

#[derive(Debug)]
pub enum EncodeError {
    Unsupported,
    InvalidInternalState,
    StatelessBackendError(StatelessBackendError),
    StatefulBackendError(StatefulBackendError),
    H264SynthesizerError(H264SynthesizerError),
    AV1SynthesizerError(AV1SynthesizerError),
}

impl fmt::Display for EncodeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            EncodeError::Unsupported => write!(f, "unsupported"),
            EncodeError::InvalidInternalState => {
                write!(f, "invalid internal state. This is likely a bug.")
            }
            EncodeError::StatelessBackendError(x) => write!(f, "{}", x.to_string()),
            EncodeError::StatefulBackendError(x) => write!(f, "{}", x.to_string()),
            EncodeError::H264SynthesizerError(x) => write!(f, "{}", x.to_string()),
            EncodeError::AV1SynthesizerError(x) => write!(f, "{}", x.to_string()),
        }
    }
}

impl From<StatelessBackendError> for EncodeError {
    fn from(err: StatelessBackendError) -> Self {
        EncodeError::StatelessBackendError(err)
    }
}

impl From<StatefulBackendError> for EncodeError {
    fn from(err: StatefulBackendError) -> Self {
        EncodeError::StatefulBackendError(err)
    }
}

impl From<H264SynthesizerError> for EncodeError {
    fn from(err: H264SynthesizerError) -> Self {
        EncodeError::H264SynthesizerError(err)
    }
}

impl From<AV1SynthesizerError> for EncodeError {
    fn from(err: AV1SynthesizerError) -> Self {
        EncodeError::AV1SynthesizerError(err)
    }
}

pub type EncodeResult<T> = Result<T, EncodeError>;

/// Generic video encoder interface.
pub trait VideoEncoder<Handle> {
    /// Changes dynamic parameters (aka [`Tunings`]) of the encoded stream. The change may not be
    /// effective right away. Depending on the used prediction structure, the `Predictor` may
    /// choose to delay the change until entire or a some part of the structure had been encoded.
    ///
    /// Note: Currently changing the variant of [`RateControl`] is not supported.
    fn tune(&mut self, tunings: Tunings) -> EncodeResult<()>;

    /// Enqueues the frame for encoding. The implementation will drop the handle after it is no
    /// longer be needed. The encoder is not required to immediately start processing the frame
    /// and yield output bitstream. It is allowed to hold frames until certain conditions are met
    /// eg. for specified prediction structures or referencing in order to further optimize
    /// the compression rate of the bitstream.
    fn encode(&mut self, meta: FrameMetadata, handle: Handle) -> Result<(), EncodeError>;

    /// Drains the encoder. This means that encoder is required to finish processing of all the
    /// frames in the internal queue and yield output bitstream by the end of the call. The output
    /// bitstream then can be polled using [`Self::poll`] function.
    ///
    /// Drain does not enforce the flush of the internal state, ie. the enqueued frame handles
    /// do not have to be returned to user (dropped) and key frame is not enforced on the next
    /// frame.
    fn drain(&mut self) -> EncodeResult<()>;

    /// Polls on the encoder for the available output bitstream with compressed frames that where
    /// submitted with [`Self::encode`].
    ///
    /// The call may also trigger a further processing aside of returning output. Therefore it
    /// *recommended* that this function is called frequently.
    fn poll(&mut self) -> EncodeResult<Option<CodedBitstreamBuffer>>;
}

pub fn simple_encode_loop<H>(
    encoder: &mut impl VideoEncoder<H>,
    frame_producer: &mut impl Iterator<Item = (FrameMetadata, H)>,
    mut coded_consumer: impl FnMut(CodedBitstreamBuffer),
) -> EncodeResult<()> {
    for (meta, handle) in frame_producer.by_ref() {
        encoder.encode(meta, handle)?;
        while let Some(coded) = encoder.poll()? {
            coded_consumer(coded);
        }
    }

    encoder.drain()?;
    while let Some(coded) = encoder.poll()? {
        coded_consumer(coded);
    }

    Ok(())
}

#[cfg(test)]
pub(crate) mod tests {
    #[cfg(feature = "v4l2")]
    use crate::encoder::FrameMetadata;
    #[cfg(feature = "v4l2")]
    use crate::utils::UserPtrFrame;
    #[cfg(feature = "v4l2")]
    use crate::Fourcc;
    #[cfg(feature = "v4l2")]
    use crate::FrameLayout;

    pub fn get_test_frame_t(ts: u64, max_ts: u64) -> f32 {
        2.0 * std::f32::consts::PI * (ts as f32) / (max_ts as f32)
    }

    pub fn gen_test_frame<F>(frame_width: usize, frame_height: usize, t: f32, mut set_pix: F)
    where
        F: FnMut(usize, usize, [f32; 3]),
    {
        let width = frame_width as f32;
        let height = frame_height as f32;
        let (sin, cos) = f32::sin_cos(t);
        let (sin2, cos2) = (sin.powi(2), cos.powi(2));

        // Pick the dot position
        let dot_col = height * (1.1 + 2.0 * sin * cos) / 2.2;
        let dot_row = width * (1.1 + sin) / 2.2;
        let dot_size2 = (width.min(height) * 0.05).powi(2);

        // Luma
        for frame_row in 0..frame_height {
            #[allow(clippy::needless_range_loop)]
            for frame_col in 0..frame_width {
                let row = frame_row as f32;
                let col = frame_col as f32;

                let dist = (dot_col - col).powi(2) + (dot_row - row).powi(2);

                let y = if dist < dot_size2 { 0.0 } else { (row + col) / (width + height) };

                let (u, v) = if dist < dot_size2 {
                    (0.5, 0.5)
                } else {
                    ((row / width) * sin2, (col / height) * cos2)
                };

                set_pix(frame_col, frame_row, [y, u, v]);
            }
        }
    }

    pub fn fill_test_frame_nm12(
        width: usize,
        height: usize,
        strides: [usize; 2],
        t: f32,
        y_plane: &mut [u8],
        uv_plane: &mut [u8],
    ) {
        gen_test_frame(width, height, t, |col, row, yuv| {
            /// Maximum value of color component for NV12
            const MAX_COMP_VAL: f32 = 0xff as f32;

            let (y, u, v) = (
                (yuv[0] * MAX_COMP_VAL).clamp(0.0, MAX_COMP_VAL) as u8,
                (yuv[1] * MAX_COMP_VAL).clamp(0.0, MAX_COMP_VAL) as u8,
                (yuv[2] * MAX_COMP_VAL).clamp(0.0, MAX_COMP_VAL) as u8,
            );
            let y_pos = row * strides[0] + col;

            y_plane[y_pos] = y;

            // Subsample with upper left pixel
            if col % 2 == 0 && row % 2 == 0 {
                let u_pos = (row / 2) * strides[1] + col;
                let v_pos = u_pos + 1;

                uv_plane[u_pos] = u;
                uv_plane[v_pos] = v;
            }
        });
    }

    pub fn fill_test_frame_nv12(
        width: usize,
        height: usize,
        strides: [usize; 2],
        offsets: [usize; 2],
        t: f32,
        raw: &mut [u8],
    ) {
        let (y_plane, uv_plane) = raw.split_at_mut(offsets[1]);
        let y_plane = &mut y_plane[offsets[0]..];

        fill_test_frame_nm12(width, height, strides, t, y_plane, uv_plane)
    }

    pub fn fill_test_frame_p010(
        width: usize,
        height: usize,
        strides: [usize; 2],
        offsets: [usize; 2],
        t: f32,
        raw: &mut [u8],
    ) {
        gen_test_frame(width, height, t, |col, row, yuv| {
            /// Maximum value of color component for P010
            const MAX_COMP_VAL: f32 = 0x3ff as f32;

            let (y, u, v) = (
                (yuv[0] * MAX_COMP_VAL).clamp(0.0, MAX_COMP_VAL) as u16,
                (yuv[1] * MAX_COMP_VAL).clamp(0.0, MAX_COMP_VAL) as u16,
                (yuv[2] * MAX_COMP_VAL).clamp(0.0, MAX_COMP_VAL) as u16,
            );
            let y_pos = offsets[0] + row * strides[0] + 2 * col;

            raw[y_pos] = ((y << 6) & 0xa0) as u8;
            raw[y_pos + 1] = (y >> 2) as u8;

            // Subsample with upper left pixel
            if col % 2 == 0 && row % 2 == 0 {
                let u_pos = offsets[1] + (row / 2) * strides[1] + 2 * col;
                let v_pos = u_pos + 2;

                raw[u_pos] = ((u << 6) & 0xa0) as u8;
                raw[u_pos + 1] = (u >> 2) as u8;
                raw[v_pos] = ((v << 6) & 0xa0) as u8;
                raw[v_pos + 1] = (v >> 2) as u8;
            }
        });
    }

    #[cfg(feature = "v4l2")]
    pub fn userptr_test_frame_generator(
        frame_count: u64,
        layout: FrameLayout,
        buffer_size: usize,
    ) -> impl Iterator<Item = (FrameMetadata, UserPtrFrame)> {
        (0..frame_count).map(move |timestamp| {
            let frame = UserPtrFrame::alloc(layout.clone(), buffer_size);

            let t = get_test_frame_t(timestamp, frame_count);

            if (frame.layout.format.0 == Fourcc::from(b"NM12")
                || frame.layout.format.0 == Fourcc::from(b"NV12"))
                && frame.layout.planes.len() == 2
                && frame.buffers.len() == 2
            {
                // SAFETY: Slice matches the allocation
                let y_plane = unsafe {
                    std::slice::from_raw_parts_mut(
                        frame.buffers[frame.layout.planes[0].buffer_index],
                        frame.mem_layout.size(),
                    )
                };
                // SAFETY: Slice matches the allocation
                let uv_plane = unsafe {
                    std::slice::from_raw_parts_mut(
                        frame.buffers[frame.layout.planes[1].buffer_index],
                        frame.mem_layout.size(),
                    )
                };

                fill_test_frame_nm12(
                    frame.layout.size.width as usize,
                    frame.layout.size.height as usize,
                    [frame.layout.planes[0].stride, frame.layout.planes[1].stride],
                    t,
                    y_plane,
                    uv_plane,
                );
            } else if frame.layout.format.0 == Fourcc::from(b"NV12")
                && frame.layout.planes.len() == 1
                && frame.buffers.len() == 1
            {
                // SAFETY: Slice matches the allocation
                let raw = unsafe {
                    std::slice::from_raw_parts_mut(
                        frame.buffers[frame.layout.planes[0].buffer_index]
                            .add(frame.layout.planes[0].offset),
                        frame.mem_layout.size(),
                    )
                };

                let uv_stride = frame.layout.planes[0].stride;
                let uv_offset = frame.layout.planes[0].stride * (frame.layout.size.height as usize);

                fill_test_frame_nv12(
                    frame.layout.size.width as usize,
                    frame.layout.size.height as usize,
                    [frame.layout.planes[0].stride, uv_stride],
                    [frame.layout.planes[0].offset, uv_offset],
                    t,
                    raw,
                );
            } else {
                panic!("Unrecognized frame layout used during test");
            }

            let meta =
                FrameMetadata { timestamp, layout: frame.layout.clone(), force_keyframe: false };

            (meta, frame)
        })
    }
}
