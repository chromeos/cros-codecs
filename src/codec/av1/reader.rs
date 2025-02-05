// Copyright 2023 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use crate::bitstream_utils::BitReader;

use crate::codec::av1::helpers;

use super::parser::AnnexBState;

pub(crate) struct Reader<'a>(pub BitReader<'a>);

impl<'a> Reader<'a> {
    pub fn new(data: &'a [u8]) -> Self {
        Self(BitReader::new(data, false))
    }

    /// Implements uvlc(): Variable length unsigned n-bit number appearing
    /// directly in the bitstream. See 4.10.3
    pub fn read_uvlc(&mut self) -> Result<u32, String> {
        let mut leading_zeroes = 0;
        loop {
            let done = self.0.read_bit()?;

            if done {
                break;
            }

            leading_zeroes += 1;
        }

        if leading_zeroes >= 32 {
            return Ok(u32::MAX);
        }

        let value = self.0.read_bits::<u32>(leading_zeroes)?;
        Ok(value + (1 << leading_zeroes) - 1)
    }

    /// Implements leb128(): Unsigned integer represented by a variable number
    /// of little-endian bytes. See 4.10.5
    pub fn read_leb128(&mut self) -> Result<u32, String> {
        let mut value = 0u64;

        for i in 0..8 {
            let byte = u64::from(self.0.read_bits_aligned::<u32>(8)?);
            value |= (byte & 0x7f) << (i * 7);

            if byte & 0x80 == 0 {
                break;
            }
        }

        Ok(value as u32)
    }

    /// Implements su(n): Signed integer converted from an n bits unsigned
    /// integer in the bitstream. (The unsigned integer corresponds to the
    /// bottom n bits of the signed integer.). See 4.10.6
    pub fn read_su(&mut self, num_bits: usize) -> Result<i32, String> {
        let mut value: i32 = self
            .0
            .read_bits::<u32>(num_bits)?
            .try_into()
            .map_err(|_| String::from("Read more than 31 signed bits!"))?;
        let sign_mask = 1 << (num_bits - 1);

        if (value & sign_mask) != 0 {
            value -= 2 * sign_mask;
        }

        Ok(value)
    }

    /// Implements ns(n): Unsigned encoded integer with maximum number of values
    /// n (i.e. output in range 0..n-1). See 4.10.7
    pub fn read_ns(&mut self, num_bits: usize) -> Result<u32, String> {
        let w = helpers::floor_log2(num_bits as u32) + 1;
        let m = (1 << w) - num_bits as u32;
        let v = self.0.read_bits::<u32>(
            usize::try_from(w).map_err(|_| String::from("Invalid num_bits"))? - 1,
        )?;

        if v < m.into() {
            return Ok(v);
        }

        let extra_bit = self.0.read_bit()?;
        Ok((v << 1) - u32::from(m) + u32::from(extra_bit))
    }

    /// Implements 5.9.13: Delta quantizer syntax.
    pub fn read_delta_q(&mut self) -> Result<i32, String> {
        let delta_coded = self.0.read_bit()?;

        if delta_coded {
            self.read_su(7)
        } else {
            Ok(0)
        }
    }

    pub fn more_data_in_bitstream(&mut self) -> bool {
        self.0.num_bits_left() > 0
    }

    pub(crate) fn consumed(&self, start_pos: u32) -> u32 {
        (self.0.position() / 8) as u32 - start_pos
    }

    /// Get the length of the current OBU in AnnexB format.
    pub fn current_annexb_obu_length(
        &mut self,
        annexb_state: &mut AnnexBState,
    ) -> Result<Option<usize>, String> {
        if !self.more_data_in_bitstream() {
            return Ok(None);
        }

        #[allow(clippy::comparison_chain)]
        if annexb_state.temporal_unit_consumed == annexb_state.temporal_unit_size {
            annexb_state.temporal_unit_size = 0;
        } else if annexb_state.temporal_unit_consumed > annexb_state.temporal_unit_size {
            return Err(format!(
                "temporal_unit_size is {} but we consumed {} bytes",
                annexb_state.temporal_unit_size, annexb_state.temporal_unit_consumed,
            ));
        }

        if annexb_state.temporal_unit_size == 0 {
            annexb_state.temporal_unit_size = self.read_leb128()?;
            if annexb_state.temporal_unit_size == 0 {
                return Ok(None);
            }
        }

        let start_pos = self.consumed(0);

        #[allow(clippy::comparison_chain)]
        if annexb_state.frame_unit_consumed == annexb_state.frame_unit_size {
            annexb_state.frame_unit_size = 0;
        } else if annexb_state.frame_unit_consumed > annexb_state.frame_unit_size {
            return Err(format!(
                "frame_unit_size is {} but we consumed {} bytes",
                annexb_state.frame_unit_size, annexb_state.frame_unit_consumed,
            ));
        }

        if annexb_state.frame_unit_size == 0 {
            annexb_state.frame_unit_size = self.read_leb128()?;
            if annexb_state.frame_unit_size == 0 {
                return Ok(None);
            }
            annexb_state.temporal_unit_consumed += self.consumed(start_pos);
        }

        let start_pos = self.consumed(0);
        let obu_length = self.read_leb128()?;
        let consumed = self.consumed(start_pos);

        annexb_state.temporal_unit_consumed += consumed;
        annexb_state.frame_unit_consumed += consumed;

        Ok(Some(obu_length.try_into().unwrap()))
    }

    /// Implements 5.3.4.
    pub fn read_trailing_bits(&mut self, mut num_bits: u64) -> Result<(), String> {
        let trailing_one_bit = self.0.read_bit()?;
        num_bits -= 1;

        if !trailing_one_bit {
            return Err("bad padding: trailing_one_bit is not set".into());
        }

        while num_bits > 0 {
            let trailing_zero_bit = self.0.read_bit()?;
            if trailing_zero_bit {
                return Err("bad padding: trailing_zero_bit is set".into());
            }
            num_bits -= 1;
        }

        Ok(())
    }

    fn decode_subexp(&mut self, num_syms: i32) -> Result<u32, String> {
        let mut i = 0;
        let mut mk = 0;
        let k = 3;

        loop {
            let b2 = if i != 0 { k + i - 1 } else { k };
            let a = 1 << b2;
            if num_syms <= mk + 3 * a {
                let num_bits = num_syms - mk;
                let subexp_final_bits = self.read_ns(num_bits as usize)?;
                return Ok(subexp_final_bits);
            } else {
                let subexp_more_bits = self.0.read_bit()?;
                if subexp_more_bits {
                    i += 1;
                    mk += a;
                } else {
                    let num_bits = b2 as usize;
                    let subexp_bits = self.0.read_bits::<u32>(num_bits)?;
                    return Ok(subexp_bits + mk as u32);
                }
            }
        }
    }

    /// Implements 5.9.27.
    pub fn decode_unsigned_subexp_with_ref(&mut self, mx: i32, r: i32) -> Result<u32, String> {
        let v = self.decode_subexp(mx)?;
        if (r << 1) <= mx {
            Ok(helpers::inverse_recenter(r, v.try_into().unwrap()).try_into().unwrap())
        } else {
            let res = mx - 1 - helpers::inverse_recenter(mx - 1 - r, v.try_into().unwrap());
            Ok(res.try_into().unwrap())
        }
    }

    /// Implements 5.9.26.
    pub fn decode_signed_subexp_with_ref(
        &mut self,
        low: i32,
        high: i32,
        r: i32,
    ) -> Result<i32, String> {
        let x = self.decode_unsigned_subexp_with_ref(high - low, r - low)?;
        Ok(i32::try_from(x).unwrap() + low)
    }

    /// Implements 5.3.5 Byte alignment syntax
    pub fn byte_alignment(&mut self) -> Result<(), String> {
        while (self.0.position() & 7) != 0 {
            self.0.read_bit()?;
        }

        Ok(())
    }
}

impl<'a> Clone for Reader<'a> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}
