// Copyright 2023 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use anyhow::anyhow;
use bitreader::BitReader;

use crate::codec::av1::helpers;

use super::parser::AnnexBState;

pub struct Reader<'a>(BitReader<'a>);

impl<'a> Reader<'a> {
    pub fn new(data: &'a [u8]) -> Self {
        Self(BitReader::new(data))
    }

    /// Read a single bit from the spec. Implements f(1) to return a bool for
    /// convenience.
    pub fn read_bit(&mut self) -> anyhow::Result<bool> {
        self.0.read_bool().map_err(|e| anyhow!(e))
    }

    /// Implements f(n): Unsigned n-bit number appearing directly in the
    /// bitstream. The bits are read from high to low order. See 4.10.2
    pub fn read_bits(&mut self, num_bits: u8) -> anyhow::Result<u32> {
        self.0.read_u32(num_bits).map_err(|e| anyhow!(e))
    }

    /// Implements uvlc(): Variable length unsigned n-bit number appearing
    /// directly in the bitstream. See 4.10.3
    pub fn read_ulvc(&mut self) -> anyhow::Result<u32> {
        let mut leading_zeroes = 0;
        loop {
            let done = self.read_bit()?;

            if done {
                break;
            }

            leading_zeroes += 1;
        }

        if leading_zeroes >= 32 {
            return Ok(u32::MAX);
        }

        let value = self.read_bits(leading_zeroes)?;
        Ok(value + (1 << leading_zeroes) - 1)
    }

    /// Implements le(n): Unsigned little-endian n-byte number appearing
    /// directly in the bitstream. See 4.10.4
    pub fn read_le(&mut self, num_bits: u8) -> anyhow::Result<u32> {
        assert!(self.0.is_aligned(1));
        let mut t = 0;

        for i in 0..num_bits {
            let byte = self.read_bits(8)?;
            t += byte << (i * 8)
        }

        Ok(t)
    }

    /// Implements leb128(): Unsigned integer represented by a variable number
    /// of little-endian bytes. See 4.10.5
    pub fn read_leb128(&mut self) -> anyhow::Result<u32> {
        assert!(self.0.is_aligned(1));

        let mut value = 0u64;
        let mut leb128bytes = 0;

        for i in 0..8 {
            let byte = u64::from(self.read_bits(8)?);
            value |= (byte & 0x7f) << (i * 7);

            leb128bytes += 1;

            if byte & 0x80 == 0 {
                break;
            }
        }

        assert!(leb128bytes < 8);
        assert!(value < u32::MAX.into());
        Ok(value as u32)
    }

    /// Implements su(n): Signed integer converted from an n bits unsigned
    /// integer in the bitstream. (The unsigned integer corresponds to the
    /// bottom n bits of the signed integer.). See 4.10.6
    pub fn read_su(&mut self, num_bits: u8) -> anyhow::Result<i32> {
        let mut value = self.read_bits(num_bits)? as i32;
        let sign_mask = 1 << (num_bits - 1);

        if (value & sign_mask) != 0 {
            value -= 2 * sign_mask;
        }

        Ok(value)
    }

    /// Implements ns(n): Unsigned encoded integer with maximum number of values
    /// n (i.e. output in range 0..n-1). See 4.10.7
    pub fn read_ns(&mut self, num_bits: u8) -> anyhow::Result<u32> {
        let w = helpers::floor_log2(u32::from(num_bits)) + 1;
        let m = (1 << w) - num_bits;
        let v = self.read_bits(u8::try_from(w)? - 1)?;

        if v < m.into() {
            return Ok(v);
        }

        let extra_bit = self.read_bit()?;
        Ok((v << 1) - u32::from(m) + u32::from(extra_bit))
    }

    /// Implements 5.9.13: Delta quantizer syntax.
    pub fn read_delta_q(&mut self) -> anyhow::Result<i32> {
        let delta_coded = self.read_bit()?;

        if delta_coded {
            self.read_su(7)
        } else {
            Ok(0)
        }
    }

    pub fn more_data_in_bitstream(&self) -> bool {
        self.0.remaining() != 0
    }

    pub(crate) fn consumed(&self, start_pos: u32) -> u32 {
        assert!(self.position() % 8 == 0);
        (self.position() / 8) as u32 - start_pos
    }

    /// Get the length of the current OBU in AnnexB format.
    pub fn current_annexb_obu_length(
        &mut self,
        annexb_state: &mut AnnexBState,
    ) -> anyhow::Result<Option<usize>> {
        if !self.more_data_in_bitstream() {
            return Ok(None);
        }

        #[allow(clippy::comparison_chain)]
        if annexb_state.temporal_unit_consumed == annexb_state.temporal_unit_size {
            annexb_state.temporal_unit_size = 0;
        } else if annexb_state.temporal_unit_consumed > annexb_state.temporal_unit_size {
            return Err(anyhow!(
                "temporal_unit_size is {} but we consumed {} bytes",
                annexb_state.temporal_unit_size,
                annexb_state.temporal_unit_consumed,
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
            return Err(anyhow!(
                "frame_unit_size is {} but we consumed {} bytes",
                annexb_state.frame_unit_size,
                annexb_state.frame_unit_consumed,
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

    /// Skips `num_bits` bits.
    pub fn skip(&mut self, num_bits: u64) -> anyhow::Result<()> {
        self.0.skip(num_bits).map_err(|e| anyhow!(e))
    }

    pub fn position(&self) -> u64 {
        self.0.position()
    }

    /// Implements 5.3.4.
    pub fn read_trailing_bits(&mut self, mut num_bits: u64) -> anyhow::Result<()> {
        let trailing_one_bit = self.read_bit()?;
        num_bits -= 1;

        if !trailing_one_bit {
            return Err(anyhow!("bad padding: trailing_one_bit is not set"));
        }

        while num_bits > 0 {
            let trailing_zero_bit = self.read_bit()?;
            if trailing_zero_bit {
                return Err(anyhow!("bad padding: trailing_zero_bit is set"));
            }
            num_bits -= 1;
        }

        Ok(())
    }

    fn decode_subexp(&mut self, num_syms: i32) -> anyhow::Result<u32> {
        let mut i = 0;
        let mut mk = 0;
        let k = 3;

        loop {
            let b2 = if i != 0 { k + i - 1 } else { k };
            let a = 1 << b2;
            if num_syms <= mk + 3 * a {
                let num_bits = u8::try_from(num_syms - mk).unwrap();
                let subexp_final_bits = self.read_ns(num_bits)?;
                return Ok(subexp_final_bits);
            } else {
                let subexp_more_bits = self.read_bit()?;
                if subexp_more_bits {
                    i += 1;
                    mk += a;
                } else {
                    let num_bits = u8::try_from(b2).unwrap();
                    let subexp_bits = self.read_bits(num_bits)?;
                    return Ok(subexp_bits + mk as u32);
                }
            }
        }
    }

    /// Implements 5.9.27.
    pub fn decode_unsigned_subexp_with_ref(&mut self, mx: i32, r: i32) -> anyhow::Result<u32> {
        let v = self.decode_subexp(mx)?;
        if (r << 1) <= mx {
            Ok(helpers::inverse_recenter(r, v.try_into().unwrap())
                .try_into()
                .unwrap())
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
    ) -> anyhow::Result<i32> {
        let x = self.decode_unsigned_subexp_with_ref(high - low, r - low)?;
        Ok(i32::try_from(x).unwrap() + low)
    }

    /// Implements 5.3.5 Byte alignment syntax
    pub fn byte_alignment(&mut self) -> anyhow::Result<()> {
        while (self.position() & 7) != 0 {
            self.read_bit()?;
        }

        Ok(())
    }

    pub fn remaining_bits(&self) -> u64 {
        self.0.remaining()
    }
}

impl<'a> Clone for Reader<'a> {
    fn clone(&self) -> Self {
        Self(self.0.relative_reader())
    }
}
