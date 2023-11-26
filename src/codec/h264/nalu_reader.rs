// Copyright 2022 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::io::Cursor;

use anyhow::anyhow;
use bytes::Buf;
use thiserror::Error;

/// A bit reader for h264 bitstreams. It properly handles emulation-prevention
/// bytes and stop bits.
pub(crate) struct NaluReader<'a> {
    /// A reference into the next unread byte in the stream.
    data: Cursor<&'a [u8]>,
    /// Contents of the current byte. First unread bit starting at position 8 -
    /// num_remaining_bits_in_curr_bytes.
    curr_byte: u32,
    /// Number of bits remaining in `curr_byte`
    num_remaining_bits_in_curr_byte: usize,
    /// Used in epb detection.
    prev_two_bytes: u32,
    /// Number of epbs (i.e. 0x000003) we found.
    num_epb: usize,
}

#[derive(Debug, Error)]
pub(crate) enum GetByteError {
    #[error("reader ran out of bits")]
    OutOfBits,
}

#[derive(Debug, Error)]
pub(crate) enum ReadBitsError {
    #[error("more than 31 ({0}) bits were requested")]
    TooManyBytesRequested(usize),
    #[error("failed to advance the current byte")]
    GetByte(#[from] GetByteError),
    #[error("failed to convert read input to target type")]
    ConversionFailed,
}

impl<'a> NaluReader<'a> {
    pub fn new(data: &'a [u8]) -> Self {
        Self {
            data: Cursor::new(data),
            curr_byte: Default::default(),
            num_remaining_bits_in_curr_byte: Default::default(),
            prev_two_bytes: 0xffff,
            num_epb: Default::default(),
        }
    }

    /// Read a single bit from the stream.
    pub fn read_bit(&mut self) -> Result<bool, ReadBitsError> {
        let bit = self.read_bits::<u32>(1)?;
        match bit {
            1 => Ok(true),
            0 => Ok(false),
            _ => panic!("Unexpected value {}", bit),
        }
    }

    /// Read up to 31 bits from the stream.
    pub fn read_bits<U: TryFrom<u32>>(&mut self, num_bits: usize) -> Result<U, ReadBitsError> {
        if num_bits > 31 {
            return Err(ReadBitsError::TooManyBytesRequested(num_bits));
        }

        let mut bits_left = num_bits;
        let mut out = 0;

        while self.num_remaining_bits_in_curr_byte < bits_left {
            out |= self.curr_byte << (bits_left - self.num_remaining_bits_in_curr_byte);
            bits_left -= self.num_remaining_bits_in_curr_byte;
            self.update_curr_byte()?;
        }

        out |= self.curr_byte >> (self.num_remaining_bits_in_curr_byte - bits_left);
        out &= (1 << num_bits) - 1;
        self.num_remaining_bits_in_curr_byte -= bits_left;

        U::try_from(out).map_err(|_| ReadBitsError::ConversionFailed)
    }

    /// Skip `num_bits` bits from the stream.
    pub fn skip_bits(&mut self, mut num_bits: usize) -> Result<(), ReadBitsError> {
        while num_bits > 0 {
            let n = std::cmp::min(num_bits, 31);
            self.read_bits::<u32>(n)?;
            num_bits -= n;
        }

        Ok(())
    }

    /// Returns the amount of bits left in the stream
    pub fn num_bits_left(&self) -> usize {
        self.data.remaining() * 8 + self.num_remaining_bits_in_curr_byte
    }

    /// Returns the number of emulation-prevention bytes read so far.
    pub fn num_epb(&self) -> usize {
        self.num_epb
    }

    /// Whether the stream still has RBSP data. Implements more_rbsp_data(). See
    /// the spec for more details.
    pub fn has_more_rsbp_data(&mut self) -> bool {
        if self.num_remaining_bits_in_curr_byte == 0 && self.update_curr_byte().is_err() {
            // no more data at all in the rbsp
            return false;
        }

        // If the next bit is the stop bit, then we should only see unset bits
        // until the end of the data.
        if (self.curr_byte & ((1 << (self.num_remaining_bits_in_curr_byte - 1)) - 1)) != 0 {
            return true;
        }

        let data = self.data.chunk();
        for data in &data[0..self.data.remaining()] {
            if *data != 0 {
                return true;
            }
        }

        self.data.advance(self.data.remaining());
        false
    }

    pub fn read_ue<U: TryFrom<u32>>(&mut self) -> anyhow::Result<U> {
        let mut num_bits = 0;
        let mut bit = self.read_bits::<u32>(1)?;

        while bit == 0 {
            num_bits += 1;
            bit = self.read_bits(1)?;
        }

        if num_bits > 31 {
            return Err(anyhow!("Invalid stream"));
        }

        let mut value = (1 << num_bits) - 1;
        let rest;

        // Check for overflow
        if num_bits == 31 {
            rest = self.read_bits::<u32>(num_bits)?;
            if rest == 0 {
                return U::try_from(value).map_err(|_| anyhow!("Conversion error"));
            } else {
                return Err(anyhow!("Invalid stream"));
            }
        }

        if num_bits > 0 {
            value += self.read_bits::<u32>(num_bits)?;
        }

        U::try_from(value).map_err(|_| anyhow!("Conversion error"))
    }

    pub fn read_ue_bounded<U: TryFrom<u32>>(&mut self, min: u32, max: u32) -> anyhow::Result<U> {
        let ue = self.read_ue()?;
        if ue > max || ue < min {
            Err(anyhow!(
                "Value out of bounds: expected {} - {}, got {}",
                min,
                max,
                ue
            ))
        } else {
            Ok(U::try_from(ue).map_err(|_| anyhow!("Conversion error"))?)
        }
    }

    pub fn read_ue_max<U: TryFrom<u32>>(&mut self, max: u32) -> anyhow::Result<U> {
        self.read_ue_bounded(0, max)
    }

    pub fn read_se<U: TryFrom<i32>>(&mut self) -> anyhow::Result<U> {
        let ue = self.read_ue::<u32>()? as i32;

        if ue % 2 == 0 {
            Ok(U::try_from(-ue / 2).map_err(|_| anyhow!("Conversion error"))?)
        } else {
            Ok(U::try_from(ue / 2 + 1).map_err(|_| anyhow!("Conversion error"))?)
        }
    }

    pub fn read_se_bounded<U: TryFrom<i32>>(&mut self, min: i32, max: i32) -> anyhow::Result<U> {
        let se = self.read_se()?;
        if se < min || se > max {
            Err(anyhow!(
                "Value out of bounds, expected between {}-{}, got {}",
                min,
                max,
                se
            ))
        } else {
            Ok(U::try_from(se).map_err(|_| anyhow!("Conversion error"))?)
        }
    }

    fn get_byte(&mut self) -> Result<u8, GetByteError> {
        if self.data.remaining() == 0 {
            return Err(GetByteError::OutOfBits);
        }

        Ok(self.data.get_u8())
    }

    fn update_curr_byte(&mut self) -> Result<(), GetByteError> {
        let mut byte = self.get_byte()?;

        if (self.prev_two_bytes & 0xffff) == 0 && byte == 0x03 {
            // We found an epb
            self.num_epb += 1;
            // Read another byte
            byte = self.get_byte()?;
            // We need another 3 bytes before another epb can happen.
            self.prev_two_bytes = 0xffff;
        }

        self.num_remaining_bits_in_curr_byte = 8;
        self.prev_two_bytes = ((self.prev_two_bytes & 0xff) << 8) | u32::from(byte);

        self.curr_byte = u32::from(byte);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::NaluReader;

    // These tests are adapted from the chromium tests at media/video/h264_bit_reader_unitttest.cc

    #[test]
    fn read_stream_without_escape_and_trailing_zero_bytes() {
        const RBSP: [u8; 6] = [0x01, 0x23, 0x45, 0x67, 0x89, 0xa0];

        let mut reader = NaluReader::new(&RBSP);
        assert_eq!(reader.read_bits::<u32>(1).unwrap(), 0);
        assert_eq!(reader.num_bits_left(), 47);
        assert!(reader.has_more_rsbp_data());

        assert_eq!(reader.read_bits::<u32>(8).unwrap(), 0x02);
        assert_eq!(reader.num_bits_left(), 39);
        assert!(reader.has_more_rsbp_data());

        assert_eq!(reader.read_bits::<u32>(31).unwrap(), 0x23456789);
        assert_eq!(reader.num_bits_left(), 8);
        assert!(reader.has_more_rsbp_data());

        assert_eq!(reader.read_bits::<u32>(1).unwrap(), 1);
        assert_eq!(reader.num_bits_left(), 7);
        assert!(reader.has_more_rsbp_data());

        assert_eq!(reader.read_bits::<u32>(1).unwrap(), 0);
        assert_eq!(reader.num_bits_left(), 6);
        assert!(!reader.has_more_rsbp_data());
    }

    #[test]
    fn single_byte_stream() {
        const RBSP: [u8; 1] = [0x18];

        let mut reader = NaluReader::new(&RBSP);
        assert_eq!(reader.num_bits_left(), 8);
        assert!(reader.has_more_rsbp_data());
        assert_eq!(reader.read_bits::<u32>(4).unwrap(), 1);
        assert!(!reader.has_more_rsbp_data());
    }

    #[test]
    fn stop_bit_occupy_full_byte() {
        const RBSP: [u8; 2] = [0xab, 0x80];

        let mut reader = NaluReader::new(&RBSP);
        assert_eq!(reader.num_bits_left(), 16);
        assert!(reader.has_more_rsbp_data());

        assert_eq!(reader.read_bits::<u32>(8).unwrap(), 0xab);
        assert_eq!(reader.num_bits_left(), 8);

        assert!(!reader.has_more_rsbp_data());
    }
}
