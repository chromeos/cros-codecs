// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
use std::io::Write;

use log::error;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum NaluWriterError {
    #[error("value increment caused value overflow")]
    Overflow,
    #[error("invalid bit count")]
    InvalidBitCount,
    #[error(transparent)]
    Io(#[from] std::io::Error),
}

pub type NaluWriterResult<T> = std::result::Result<T, NaluWriterError>;

/// A writer for H.264 bitstream. It is capable of outputing bitstream with
/// emulation-prevention.
pub struct NaluWriter<W: Write> {
    out: W,

    nth_bit: usize,
    curr_byte: u8,
    prev_bytes: [Option<u8>; 2],

    /// Emulation prevention enabled.
    ep_enabled: bool,
}

impl<W: Write> NaluWriter<W> {
    pub fn new(writer: W, ep_enabled: bool) -> Self {
        Self {
            out: writer,
            curr_byte: 0,
            prev_bytes: [None; 2],
            nth_bit: 0,
            ep_enabled,
        }
    }

    /// Writes fixed bit size integer (up to 32 bit) output with emulation
    /// prevention if enabled. Corresponds to `f(n)` in H.264 spec.
    pub fn write_f<T: Into<u32>>(&mut self, bits: usize, value: T) -> NaluWriterResult<usize> {
        let value = value.into();

        if bits > 32 {
            return Err(NaluWriterError::InvalidBitCount);
        }

        let mut written = 0;
        for bit in (0..bits).rev() {
            let bit = (1 << bit) as u32;

            self.write_bit((value & bit) == bit)?;
            written += 1;
        }

        Ok(written)
    }

    /// An alias to [`Self::write_f`] Corresponds to `n(n)` in H.264 spec.
    pub fn write_u<T: Into<u32>>(&mut self, bits: usize, value: T) -> NaluWriterResult<usize> {
        self.write_f(bits, value)
    }

    /// Writes a number in exponential golumb format.
    pub fn write_exp_golumb(&mut self, value: u32) -> NaluWriterResult<()> {
        let value = value.checked_add(1).ok_or(NaluWriterError::Overflow)?;
        let bits = 32 - value.leading_zeros() as usize;
        let zeros = bits - 1;

        self.write_f(zeros, 0u32)?;
        self.write_f(bits, value)?;

        Ok(())
    }

    /// Writes a unsigned integer in exponential golumb format.
    /// Coresponds to `ue(v)` in H.264 spec.
    pub fn write_ue<T: Into<u32>>(&mut self, value: T) -> NaluWriterResult<()> {
        let value = value.into();

        self.write_exp_golumb(value)
    }

    /// Writes a signed integer in exponential golumb format.
    /// Coresponds to `se(v)` in H.264 spec.
    pub fn write_se<T: Into<i32>>(&mut self, value: T) -> NaluWriterResult<()> {
        let value: i32 = value.into();
        let abs_value: u32 = value.unsigned_abs();

        if value <= 0 {
            self.write_ue(2 * abs_value)
        } else {
            self.write_ue(2 * abs_value - 1)
        }
    }

    /// Returns `true` if ['Self`] hold data that wasn't written to [`std::io::Write`]
    pub fn has_data_pending(&self) -> bool {
        self.nth_bit != 0 || self.prev_bytes[0].is_some() || self.prev_bytes[1].is_some()
    }

    /// Takes a single bit that will be outputed to [`std::io::Write`]
    fn write_bit(&mut self, bit: bool) -> NaluWriterResult<()> {
        self.curr_byte |= (bit as u8) << (7u8 - self.nth_bit as u8);
        self.nth_bit += 1;

        if self.nth_bit == 8 {
            self.output_byte()?
        }

        Ok(())
    }

    /// Outputs a currently cached bits value and writes to [`std::io::Write`]
    /// with emulation-prevention if enabled.
    fn output_byte(&mut self) -> NaluWriterResult<()> {
        if self.nth_bit == 0 {
            return Ok(());
        } else if !self.ep_enabled {
            self.out.write_all(&[self.curr_byte])?;
            self.nth_bit = 0;
            self.curr_byte = 0;
            return Ok(());
        }

        if self.prev_bytes[1] == Some(0x00)
            && self.prev_bytes[0] == Some(0x00)
            && self.curr_byte <= 0x03
        {
            self.out.write_all(&[0x00, 0x00, 0x03, self.curr_byte])?;
            self.prev_bytes = [None; 2];
        } else {
            if let Some(byte) = self.prev_bytes[1] {
                self.out.write_all(&[byte])?;
            }

            self.prev_bytes[1] = self.prev_bytes[0];
            self.prev_bytes[0] = Some(self.curr_byte);
        }

        self.nth_bit = 0;
        self.curr_byte = 0;

        Ok(())
    }

    /// Writes a H.264 NALU header.
    pub fn write_header(&mut self, idc: u8, _type: u8) -> NaluWriterResult<()> {
        self.flush()?;

        self.out.write_all(&[
            0x00,
            0x00,
            0x00,
            0x01,
            (idc & 0b11) << 5 | (_type & 0b11111),
        ])?;

        Ok(())
    }

    /// Returns `true` if next bits will be aligned to 8
    pub fn aligned(&self) -> bool {
        self.nth_bit == 0
    }

    /// Immediately outputs any cached bits to [`std::io::Write`]
    fn flush(&mut self) -> NaluWriterResult<()> {
        if let Some(byte) = self.prev_bytes[1] {
            self.out.write_all(&[byte])?;
        }
        if let Some(byte) = self.prev_bytes[0] {
            self.out.write_all(&[byte])?;
        }

        self.prev_bytes = [None; 2];
        if self.nth_bit != 0 {
            self.out.write_all(&[self.curr_byte])?;
            self.nth_bit = 0;
        }

        self.out.flush()?;
        Ok(())
    }
}

impl<W: Write> Drop for NaluWriter<W> {
    fn drop(&mut self) {
        if let Err(e) = self.flush() {
            error!("Unable to flush bits {e:?}");
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::codec::h264::nalu_reader::NaluReader;

    #[test]
    fn simple_bits() {
        let mut buf = Vec::<u8>::new();
        {
            let mut writer = NaluWriter::new(&mut buf, false);
            writer.write_f(1, true).unwrap();
            writer.write_f(1, false).unwrap();
            writer.write_f(1, false).unwrap();
            writer.write_f(1, false).unwrap();
            writer.write_f(1, true).unwrap();
            writer.write_f(1, true).unwrap();
            writer.write_f(1, true).unwrap();
            writer.write_f(1, true).unwrap();
        }
        assert_eq!(buf, vec![0b10001111u8]);
    }

    #[test]
    fn simple_first_few_ue() {
        fn single_ue(value: u32) -> Vec<u8> {
            let mut buf = Vec::<u8>::new();
            {
                let mut writer = NaluWriter::new(&mut buf, false);
                writer.write_ue(value).unwrap();
            }
            buf
        }

        assert_eq!(single_ue(0), vec![0b10000000u8]);
        assert_eq!(single_ue(1), vec![0b01000000u8]);
        assert_eq!(single_ue(2), vec![0b01100000u8]);
        assert_eq!(single_ue(3), vec![0b00100000u8]);
        assert_eq!(single_ue(4), vec![0b00101000u8]);
        assert_eq!(single_ue(5), vec![0b00110000u8]);
        assert_eq!(single_ue(6), vec![0b00111000u8]);
        assert_eq!(single_ue(7), vec![0b00010000u8]);
        assert_eq!(single_ue(8), vec![0b00010010u8]);
        assert_eq!(single_ue(9), vec![0b00010100u8]);
    }

    #[test]
    fn writer_reader() {
        let mut buf = Vec::<u8>::new();
        {
            let mut writer = NaluWriter::new(&mut buf, false);
            writer.write_ue(10u32).unwrap();
            writer.write_se(-42).unwrap();
            writer.write_se(3).unwrap();
            writer.write_ue(5u32).unwrap();
        }

        let mut reader = NaluReader::new(&buf);

        assert_eq!(reader.read_ue::<u32>().unwrap(), 10);
        assert_eq!(reader.read_se::<i32>().unwrap(), -42);
        assert_eq!(reader.read_se::<i32>().unwrap(), 3);
        assert_eq!(reader.read_ue::<u32>().unwrap(), 5);

        let mut buf = Vec::<u8>::new();
        {
            let mut writer = NaluWriter::new(&mut buf, false);
            writer.write_se(30).unwrap();
            writer.write_ue(100u32).unwrap();
            writer.write_se(-402).unwrap();
            writer.write_ue(50u32).unwrap();
        }

        let mut reader = NaluReader::new(&buf);

        assert_eq!(reader.read_se::<i32>().unwrap(), 30);
        assert_eq!(reader.read_ue::<u32>().unwrap(), 100);
        assert_eq!(reader.read_se::<i32>().unwrap(), -402);
        assert_eq!(reader.read_ue::<u32>().unwrap(), 50);
    }

    #[test]
    fn writer_emulation_prevention() {
        fn test(input: &[u8], bitstream: &[u8]) {
            let mut buf = Vec::<u8>::new();
            {
                let mut writer = NaluWriter::new(&mut buf, true);
                for byte in input {
                    writer.write_f(8, *byte).unwrap();
                }
            }
            assert_eq!(buf, bitstream);
            {
                let mut reader = NaluReader::new(&buf);
                for byte in input {
                    assert_eq!(*byte, reader.read_bits::<u8>(8).unwrap());
                }
            }
        }

        test(&[0x00, 0x00, 0x00], &[0x00, 0x00, 0x03, 0x00]);
        test(&[0x00, 0x00, 0x01], &[0x00, 0x00, 0x03, 0x01]);
        test(&[0x00, 0x00, 0x02], &[0x00, 0x00, 0x03, 0x02]);
        test(&[0x00, 0x00, 0x03], &[0x00, 0x00, 0x03, 0x03]);

        test(&[0x00, 0x00, 0x00, 0x00], &[0x00, 0x00, 0x03, 0x00, 0x00]);
        test(&[0x00, 0x00, 0x00, 0x01], &[0x00, 0x00, 0x03, 0x00, 0x01]);
        test(&[0x00, 0x00, 0x00, 0x02], &[0x00, 0x00, 0x03, 0x00, 0x02]);
        test(&[0x00, 0x00, 0x00, 0x03], &[0x00, 0x00, 0x03, 0x00, 0x03]);
    }
}
