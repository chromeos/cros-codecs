// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
use std::fmt;
use std::io::Write;

use crate::bitstream_utils::BitWriter;
use crate::bitstream_utils::BitWriterError;

/// Internal wrapper over [`std::io::Write`] for possible emulation prevention
struct EmulationPrevention<W: Write> {
    out: W,
    prev_bytes: [Option<u8>; 2],

    /// Emulation prevention enabled.
    ep_enabled: bool,
}

impl<W: Write> EmulationPrevention<W> {
    fn new(writer: W, ep_enabled: bool) -> Self {
        Self { out: writer, prev_bytes: [None; 2], ep_enabled }
    }

    fn write_byte(&mut self, curr_byte: u8) -> std::io::Result<()> {
        if self.prev_bytes[1] == Some(0x00) && self.prev_bytes[0] == Some(0x00) && curr_byte <= 0x03
        {
            self.out.write_all(&[0x00, 0x00, 0x03])?;
            // `curr_byte` stays pending, so that a following `0x00 0x00` pair
            // starting with it is escaped as well.
            self.prev_bytes = [Some(curr_byte), None];
        } else {
            if let Some(byte) = self.prev_bytes[1] {
                self.out.write_all(&[byte])?;
            }

            self.prev_bytes[1] = self.prev_bytes[0];
            self.prev_bytes[0] = Some(curr_byte);
        }

        Ok(())
    }

    /// Writes a H.265 NALU header. See 7.3.1.2
    fn write_header(
        &mut self,
        nalu_type: u8,
        nuh_layer_id: u8,
        nuh_temporal_id_plus1: u8,
    ) -> NaluWriterResult<()> {
        // forbidden_zero_bit | nal_unit_type | first bit of nuh_layer_id
        let byte0 = (nalu_type & 0b111111) << 1 | (nuh_layer_id >> 5) & 0b1;
        // remaining bits of nuh_layer_id | nuh_temporal_id_plus1
        let byte1 = (nuh_layer_id & 0b11111) << 3 | (nuh_temporal_id_plus1 & 0b111);

        self.out.write_all(&[0x00, 0x00, 0x00, 0x01, byte0, byte1])?;

        Ok(())
    }

    fn has_data_pending(&self) -> bool {
        self.prev_bytes[0].is_some() || self.prev_bytes[1].is_some()
    }
}

impl<W: Write> Write for EmulationPrevention<W> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        if !self.ep_enabled {
            self.out.write_all(buf)?;
            return Ok(buf.len());
        }

        for byte in buf {
            self.write_byte(*byte)?;
        }

        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        if let Some(byte) = self.prev_bytes[1].take() {
            self.out.write_all(&[byte])?;
        }

        if let Some(byte) = self.prev_bytes[0].take() {
            self.out.write_all(&[byte])?;
        }

        self.out.flush()
    }
}

impl<W: Write> Drop for EmulationPrevention<W> {
    fn drop(&mut self) {
        if let Err(e) = self.flush() {
            log::error!("Unable to flush pending bytes {e:?}");
        }
    }
}

#[derive(Debug)]
pub enum NaluWriterError {
    Overflow,
    Io(std::io::Error),
    BitWriterError(BitWriterError),
}

impl fmt::Display for NaluWriterError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            NaluWriterError::Overflow => write!(f, "value increment caused value overflow"),
            NaluWriterError::Io(x) => write!(f, "{}", x.to_string()),
            NaluWriterError::BitWriterError(x) => write!(f, "{}", x.to_string()),
        }
    }
}

impl From<std::io::Error> for NaluWriterError {
    fn from(err: std::io::Error) -> Self {
        NaluWriterError::Io(err)
    }
}

impl From<BitWriterError> for NaluWriterError {
    fn from(err: BitWriterError) -> Self {
        NaluWriterError::BitWriterError(err)
    }
}

pub type NaluWriterResult<T> = std::result::Result<T, NaluWriterError>;

/// A writer for H.265 bitstream. It is capable of outputing bitstream with
/// emulation-prevention.
pub struct NaluWriter<W: Write>(BitWriter<EmulationPrevention<W>>);

impl<W: Write> NaluWriter<W> {
    pub fn new(writer: W, ep_enabled: bool) -> Self {
        Self(BitWriter::new(EmulationPrevention::new(writer, ep_enabled)))
    }

    /// Writes fixed bit size integer (up to 32 bit) output with emulation
    /// prevention if enabled. Corresponds to `f(n)` in H.265 spec.
    pub fn write_f<T: Into<u32>>(&mut self, bits: usize, value: T) -> NaluWriterResult<usize> {
        self.0.write_f(bits, value).map_err(NaluWriterError::BitWriterError)
    }

    /// An alias to [`Self::write_f`] Corresponds to `u(n)` in H.265 spec.
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
    /// Coresponds to `ue(v)` in H.265 spec.
    pub fn write_ue<T: Into<u32>>(&mut self, value: T) -> NaluWriterResult<()> {
        let value = value.into();

        self.write_exp_golumb(value)
    }

    /// Writes a signed integer in exponential golumb format.
    /// Coresponds to `se(v)` in H.265 spec.
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
        self.0.has_data_pending() || self.0.inner().has_data_pending()
    }

    /// Writes a H.265 NALU header. Unlike H.264, the header is two bytes long.
    /// See 7.3.1.2.
    pub fn write_header(
        &mut self,
        nalu_type: u8,
        nuh_layer_id: u8,
        nuh_temporal_id_plus1: u8,
    ) -> NaluWriterResult<()> {
        self.0.flush()?;
        self.0.inner_mut().write_header(nalu_type, nuh_layer_id, nuh_temporal_id_plus1)?;
        Ok(())
    }

    /// Returns `true` if next bits will be aligned to 8
    pub fn aligned(&self) -> bool {
        !self.0.has_data_pending()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bitstream_utils::BitReader;
    use crate::codec::h264::nalu::Header;
    use crate::codec::h265::parser::Nalu;
    use crate::codec::h265::parser::NaluType;
    use std::io::Cursor;

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

        let mut reader = BitReader::new(&buf, true);

        assert_eq!(reader.read_ue::<u32>().unwrap(), 10);
        assert_eq!(reader.read_se::<i32>().unwrap(), -42);
        assert_eq!(reader.read_se::<i32>().unwrap(), 3);
        assert_eq!(reader.read_ue::<u32>().unwrap(), 5);
    }

    /// The two byte NALU header must be parsed back by the H.265 parser.
    #[test]
    fn writer_header() {
        for (type_, layer_id, temporal_id_plus1) in
            [(NaluType::VpsNut, 0u8, 1u8), (NaluType::TrailR, 63, 7), (NaluType::IdrWRadl, 1, 3)]
        {
            let mut buf = Vec::<u8>::new();
            {
                let mut writer = NaluWriter::new(&mut buf, true);
                writer.write_header(type_ as u8, layer_id, temporal_id_plus1).unwrap();
                // A minimal payload, as NALUs are found by looking for the
                // start code of the *next* one.
                writer.write_f(8, 0xffu8).unwrap();
            }

            let mut cursor = Cursor::new(&buf[..]);
            let nalu = Nalu::next(&mut cursor).unwrap();

            assert_eq!(nalu.header.type_, type_);
            assert_eq!(nalu.header.nuh_layer_id, layer_id);
            assert_eq!(nalu.header.nuh_temporal_id_plus1, temporal_id_plus1);
            assert_eq!(nalu.header.len(), 2);
        }
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
                let mut reader = BitReader::new(&buf, true);
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

        // A run of zeroes must be escaped again every two bytes, i.e. the byte
        // that followed an escape counts towards the next pair.
        test(&[0x00; 5], &[0x00, 0x00, 0x03, 0x00, 0x00, 0x03, 0x00]);
        test(&[0x00, 0x00, 0x00, 0x00, 0x01], &[0x00, 0x00, 0x03, 0x00, 0x00, 0x03, 0x01]);
    }
}
