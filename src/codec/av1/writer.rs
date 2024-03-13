// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::io::Write;

use thiserror::Error;

use crate::utils::BitWriter;
use crate::utils::BitWriterError;

#[derive(Error, Debug)]
pub enum ObuWriterError {
    #[error(transparent)]
    BitWriterError(#[from] BitWriterError),

    #[error("attemped to write leb128 on unaligned position")]
    UnalignedLeb128,
}

pub type ObuWriterResult<T> = std::result::Result<T, ObuWriterError>;

pub struct ObuWriter<W: Write>(BitWriter<W>);

impl<W: Write> ObuWriter<W> {
    pub fn new(writer: W) -> Self {
        Self(BitWriter::new(writer))
    }

    /// Writes fixed bit size integer. Corresponds to `f(n)` in AV1 spec defined in 4.10.2.
    pub fn write_f<T: Into<u32>>(&mut self, bits: usize, value: T) -> ObuWriterResult<usize> {
        self.0
            .write_f(bits, value)
            .map_err(ObuWriterError::BitWriterError)
    }

    /// Writes variable length unsigned n-bit number. Corresponds to `uvlc()` in AV1 spec
    /// defined in 4.10.3.
    pub fn write_uvlc<T: Into<u32>>(&mut self, value: T) -> ObuWriterResult<usize> {
        let value: u32 = value.into();
        if value == u32::MAX {
            return self.write_f(32, 0u32);
        }

        let value = value + 1;
        let leading_zeros = (32 - value.leading_zeros()) as usize;

        Ok(self.write_f(leading_zeros - 1, 0u32)? + self.write_f(leading_zeros, value)?)
    }

    /// Writes unsigned little-endian n-byte integer. Corresponds to `le(n)` in AV1 spec
    /// defined in 4.10.4.
    pub fn write_le<T: Into<u32>>(&mut self, n: usize, value: T) -> ObuWriterResult<usize> {
        let value: u32 = value.into();
        let mut value = value.to_le();

        for _ in 0..n {
            self.write_f(4, value & 0xff)?;
            value >>= 8;
        }

        Ok(n)
    }

    /// Writes unsigned integer represented by a variable number of little-endian bytes.
    /// Corresponds to `leb128()` in AV1 spec defined in 4.10.4.
    ///
    /// Note: Despite the name, the AV1 4.10.4 limits the value to [`u32::MAX`] = (1 << 32) - 1.
    pub fn write_leb128<T: Into<u32>>(
        &mut self,
        value: T,
        min_bytes: usize,
    ) -> ObuWriterResult<usize> {
        if !self.aligned() {
            return Err(ObuWriterError::UnalignedLeb128);
        }

        let value: u32 = value.into();
        let mut value: u32 = value.to_le();
        let mut bytes = 0;

        for _ in 0..8 {
            bytes += 1;

            if value >= 0x7f || bytes < min_bytes {
                self.write_f(8, 0x80 | (value & 0x7f))?;
                value >>= 7;
            } else {
                self.write_f(8, value & 0x7f)?;
                break;
            }
        }

        assert!(value < 0x7f);

        Ok(bytes)
    }

    pub fn write_su<T: Into<i32>>(&mut self, bits: usize, value: T) -> ObuWriterResult<usize> {
        let mut value: i32 = value.into();
        if value < 0 {
            value += 1 << bits;
        }

        assert!(value >= 0);
        self.write_f(bits, value.unsigned_abs())
    }

    pub fn aligned(&self) -> bool {
        !self.0.has_data_pending()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::codec::av1::reader::Reader;

    const TEST_VECTOR: &[u32] = &[
        // some random test values
        u32::MAX,
        1,
        2,
        3,
        4,
        10,
        20,
        7312,
        8832,
        10123,
        47457,
        21390213,
        u32::MIN,
        u32::MAX - 1,
    ];

    #[test]
    fn test_uvlc() {
        for &value in TEST_VECTOR {
            let mut buf = Vec::<u8>::new();

            ObuWriter::new(&mut buf).write_uvlc(value).unwrap();

            if value == u32::MAX {
                // force stop uvlc
                buf.push(0x80);
            }

            let read = Reader::new(&buf).read_uvlc().unwrap();

            assert_eq!(read, value, "failed testing {}", value);
        }
    }

    #[test]
    fn test_leb128() {
        for &value in TEST_VECTOR {
            let mut buf = Vec::<u8>::new();

            ObuWriter::new(&mut buf).write_leb128(value, 0).unwrap();
            let read = Reader::new(&buf).read_leb128().unwrap();

            assert_eq!(read, value, "failed testing {}", value);
        }
    }

    #[test]
    fn test_su() {
        let vector = TEST_VECTOR
            .iter()
            .map(|e| *e as i32)
            .chain(TEST_VECTOR.iter().map(|e| -(*e as i32)));

        for value in vector {
            let bits = 32 - value.abs().leading_zeros() as usize + 1; // For sign
            if bits >= 32 {
                // Skip too big nubmers
                continue;
            }

            let mut buf = Vec::<u8>::new();

            ObuWriter::new(&mut buf).write_su(bits, value).unwrap();

            let read = Reader::new(&buf).read_su(bits as u8).unwrap();

            assert_eq!(read, value, "failed testing {}", value);
        }
    }
}
