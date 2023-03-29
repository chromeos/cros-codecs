use anyhow::anyhow;
use anyhow::Result;
use bytes::Buf;
use std::fmt::Debug;
use std::io::Cursor;

pub trait Header: Sized {
    /// Parse the NALU header, returning it.
    fn parse<T: AsRef<[u8]>>(cursor: &Cursor<T>) -> Result<Self>;
    /// Whether this header type indicates EOS.
    fn is_end(&self) -> bool;
    /// The length of the header.
    fn len(&self) -> usize;
}

#[derive(Debug)]
pub struct Nalu<T, U> {
    header: U,
    /// The mapping that backs this NALU. Possibly shared with the other NALUs
    /// in the Access Unit.
    data: T,

    size: usize,
    offset: usize,
    sc_offset: usize,
}

impl<T, U> Nalu<T, U>
where
    T: AsRef<[u8]>,
    U: Debug + Header,
{
    fn find_start_code(data: &mut Cursor<T>, offset: usize) -> Option<usize> {
        // discard all zeroes until the start code pattern is found
        data.get_ref().as_ref()[offset..]
            .windows(3)
            .position(|window| window == [0x00, 0x00, 0x01])
    }

    /// Find the next Annex B encoded NAL unit.
    pub fn next(cursor: &mut Cursor<T>, bitstream: T) -> Result<Option<Nalu<T, U>>> {
        let pos = usize::try_from(cursor.position())?;

        // Find the start code for this NALU
        let current_nalu_offset = match Nalu::<T, U>::find_start_code(cursor, pos) {
            Some(offset) => offset,
            None => return Err(anyhow!("No NAL found")),
        };

        let mut start_code_offset = pos + current_nalu_offset;

        // If the preceding byte is 00, then we actually have a four byte SC,
        // i.e. 00 00 00 01 Where the first 00 is the "zero_byte()"
        if start_code_offset > 0 && cursor.get_ref().as_ref()[start_code_offset - 1] == 00 {
            start_code_offset -= 1;
        }

        // The NALU offset is its offset + 3 bytes to skip the start code.
        let nalu_offset = pos + current_nalu_offset + 3;

        // Set the bitstream position to the start of the current NALU
        cursor.set_position(u64::try_from(nalu_offset)?);

        let hdr = U::parse(cursor)?;

        // Find the start of the subsequent NALU.
        let mut next_nalu_offset = match Nalu::<T, U>::find_start_code(cursor, nalu_offset) {
            Some(offset) => offset,
            None => cursor.chunk().len(), // Whatever data is left must be part of the current NALU
        };

        while next_nalu_offset > 0
            && cursor.get_ref().as_ref()[nalu_offset + next_nalu_offset - 1] == 00
        {
            // Discard trailing_zero_8bits
            next_nalu_offset -= 1;
        }

        let nal_size = if hdr.is_end() { 1 } else { next_nalu_offset };

        Ok(Some(Nalu {
            header: hdr,
            data: bitstream,
            size: nal_size,
            offset: nalu_offset,
            sc_offset: start_code_offset,
        }))
    }

    /// Get a reference to the nalu's header.
    pub fn header(&self) -> &U {
        &self.header
    }

    /// Get a reference to the nalu's data.
    pub fn data(&self) -> &T {
        &self.data
    }

    /// Get a reference to the nalu's size.
    pub fn size(&self) -> usize {
        self.size
    }

    /// Get a reference to the nalu's offset.
    pub fn offset(&self) -> usize {
        self.offset
    }

    /// Get a reference to the nalu's sc offset.
    pub fn sc_offset(&self) -> usize {
        self.sc_offset
    }
}

impl<T: AsRef<[u8]>, U> AsRef<[u8]> for Nalu<T, U> {
    fn as_ref(&self) -> &[u8] {
        let data = self.data.as_ref();
        &data[self.offset..self.offset + self.size]
    }
}
