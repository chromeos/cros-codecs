// Copyright 2022 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::io::Cursor;
use std::io::Read;

use bytes::Buf;

#[cfg(test)]
pub(crate) mod dummy;
pub mod nalu;
pub(crate) mod nalu_reader;
#[cfg(feature = "vaapi")]
pub mod vaapi;

/// Read and return the data from the next IVF packet. Returns `None` if there is no more data
/// to read.
pub fn read_ivf_packet(cursor: &mut Cursor<&[u8]>) -> Option<Box<[u8]>> {
    if !cursor.has_remaining() {
        return None;
    }

    let len = cursor.get_u32_le();
    // Skip PTS.
    let _ = cursor.get_u64_le();

    let mut buf = vec![0u8; len as usize];
    cursor.read_exact(&mut buf).unwrap();

    Some(buf.into_boxed_slice())
}
