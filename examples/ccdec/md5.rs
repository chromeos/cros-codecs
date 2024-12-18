// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

// Simple implementation of MD5 based heavily around the pseudocode in the
// Wikipedia article on this topic: https://en.wikipedia.org/wiki/MD5

// The Wikipedia article pseudocode is not idiomatic Rust. To keep the variables
// as closely matched to the pseudocode some of the warning are turned off.
#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]
const shift_amounts: [u8; 64] = [
    7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 5, 9, 14, 20, 5, 9, 14, 20, 5, 9,
    14, 20, 5, 9, 14, 20, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 6, 10, 15,
    21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21,
];

const K: [u32; 64] = [
    0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee, 0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501,
    0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be, 0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821,
    0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa, 0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8,
    0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed, 0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a,
    0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c, 0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70,
    0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05, 0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665,
    0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039, 0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1,
    0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1, 0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391,
];

const init_a: u32 = 0x67452301;
const init_b: u32 = 0xefcdab89;
const init_c: u32 = 0x98badcfe;
const init_d: u32 = 0x10325476;

fn bytes_to_words(chunk: &[u8]) -> Vec<u32> {
    assert_eq!(chunk.len() % 4, 0);
    let mut ret: Vec<u32> = vec![0; chunk.len() / 4];
    for i in 0..chunk.len() {
        ret[i / 4] |= u32::from(chunk[i]) << ((i % 4) * 8);
    }

    ret
}

fn left_rotate(x: u32, n: u8) -> u32 {
    (x << n) | (x >> (32 - n))
}

fn little_to_big_endian(x: u32) -> u32 {
    ((x >> 24) & 0x000000FF)
        | ((x >> 8) & 0x0000FF00)
        | ((x << 8) & 0x00FF0000)
        | ((x << 24) & 0xFF000000)
}

#[allow(unused_comparisons)]
fn process_chunk(byte_chunk: &[u8], a0: u32, b0: u32, c0: u32, d0: u32) -> (u32, u32, u32, u32) {
    assert_eq!(byte_chunk.len(), 64);

    let mut A = a0;
    let mut B = b0;
    let mut C = c0;
    let mut D = d0;

    let chunk = bytes_to_words(byte_chunk);

    for i in 0..64 {
        let mut F: u32 = 0;
        let mut g: usize = 0;
        if 0 <= i && i <= 15 {
            F = (B & C) | ((!B) & D);
            g = i
        } else if 16 <= i && i <= 31 {
            F = (D & B) | ((!D) & C);
            g = (5 * i + 1) % 16;
        } else if 32 <= i && i <= 47 {
            F = B ^ C ^ D;
            g = (3 * i + 5) % 16;
        } else if 48 <= i && i <= 63 {
            F = C ^ (B | (!D));
            g = (7 * i) % 16;
        }
        F = F.wrapping_add(A).wrapping_add(K[i]).wrapping_add(chunk[g]);
        A = D;
        D = C;
        C = B;
        B = B.wrapping_add(left_rotate(F, shift_amounts[i]));
    }

    (
        a0.wrapping_add(A),
        b0.wrapping_add(B),
        c0.wrapping_add(C),
        d0.wrapping_add(D),
    )
}

fn pad(input: &[u8], len_already_processed: u64) -> Vec<u8> {
    let orig_len = (input.len() as u64)
        .wrapping_mul(8)
        .wrapping_add(len_already_processed);
    let mut ret = input.to_vec();

    ret.push(0x80);
    while (ret.len() % 64) != 56 {
        ret.push(0x00);
    }

    ret.push((orig_len & 0xFF) as u8);
    ret.push(((orig_len >> 8) & 0xFF) as u8);
    ret.push(((orig_len >> 16) & 0xFF) as u8);
    ret.push(((orig_len >> 24) & 0xFF) as u8);
    ret.push(((orig_len >> 32) & 0xFF) as u8);
    ret.push(((orig_len >> 40) & 0xFF) as u8);
    ret.push(((orig_len >> 48) & 0xFF) as u8);
    ret.push(((orig_len >> 56) & 0xFF) as u8);

    ret
}

fn md5_digest_padded(
    padded_input: &[u8],
    mut A: u32,
    mut B: u32,
    mut C: u32,
    mut D: u32,
) -> String {
    assert_eq!(padded_input.len() % 64, 0);

    for i in 0..(padded_input.len() / 64) {
        (A, B, C, D) = process_chunk(&padded_input[(i * 64)..((i + 1) * 64)], A, B, C, D);
    }

    // The final hash should just be bytes, not little endian words.
    A = little_to_big_endian(A);
    B = little_to_big_endian(B);
    C = little_to_big_endian(C);
    D = little_to_big_endian(D);

    format!("{:08x}{:08x}{:08x}{:08x}", A, B, C, D)
}

pub fn md5_digest(input: &[u8]) -> String {
    let padded_input = pad(input, 0);
    md5_digest_padded(&padded_input, init_a, init_b, init_c, init_d)
}

pub struct MD5Context {
    A: u32,
    B: u32,
    C: u32,
    D: u32,
    len_already_processed: u64,
    buffer: Vec<u8>,
}

impl MD5Context {
    pub fn new() -> Self {
        Self {
            A: init_a,
            B: init_b,
            C: init_c,
            D: init_d,
            len_already_processed: 0,
            buffer: Vec::new(),
        }
    }

    pub fn consume(&mut self, input: &[u8]) -> () {
        self.buffer.append(&mut input.to_vec());
        while self.buffer.len() >= 64 {
            (self.A, self.B, self.C, self.D) =
                process_chunk(&self.buffer[0..64], self.A, self.B, self.C, self.D);
            self.len_already_processed = self.len_already_processed.wrapping_add(64 * 8);
            self.buffer.drain(0..64);
        }
    }

    pub fn flush(&mut self) -> String {
        let padded_buffer = pad(&self.buffer[..], self.len_already_processed);
        let ret = md5_digest_padded(&padded_buffer, self.A, self.B, self.C, self.D);
        *self = Self::new();
        ret
    }
}

#[cfg(test)]
mod tests {
    use super::md5_digest;

    #[test]
    fn digest_simple_strings() {
        assert_eq!(
            md5_digest("".as_bytes()),
            "d41d8cd98f00b204e9800998ecf8427e"
        );
        assert_eq!(
            md5_digest("The quick brown fox jumps over the lazy dog".as_bytes()),
            "9e107d9d372bb6826bd81d3542a419d6"
        );
        assert_eq!(
            md5_digest("The quick brown fox jumps over the lazy dog.".as_bytes()),
            "e4d909c290d0fb1ca068ffaddf22cbd0"
        );
    }
}
