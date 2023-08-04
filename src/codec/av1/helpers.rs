// Copyright 2023 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use crate::codec::av1::parser::NUM_REF_FRAMES;

const DIV_LUT: [i32; 257] = [
    16384, 16320, 16257, 16194, 16132, 16070, 16009, 15948, 15888, 15828, 15768, 15709, 15650,
    15592, 15534, 15477, 15420, 15364, 15308, 15252, 15197, 15142, 15087, 15033, 14980, 14926,
    14873, 14821, 14769, 14717, 14665, 14614, 14564, 14513, 14463, 14413, 14364, 14315, 14266,
    14218, 14170, 14122, 14075, 14028, 13981, 13935, 13888, 13843, 13797, 13752, 13707, 13662,
    13618, 13574, 13530, 13487, 13443, 13400, 13358, 13315, 13273, 13231, 13190, 13148, 13107,
    13066, 13026, 12985, 12945, 12906, 12866, 12827, 12788, 12749, 12710, 12672, 12633, 12596,
    12558, 12520, 12483, 12446, 12409, 12373, 12336, 12300, 12264, 12228, 12193, 12157, 12122,
    12087, 12053, 12018, 11984, 11950, 11916, 11882, 11848, 11815, 11782, 11749, 11716, 11683,
    11651, 11619, 11586, 11555, 11523, 11491, 11460, 11429, 11398, 11367, 11336, 11305, 11275,
    11245, 11215, 11185, 11155, 11125, 11096, 11067, 11038, 11009, 10980, 10951, 10923, 10894,
    10866, 10838, 10810, 10782, 10755, 10727, 10700, 10673, 10645, 10618, 10592, 10565, 10538,
    10512, 10486, 10460, 10434, 10408, 10382, 10356, 10331, 10305, 10280, 10255, 10230, 10205,
    10180, 10156, 10131, 10107, 10082, 10058, 10034, 10010, 9986, 9963, 9939, 9916, 9892, 9869,
    9846, 9823, 9800, 9777, 9754, 9732, 9709, 9687, 9664, 9642, 9620, 9598, 9576, 9554, 9533, 9511,
    9489, 9468, 9447, 9425, 9404, 9383, 9362, 9341, 9321, 9300, 9279, 9259, 9239, 9218, 9198, 9178,
    9158, 9138, 9118, 9098, 9079, 9059, 9039, 9020, 9001, 8981, 8962, 8943, 8924, 8905, 8886, 8867,
    8849, 8830, 8812, 8793, 8775, 8756, 8738, 8720, 8702, 8684, 8666, 8648, 8630, 8613, 8595, 8577,
    8560, 8542, 8525, 8508, 8490, 8473, 8456, 8439, 8422, 8405, 8389, 8372, 8355, 8339, 8322, 8306,
    8289, 8273, 8257, 8240, 8224, 8208, 8192,
];

const DIV_LUT_BITS: u32 = 8;
const DIV_LUT_PREC_BITS: u32 = 14;

/// Implements FloorLog2(x), which is defined to be the floor of the base 2
/// logarithm of the input x.
///
/// The input x will always be an integer, and will always be greater than or equal to 1.
/// This function extracts the location of the most significant bit in x.
pub fn floor_log2(mut x: u32) -> u32 {
    assert!(x > 0);
    let mut s = 0;

    while x != 0 {
        x >>= 1;
        s += 1;
    }

    s - 1
}

/// Implements 5.9.3. Get relative distance function
pub fn get_relative_dist(enable_order_hint: bool, order_hint_bits: i32, a: i32, b: i32) -> i32 {
    if !enable_order_hint {
        0
    } else {
        let diff = a - b;
        let m = 1 << (order_hint_bits - 1);
        (diff & (m - 1)) - (diff & m)
    }
}

/// Implements find_latest_backward from section 7.8.
pub fn find_latest_backward(
    shifted_order_hints: &[i32; NUM_REF_FRAMES],
    used_frame: &[bool; NUM_REF_FRAMES],
    cur_frame_hint: i32,
    latest_order_hint: &mut i32,
) -> i32 {
    let mut _ref = -1;

    for i in 0..NUM_REF_FRAMES {
        let hint = shifted_order_hints[i];
        if !used_frame[i] && hint >= cur_frame_hint && (_ref < 0 || hint >= *latest_order_hint) {
            _ref = i as i32;
            *latest_order_hint = hint;
        }
    }

    _ref
}

/// Implements find_earliest_backward from section 7.8.
pub fn find_earliest_backward(
    shifted_order_hints: &[i32; NUM_REF_FRAMES],
    used_frame: &[bool; NUM_REF_FRAMES],
    cur_frame_hint: i32,
    earliest_order_hint: &mut i32,
) -> i32 {
    let mut _ref = -1;

    for i in 0..NUM_REF_FRAMES {
        let hint = shifted_order_hints[i];
        if !used_frame[i] && hint >= cur_frame_hint && (_ref < 0 || hint < *earliest_order_hint) {
            _ref = i as i32;
            *earliest_order_hint = hint;
        }
    }

    _ref
}

/// Implements find_latest_forward from section 7.8.
pub fn find_latest_forward(
    shifted_order_hints: &[i32; NUM_REF_FRAMES],
    used_frame: &[bool; NUM_REF_FRAMES],
    cur_frame_hint: i32,
    latest_order_hint: &mut i32,
) -> i32 {
    let mut _ref = -1;
    for i in 0..NUM_REF_FRAMES {
        let hint = shifted_order_hints[i];
        if !used_frame[i] && hint < cur_frame_hint && (_ref < 0 || hint >= *latest_order_hint) {
            _ref = i as i32;
            *latest_order_hint = hint;
        }
    }

    _ref
}

pub fn tile_log2(blk_size: u32, target: u32) -> u32 {
    let mut k = 0;

    while (blk_size << k) < target {
        k += 1;
    }

    k
}

pub fn clip3(x: i32, y: i32, z: i32) -> i32 {
    if z < x {
        x
    } else if z > y {
        y
    } else {
        z
    }
}

/// 5.9.29
pub fn inverse_recenter(r: i32, v: i32) -> i32 {
    if v > 2 * r {
        v
    } else if v & 1 != 0 {
        r - ((v + 1) >> 1)
    } else {
        r + (v >> 1)
    }
}

/// Implements Round2. See 4.7: mathematical functions.
pub fn round2(x: u32, n: u32) -> u32 {
    (x + 2u32.pow(n - 1)) / 2u32.pow(n)
}

/// Implements Round2Signed. See 4.7: mathematical functions.
pub fn round2signed(x: i32, n: u32) -> anyhow::Result<i32> {
    if x >= 0 {
        i32::try_from(round2(x as u32, n)).map_err(|e| anyhow::anyhow!(e))
    } else {
        let val = i32::try_from(round2(-x as u32, n)).map_err(|e| anyhow::anyhow!(e))?;
        Ok(-val)
    }
}

/// Implements 7.11.3.7. Resolve divisor process
pub fn resolve_divisor(d: i32) -> anyhow::Result<(u32, i32)> {
    let abs_d = u32::try_from(d.abs())?;
    let n = floor_log2(abs_d);
    let e = abs_d - (1 << n);

    let f = if n > DIV_LUT_BITS {
        round2(e, n - DIV_LUT_BITS)
    } else {
        e << (DIV_LUT_BITS - n)
    };

    let div_shift = n + DIV_LUT_PREC_BITS;
    let div_factor = if d < 0 {
        -DIV_LUT[f as usize]
    } else {
        DIV_LUT[f as usize]
    };

    Ok((div_shift, div_factor))
}
