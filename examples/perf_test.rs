// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use cros_codecs::image_processing::*;
use std::time::{Duration, Instant};

const K_NUMBER_OF_TEST_CYCLES: u64 = 1000;

fn test_mm21_to_nv12_perf() {
    let test_input = include_bytes!("../src/test_data/puppets-480x270_20230825.mm21.yuv");
    let test_expected_output = include_bytes!("../src/test_data/puppets-480x270_20230825.nv12.yuv");

    let mut test_output = [0u8; 480 * 288 * 3 / 2];
    let (test_y_output, test_uv_output) = test_output.split_at_mut(480 * 288);

    let start_time = Instant::now();
    for _cycle in 0..K_NUMBER_OF_TEST_CYCLES {
        let _ = mm21_to_nv12(
            &test_input[0..480 * 288],
            test_y_output,
            &test_input[480 * 288..480 * 288 * 3 / 2],
            test_uv_output,
            480,
            288,
        );
    }

    let duration_us = ((Instant::now() - start_time).as_micros()) as f64;

    let fps = K_NUMBER_OF_TEST_CYCLES as f64 / (duration_us / 1000000f64);

    println!("MM21 to NV12 Perf Test Results");
    println!("-----------------------------------");
    println!("Frames Decoded: {}", K_NUMBER_OF_TEST_CYCLES);
    println!("TotalDurationMs: {}", duration_us);
    println!("FramesPerSecond: {}\n\n", fps);

    assert_eq!(test_output, *test_expected_output);
}

fn test_nv12_to_i420_perf() {
    let test_input = include_bytes!("../src/test_data/puppets-480x270_20230825.nv12.yuv");
    let test_expected_output = include_bytes!("../src/test_data/puppets-480x270_20230825.i420.yuv");

    let mut test_output = [0u8; 480 * 288 * 3 / 2];
    let (test_y_output, test_uv_output) = test_output.split_at_mut(480 * 288);
    let (test_u_output, test_v_output) = test_uv_output.split_at_mut(480 * 288 / 4);

    let start_time = Instant::now();
    for _cycle in 0..K_NUMBER_OF_TEST_CYCLES {
        let _ = nv12_to_i420(
            &test_input[0..480 * 288],
            test_y_output,
            &test_input[480 * 288..480 * 288 * 3 / 2],
            test_u_output,
            test_v_output,
        );
    }

    let duration_us = ((Instant::now() - start_time).as_micros()) as f64;

    let fps = K_NUMBER_OF_TEST_CYCLES as f64 / (duration_us / 1000000f64);

    println!("NV12 to I420 Perf Test Results");
    println!("-----------------------------------");
    println!("Frames Decoded: {}", K_NUMBER_OF_TEST_CYCLES);
    println!("TotalDurationMs: {}", duration_us);
    println!("FramesPerSecond: {}\n\n", fps);

    assert_eq!(test_output, *test_expected_output);
}

fn test_i420_to_nv12_perf() {
    let test_input = include_bytes!("../src/test_data/puppets-480x270_20230825.i420.yuv");
    let test_expected_output = include_bytes!("../src/test_data/puppets-480x270_20230825.nv12.yuv");

    let mut test_output = [0u8; 480 * 288 * 3 / 2];
    let (test_y_output, test_uv_output) = test_output.split_at_mut(480 * 288);

    let start_time = Instant::now();
    for _cycle in 0..K_NUMBER_OF_TEST_CYCLES {
        let _ = i420_to_nv12(
            &test_input[0..(480 * 288)],
            test_y_output,
            &test_input[(480 * 288)..(480 * 288 * 5 / 4)],
            &test_input[(480 * 288 * 5 / 4)..(480 * 288 * 3 / 2)],
            test_uv_output,
        );
    }

    let duration_us = ((Instant::now() - start_time).as_micros()) as f64;

    let fps = K_NUMBER_OF_TEST_CYCLES as f64 / (duration_us / 1000000f64);

    println!("I420 to NV12 Perf Test Results");
    println!("-----------------------------------");
    println!("Frames Decoded: {}", K_NUMBER_OF_TEST_CYCLES);
    println!("TotalDurationMs: {}", duration_us);
    println!("FramesPerSecond: {}\n\n", fps);

    assert_eq!(test_output, *test_expected_output);
}

fn main() {
    test_mm21_to_nv12_perf();
    test_nv12_to_i420_perf();
    test_i420_to_nv12_perf();
}
