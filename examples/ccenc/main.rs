// Copyright 2024 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use std::fs::File;

mod util;
use util::Args;

#[cfg(feature = "vaapi")]
mod vaapi_encoder;
#[cfg(feature = "vaapi")]
use vaapi_encoder::do_encode;

#[cfg(feature = "v4l2")]
mod v4l2_stateful_encoder;
#[cfg(feature = "v4l2")]
use v4l2_stateful_encoder::do_encode;

fn main() {
    env_logger::init();

    let args: Args = argh::from_env();

    let input = File::open(&args.input).expect("error opening input file");

    do_encode(input, args);
}
