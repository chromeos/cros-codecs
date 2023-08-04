// Copyright 2023 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

//! Parsers for various kinds of encoded streams.
//!
//! This module does not provide any actual decoding tools - that's the job of the
//! [crate::decoder] module. However the parsers of this module are heavily used in order to
//! implement stateless decoding.
//!
//! There shall be no dependencies from other modules of this crate to this module, so that it
//! can be turned into a crate of its own if needed in the future.

pub mod av1;
pub mod h264;
pub mod h265;
pub mod vp8;
pub mod vp9;
