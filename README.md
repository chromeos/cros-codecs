# Cros-codecs

A lightweight, simple, low-dependency, and hopefully safe crate for hardware-accelerated video
decoding and encoding on Linux.

It is developed for use in ChromeOS (particularly [crosvm](https://github.com/google/crosvm)), but
has no dependency to ChromeOS and should be usable anywhere.

This crate is still under heavy development and is not recommended for use yet.

## Current features

* Simple decoder API,
* VAAPI decoder support (using [cros-libva](https://github.com/chromeos/cros-libva)) for H.264, VP8
  and VP9.

## Planned features:

* Stateful V4L2 decoder support.
* Stateless V4L2 decoder support.
* Vaapi encoder support.
* V4L2 encoder support.
* Support for H.265 and AV1.
* C API to be used in non-Rust projects.

## Non-goals

* Support for systems other than Linux.

## Example programs

The `ccdec` example program is included. It can decode an encoded stream and
write the decoded frames to a file.
