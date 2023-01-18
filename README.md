# Cros-codecs

A lightweight, simple, low-dependencies, and hopefully safe crate for using hardware-accelerated
decoding and encoding on Linux.

It is developed for use in ChromeOS (particularly [crosvm](https://github.com/google/crosvm)), but
has no ChromeOS dependencies and should this be usable anywhere.

This crate is still under heavy development. Currently implemented features are:

* Stateless decoder support.
* VAAPI decoder support (using [cros-libva](https://github.com/chromeos/cros-libva)) for H.264, VP8
  and VP9.

Future features:

* Stateful V4L2 decoder support.
* Stateless V4L2 decoder support.
* Vaapi encoder support.
* V4L2 encoder support.
* Support for stateless H.265 and AV1.
* C API to be used in non-Rust projects.
