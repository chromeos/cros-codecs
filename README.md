# Cros-codecs

[<img alt="crates.io" src="https://img.shields.io/crates/v/cros-codecs">](https://crates.io/crates/cros-codecs)
[<img alt="docs.rs" src="https://img.shields.io/docsrs/cros-codecs">](https://docs.rs/cros-codecs/latest/cros_codecs/)

A lightweight, simple, low-dependency, and hopefully safe crate for
hardware-accelerated video decoding and encoding on Linux.

It is developed for use in ChromeOS (particularly
[crosvm](https://github.com/google/crosvm)), but has no dependency to ChromeOS
and should be usable anywhere.

## Current features

- Simple decoder API,
- VAAPI decoder support (using
  [cros-libva](https://github.com/chromeos/cros-libva)) for H.264, H.265, VP8,
  VP9 and AV1,
- VAAPI encoder support for H.264, VP9 and AV1,
- Stateful V4L2 encoder support.

## Planned features

- Stateful V4L2 decoder support,
- Stateless V4L2 decoder support,
- Support for more encoder codecs,
- C API to be used in non-Rust projects.

## Non-goals

- Support for systems other than Linux.

## Example programs

The `ccdec` example program can decode an encoded stream and write the decoded
frames to a file. As such it can be used for testing purposes.

```shell
$ cargo build --examples
$ ./target/debug/examples/ccdec --help
Usage: ccdec <input> [--output <output>] --input-format <input-format> [--output-format <output-format>] [--synchronous] [--compute-md5 <compute-md5>]

Simple player using cros-codecs

Positional Arguments:
  input             input file

Options:
  --output          output file to write the decoded frames to
  --input-format    input format to decode from.
  --output-format   pixel format to decode into. Default: i420
  --synchronous     whether to decode frames synchronously
  --compute-md5     whether to display the MD5 of the decoded stream, and at
                    which granularity (stream or frame)
  --help            display usage information
```

## Testing

Fluster can be used for testing, using the `ccdec` example program described
above. [This branch](https://github.com/Gnurou/fluster/tree/cros-codecs)
contains support for cros-codecs testing. Just make sure the `ccdec` binary is
in your `PATH`, and run Fluster using one of the `ccdec` decoders, e.g.

```shell
python fluster.py run -d ccdec-H.264 -ts JVT-AVC_V1
```

## Credits

The majority of the code in the initial commit has been written by Daniel
Almeida as a VAAPI backend for crosvm, before being split into this crate.
