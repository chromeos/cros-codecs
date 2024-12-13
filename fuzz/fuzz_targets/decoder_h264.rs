#![no_main]

use cros_codecs::codec::h264::parser::Nalu;
use cros_codecs::decoder::stateless::h264::H264;
use cros_codecs::decoder::stateless::StatelessDecoder;
use cros_codecs::utils::simple_playback_loop;
use cros_codecs::utils::simple_playback_loop_owned_frames;
use cros_codecs::utils::NalIterator;
use cros_codecs::DecodedFormat;
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    let mut decoder = StatelessDecoder::<H264, _>::new_dummy(Default::default()).unwrap();

    let _ = simple_playback_loop(
        &mut decoder,
        NalIterator::<Nalu>::new(data),
        &mut |_| (),
        &mut simple_playback_loop_owned_frames,
        DecodedFormat::NV12,
        Default::default(),
    );
});
