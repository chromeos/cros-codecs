#![no_main]

use cros_codecs::decoder::stateless::vp8::Vp8;
use cros_codecs::decoder::stateless::StatelessDecoder;
use cros_codecs::utils::simple_playback_loop;
use cros_codecs::utils::simple_playback_loop_owned_frames;
use cros_codecs::utils::IvfIterator;
use cros_codecs::DecodedFormat;
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    let mut decoder = StatelessDecoder::<Vp8, _>::new_dummy(Default::default()).unwrap();

    let _ = simple_playback_loop(
        &mut decoder,
        IvfIterator::new(data),
        &mut |_| (),
        &mut simple_playback_loop_owned_frames,
        DecodedFormat::NV12,
        Default::default(),
    );
});
