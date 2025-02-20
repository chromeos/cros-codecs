// Copyright 2025 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#[cfg(test)]
mod tests {
    const CCDEC_BINARY: &str = "ccdec";
    use std::env::current_exe;
    use std::fs;
    use std::path::Path;
    use std::path::PathBuf;
    use std::process::{Command, ExitStatus};

    fn cros_codecs_decode(codec_path: &str, input_format: &str) {
        let (test_binary_path, test_file_path) = get_test_paths(codec_path);
        assert!(Path::new(codec_path).is_dir(), "{} is not a valid path", codec_path);

        let mut all_execute_success = true;
        for entry in fs::read_dir(codec_path).unwrap().flatten() {
            let path = entry.path();
            // Only run the test on bitstreams with available json file containing its md5 checksum.
            let test_file_name = path.file_name().unwrap().to_str().unwrap();
            let json_file_path = test_file_path.join(format!("{}.json", test_file_name));

            if path.is_file() && json_file_path.exists() {
                let ccdec_args =
                    get_cros_codecs_decode_args(&test_file_path, test_file_name, input_format);
                let ccdec_args_str = ccdec_args.iter().map(String::as_str).collect::<Vec<_>>();

                match execute(&test_binary_path, &ccdec_args_str) {
                    Ok(_) => log::info!("Cros-codecs decode test succeeded: {}", test_file_name),
                    Err(err) => {
                        log::error!(
                            "Cros-codecs decode test failed: {} with error: {}",
                            test_file_name,
                            err
                        );
                        all_execute_success = false;
                    }
                }
            }
        }
        assert!(all_execute_success, "One or more cros-codecs decode tests failed.");
    }

    type ExecuteResult = Result<ExitStatus, String>;
    fn execute(test_binary_path: &PathBuf, args: &[&str]) -> ExecuteResult {
        let mut command = Command::new(test_binary_path);
        command.args(args);
        let output = command.status();

        match output {
            Ok(status) => {
                if status.success() {
                    log::info!("Command succeeded: {:?}", command);
                    Ok(status)
                } else {
                    log::error!("Command failed: {:?} with status: {:?}", command, status);
                    Err(format!("Command failed with status: {:?}", status))
                }
            }
            Err(err) => Err(format!("Error executing command: {}", err)),
        }
    }

    fn get_test_paths(codec_path: &str) -> (PathBuf, PathBuf) {
        let parent_test_path = current_exe()
            .unwrap()
            .parent()
            .expect("Could not get parent directory of executable")
            .to_path_buf();
        let test_binary_path = parent_test_path.join(CCDEC_BINARY);
        let test_file_path = parent_test_path.join(codec_path);

        (test_binary_path, test_file_path)
    }

    fn get_cros_codecs_decode_args(
        test_file_path: &Path,
        file_name: &str,
        input_format: &str,
    ) -> Vec<String> {
        let json_file_name = format!("{}.json", file_name);
        vec![
            test_file_path.join(file_name).display().to_string(),
            "--golden".to_string(),
            test_file_path.join(json_file_name).display().to_string(),
            "--input-format".to_string(),
            input_format.to_string(),
        ]
    }

    #[test]
    #[cfg(target_arch = "x86_64")]
    fn av1_decode() {
        const AV1_DATA_PATH: &str = "src/codec/av1/test_data";
        cros_codecs_decode(AV1_DATA_PATH, "av1");
    }

    #[test]
    fn h264_decode() {
        const H264_DATA_PATH: &str = "src/codec/h264/test_data";
        cros_codecs_decode(H264_DATA_PATH, "h264");
    }

    #[test]
    #[cfg(target_arch = "x86_64")]
    fn h265_decode() {
        const H265_DATA_PATH: &str = "src/codec/h265/test_data";
        cros_codecs_decode(H265_DATA_PATH, "h265");
    }

    #[test]
    fn vp8_decode() {
        const VP8_DATA_PATH: &str = "src/codec/vp8/test_data";
        cros_codecs_decode(VP8_DATA_PATH, "vp8");
    }

    #[test]
    fn vp9_decode() {
        const VP9_DATA_PATH: &str = "src/codec/vp9/test_data";
        cros_codecs_decode(VP9_DATA_PATH, "vp9");
    }
}
