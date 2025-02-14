// Copyright 2025 The ChromiumOS Authors
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#[cfg(test)]
mod tests {
    const CCDEC_BINARY: &str = "ccdec";
    use std::env::current_exe;
    use std::path::Path;
    use std::path::PathBuf;
    use std::process::Command;

    fn cros_codecs_decode(codec_path: &str, file_name: &str, input_format: &str) {
        let (test_binary_path, test_file_path) = get_test_paths(codec_path);
        let ccdec_args = get_cros_codecs_decode_args(&test_file_path, file_name, input_format);

        let ccdec_args_str = ccdec_args.iter().map(String::as_str).collect::<Vec<_>>();
        execute(&test_binary_path, &ccdec_args_str);
    }

    fn execute(test_binary_path: &PathBuf, args: &[&str]) {
        let mut command = Command::new(test_binary_path);
        command.args(args);
        let output = command.status();

        match output {
            Ok(status) => {
                assert!(status.success(), "Command failed: {:?}", command);
            }
            Err(e) => {
                panic!("Error executing command: {}", e);
            }
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
    fn av1_decode() {
        const AV1_DATA_PATH: &str = "src/codec/av1/test_data";
        const FILE_NAME: &str = "test-25fps.av1.ivf";
        cros_codecs_decode(AV1_DATA_PATH, FILE_NAME, "av1");
    }

    #[test]
    fn h264_decode() {
        const H264_DATA_PATH: &str = "src/codec/h264/test_data";
        const FILE_NAME: &str = "test-25fps.h264";
        cros_codecs_decode(H264_DATA_PATH, FILE_NAME, "h264");
    }

    #[test]
    fn h265_decode() {
        const H265_DATA_PATH: &str = "src/codec/h265/test_data";
        const FILE_NAME: &str = "test-25fps.h265";
        cros_codecs_decode(H265_DATA_PATH, FILE_NAME, "h265");
    }

    #[test]
    fn vp8_decode() {
        const VP8_DATA_PATH: &str = "src/codec/vp8/test_data";
        const FILE_NAME: &str = "test-25fps.vp8";
        cros_codecs_decode(VP8_DATA_PATH, FILE_NAME, "vp8");
    }

    #[test]
    fn vp9_decode() {
        const VP9_DATA_PATH: &str = "src/codec/vp9/test_data";
        const FILE_NAME: &str = "test-25fps.vp9";
        cros_codecs_decode(VP9_DATA_PATH, FILE_NAME, "vp9");
    }
}
