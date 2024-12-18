import glob
import os
import subprocess

CCDEC_BINARY_PATH = 'target/debug/examples/ccdec'

def validate_decode(bitstream_path, codec):
  if not os.path.isfile(bitstream_path + '.md5'): # Missing golden, skip this vector
    return

  with open(bitstream_path + '.md5', 'r') as golden_file:
    golden = golden_file.read()

  ccdec_process = subprocess.run([CCDEC_BINARY_PATH, bitstream_path, '--frame-memory', 'prime', '--input-format', codec, '--output-format', 'nv12', '--compute-md5', 'frame'], capture_output=True)
  if ccdec_process.returncode != 0:
    print('Error running ccdec for bitstream ' + bitstream_path)
    print(ccdec_process.stderr.decode('utf-8'))
    return

  test_output = ccdec_process.stdout.decode('utf-8')
  if test_output != golden:
    print('MD5 mistmatch on bitstream ' + bitstream_path)
    test_lines = test_output.split('\n')
    golden_lines = golden.split('\n')
    if len(test_lines) > len(golden_lines):
      print('Found more frames in ccdec output than expected')
    elif len(test_lines) < len(golden_lines):
      print('Found fewer frames in ccdec output than expected')
    else:
      for i in range(0, len(test_lines)):
        if test_lines[i] != golden_lines[i]:
          print('First mismatch on frame ' + str(i))
          print('Expected: ' + golden_lines[i])
          print('Got: ' + test_lines[i])
          break


if not os.path.isfile(CCDEC_BINARY_PATH):
  print('Error! ccdec not found. Have you compiled examples yet?')

extensions = ['.h264', '.h265', '.av1', '.vp8', '.vp9', '.ivf']
for extension in extensions:
  for bitstream_path in glob.glob('src/codec/*/test_data/*' + extension):
    codec = bitstream_path[len('src/codec/'):]
    codec = codec[:codec.find('/')]
    validate_decode(bitstream_path, codec)
