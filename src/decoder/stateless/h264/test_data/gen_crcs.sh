#!/bin/bash

# Generates the CRCs for all .h264 files in the current directory using ffmpeg.

for f in `ls *.h264`; do
	ffmpeg -i $f -pix_fmt nv12 -f framehash -hash crc32 - |grep -v '^#' |awk '{print $6}' >$f.crc
done
