#!/bin/bash

# Generates the CRCs for all .av1 files in the current directory using ffmpeg.

for f in `ls *.av1`; do
	ffmpeg -i $f -pix_fmt nv12 -f framehash -hash crc32 - |grep -v '^#' |awk '{print $6}' >$f.crc
	ffmpeg -i $f -pix_fmt nv12 -f framehash -hash md5 - |grep -v '^#' |awk '{print $6}' >$f.md5
done
