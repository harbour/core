#!/bin/sh -x

# Copyright 2015-2016 Viktor Szakats (vszakats.net/harbour)

# - Requires Git for Windows or busybox to run on Windows
# - Requires VER_* envvars

# Quit if any of the lines fail
set -e

# Dependencies for the Windows distro package

curl -fsS -o pack.bin -L 'http://www.7-zip.org/a/7z1514-extra.7z'
openssl dgst -sha256 pack.bin | grep -q 4fb7b51e93cabbede23281eae0d024a63f485dc339c85e20c305f328a76e90c0
7z x -y -o7z pack.bin > /dev/null

curl -fsS -o pack.bin 'http://7zsfx.info/files/7zsd_150_2712.7z'
openssl dgst -sha256 pack.bin | grep -q e5a2a05997553cde6318149951da1e449b0fd277a6e671ac06bfde8572754739
7z x -y -o7zsfx pack.bin > /dev/null

curl -fsS -o pack.bin -L --proto-redir =https 'https://fossies.org/windows/misc/upx391w.zip'
openssl dgst -sha256 pack.bin | grep -q d7d4817f46d2616c209c46fb8bce44e4bec93ab5adef5e4dfc93ee879527be1b
7z e -y -oupx pack.bin > /dev/null

# curl -fsS -o pack.bin 'https://www.mirrorservice.org/sites/dl.sourceforge.net/pub/sourceforge/m/mi/mingw-w64/Toolchains%20targetting%20Win32/Personal%20Builds/mingw-builds/5.3.0/threads-posix/dwarf/i686-5.3.0-release-posix-dwarf-rt_v4-rev0.7z'
# openssl dgst -sha256 pack.bin | grep -q 6e067b2917583e9c654b611263d5d5e8c3215b67d76d55fa3f5f484f16f0f0b6
# curl -fsS -o pack.bin 'https://www.mirrorservice.org/sites/dl.sourceforge.net/pub/sourceforge/m/mi/mingw-w64/Toolchains%20targetting%20Win64/Personal%20Builds/mingw-builds/5.3.0/threads-posix/seh/x86_64-5.3.0-release-posix-seh-rt_v4-rev0.7z'
# openssl dgst -sha256 pack.bin | grep -q 7f0e1f081d173b4a98bde3f9d1a90daf391219e6738f1f40120336b40545f090
# curl -fsS -o pack.bin 'https://www.mirrorservice.org/sites/dl.sourceforge.net/pub/sourceforge/m/mi/mingw-w64/Toolchains%20targetting%20Win64/Personal%20Builds/mingw-builds/5.3.0/threads-posix/sjlj/x86_64-5.3.0-release-posix-sjlj-rt_v4-rev0.7z'
  curl -fsS -o pack.bin -L 'https://downloads.sourceforge.net/mingw-w64/Toolchains%20targetting%20Win64/Personal%20Builds/mingw-builds/5.3.0/threads-posix/sjlj/x86_64-5.3.0-release-posix-sjlj-rt_v4-rev0.7z'
openssl dgst -sha256 pack.bin | grep -q ec28b6640ad4f183be7afcd6e9c5eabb24b89729ca3fec7618755555b5d70c19
# Will unpack into "./mingw64"
7z x -y pack.bin > /dev/null

# Dependencies for Windows builds

curl -fsS -o pack.bin -L --proto-redir =https "https://dl.bintray.com/vszakats/generic/openssl-${VER_OPENSSL}-win32-mingw.7z"
openssl dgst -sha256 pack.bin | grep -q 5247ef88294f873e81f31012e947fbb9cfc45ef5048070345cd0b25df5d0a4a3
7z x -y pack.bin > /dev/null

curl -fsS -o pack.bin -L --proto-redir =https "https://dl.bintray.com/vszakats/generic/openssl-${VER_OPENSSL}-win64-mingw.7z"
openssl dgst -sha256 pack.bin | grep -q 378c05aa9752a49619d092d3968382b1f03bceb4fb258b77b2e54c997cb038be
7z x -y pack.bin > /dev/null

curl -fsS -o pack.bin -L --proto-redir =https "https://dl.bintray.com/vszakats/generic/curl-${VER_CURL}-win32-mingw.7z"
openssl dgst -sha256 pack.bin | grep -q b8c2d7ab620b198aeabf9573ed63059bc28ea5921d008c267c16db7409fbc4cb
7z x -y pack.bin > /dev/null

curl -fsS -o pack.bin -L --proto-redir =https "https://dl.bintray.com/vszakats/generic/curl-${VER_CURL}-win64-mingw.7z"
openssl dgst -sha256 pack.bin | grep -q e9bbd8d4fefe484f583c7abfc4ebd9cbd81bf2e49557fc21c72d0c46063d3681
7z x -y pack.bin > /dev/null
