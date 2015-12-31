#!/bin/sh -x

# Copyright 2015 Viktor Szakats (vszakats.net/harbour)

# - Requires Git for Windows or busybox to run on Windows
# - Requires HB_DL_ROOT, VER_* envvars

# Quit if any of the lines fail
set -e

HB_DL_ROOT="$(echo "${HB_DL_ROOT}" | sed 's|\\|/|g')"

mkdir -p "${HB_DL_ROOT}"

# Dependencies for the Windows distro package

curl -fsS -o pack.bin -L 'http://www.7-zip.org/a/7z1512-extra.7z'
openssl dgst -sha256 pack.bin | grep -q 155b6dac5b8490c1ee0680e2062b0ce6f0cce3ecd2645dda858e92b5e25c67fd
7z x -y "-o${HB_DL_ROOT}/7z" pack.bin > /dev/null

curl -fsS -o pack.bin 'http://7zsfx.info/files/7zsd_150_2712.7z'
openssl dgst -sha256 pack.bin | grep -q e5a2a05997553cde6318149951da1e449b0fd277a6e671ac06bfde8572754739
7z x -y "-o${HB_DL_ROOT}/7zsfx" pack.bin > /dev/null

curl -fsS -o pack.bin -L --proto-redir =https 'https://fossies.org/windows/misc/upx391w.zip'
openssl dgst -sha256 pack.bin | grep -q d7d4817f46d2616c209c46fb8bce44e4bec93ab5adef5e4dfc93ee879527be1b
7z e -y "-o${HB_DL_ROOT}/upx" pack.bin > /dev/null

# curl -fsS -o pack.bin 'https://www.mirrorservice.org/sites/dl.sourceforge.net/pub/sourceforge/m/mi/mingw-w64/Toolchains%20targetting%20Win32/Personal%20Builds/mingw-builds/5.2.0/threads-posix/dwarf/i686-5.2.0-release-posix-dwarf-rt_v4-rev0.7z'
# openssl dgst -sha256 pack.bin | grep -q 25de3b1164df7a3d06978900664462fa2f651036491291d90ca8870be451f439
# curl -fsS -o pack.bin 'https://www.mirrorservice.org/sites/dl.sourceforge.net/pub/sourceforge/m/mi/mingw-w64/Toolchains%20targetting%20Win64/Personal%20Builds/mingw-builds/5.2.0/threads-posix/seh/x86_64-5.2.0-release-posix-seh-rt_v4-rev0.7z'
# openssl dgst -sha256 pack.bin | grep -q 3876e8a73218f07761f0f2966725510dfc7294160ba728d1a0e6bdfca93f03f5
curl -fsS -o pack.bin 'https://www.mirrorservice.org/sites/dl.sourceforge.net/pub/sourceforge/m/mi/mingw-w64/Toolchains%20targetting%20Win64/Personal%20Builds/mingw-builds/5.2.0/threads-posix/sjlj/x86_64-5.2.0-release-posix-sjlj-rt_v4-rev0.7z'
openssl dgst -sha256 pack.bin | grep -q c0536c55a1d12882987afd0a9be377413eaf6cee105e921c949899fa9b308b35
# Will unpack into "${HB_DL_ROOT}/mingw64"
7z x -y "-o${HB_DL_ROOT}" pack.bin > /dev/null

# Dependencies for Windows builds

curl -fsS -o pack.bin -L --proto-redir =https "https://dl.bintray.com/vszakats/generic/openssl-${VER_OPENSSL}-win32-mingw.7z"
openssl dgst -sha256 pack.bin | grep -q f9191a81fedbc42eae6249b838ffbab24e43ab17dcc47b7902bbacde4998b8ec
7z x -y "-o${HB_DL_ROOT}" pack.bin > /dev/null

curl -fsS -o pack.bin -L --proto-redir =https "https://dl.bintray.com/vszakats/generic/openssl-${VER_OPENSSL}-win64-mingw.7z"
openssl dgst -sha256 pack.bin | grep -q 82d25df66124ca94b3c52545a0cc3266f2db8f1bcc758709d922aaa32477f5c0
7z x -y "-o${HB_DL_ROOT}" pack.bin > /dev/null

curl -fsS -o pack.bin -L --proto-redir =https "https://dl.bintray.com/vszakats/generic/curl-${VER_CURL}-win32-mingw.7z"
openssl dgst -sha256 pack.bin | grep -q 894f4d3e79bc4684fda1919ff53cd04e81bdc54916b3f61817a1be39a0486800
7z x -y "-o${HB_DL_ROOT}" pack.bin > /dev/null

curl -fsS -o pack.bin -L --proto-redir =https "https://dl.bintray.com/vszakats/generic/curl-${VER_CURL}-win64-mingw.7z"
openssl dgst -sha256 pack.bin | grep -q 75916c7b81445e23a49f9a958ce3af8e8ea13d732d0ee8ca1da6f1d01ba3a729
7z x -y "-o${HB_DL_ROOT}" pack.bin > /dev/null
