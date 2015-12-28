#!/bin/sh -x

# Copyright 2015 Viktor Szakats (vszakats.net/harbour)

# NOTE: Requires Git for Windows or busybox to run on Windows

mkdir -p "${HB_DL_ROOT}"

# Dependencies for the Windows distro package

curl -fsS -L 'http://www.7-zip.org/a/7z1512-extra.7z' -o 7z.7z
openssl dgst -sha256 7z.7z | grep -q 155b6dac5b8490c1ee0680e2062b0ce6f0cce3ecd2645dda858e92b5e25c67fd || exit 1
7z x -y "-o${HB_DL_ROOT}7z" 7z.7z > /dev/null

curl -fsS 'http://7zsfx.info/files/7zsd_150_2712.7z' -o 7zsfx.7z
openssl dgst -sha256 7zsfx.7z | grep -q e5a2a05997553cde6318149951da1e449b0fd277a6e671ac06bfde8572754739 || exit 1
7z x -y "-o${HB_DL_ROOT}7zsfx" 7zsfx.7z > /dev/null

curl -fsS -L --proto-redir =https 'https://fossies.org/windows/misc/upx391w.zip' -o upx.zip
openssl dgst -sha256 upx.zip | grep -q d7d4817f46d2616c209c46fb8bce44e4bec93ab5adef5e4dfc93ee879527be1b || exit 1
7z e -y "-o${HB_DL_ROOT}upx" upx.zip > /dev/null

# curl -fsS 'https://www.mirrorservice.org/sites/dl.sourceforge.net/pub/sourceforge/m/mi/mingw-w64/Toolchains%20targetting%20Win32/Personal%20Builds/mingw-builds/5.2.0/threads-posix/dwarf/i686-5.2.0-release-posix-dwarf-rt_v4-rev0.7z' -o mingw.7z
# openssl dgst -sha256 mingw.7z | grep -q 25de3b1164df7a3d06978900664462fa2f651036491291d90ca8870be451f439 || exit 1
# curl -fsS 'https://www.mirrorservice.org/sites/dl.sourceforge.net/pub/sourceforge/m/mi/mingw-w64/Toolchains%20targetting%20Win64/Personal%20Builds/mingw-builds/5.2.0/threads-posix/seh/x86_64-5.2.0-release-posix-seh-rt_v4-rev0.7z' -o mingw.7z
# openssl dgst -sha256 mingw.7z | grep -q 3876e8a73218f07761f0f2966725510dfc7294160ba728d1a0e6bdfca93f03f5 || exit 1
curl -fsS 'https://www.mirrorservice.org/sites/dl.sourceforge.net/pub/sourceforge/m/mi/mingw-w64/Toolchains%20targetting%20Win64/Personal%20Builds/mingw-builds/5.2.0/threads-posix/sjlj/x86_64-5.2.0-release-posix-sjlj-rt_v4-rev0.7z' -o mingw.7z
openssl dgst -sha256 mingw.7z | grep -q c0536c55a1d12882987afd0a9be377413eaf6cee105e921c949899fa9b308b35 || exit 1
# Will unpack into "${HB_DL_ROOT}mingw64"
7z x -y "-o${HB_DL_ROOT}" mingw.7z > /dev/null

# Dependencies for Windows builds

curl -fsS -L --proto-redir =https 'https://dl.bintray.com/vszakats/generic/openssl-${VER_OPENSSL}-win32-mingw.7z' -o openssl-win-mingw.7z
openssl dgst -sha256 openssl-win-mingw.7z | grep -q f9191a81fedbc42eae6249b838ffbab24e43ab17dcc47b7902bbacde4998b8ec || exit 1
7z x -y "-o${HB_DL_ROOT}" openssl-win-mingw.7z > /dev/null

curl -fsS -L --proto-redir =https 'https://dl.bintray.com/vszakats/generic/openssl-${VER_OPENSSL}-win64-mingw.7z' -o openssl-win-mingw64.7z
openssl dgst -sha256 openssl-win-mingw64.7z | grep -q 82d25df66124ca94b3c52545a0cc3266f2db8f1bcc758709d922aaa32477f5c0 || exit 1
7z x -y "-o${HB_DL_ROOT}" openssl-win-mingw64.7z > /dev/null

curl -fsS -L --proto-redir =https 'https://dl.bintray.com/vszakats/generic/curl-${VER_CURL}-win32-mingw.7z' -o curl-win-mingw.7z
openssl dgst -sha256 curl-win-mingw.7z | grep -q 5b5041f2fdd6bb4ea6209d2fa13a7f364e864497da318e9d5b4465bc987359ab || exit 1
7z x -y "-o${HB_DL_ROOT}" curl-win-mingw.7z > /dev/null

curl -fsS -L --proto-redir =https 'https://dl.bintray.com/vszakats/generic/curl-${VER_CURL}-win64-mingw.7z' -o curl-win-mingw64.7z
openssl dgst -sha256 curl-win-mingw64.7z | grep -q a231f16d866c6b594a7bfe029e12df46fa536f64cb237be0198ca8f3fae62d2b || exit 1
7z x -y "-o${HB_DL_ROOT}" curl-win-mingw64.7z > /dev/null
