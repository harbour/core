#!/bin/sh

# ---------------------------------------------------------------
# Copyright 2015-2016 Viktor Szakats (vszakats.net/harbour)
# See LICENSE.txt for licensing terms.
# ---------------------------------------------------------------

# - Requires '[PACKAGE]_VER' and '[PACKAGE]_HASH_[32|64]' envvars

_BRANCH="${APPVEYOR_REPO_BRANCH}${TRAVIS_BRANCH}${CI_BUILD_REF_NAME}${GIT_BRANCH}"

set | grep '_VER='

# Quit if any of the lines fail
set -e

alias curl='curl -fsS --connect-timeout 15 --retry 3'
alias gpg='gpg --batch --keyid-format LONG'

gpg --version | grep gpg

# Dependencies for the Windows distro package

(
   set -x

   curl -o pack.bin -L 'http://www.7-zip.org/a/7z1602-extra.7z'
   openssl dgst -sha256 pack.bin | grep -q f6c412e8bc45e4a88e675976024c21ed7a23eeb7eb0af452aa7a9b9a97843aa2
   7z x -y -o7z pack.bin > /dev/null

   curl -o pack.bin 'http://7zsfx.info/files/7zsd_extra_160_2712.7z'
   openssl dgst -sha256 pack.bin | grep -q eadee3f1fb5a8e3d4cede8a8a4c2eec7687a3b3ee5856216fdb69f4124709605
   7z x -y -o7zsfx pack.bin > /dev/null

   curl -o pack.bin -L --proto-redir =https 'https://fossies.org/windows/misc/upx391w.zip'
   openssl dgst -sha256 pack.bin | grep -q d7d4817f46d2616c209c46fb8bce44e4bec93ab5adef5e4dfc93ee879527be1b
   7z e -y -oupx pack.bin > /dev/null

   if [ "${_BRANCH#*msysmingw*}" = "${_BRANCH}" ] ; then
      readonly mingwbase='https://downloads.sourceforge.net'; readonly option='-L'
#     curl -o pack.bin "${option}" "${mingwbase}/mingw-w64/Toolchains%20targetting%20Win32/Personal%20Builds/mingw-builds/5.3.0/threads-posix/dwarf/i686-5.3.0-release-posix-dwarf-rt_v4-rev0.7z"
#     openssl dgst -sha256 pack.bin | grep -q 6e067b2917583e9c654b611263d5d5e8c3215b67d76d55fa3f5f484f16f0f0b6
#     curl -o pack.bin "${option}" "${mingwbase}/mingw-w64/Toolchains%20targetting%20Win64/Personal%20Builds/mingw-builds/5.3.0/threads-posix/seh/x86_64-5.3.0-release-posix-seh-rt_v4-rev0.7z"
#     openssl dgst -sha256 pack.bin | grep -q 7f0e1f081d173b4a98bde3f9d1a90daf391219e6738f1f40120336b40545f090
      curl -o pack.bin "${option}" "${mingwbase}/mingw-w64/Toolchains%20targetting%20Win64/Personal%20Builds/mingw-builds/5.3.0/threads-posix/sjlj/x86_64-5.3.0-release-posix-sjlj-rt_v4-rev0.7z"
      openssl dgst -sha256 pack.bin | grep -q ec28b6640ad4f183be7afcd6e9c5eabb24b89729ca3fec7618755555b5d70c19
      # Will unpack into "./mingw64"
      7z x -y pack.bin > /dev/null
   else
      # Bad hack to avoid duplicate manifests being linked into slightly
      # "off" binaries, that are in turn impossible to UPX.
      #    https://github.com/Alexpux/MSYS2-packages/issues/454
      #    https://gcc.gnu.org/bugzilla/show_bug.cgi?id=69880
      for file in \
         /usr/lib/default-manifest.o \
         /mingw32/i686-w64-mingw32/lib/default-manifest.o \
         /mingw64/x86_64-w64-mingw32/lib/default-manifest.o ; do
         [ -f "${file}" ] && mv -f "${file}" "${file}-ORI"
      done
   fi
)

# Dependencies for Windows builds

# Bintray public key
gpg --keyserver hkps://pgp.mit.edu --recv-keys 8756C4F765C9AC3CB6B85D62379CE192D401AB61

# Builder public key
curl 'https://bintray.com/user/downloadSubjectPublicKey?username=vszakats' | \
   gpg --import

readonly base='https://bintray.com/artifact/download/vszakats/generic/'

for plat in '32' '64' ; do
   for name in \
      'nghttp2' \
      'openssl' \
      'libssh2' \
      'curl' \
   ; do
      eval ver="\$$(echo "${name}" | tr '[:lower:]' '[:upper:]' 2> /dev/null)_VER"
      eval hash="\$$(echo "${name}" | tr '[:lower:]' '[:upper:]' 2> /dev/null)_HASH_${plat}"
      # shellcheck disable=SC2154
      (
         set -x
         curl -o pack.bin -L --proto-redir =https "${base}${name}-${ver}-win${plat}-mingw.7z"
         curl -o pack.sig -L --proto-redir =https "${base}${name}-${ver}-win${plat}-mingw.7z.asc"
         gpg --verify pack.sig pack.bin
         openssl dgst -sha256 pack.bin | grep -q "${hash}"
         7z x -y pack.bin > /dev/null
         mv "${name}-${ver}-win${plat}-mingw" "${name}-mingw${plat}"
      )
   done
done
