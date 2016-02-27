#!/bin/sh

# ---------------------------------------------------------------
# Copyright 2015-2016 Viktor Szakats (vszakats.net/harbour)
# See LICENSE.txt for licensing terms.
# ---------------------------------------------------------------

[ "${CI}" = 'True' ] || exit

cd "$(dirname "$0")/.." || exit

_BRANCH="${APPVEYOR_REPO_BRANCH}${TRAVIS_BRANCH}${GIT_BRANCH}"
_BRANC4="$(echo "${_BRANCH}" | cut -c -4)"
_ROOT="$(realpath '.')"

# Don't remove these markers.
#hashbegin
export NGHTTP2_VER='1.8.0'
export NGHTTP2_HASH_32='90b0818a1709b2134b419d1fe050b7eee61073187df3d20779cb22eccd0a39ee'
export NGHTTP2_HASH_64='1b47b5fece19fbff9b12750aed728780a7d96d6a3070379a0d91f197bb4370b8'
export OPENSSL_VER='1.0.2f'
export OPENSSL_HASH_32='00a2ba8e0a1c7e274299bb1cab85e4dc996b8630c68f69a0560b97ce6543c645'
export OPENSSL_HASH_64='a740e955cd273247ec1543dca067d7199e9f5fd5c776b271a8ff1e47b8f2ada0'
export LIBSSH2_VER='1.7.0'
export LIBSSH2_HASH_32='06cbf36b4e395a5f316b53de49df7a2806de6fd092d2a4490a92d566c41bb5a7'
export LIBSSH2_HASH_64='3f0c9193d965471f816bfb7112f2de9257936e2fbd5773d2f1da58a05fae3c92'
export CURL_VER='7.47.1'
export CURL_HASH_32='68e52d7fd466a28879596994ff419c6fcf4e94c54a49bcbc3df17939811ceae9'
export CURL_HASH_64='679da7daea29699a3e8bb476e58abc8b8a70fe4c3b04adff6548338505d8a8af'
#hashend

# debug

# export HB_BUILD_CONTRIBS='no'
# export HB_MKFLAGS="${HB_MKFLAGS} HB_BUILD_OPTIM=no"
# export HB_BUILD_VERBOSE='yes'
# export _HB_BUNDLE_3RDLIB='yes'

[ "${_BRANC4}" = 'msvc' ] || "$(dirname "$0")/mpkg_win_dl.sh" || exit

export HB_VF='daily'
export HB_RT="${_ROOT}"
export HB_MKFLAGS="clean install HB_VERSION=${HB_VF}"
[ "${_BRANCH#*lto*}" != "${_BRANCH}" ] && export HB_BASE='64'
[ "${HB_BASE}" != '64' ] && export HB_SFX_7Z="${HB_RT}/7zsfx/7zsd_All.sfx"
# [ "${HB_BASE}"  = '64' ] && export HB_SFX_7Z="${HB_RT}/7zsfx/7zsd_All_x64.sfx"
export HB_DIR_7Z="${HB_RT}/7z/"
export HB_DIR_UPX="${HB_RT}/upx/"
_ORI_PATH="${PATH}"

# common settings

[ "${_BRANCH#*lto*}" != "${_BRANCH}" ] && export HB_BUILD_CONTRIBS='hbrun hbformat/utils hbct hbcurl hbhpdf hbmzip hbwin hbsqlit3 hbtip hbssl hbexpat hbmemio rddsql hbzebra sddsqlt3 sddodbc hbunix hbmisc hbmxml hbcups hbtest hbtcpio hbcomio hbcrypto hbnetio hbpipeio hbgzio hbbz2io'
export HB_BUILD_STRIP='bin'
export HB_BUILD_PKG='yes'
export _HB_BUILD_PKG_ARCHIVE='no'

# can disable to save time/space

[ "${_BRANC4}" = 'msvc' ] || export _HB_BUNDLE_3RDLIB='yes'
export HB_INSTALL_3RDDYN='yes'
export HB_BUILD_CONTRIB_DYN='yes'
export HB_BUILD_POSTRUN='"./hbtest -noenv" "./hbspeed --noenv --stdout"'

# debug

# export HB_BUILD_CONTRIBS='no'
# export HB_MKFLAGS="${HB_MKFLAGS} HB_BUILD_OPTIM=no"
# export HB_BUILD_VERBOSE='yes'
# export _HB_PKG_DEBUG='yes'
# export _HB_BUNDLE_3RDLIB='yes'

# mingw

if [ "${_BRANC4}" != 'msvc' ] ; then

   [ "${_BRANCH#*lto*}" != "${_BRANCH}" ] && export HB_USER_CFLAGS="${HB_USER_CFLAGS} -flto -ffat-lto-objects"
   [ "${HB_BUILD_MODE}" = 'cpp' ] && export HB_USER_LDFLAGS="${HB_USER_LDFLAGS} -static-libstdc++"

   # Disable Unicows support in 32-bit harbour-*.dll.
   # Get double the build speed in return.
   export __HB_HARBOUR_DLL_UNICOWS=no

   HB_DIR_MINGW_32='/mingw32'
   HB_DIR_MINGW_64='/mingw64'
   export HB_DIR_MINGW="${HB_RT}/mingw64"
   export HB_DIR_OPENSSL_32="${HB_RT}/openssl-mingw32/"
   export HB_DIR_OPENSSL_64="${HB_RT}/openssl-mingw64/"
   export HB_DIR_LIBSSH2_32="${HB_RT}/libssh2-mingw32/"
   export HB_DIR_LIBSSH2_64="${HB_RT}/libssh2-mingw64/"
   export HB_DIR_NGHTTP2_32="${HB_RT}/nghttp2-mingw32/"
   export HB_DIR_NGHTTP2_64="${HB_RT}/nghttp2-mingw64/"
   export HB_DIR_CURL_32="${HB_RT}/curl-mingw32/"
   export HB_DIR_CURL_64="${HB_RT}/curl-mingw64/"

   #

   export HB_WITH_CURL="${HB_DIR_CURL_32}include"
   export HB_WITH_OPENSSL="${HB_DIR_OPENSSL_32}include"
 # export HB_WITH_QT='C:\Qt\5.5\mingw492_32\include'
   if [ -d "${HB_DIR_MINGW}/bin" ] ; then
      export PATH="${HB_DIR_MINGW}/bin:${_ORI_PATH}"
      gcc -v 2> BUILD-mingw.txt
   else
      export PATH="${HB_DIR_MINGW_32}/bin:${_ORI_PATH}"
      gcc -v 2> BUILD-mingw32.txt
   fi
   # shellcheck disable=SC2086
   mingw32-make ${HB_MKFLAGS} HB_COMPILER=mingw HB_CPU=x86

   export HB_WITH_CURL="${HB_DIR_CURL_64}include"
   export HB_WITH_OPENSSL="${HB_DIR_OPENSSL_64}include"
 # export HB_WITH_QT=
   if [ -d "${HB_DIR_MINGW}/bin" ] ; then
      export PATH="${HB_DIR_MINGW}/bin:${_ORI_PATH}"
   else
      export PATH="${HB_DIR_MINGW_64}/bin:${_ORI_PATH}"
      gcc -v 2> BUILD-mingw64.txt
   fi
   # shellcheck disable=SC2086
   mingw32-make ${HB_MKFLAGS} HB_COMPILER=mingw64 HB_CPU=x86_64

   if [ ! -d "${HB_DIR_MINGW}/bin" ] ; then
      [ "${HB_BASE}" != '64' ] && HB_DIR_MINGW="${HB_DIR_MINGW_32}"
      [ "${HB_BASE}"  = '64' ] && HB_DIR_MINGW="${HB_DIR_MINGW_64}"
   fi
fi

# msvc

if [ "${_BRANC4}" = 'msvc' ] ; then

   export PATH="${_ORI_PATH}"
   export HB_USER_CFLAGS=
   export HB_USER_LDFLAGS=
   export HB_WITH_CURL=
   export HB_WITH_OPENSSL=
   export HB_WITH_QT=

 # export _HB_MSVC_ANALYZE='yes'

   export HB_COMPILER_VER

   [ "${_BRANCH}" = 'msvc2008' ] && HB_COMPILER_VER='1500' && _VCVARSALL='9.0'
   [ "${_BRANCH}" = 'msvc2010' ] && HB_COMPILER_VER='1600' && _VCVARSALL='10.0'
   [ "${_BRANCH}" = 'msvc2012' ] && HB_COMPILER_VER='1700' && _VCVARSALL='11.0'
   [ "${_BRANCH}" = 'msvc2013' ] && HB_COMPILER_VER='1800' && _VCVARSALL='12.0'
   [ "${_BRANCH}" = 'msvc2015' ] && HB_COMPILER_VER='1900' && _VCVARSALL='14.0'

   export _VCVARSALL="%ProgramFiles(x86)%\Microsoft Visual Studio ${_VCVARSALL}\VC\vcvarsall.bat"

   [ "${_BRANCH}" = 'msvc2013' ] && HB_WITH_QT='C:\Qt\5.5\msvc2013\include'

   if [ -n "${_VCVARSALL}" ] ; then
      cat << EOF > _make.bat
         call "%_VCVARSALL%" x86
         win-make.exe %HB_MKFLAGS% HB_COMPILER=msvc
EOF
      ./_make.bat
      rm _make.bat
   fi

   # 64-bit target not supported by these MSVC versions
   [ "${_BRANCH}" = 'msvc2008' ] && _VCVARSALL=
   [ "${_BRANCH}" = 'msvc2010' ] && _VCVARSALL=

   [ "${_BRANCH}" = 'msvc2013' ] && HB_WITH_QT='C:\Qt\5.5\msvc2013_64\include'

   if [ -n "${_VCVARSALL}" ] ; then
      cat << EOF > _make.bat
         call "%_VCVARSALL%" x86_amd64
         win-make.exe %HB_MKFLAGS% HB_COMPILER=msvc64
EOF
      ./_make.bat
      rm _make.bat
   fi
fi

# packaging

[ "${_BRANC4}" = 'msvc' ] || "$(dirname "$0")/mpkg_win.sh"
