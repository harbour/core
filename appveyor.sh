#!/bin/sh

# ---------------------------------------------------------------
# Copyright 2015-2016 Viktor Szakats (vszakats.net/harbour)
# See LICENSE.txt for licensing terms.
# ---------------------------------------------------------------

[ "${APPVEYOR}" = 'True' ] || exit

cd "$(dirname "$0")" || exit

_BRANCH="${APPVEYOR_REPO_BRANCH}${TRAVIS_BRANCH}${GIT_BRANCH}"
_BRANC4="$(echo "${_BRANCH}" | cut -c -4)"
_ROOT="${APPVEYOR_BUILD_FOLDER}"

# Don't remove these markers.
#hashbegin
export NGHTTP2_VER='1.6.0'
export NGHTTP2_HASH_32='0e20a0a3c33123aba7e032cae2a0c3db7383e2b53e91e04c12a579075e08659e'
export NGHTTP2_HASH_64='2f3050587c7147643c1ed267c671bd71234711028e941b0dc4f852f77e498abe'
export OPENSSL_VER='1.0.2e'
export OPENSSL_HASH_32='46cca8b7e073f948266819c878b9e95eff06c1b277c88c3d84e48a06cc615d78'
export OPENSSL_HASH_64='337e595354327776a974f71b026d24b1d349181867de1259f918d1f93df87663'
export LIBSSH2_VER='1.6.0'
export LIBSSH2_HASH_32='b955334c27c0fdb9e17b003050b05e43bdada9ac73f0d4a6e956585bc58b5c6c'
export LIBSSH2_HASH_64='4882e42b6ecfd77a0167e77ffdda1006b020058660e6684e7e4b7fc06f95dbac'
export CURL_VER='7.46.0'
export CURL_HASH_32='c7bfd0a3edccb25f84ce6c479e523e235cdb0514cecb891bdb0427a3f7313365'
export CURL_HASH_64='5317dc93f276ccb9282bdd453a2e31d53d289574256807b59997dbb970c2eba2'
#hashend

# debug

# export HB_BUILD_CONTRIBS='no'
# export HB_MKFLAGS="${HB_MKFLAGS} HB_BUILD_OPTIM=no"
# export HB_BUILD_VERBOSE='yes'
# export _HB_BUNDLE_3RDLIB='yes'

[ "${_BRANC4}" = 'msvc' ] || ./package/mpkg_win_dl.sh

export HB_VF='daily'
export HB_RT="${_ROOT}"
export HB_MKFLAGS="clean install HB_VERSION=${HB_VF}"
[ "${_BRANCH}" = 'lto' ] && export HB_BASE='64'
[ "${HB_BASE}" != '64' ] && export HB_SFX_7Z="${HB_RT}/7zsfx/7zsd_All.sfx"
# [ "${HB_BASE}"  = '64' ] && export HB_SFX_7Z="${HB_RT}/7zsfx/7zsd_All_x64.sfx"
export HB_DIR_7Z="${HB_RT}/7z/"
export HB_DIR_UPX="${HB_RT}/upx/"
_ORI_PATH="${PATH}"

# common settings

[ "${_BRANCH}" = 'lto' ] && export HB_BUILD_CONTRIBS='hbrun hbformat/utils hbct hbcurl hbhpdf hbmzip hbwin hbsqlit3 hbtip hbssl hbexpat hbmemio rddsql hbzebra sddsqlt3 sddodbc hbunix hbmisc hbmxml hbcups hbtest hbtcpio hbcomio hbcrypto hbnetio hbpipeio hbgzio hbbz2io'
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

   [ "${_BRANCH}" = 'lto' ] && export HB_USER_CFLAGS="${HB_USER_CFLAGS} -flto -ffat-lto-objects"
   [ "${HB_BUILD_MODE}" = 'cpp' ] && export HB_USER_LDFLAGS="${HB_USER_LDFLAGS} -static-libgcc -static-libstdc++"

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
   export PATH="${HB_DIR_MINGW}/bin:${_ORI_PATH}"
   # shellcheck disable=SC2086
   ./win-make.exe ${HB_MKFLAGS} HB_COMPILER=mingw HB_CPU=x86

   export HB_WITH_CURL="${HB_DIR_CURL_64}include"
   export HB_WITH_OPENSSL="${HB_DIR_OPENSSL_64}include"
 # export HB_WITH_QT=
   export PATH="${HB_DIR_MINGW}/bin:${_ORI_PATH}"
   # shellcheck disable=SC2086
   ./win-make.exe ${HB_MKFLAGS} HB_COMPILER=mingw64 HB_CPU=x86_64
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
   fi
fi

# packaging

[ "${_BRANC4}" = 'msvc' ] || ./package/mpkg_win.sh
