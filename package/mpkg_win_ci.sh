#!/bin/sh

# ---------------------------------------------------------------
# Copyright 2015-2016 Viktor Szakats (vszakats.net/harbour)
# See LICENSE.txt for licensing terms.
# ---------------------------------------------------------------

[ "${CI}" = 'True' ] || exit

cd "$(dirname "$0")/.." || exit

_BRANCH="${APPVEYOR_REPO_BRANCH}${TRAVIS_BRANCH}${CI_BUILD_REF_NAME}${GIT_BRANCH}"
_BRANC4="$(echo "${_BRANCH}" | cut -c -4)"
_ROOT="$(realpath '.')"

# Don't remove these markers.
#hashbegin
export NGHTTP2_VER='1.12.0'
export NGHTTP2_HASH_32='6c4db79ef62346995837d8c2884b27d0c23444a88dab1e15eee0b92a377f7cdc'
export NGHTTP2_HASH_64='2cda7bfb848f232a24bdc8ab837d99cbaba007fc950353e14bbfdebf0abe8961'
export OPENSSL_VER='1.0.2h'
export OPENSSL_HASH_32='52fc4efa68fdd49339e2c96b757ca75ab8cdb75ad0c14f0d68c2656c7b1f8ce5'
export OPENSSL_HASH_64='18fd4fec935e7c70e5ed1f06263ba789d25c7f0020dabe4f89fe82332d45229b'
export LIBSSH2_VER='1.7.0'
export LIBSSH2_HASH_32='432d6a4ebe5ed76c1ab5eec6482dff71ceacdb6317a45f0519c67c4317652213'
export LIBSSH2_HASH_64='5390dcb28c6b359167d4777a33d15959ef75c3e0d8812182c0668a80d13bd418'
export CURL_VER='7.49.1'
export CURL_HASH_32='87132b3320f3c7ded8238dedfce939a465c5de56be9aff7414139f6d95be5fce'
export CURL_HASH_64='bcf1b5bd97c462e2044177faa5d1d30bcf2a46a191001c752e5b69427e84ba61'
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
_ori_path="${PATH}"

# common settings

[ "${_BRANCH#*lto*}" != "${_BRANCH}" ] && export HB_BUILD_CONTRIBS='hbrun hbformat/utils hbct hbcurl hbhpdf hbmzip hbwin hbsqlit3 hbtip hbssl hbexpat hbmemio rddsql hbzebra sddsqlt3 sddodbc hbunix hbmisc hbmxml hbcups hbtest hbtcpio hbcomio hbcrypto hbnetio hbpipeio hbgzio hbbz2io'
export HB_BUILD_STRIP='bin'
export HB_BUILD_PKG='yes'
export _HB_BUILD_PKG_ARCHIVE='no'

# can disable to save time/space

[ "${_BRANC4}" = 'msvc' ] || export _HB_BUNDLE_3RDLIB='yes'
export HB_INSTALL_3RDDYN='yes'
export HB_BUILD_CONTRIB_DYN='yes'
export HB_BUILD_POSTRUN='"./hbmk2 --version" "./hbtest -noenv" "./hbspeed --noenv --stdout"'

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

   export HB_DIR_MINGW="${HB_RT}/mingw64"
   if [ -d "${HB_DIR_MINGW}/bin" ] ; then
      # Use the same toolchain for both targets
      export HB_DIR_MINGW_32="${HB_DIR_MINGW}"
      export HB_DIR_MINGW_64="${HB_DIR_MINGW}"
      _build_info_32='BUILD-mingw.txt'
      _build_info_64=/dev/null
   else
      export HB_DIR_MINGW_32='/mingw32'
      export HB_DIR_MINGW_64='/mingw64'
      [ "${HB_BASE}" != '64' ] && HB_DIR_MINGW="${HB_DIR_MINGW_32}"
      [ "${HB_BASE}"  = '64' ] && HB_DIR_MINGW="${HB_DIR_MINGW_64}"
      _build_info_32='BUILD-mingw32.txt'
      _build_info_64='BUILD-mingw64.txt'
   fi

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
   export PATH="${HB_DIR_MINGW_32}/bin:${_ori_path}"
   gcc -v 2> "${_build_info_32}"
   # shellcheck disable=SC2086
   mingw32-make ${HB_MKFLAGS} HB_COMPILER=mingw HB_CPU=x86 || exit 1

   export HB_WITH_CURL="${HB_DIR_CURL_64}include"
   export HB_WITH_OPENSSL="${HB_DIR_OPENSSL_64}include"
   export PATH="${HB_DIR_MINGW_64}/bin:${_ori_path}"
   gcc -v 2> "${_build_info_64}"
   # shellcheck disable=SC2086
   mingw32-make ${HB_MKFLAGS} HB_COMPILER=mingw64 HB_CPU=x86_64 || exit 1
fi

# msvc

if [ "${_BRANC4}" = 'msvc' ] ; then

   export PATH="${_ori_path}"
   export HB_USER_CFLAGS=
   export HB_USER_LDFLAGS=
   export HB_WITH_CURL=
   export HB_WITH_OPENSSL=

 # export _HB_MSVC_ANALYZE='yes'

   export HB_COMPILER_VER

   [ "${_BRANCH}" = 'msvc2008' ] && HB_COMPILER_VER='1500' && _VCVARSALL='9.0'
   [ "${_BRANCH}" = 'msvc2010' ] && HB_COMPILER_VER='1600' && _VCVARSALL='10.0'
   [ "${_BRANCH}" = 'msvc2012' ] && HB_COMPILER_VER='1700' && _VCVARSALL='11.0'
   [ "${_BRANCH}" = 'msvc2013' ] && HB_COMPILER_VER='1800' && _VCVARSALL='12.0'
   [ "${_BRANCH}" = 'msvc2015' ] && HB_COMPILER_VER='1900' && _VCVARSALL='14.0'
   [ "${_BRANCH}" = 'msvc15'   ] && HB_COMPILER_VER='2000' && _VCVARSALL='15.0'

   export _VCVARSALL="%ProgramFiles(x86)%\Microsoft Visual Studio ${_VCVARSALL}\VC\vcvarsall.bat"

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
