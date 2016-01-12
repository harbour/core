#!/bin/sh

# ---------------------------------------------------------------
# Copyright 2009-2016 Viktor Szakats (vszakats.net/harbour)
# See LICENSE.txt for licensing terms.
# ---------------------------------------------------------------

cd "$(dirname "$0")" || exit

if [ "${_HB_PKG_DEBUG}" = "yes" ] ; then
   set -x
else
   set +x
fi

# - Requires Git for Windows or busybox (untested) to run on Windows
# - Adjust target dir, MinGW dirs, set HB_DIR_UPX, HB_DIR_7Z, HB_DIR_MINGW,
#   create required packages beforehand.
# - Run this from vanilla official source tree only.
# - Requires BCC in PATH or HB_DIR_BCC_IMPLIB (only when including BCC build).
# - Requires GNU sed, touch and OpenSSL tools in PATH
# - Optional HB_SFX_7Z envvar pointed to 7z SFX module

echo "! Self: $0"

readonly HB_VS_DEF=34
readonly HB_VL_DEF=340
readonly HB_VM_DEF=3.4
readonly HB_VF_DEF=3.4.0dev
readonly HB_RT_DEF=C:/hb

[ -z "${HB_VS}" ] && HB_VS="${HB_VS_DEF}"
[ -z "${HB_VL}" ] && HB_VL="${HB_VL_DEF}"
[ -z "${HB_VM}" ] && HB_VM="${HB_VM_DEF}"
[ -z "${HB_VF}" ] && HB_VF="${HB_VF_DEF}"
[ -z "${HB_RT}" ] && HB_RT="${HB_RT_DEF}"

HB_RT="$(echo "${HB_RT}" | sed 's|\\|/|g')"
HB_DIR_MINGW="$(echo "${HB_DIR_MINGW}" | sed 's|\\|/|g')"

HB_DR="hb${HB_VS}/"
HB_ABSROOT="${HB_RT}/${HB_DR}"

_BRANCH="$(git rev-parse --abbrev-ref HEAD)"
_SCRIPT="$(realpath "$(pwd)/mpkg.hb")"
_ROOT="$(realpath "$(pwd)/..")"

# Hack for Git for Windows. Windows system paths may override
# standard tools.
case "$(uname)" in
   *_NT*) alias find=/usr/bin/find;;
esac

if [ -z "${HB_BASE}" ] ; then
   # Auto-detect the base bitness, by default it will be 32-bit,
   # and 64-bit if it's the only one available.
   if [ -d "../pkg/win/mingw/harbour-${HB_VF}-win-mingw" ] ; then
      # MinGW 32-bit base system
      LIB_TARGET='32'
   elif [ -d "../pkg/win/mingw64/harbour-${HB_VF}-win-mingw64" ] ; then
      # MinGW 64-bit base system
      LIB_TARGET='64'
   fi
else
   LIB_TARGET="${HB_BASE}"
fi

echo "! Creating ${LIB_TARGET}-bit hosted package"

# Assemble package from per-target builds

[ ! -d "${HB_ABSROOT}" ] || rm -f -r "${HB_ABSROOT}"
mkdir -p "${HB_ABSROOT}"

(
   cd .. || exit
   # shellcheck disable=SC2046
   cp -f -p --parents $(find 'addons' -type f -name '*.txt') "${HB_ABSROOT}"
   # shellcheck disable=SC2046
   cp -f -p --parents $(find 'extras' -type f -name '*')     "${HB_ABSROOT}"
   # shellcheck disable=SC2046
   cp -f -p --parents $(find 'tests'  -type f -name '*')     "${HB_ABSROOT}"
)

mkdir -p "${HB_ABSROOT}bin/"

# Copy these first to let 3rd party .dlls with overlapping names
# be overwritten by selected native target's binaries.
if ls       ../pkg/wce/mingwarm/harbour-${HB_VF}-wce-mingwarm/bin/*.dll > /dev/null 2>&1 ; then
   cp -f -p ../pkg/wce/mingwarm/harbour-${HB_VF}-wce-mingwarm/bin/*.dll "${HB_ABSROOT}bin/"
fi

if [ "${LIB_TARGET}" = "32" ] ; then
   if ls       ../pkg/win/mingw64/harbour-${HB_VF}-win-mingw64/bin/*.dll > /dev/null 2>&1 ; then
      cp -f -p ../pkg/win/mingw64/harbour-${HB_VF}-win-mingw64/bin/*.dll "${HB_ABSROOT}bin/"
   fi
   ( cd "../pkg/win/mingw/harbour-${HB_VF}-win-mingw" && cp -f -p -R ./* "${HB_ABSROOT}" )
elif [ "${LIB_TARGET}" = "64" ] ; then
   if ls       ../pkg/win/mingw/harbour-${HB_VF}-win-mingw/bin/*.dll > /dev/null 2>&1 ; then
      cp -f -p ../pkg/win/mingw/harbour-${HB_VF}-win-mingw/bin/*.dll "${HB_ABSROOT}bin/"
   fi
   ( cd "../pkg/win/mingw64/harbour-${HB_VF}-win-mingw64" && cp -f -p -R ./* "${HB_ABSROOT}" )
fi

for dir in \
   "../pkg/dos/watcom/hb${HB_VL}wa" \
   "../pkg/os2/watcom/harbour-${HB_VF}-os2-watcom" \
   "../pkg/wce/mingwarm/harbour-${HB_VF}-wce-mingwarm" \
   "../pkg/win/bcc/harbour-${HB_VF}-win-bcc" \
   "../pkg/win/bcc64/harbour-${HB_VF}-win-bcc64" \
   "../pkg/win/mingw/harbour-${HB_VF}-win-mingw" \
   "../pkg/win/mingw64/harbour-${HB_VF}-win-mingw64" \
   "../pkg/win/msvc/harbour-${HB_VF}-win-msvc" \
   "../pkg/win/msvc64/harbour-${HB_VF}-win-msvc64" \
   "../pkg/win/watcom/harbour-${HB_VF}-win-watcom" ; do
   if [ -d "${dir}" ] ; then
      (
         cd "${dir}" || exit
         # shellcheck disable=SC2046
         cp -f -p --parents $(find 'lib' -type f -name '*') "${HB_ABSROOT}"
      )
   fi
done

# Create special implibs for Borland (requires BCC in PATH)
# NOTE: Using intermediate .def files, because direct .dll to .lib conversion
#       is buggy in BCC55 and BCC58 (no other versions tested), leaving off
#       leading underscore from certain ("random") symbols, resulting in
#       unresolved externals, when trying to use it. [vszakats]
if [ -d "${HB_ABSROOT}lib/win/bcc" ] ; then
   for file in ${HB_ABSROOT}bin/*-${HB_VS}.dll ; do
      bfile="$(basename "${file}")"
      "${HB_DIR_BCC_IMPLIB}impdef.exe" -a "${HB_ABSROOT}lib/win/bcc/${bfile}-bcc.defraw" "${file}"
      sed -f "s/LIBRARY     ${bfile}.DLL/LIBRARY     \"${bfile}.dll\"/Ig" < "${HB_ABSROOT}lib/win/bcc/${bfile}-bcc.defraw" > "${HB_ABSROOT}lib/win/bcc/${bfile}-bcc.def"
      "${HB_DIR_BCC_IMPLIB}implib.exe" -c -a "${HB_ABSROOT}lib/win/bcc/${bfile}-bcc.lib" "${HB_ABSROOT}lib/win/bcc/${bfile}-bcc.def"
      touch -c "${HB_ABSROOT}lib/win/bcc/${bfile}-bcc.lib" -r "${HB_ABSROOT}README.md"
      rm -f "${HB_ABSROOT}lib/win/bcc/${bfile}-bcc.defraw" "${HB_ABSROOT}lib/win/bcc/${bfile}-bcc.def"
   done
fi

# Workaround for ld --no-insert-timestamp bug that exist as of
# binutils 2.25, when the PE build timestamp field is often
# filled with random bytes instead of zeroes. -s option is not
# fixing this, 'strip' randomly fails either, so we're
# patching manually. Do this while only Harbour built binaries are
# copied into the bin directory to not modify 3rd party binaries.
cp -f -p "${HB_ABSROOT}bin/hbmk2.exe" "${HB_ABSROOT}bin/hbmk2-temp.exe"
"${HB_ABSROOT}bin/hbmk2-temp.exe" "${_SCRIPT}" pe "${_ROOT}" "${HB_ABSROOT}bin/*.exe"
"${HB_ABSROOT}bin/hbmk2-temp.exe" "${_SCRIPT}" pe "${_ROOT}" "${HB_ABSROOT}bin/*.dll"
rm -f "${HB_ABSROOT}bin/hbmk2-temp.exe"

# Workaround for ld --no-insert-timestamp issue in that it
# won't remove internal timestamps from generated implibs.
# Slow. Requires binutils 2.23 (maybe 2.24/2.25).
# Short synonym '-D' is not recognized as of binutils 2.25.
for files in \
   "${HB_ABSROOT}lib/win/mingw/*-*.*" \
   "${HB_ABSROOT}lib/win/mingw64/*-*.*" \
   "${HB_ABSROOT}lib/win/mingw/*_dll*.*" \
   "${HB_ABSROOT}lib/win/mingw64/*_dll*.*" \
   "${HB_ABSROOT}lib/win/msvc/*.lib" \
   "${HB_ABSROOT}lib/win/msvc64/*.lib" ; do
   # shellcheck disable=SC2086
   if ls ${files} > /dev/null 2>&1 ; then
      "${HB_DIR_MINGW}/bin/strip" -p --enable-deterministic-archives -g "${files}"
   fi
done

# Copy upx

if [ -n "${HB_DIR_UPX}" ] ; then
   cp -f -p "${HB_DIR_UPX}upx.exe" "${HB_ABSROOT}bin/"
   cp -f -p "${HB_DIR_UPX}LICENSE" "${HB_ABSROOT}LICENSE_upx.txt"
fi

# Copy 7z

if [ -n "${HB_DIR_7Z}" ] ; then
   if [ "${LIB_TARGET}" = "64" ] ; then
      cp -f -p "${HB_DIR_7Z}x64/7za.exe" "${HB_ABSROOT}bin/"
   else
      cp -f -p "${HB_DIR_7Z}7za.exe"     "${HB_ABSROOT}bin/"
   fi
   cp -f -p "${HB_DIR_7Z}License.txt" "${HB_ABSROOT}LICENSE_7z.txt"
fi

# Copy curl

if [ "${LIB_TARGET}" = "64" ] ; then
   HB_DIR_CURL="${HB_DIR_CURL_64}"
else
   HB_DIR_CURL="${HB_DIR_CURL_32}"
fi
if [ -n "${HB_DIR_CURL}" ] ; then
   cp -f -p "${HB_DIR_CURL}bin/curl.exe"           "${HB_ABSROOT}bin/"
   cp -f -p "${HB_DIR_CURL}bin/curl-ca-bundle.crt" "${HB_ABSROOT}bin/"
   cp -f -p "${HB_DIR_CURL}COPYING.txt"            "${HB_ABSROOT}LICENSE_curl.txt"
fi

# Copy 3rd party static libraries

if [ "${_HB_BUNDLE_3RDLIB}" = "yes" ] ; then
   for name in \
         'openssl' \
         'libssh2' \
         'nghttp2' \
         'curl' \
   ; do
      dir_32="HB_DIR_$(echo "${name}" | tr '[:lower:]' '[:upper:]' 2> /dev/null)_32"
      dir_32=$(echo "${!dir_32}" | sed 's|\\|/|g')
      dir_64="HB_DIR_$(echo "${name}" | tr '[:lower:]' '[:upper:]' 2> /dev/null)_64"
      dir_64=$(echo "${!dir_64}" | sed 's|\\|/|g')
      for file in ${dir_32}lib/*.a ; do
         if [ -f "${file}" ] && echo "${file}" | grep -v 'dll' > /dev/null 2>&1 ; then
            cp -f -p "${file}" "${HB_ABSROOT}lib/win/mingw/"
         fi
      done
      for file in ${dir_64}lib/*.a ; do
         if [ -f "${file}" ] && echo "${file}" | grep -v 'dll' > /dev/null 2>&1 ; then
            cp -f -p "${file}" "${HB_ABSROOT}lib/win/mingw64/"
         fi
      done
      [ -f "${dir_64}COPYING.txt" ] && cp -f -p "${dir_64}COPYING.txt" "${HB_ABSROOT}LICENSE_${name}.txt"
      [ -f "${dir_64}LICENSE.txt" ] && cp -f -p "${dir_64}LICENSE.txt" "${HB_ABSROOT}LICENSE_${name}.txt"
   done
fi

# Copy core 3rd party headers

(
   cd .. || exit
   # shellcheck disable=SC2046
   cp -f -p --parents $(find 'src/3rd' -name '*.h') "${HB_ABSROOT}"
)

# TODO: This whole section should only be relevant
#       if the distro is MinGW based. Much of it is
#       useful only if MinGW _is_ actually bundled
#       with the package, which is probably something
#       that should be avoided in the future.

# Copy MinGW runtime .dlls

# Pick the ones from a multi-target MinGW distro
# that match the bitness of our base target.
_MINGW_DLL_DIR="${HB_DIR_MINGW}/bin"
[ "${LIB_TARGET}" = "32" ] && [ -d "${HB_DIR_MINGW}/x86_64-w64-mingw32/lib32" ] && _MINGW_DLL_DIR="${HB_DIR_MINGW}/x86_64-w64-mingw32/lib32"
[ "${LIB_TARGET}" = "64" ] && [ -d "${HB_DIR_MINGW}/i686-w64-mingw32/lib64"   ] && _MINGW_DLL_DIR="${HB_DIR_MINGW}/i686-w64-mingw32/lib64"

# shellcheck disable=SC2086
if ls       ${_MINGW_DLL_DIR}/libgcc_s_*.dll > /dev/null 2>&1 ; then
   cp -f -p ${_MINGW_DLL_DIR}/libgcc_s_*.dll "${HB_ABSROOT}bin/"
fi
# shellcheck disable=SC2086
if ls       ${_MINGW_DLL_DIR}/mingwm*.dll > /dev/null 2>&1 ; then
   cp -f -p ${_MINGW_DLL_DIR}/mingwm*.dll "${HB_ABSROOT}bin/"
fi
# shellcheck disable=SC2086
if ls       ${_MINGW_DLL_DIR}/libwinpthread-*.dll > /dev/null 2>&1 ; then
   cp -f -p ${_MINGW_DLL_DIR}/libwinpthread-*.dll "${HB_ABSROOT}bin/"
fi

# Copy getmingw.hb with some burn-in

sed -e "s/_HB_VF_DEF_/${HB_VF_DEF}/g" 'getmingw.hb' > "${HB_ABSROOT}bin/getmingw.hb"
touch -c "${HB_ABSROOT}bin/getmingw.hb" -r "${HB_ABSROOT}README.md"

cp -f -p 'getsrc.hb' "${HB_ABSROOT}bin/"

# Burn build information into RELNOTES.txt

_HB_VER="${HB_VF}"
if [ "${HB_VF}" != "${HB_VF_DEF}" ] ; then
   _HB_VER="${HB_VF_DEF} ${_HB_VER}"
fi

VCS_ID="$(git rev-parse --short HEAD)"
sed -e "s/_VCS_ID_/${VCS_ID}/g" \
    -e "s/_HB_VERSION_/${_HB_VER}/g" 'RELNOTES.txt' > "${HB_ABSROOT}RELNOTES.txt"
touch -c "${HB_ABSROOT}RELNOTES.txt" -r "${HB_ABSROOT}README.md"

# Create tag update JSON request
# https://developer.github.com/v3/git/refs/#update-a-reference

echo "{\"sha\":\"$(git rev-parse --verify HEAD)\",\"force\":true}" > "${_ROOT}/git_tag_patch.json"

# Register build information

(
   "${HB_ABSROOT}bin/harbour" -build 2>&1 | grep -Ev '^(Version:|Platform:|Extra )'
   set | grep '_VER=' | grep -v '^_'
   echo ---------------------------
   set | grep -E '^(HB_USER_|HB_BUILD_|HB_WITH_|HB_STATIC_)' | grep -Ev '(HB_BUILD_POSTRUN=|HB_BUILD_PKG=)'
   echo ---------------------------
   cd "${HB_ABSROOT}lib" || exit
   find . -type d | grep -Eo '\./[a-z]+?/[a-z0-9]+?$' | cut -c 3-
) >> "${HB_ABSROOT}BUILD.txt"
touch -c "${HB_ABSROOT}BUILD.txt" -r "${HB_ABSROOT}README.md"

# Convert EOLs

"${HB_ABSROOT}bin/hbmk2.exe" "${_SCRIPT}" nl "${HB_ABSROOT}*.md"
"${HB_ABSROOT}bin/hbmk2.exe" "${_SCRIPT}" nl "${HB_ABSROOT}*.txt"
"${HB_ABSROOT}bin/hbmk2.exe" "${_SCRIPT}" nl "${HB_ABSROOT}addons/*.txt"
"${HB_ABSROOT}bin/hbmk2.exe" "${_SCRIPT}" nl "${HB_ABSROOT}doc/*.txt"

# Reset Windows attributes

case "$(uname)" in
   *_NT*) find "$(echo "${HB_ABSROOT}" | sed 's|/$||g')" -exec attrib +A -R {} \;
esac

# Create installer/archive
(
   cd "${HB_RT}" || exit

   (
      echo '*.md'
      echo '*.txt'
      echo 'bin/*.crt'
      echo 'bin/*.dll'
      echo 'bin/*.exe'
      echo 'bin/*.hb'
      echo 'include/*'
      echo 'lib/*'
      echo 'src/*'
      echo 'doc/*'
      echo 'contrib/*'
      echo 'extras/*'
      echo 'tests/*'
      echo 'addons/*.txt'
   ) >> "${_ROOT}/_hbfiles"

   _PKGNAME="${_ROOT}/harbour-${HB_VF}-win.7z"

   rm -f "${_PKGNAME}"
   (
      cd "${HB_DR}" || exit
      bin/hbmk2.exe "${_SCRIPT}" ts "${_ROOT}"
      # NOTE: add -stl option after updating to 15.12 or upper
      "${HB_DIR_7Z}7za" a -r -mx "${_PKGNAME}" "@${_ROOT}/_hbfiles" > /dev/null
   )

   if [ -f "${HB_SFX_7Z}" ] ; then

      cat << EOF > "_7zconf"
;!@Install@!UTF-8!
Title="Harbour ${HB_VF}"
BeginPrompt="Do you want to install Harbour ${HB_VF}?"
CancelPrompt="Do you want to cancel installation?"
ExtractPathText="Select destination path"
ExtractPathTitle="Harbour ${HB_VF}"
ExtractTitle="Extracting"
ExtractDialogText="Please wait..."
ExtractCancelText="Abort"
Progress="yes"
GUIFlags="8+64+256+4096"
GUIMode="1"
OverwriteMode="0"
InstallPath="C:\hb${HB_VS}"
Shortcut="Du,{cmd.exe},{/k cd /d \"%%T\\\\bin\\\\\"},{},{},{Harbour Shell},{%%T\\\\bin\\\\},{%%T\\\\bin\\\\hbmk2.exe},{0}"
RunProgram="nowait:notepad.exe \"%%T\\\\RELNOTES.txt\""
;RunProgram="hbmk2.exe \"%%T\"\\\\install.hb"
;Delete=""
;!@InstallEnd@!
EOF

      cat "${HB_SFX_7Z}" _7zconf "${_PKGNAME}" > "${_PKGNAME}.exe"

      rm "${_PKGNAME}"

      _PKGNAME="${_PKGNAME}.exe"
   fi

   rm "${_ROOT}/_hbfiles"

   touch -c "${_PKGNAME}" -r "${HB_ABSROOT}README.md"

   # <filename>: <size> bytes <YYYY-MM-DD> <HH:MM>
   case "$(uname)" in
      *BSD|Darwin) stat -f '%N: %z bytes %Sm' -t '%Y-%m-%d %H:%M' "${_PKGNAME}";;
      *)           stat -c '%n: %s bytes %y' "${_PKGNAME}";;
   esac
   openssl dgst -sha256 "${_PKGNAME}"
)

if [ "${_BRANCH}" = "master" ] ; then
   (
      set +x
      curl -sS \
         -H "Authorization: token ${GITHUB_TOKEN}" \
         -X PATCH "https://api.github.com/repos/vszakats/harbour-core/git/refs/tags/v${HB_VF_DEF}" \
         -d "@${_ROOT}/git_tag_patch.json"
   )
fi
