#!/bin/sh

# ---------------------------------------------------------------
# Copyright 2009-2016 Viktor Szakats (vszakats.net/harbour)
# See LICENSE.txt for licensing terms.
# ---------------------------------------------------------------

cd "$(dirname "$0")" || exit

# - Requires MSYS2 or Git for Windows to run on Windows
# - Requires 7z in PATH
# - Adjust target dir, MinGW dirs, set HB_DIR_UPX, HB_DIR_7Z, HB_DIR_MINGW,
#   create required packages beforehand.
# - Optional HB_SFX_7Z envvar pointed to 7z SFX module
# - Run this from vanilla official source tree only.

# TOFIX: hbmk2.exe invocations break cross-builds.
#        A native hbmk2 copy would need to be called instead.

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

_BRANCH="${APPVEYOR_REPO_BRANCH}${TRAVIS_BRANCH}${GIT_BRANCH}"
_SCRIPT="$(realpath 'mpkg.hb')"
_ROOT="$(realpath '..')"

echo "! Branch: '${_BRANCH}'"

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

if [ "${LIB_TARGET}" = '32' ] ; then
   if ls       ../pkg/win/mingw64/harbour-${HB_VF}-win-mingw64/bin/*.dll > /dev/null 2>&1 ; then
      cp -f -p ../pkg/win/mingw64/harbour-${HB_VF}-win-mingw64/bin/*.dll "${HB_ABSROOT}bin/"
   fi
   ( cd "../pkg/win/mingw/harbour-${HB_VF}-win-mingw" && cp -f -p -R ./* "${HB_ABSROOT}" )
elif [ "${LIB_TARGET}" = '64' ] ; then
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

# Workaround for ld --no-insert-timestamp bug that exist as of
# binutils 2.25, when the PE build timestamp field is often
# filled with random bytes instead of zeroes. -s option is not
# fixing this, 'strip' randomly fails either, so we're
# patching manually. Do this while only Harbour built binaries are
# present in the bin directory to not modify 3rd party binaries.
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
      if [ -d "${HB_DIR_MINGW}/bin" ] ; then
         "${HB_DIR_MINGW}/bin/strip" -p --enable-deterministic-archives -g "${files}"
      else
         strip -p --enable-deterministic-archives -g "${files}"
      fi
   fi
done

# Copy upx

if [ -n "${HB_DIR_UPX}" ] ; then
   cp -f -p "${HB_DIR_UPX}upx.exe" "${HB_ABSROOT}bin/"
   cp -f -p "${HB_DIR_UPX}LICENSE" "${HB_ABSROOT}LICENSE_upx.txt"
fi

# Copy 7z

if [ -n "${HB_DIR_7Z}" ] ; then
   if [ "${LIB_TARGET}" = '64' ] ; then
      cp -f -p "${HB_DIR_7Z}x64/7za.exe" "${HB_ABSROOT}bin/"
   else
      cp -f -p "${HB_DIR_7Z}7za.exe"     "${HB_ABSROOT}bin/"
   fi
   cp -f -p "${HB_DIR_7Z}License.txt" "${HB_ABSROOT}LICENSE_7z.txt"
fi

# Copy curl

if [ "${LIB_TARGET}" = '64' ] ; then
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

if [ "${_HB_BUNDLE_3RDLIB}" = 'yes' ] ; then
   for name in \
         'openssl' \
         'libssh2' \
         'nghttp2' \
         'curl' \
   ; do
      eval dir_32="\$$(echo "HB_DIR_${name}_32" | tr '[:lower:]' '[:upper:]' 2> /dev/null)"
      dir_32=$(echo "${dir_32}" | sed 's|\\|/|g')
      eval dir_64="\$$(echo "HB_DIR_${name}_64" | tr '[:lower:]' '[:upper:]' 2> /dev/null)"
      dir_64=$(echo "${dir_64}" | sed 's|\\|/|g')
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
if [ -d "${_MINGW_DLL_DIR}" ] ; then

   [ "${LIB_TARGET}" = '32' ] && [ -d "${HB_DIR_MINGW}/x86_64-w64-mingw32/lib32" ] && _MINGW_DLL_DIR="${HB_DIR_MINGW}/x86_64-w64-mingw32/lib32"
   [ "${LIB_TARGET}" = '64' ] && [ -d "${HB_DIR_MINGW}/i686-w64-mingw32/lib64"   ] && _MINGW_DLL_DIR="${HB_DIR_MINGW}/i686-w64-mingw32/lib64"

   # shellcheck disable=SC2086
   if ls       ${_MINGW_DLL_DIR}/libgcc_s_*.dll > /dev/null 2>&1 ; then
      cp -f -p ${_MINGW_DLL_DIR}/libgcc_s_*.dll "${HB_ABSROOT}bin/"
   fi
   # shellcheck disable=SC2086
   if ls       ${_MINGW_DLL_DIR}/libwinpthread-*.dll > /dev/null 2>&1 ; then
      cp -f -p ${_MINGW_DLL_DIR}/libwinpthread-*.dll "${HB_ABSROOT}bin/"
   fi
   # shellcheck disable=SC2086
   if ls       ${_MINGW_DLL_DIR}/mingwm*.dll > /dev/null 2>&1 ; then
      cp -f -p ${_MINGW_DLL_DIR}/mingwm*.dll "${HB_ABSROOT}bin/"
   fi
fi

# Copy getmingw.hb with some burn-in

sed "s/_HB_VF_DEF_/${HB_VF_DEF}/g" 'getmingw.hb' > "${HB_ABSROOT}bin/getmingw.hb"
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

# Copy optional text files containing compiler details

if ls       ../BUILD*.txt > /dev/null 2>&1 ; then
   cp -f -p ../BUILD*.txt "${HB_ABSROOT}"
fi

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
   7z a -bd -r -mx "${_PKGNAME}" "@${_ROOT}/_hbfiles" > /dev/null
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

cd - || exit

if [ "${_BRANCH}" = 'lto' ] ; then
   (
      # https://pushover.net/api
      set +x
      curl -sS \
         --form-string "user=${PUSHOVER_USER}" \
         --form-string "token=${PUSHOVER_TOKEN}" \
         --form-string 'title=Harbour' \
         --form-string "message=Build ready: ${_BRANCH}" \
         --form-string 'html=1' \
         --form-string 'priority=1' \
         https://api.pushover.net/1/messages.json
      echo
      echo "! Push notification: Build ready."
   )
fi

if [ "${_BRANCH}" = 'master' ] ; then
   (
      set +x
      curl -sS \
         -H "Authorization: token ${GITHUB_TOKEN}" \
         -X PATCH "https://api.github.com/repos/vszakats/harbour-core/git/refs/tags/v${HB_VF_DEF}" \
         -d "@${_ROOT}/git_tag_patch.json"
   )
fi

# https://www.virustotal.com/en/documentation/public-api/#scanning-files
if [ "$(wc -c < "${_PKGNAME}")" -lt 32000000 ]; then
   (
      set +x
      out="$(curl -sS \
         -X POST https://www.virustotal.com/vtapi/v2/file/scan \
         --form-string "apikey=${VIRUSTOTAL_APIKEY}" \
         --form "file=@${_PKGNAME}")"
      echo "${out}"
      echo "VirusTotal URL for '${_PKGNAME}':"
      echo "${out}" | grep -o 'https://[a-zA-Z0-9./]*'
   )
else
   echo "! File too large for VirusTotal Public API. Upload skipped."
fi
