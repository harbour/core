#!/usr/bin/env bash

# ---------------------------------------------------------------
# Copyright 2016 Viktor Szakats (vszakats.net/harbour)
# See LICENSE.txt for licensing terms.
# ---------------------------------------------------------------

# Extract dependency versions and their hashes from `harbour-deps` online build log

f="$(curl -fsS 'https://ci.appveyor.com/api/projects/vsz/harbour-deps/branch/master')"

if [[ "${f}" =~ \"jobId\":\"([a-z0-9]+)\" ]] ; then

   f="$(curl -fsS "https://ci.appveyor.com/api/buildjobs/${BASH_REMATCH[1]}/log" | grep 'SHA256(')"

   out=

   for name in \
      'nghttp2' \
      'openssl' \
      'libssh2' \
      'curl' \
   ; do
      nameu="$(echo "${name}" | tr '[:lower:]' '[:upper:]' 2> /dev/null)"
      for plat in '32' '64' ; do
         if [[ "${f}" =~ ${name}-([0-9a-zA-Z.\-]+)-win${plat}-mingw.7z\)=\ ([0-9a-z]{64}) ]] ; then
            if [ "${plat}" = '32' ] ; then
               out="${out}export ${nameu}_VER='${BASH_REMATCH[1]}'"$'\n'
            fi
            out="${out}export ${nameu}_HASH_${plat}='${BASH_REMATCH[2]}'"$'\n'
         fi
      done
   done

   if [ -n "${out}" ] ; then
      # remove ending EOL
      # shellcheck disable=SC2116
      out="$(echo "${out}")"
      echo "${out}"
      awk -v "NEW=#hashbegin\n${out}\n#hashend" \
         'BEGIN{n=0} /#hashbegin/ {n=1} {if (n==0) {print $0}} /#hashend/ {print NEW; n=0}' \
         < mpkg_win_ci.sh > _tmp && cp _tmp mpkg_win_ci.sh
      rm _tmp
   else
      echo 'Error: Hashes not found. Something went wrong.'
   fi
fi
