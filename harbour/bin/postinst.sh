#!/bin/sh
#
# $Id$
#

# ---------------------------------------------------------------
# Copyright 2003 Przemyslaw Czerpak <druzus@priv.onet.pl>
# simple script run after Harbour make install to finish install
# process
#
# See COPYING for licensing terms.
# ---------------------------------------------------------------

if [ -z "${__running_in_posix_sh__}" ]
then
   __running_in_posix_sh__=1
   export __running_in_posix_sh__

   [ -x /usr/bin/posix/sh ] && \
      exec /usr/bin/posix/sh "$0" ${1+"$@"}
   [ -x /usr/xpg4/bin/sh ] && \
      exec /usr/xpg4/bin/sh "$0" ${1+"$@"}

   exec /bin/sh "$0" ${1+"$@"}
fi

unset __running_in_posix_sh__

__builtin_which()
{
   what="$1"
   ret=1
   oIFS="$IFS"
   IFS=:
   for pathcomp in $PATH
   do
      if [ -x "${pathcomp}"/"${what}" ]
      then
         ret=0
         echo "${pathcomp}"/"${what}"
         break
      fi
   done
   IFS="$oIFS"
   return $ret
}

if [ -z "$HB_PLATFORM" ] || [ -z "$HB_COMPILER" ] || \
   [ -z "$HB_BIN_INSTALL" ] || \
   [ -z "$HB_INC_INSTALL" ] || \
   [ -z "$HB_LIB_INSTALL" ]
then
   echo "The following envvars must be set:"
   echo "   HB_PLATFORM"
   echo "   HB_COMPILER"
   echo "   HB_BIN_INSTALL"
   echo "   HB_INC_INSTALL"
   echo "   HB_LIB_INSTALL"
   exit 1
fi

hb_root=`dirname "$0"`
if [ "${hb_root}" = "." ]
then
   hb_root=".."
else
   hb_root=`dirname "${hb_root}"`
fi
if [ ! -f ${hb_root}/bin/hb-func.sh ] && [ -f ./bin/hb-func.sh ]
then
   hb_root="."
fi

. ${hb_root}/bin/hb-func.sh

__install="` __builtin_which install `"

# chmod 644 ${HB_INC_INSTALL}/*

if [ "$HB_COMPILER" = "gcc" ] || \
   [ "$HB_COMPILER" = "mingw" ] || \
   [ "$HB_COMPILER" = "mingw64" ] || \
   [ "$HB_COMPILER" = "mingwarm" ] || \
   [ "$HB_COMPILER" = "cygwin" ] || \
   [ "$HB_COMPILER" = "djgpp" ] || \
   [ "$HB_COMPILER" = "icc" ] || \
   [ "$HB_COMPILER" = "sunpro" ] || \
   [ "$HB_COMPILER" = "open64" ] || \
   [ "$HB_COMPILER" = "clang" ]
then
   if [ -n "${HB_TOOLS_PREF}" ]; then
      hb_mkdyn="${HB_BIN_INSTALL}/${HB_TOOLS_PREF}-mkdyn"
      rm -f "${hb_mkdyn}"
      sed -e "s!^# HB_PLATFORM=\"\"\$!HB_PLATFORM=\"${HB_PLATFORM}\"!g" \
          -e "s!^# HB_CCPREFIX=\"\"\$![ -n \"\${HB_CCPREFIX}\" ] || HB_CCPREFIX=\"${HB_CCPREFIX}\"!g" \
          -e "s!^# HB_CCPATH=\"\"\$![ -n \"\${HB_CCPATH}\" ] || HB_CCPATH=\"${HB_CCPATH}\"!g" \
          "${hb_root}/bin/hb-mkdyn.sh" > "${hb_mkdyn}" && \
      chmod 755 "${hb_mkdyn}"
   elif [ "$HB_COMPILER" = "icc" ]; then
      hb_mkdyn="${HB_BIN_INSTALL}/hb-mkdyn"
      rm -f "${hb_mkdyn}"
      sed -e "s/gcc/icc/g" "${hb_root}/bin/hb-mkdyn.sh" > "${hb_mkdyn}" && \
      chmod 755 "${hb_mkdyn}"
   elif [ "$HB_COMPILER" = "sunpro" ]; then
      hb_mkdyn="${HB_BIN_INSTALL}/hb-mkdyn"
      rm -f "${hb_mkdyn}"
      lnopt=""
      [ "$HB_BUILD_OPTIM" = "no" ] || lnopt="-fast -xnolibmopt $lnopt"
      sed -e "s/gcc -shared/suncc -G ${lnopt} ${HB_ISAOPT}/g" \
          "${hb_root}/bin/hb-mkdyn.sh" > "${hb_mkdyn}" && \
      chmod 755 "${hb_mkdyn}"
   elif [ "$HB_COMPILER" = "open64" ]; then
      hb_mkdyn="${HB_BIN_INSTALL}/hb-mkdyn"
      rm -f "${hb_mkdyn}"
      sed -e "s/gcc/opencc/g" "${hb_root}/bin/hb-mkdyn.sh" > "${hb_mkdyn}" && \
      chmod 755 "${hb_mkdyn}"
   elif [ "${HB_PLATFORM}" = "sunos" ] || \
        [ "${HB_PLATFORM}" = "hpux" ] || \
        [ -z "${__install}" ]; then
      hb_mkdyn="${HB_BIN_INSTALL}/hb-mkdyn"
      rm -f "${hb_mkdyn}"
      cp "${hb_root}/bin/hb-mkdyn.sh" "${hb_mkdyn}" && \
      chmod 755 "${hb_mkdyn}"
   elif [ "${HB_PLATFORM}" != "dos" ]; then
      hb_mkdyn="${HB_BIN_INSTALL}/hb-mkdyn"
      # Without -c some OSes _move_ the file instead of copying it!
      ${__install} -c -m 755 "${hb_root}/bin/hb-mkdyn.sh" "${hb_mkdyn}"
   fi
fi
