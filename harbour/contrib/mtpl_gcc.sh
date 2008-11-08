#!/bin/sh

#
# $Id$
#

# ---------------------------------------------------------------
# Partly based on harbour/make_gcc.sh and harbour/contrib/mtpl_*.bat
# by :
#   Copyright 2007 Przemyslaw Czerpak (druzus/at/priv.onet.pl),
#   Copyright 1999-2001 Viktor Szakats (viktor.szakats@syenar.hu)
# See doc/license.txt for licensing terms.
# ---------------------------------------------------------------

# ---------------------------------------------------------------
# This is a generic template file, if it doesn't fit your own needs
# please DON'T MODIFY IT.
#
# Instead, make a local copy and modify that one, or make a call to
# this batch file from your customized one. [vszakats]
#
# export any of the below settings to customize your build process:
#    export HB_MAKE_PROGRAM=
#    export MK_USR=
# ---------------------------------------------------------------

if [ -z "$HB_ARCHITECTURE" ]; then
   if [ "$OSTYPE" = "msdosdjgpp" ]; then
      hb_arch="dos"
   else
      hb_arch=`uname -s | tr -d "[-]" | tr '[A-Z]' '[a-z]' 2>/dev/null`
      case "$hb_arch" in
         *windows*|*mingw32*|msys*) hb_arch="w32" ;;
         *dos)                      hb_arch="dos" ;;
         *bsd)                      hb_arch="bsd" ;;
      esac
   fi
   export HB_ARCHITECTURE="$hb_arch" _HB_ARCH_SAVED=1
fi

#**************************************************************

export _HB_CC_NAME=${HB_CC_NAME}
export _HB_MAKE_PROGRAM=${HB_MAKE_PROGRAM}
export _HB_MAKEFILE=${HB_MAKEFILE}

if [ -z "$_HB_CC_NAME" ]; then
   case "$HB_ARCHITECTURE" in
      w32) _HB_CC_NAME="mingw" ;;
      dos) _HB_CC_NAME="djgpp" ;;
      *)   _HB_CC_NAME="gcc" ;;
   esac
   export _HB_CC_NAME
fi

if [ -z "${_HB_MAKE_PROGRAM}" ]; then export _HB_MAKE_PROGRAM=make; fi
if [ -z "${_HB_MAKEFILE}" ];     then export _HB_MAKEFILE=../mtpl_gcc.mak; fi

export HB_EXIT_LEVEL=

# ---------------------------------------------------------------

[ -z "$CC" ] && export CC="gcc"
[ -z "$LD" ] && export LD="gcc"

${_HB_MAKE_PROGRAM} ${MK_USR} -f ${_HB_MAKEFILE} $1 $2 $3 || export HB_EXIT_LEVEL=1

# ---------------------------------------------------------------

unset _HB_CC_NAME
unset _HB_MAKE_PROGRAM
unset _HB_MAKEFILE

if [ -n "$_HB_ARCH_SAVED" ]; then unset HB_ARCHITECTURE _HB_ARCH_SAVED; fi
