#!/bin/sh
[ "$BASH" ] || exec bash `which $0` ${1+"$@"}
#
# $Id$
#
# This script simplifies cross-compiling Harbour for Windows-CE from Unix systems.
#
# Copyright 2007 by Przemyslaw Czerpak (druzus/at/priv.onet.pl)
#

UNAME=`uname`
UNAMEL=`echo "$UNAME"|tr A-Z a-z`
UNAMEU=`echo "$UNAME"|tr a-z A-Z`

export HB_ARCHITECTURE=wce
export HB_COMPILER=mingwarm

if [ "$OSTYPE" = "msdosdjgpp" ]; then
    HB_HOST_ARCH="dos"
    HB_HOST_COMP="djgpp"
else
    HB_HOST_ARCH="${UNAMEL}"
    HB_HOST_COMP="gcc"
    case "$HB_HOST_ARCH" in
        *windows*|*mingw32*|msys*)  HB_HOST_ARCH="win"; HB_HOST_COMP="mingw" ;;
        *dos)    HB_HOST_ARCH="dos" ;;
        *bsd)    HB_HOST_ARCH="bsd" ;;
    esac
fi

if [ "$HB_HOST_ARCH" != "win" ] && \
   [ "$HB_HOST_ARCH" != "wce" ]; then
    export CC_HB_USER_PRGFLAGS="-D__PLATFORM__WINDOWS -undef:__PLATFORM__UNIX -undef:__PLATFORM__$UNAMEU"
fi

[ -n "$HB_INSTALL_PREFIX" ] || \
export HB_INSTALL_PREFIX="/usr/local/arm-wince-mingwce-harbour"

# default mingwce instalation path
[ -z "$HB_CCPATH" ] && HB_CCPATH="/opt/mingw32ce/bin"

# mingwce executables prefix - this
if [ -z "$HB_CCPREFIX" ]; then
    if [ -x "${HB_CCPATH}/arm-wince-mingw32ce-gcc" ]; then
        HB_CCPREFIX="arm-wince-mingw32ce-"
    else
        if [ -x "${HB_CCPATH}/arm-mingw32ce-gcc" ]; then
            HB_CCPREFIX="arm-mingw32ce-"
        else
            echo "mingwce compiler executable not found. Ensure you have mingwce package installed in"
            echo "/opt/mingw32ce dir, or (alternatively) set environment variable HB_CCPATH to a mingwce"
            echo "installation directory"
            exit 1
        fi
    fi
fi

export HB_CCPATH="${HB_CCPATH}:"
export PATH="${HB_CCPATH}${PATH}"
export HB_CCPREFIX
export HB_TOOLS_PREF="hbce"
export HB_XBUILD="wce"
[ "${HB_HOST_BUILD}" = "all" ] || export HB_HOST_BUILD="lib"

case "$1" in
    tgz)
        ext=$1
        shift
        . `dirname $0`/make_${ext}.sh "$@"
        ;;
    *)
        if [ "$HB_HOST_ARCH" = "bsd" ] || [ "$HB_HOST_ARCH" = "hpux" ]
        then
           gmake $*
        else
           make $*
        fi
        ;;
esac
