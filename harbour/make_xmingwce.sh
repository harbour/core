#!/bin/sh
[ "$BASH" ] || exec bash `which $0` ${1+"$@"}
#
# $Id$
#
# This script simplifies cross-compiling Harbour for Windows-CE from Unix systems.
#
# Copyright 2007 by Przemyslaw Czerpak (druzus/at/priv.onet.pl)
#

cleanup()
{
    rm -fR "${HB_BIN_COMPILE}"
}

export HB_ARCHITECTURE=w32
export HB_COMPILER=cemgw

UNAME=`uname -s | tr -d "[-]" 2>/dev/null`

if [ "$OSTYPE" = "msdosdjgpp" ]; then
    HB_HOST_ARCH="dos"
    HB_HOST_CC="djgpp"
else
    HB_HOST_ARCH=`echo "$UNAME"|tr '[A-Z]' '[a-z]'`
    HB_HOST_CC="gcc"
    case "$HB_HOST_ARCH" in
        *windows*|*mingw32*|msys*)  HB_HOST_ARCH="w32"; HB_HOST_CC="mingw32" ;;
        *dos)    HB_HOST_ARCH="dos" ;;
        *bsd)    HB_HOST_ARCH="bsd" ;;
        *darwin) HB_HOST_ARCH="darwin" ;;
    esac
fi

CC_PRG_USR="-D__PLATFORM__WINCE"
if [ "$HB_HOST_ARCH" != "w32" ]; then
    CC_PRG_USR="$CC_PRG_USR -D__PLATFORM__Windows -undef:__PLATFORM__UNIX -undef:__PLATFORM__$UNAME"
fi

[ -z "$HB_INSTALL_PREFIX" ] && \
export HB_INSTALL_PREFIX="/usr/local/arm-wince-cemgw-harbour"
export CC_C_USR=""
export C_USR="$CC_C_USR $C_USR"
export CC_PRG_USR
export PRG_USR="$CC_PRG_USR $PRG_USR"

export CCPATH="/opt/mingw32ce/bin:"
export CCPREFIX="arm-wince-mingw32ce-"
export PATH="$CCPATH$PATH"

export HB_TOOLS_PREF="hbce"
export HB_XBUILD="wce"
export HB_HOST_BUILD="lib"
export HB_GT_LIB="gtwvt"

export HB_BIN_COMPILE="/tmp/hb-${CCPREFIX}-$$"
rm -fR "${HB_BIN_COMPILE}"
trap cleanup EXIT &>/dev/null
mkdir ${HB_BIN_COMPILE}

DIR=`cd $(dirname $0);pwd`
if which harbour &> /dev/null; then
    HB_COMP_PATH=`which harbour 2> /dev/null`
else
    HB_COMP_PATH="$DIR/source/main/$HB_HOST_ARCH/$HB_HOST_CC/harbour"
fi
if [ -x "${HB_COMP_PATH}" ]; then
    ln -s "${HB_COMP_PATH}" ${HB_BIN_COMPILE}/harbour.exe
else
    echo "You must have a working Harbour executable for your platform on your PATH."
    exit 1
fi

ln -s "$DIR/source/pp/$HB_HOST_ARCH/$HB_HOST_CC/hbpp" ${HB_BIN_COMPILE}/hbpp.exe
export HB_PPGEN_PATH=${HB_BIN_COMPILE}

case "$1" in
    tgz|gnu)
        ext=$1
        shift
        . `dirname $0`/make_${ext}.sh "$@"
        ;;
    *)
        . `dirname $0`/make_gnu.sh "$@"
        ;;
esac

stat="$?"
cleanup
exit "${stat}"
