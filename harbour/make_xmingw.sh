#!/bin/sh
[ "$BASH" ] || exec bash `which $0` ${1+"$@"}
#
# $Id$
#
# This script simplifies cross-compiling Harbour for Windows from Unix systems.
#
# Copyright 2003-2005 by Phil Krylov <phil a t newstar.rinet.ru>
#

cleanup()
{
    rm -fR "${HB_BIN_COMPILE}"
}

UNAME=`uname`
UNAMEL=`echo "$UNAME"|tr A-Z a-z`

export HB_ARCHITECTURE=w32
export HB_COMPILER=mingw32

[ -z "$HB_INSTALL_PREFIX" ] && export HB_INSTALL_PREFIX=/usr/local/mingw32-harbour
export CC_C_USR=""
export C_USR="$CC_C_USR $C_USR"
export CC_PRG_USR="-D__PLATFORM__Windows -undef:__PLATFORM__UNIX -undef:__PLATFORM__$UNAME"
export PRG_USR="$CC_PRG_USR $PRG_USR"

if [ -f /etc/debian-version ]; then
    MINGW_PREFIX=/usr
    TARGET=i586-mingw32msvc
    CCPREFIX="$TARGET-"
elif [ -f /etc/gentoo-release ]; then
    MINGW_PREFIX=/opt/xmingw
    TARGET=i386-mingw32msvc
    CCPREFIX="$TARGET-"
elif [ "$UNAME" = "FreeBSD" ]; then
    MINGW_PREFIX=/usr/local/mingw32
    TARGET="."
    CCPREFIX=""
elif find /usr/local/bin -name "i[3456]86-mingw*-gcc" -maxdepth 1 &>/dev/null; then
    MINGW_PREFIX=/usr/local
    TARGET=`find /usr/local/bin -name "i[3456]86-mingw*-gcc" -maxdepth 1|sed -e '1 !d' -e 's/.*\(i[3456]86-mingw[^-]*\).*/\1/g'`
    CCPREFIX="$TARGET-"
else
    echo "Can't determine the location for the MinGW32 cross-compiler."
    echo "Please install it or add your platform to the $0 script."
    exit 1
fi
CCPATH="$MINGW_PREFIX/bin:$MINGW_PREFIX/$TARGET/bin:"
PATH="$CCPATH$PATH"

export PATH CCPATH CCPREFIX

export HB_TOOLS_PREF="hbw"
export HB_XBUILD="w32"
export HB_HOST_BUILD="lib"

export HB_BIN_COMPILE=/tmp/hb-xmingw-$$
rm -fR "${HB_BIN_COMPILE}"
trap cleanup EXIT &>/dev/null
mkdir ${HB_BIN_COMPILE}

DIR=`cd $(dirname $0);pwd`
if which harbour &> /dev/null; then
    HB_COMP_PATH=`which harbour 2> /dev/null`
else
    HB_COMP_PATH="$DIR/source/main/$UNAMEL/gcc/harbour"
fi

if [ -x "${HB_COMP_PATH}" ]; then
    ln -s "${HB_COMP_PATH}" ${HB_BIN_COMPILE}/harbour.exe
else
    echo "You must have a working Harbour executable for your platform on your PATH."
    exit 1
fi

ln -s "$DIR/source/pp/$UNAMEL/gcc/hbppgen" ${HB_BIN_COMPILE}/hbppgen.exe
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
