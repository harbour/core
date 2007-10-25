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

UNAME=`uname`

export HB_ARCHITECTURE=w32
export HB_COMPILER=cemgw

[ -z "$HB_INSTALL_PREFIX" ] && \
export HB_INSTALL_PREFIX="/usr/local/arm-wince-cemgw-harbour"
export CC_C_USR="-DHOST_OS_UNIX_COMPATIBLE"
export C_USR="$CC_C_USR $C_USR"
export CC_PRG_USR="-D__PLATFORM__Windows -D__PLATFORM__WINCE -undef:__PLATFORM__UNIX -undef:__PLATFORM__$UNAME"
export PRG_USR="$CC_PRG_USR $PRG_USR"

export CCPATH="/opt/mingw32ce/bin:"
export CCPREFIX="arm-wince-mingw32ce-"
export PATH="$CCPATH$PATH"

export HB_BIN_COMPILE="/tmp/hb-${CCPREFIX}-$$"
rm -fR "${HB_BIN_COMPILE}"
trap cleanup EXIT &>/dev/null
mkdir ${HB_BIN_COMPILE}

if which harbour &> /dev/null; then
    ln -s `which harbour` ${HB_BIN_COMPILE}/harbour.exe
else
    echo "You must have a working Harbour executable for your platform on your PATH."
    exit 1
fi

(cd `dirname $0`
ln -s `pwd`/source/pp/`echo "$UNAME"|tr A-Z a-z`/gcc/ppgen \
      ${HB_BIN_COMPILE}/ppgen.exe)
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
