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
UNAMEU=`echo "$UNAME"|tr a-z A-Z`

export HB_ARCHITECTURE=win
export HB_COMPILER=mingw

if [ "$OSTYPE" = "msdosdjgpp" ]; then
    HB_HOST_ARCH="dos"
    HB_HOST_CC="djgpp"
else
    HB_HOST_ARCH="${UNAMEL}"
    HB_HOST_CC="gcc"
    case "$HB_HOST_ARCH" in
        *windows*|*mingw32*|msys*)  HB_HOST_ARCH="win"; HB_HOST_CC="mingw" ;;
        *dos)    HB_HOST_ARCH="dos" ;;
        *bsd)    HB_HOST_ARCH="bsd" ;;
    esac
fi

CC_HB_USER_PRGFLAGS=""
if [ "$HB_HOST_ARCH" != "win" ]; then
    CC_HB_USER_PRGFLAGS="-D__PLATFORM__WINDOWS -undef:__PLATFORM__UNIX -undef:__PLATFORM__$UNAMEU"
fi

[ -z "$HB_INSTALL_PREFIX" ] && \
export HB_INSTALL_PREFIX="/usr/local/mingw32-harbour"
export CC_HB_USER_CFLAGS=""
export HB_USER_CFLAGS="$CC_HB_USER_CFLAGS $HB_USER_CFLAGS"
export CC_HB_USER_PRGFLAGS
export HB_USER_PRGFLAGS="$CC_HB_USER_PRGFLAGS $HB_USER_PRGFLAGS"

# try to detect MinGW cross-compiler location
# using some default platform settings
if [ -f /etc/debian_version ]; then
    MINGW_PREFIX=/usr
    TARGET=i586-mingw32msvc
    HB_CCPREFIX="$TARGET-"
elif [ -f /etc/gentoo-release ]; then
    if [ -x /opt/xmingw/bin/i386-mingw32-gcc ]; then
        MINGW_PREFIX=/opt/xmingw
        TARGET=i386-mingw32msvc
    else
        MINGW_PREFIX=/usr
        TARGET=i686-mingw32
    fi
    HB_CCPREFIX="$TARGET-"
elif [ "$UNAME" = "FreeBSD" ]; then
    MINGW_PREFIX=/usr/local/mingw32
    TARGET="."
    HB_CCPREFIX=""
    UNAMEL=bsd
elif [ -x /usr/local/bin/i[3456]86-mingw*-gcc ]; then
    MINGW_PREFIX=/usr/local
    TARGET=`echo /usr/local/bin/i[3456]86-mingw*-gcc|sed -e '1 !d' -e 's/.*\(i[3456]86-mingw[^-]*\).*/\1/g'`
    HB_CCPREFIX="$TARGET-"
fi

if [ -z "${MINGW_PREFIX}" ] || \
   ( [ ! -x ${MINGW_PREFIX}/bin/${HB_CCPREFIX}gcc ] && \
     [ ! -x ${MINGW_PREFIX}/${TARGET}/bin/${HB_CCPREFIX}gcc ] ); then
    # MinGW cross-compiler not found in default location
    # scan some usually used locations and names
    for d in /usr /usr/local /usr/local/mingw32 /opt/xmingw; do
        if [ -z "${MINGW_PREFIX}" ] && [ -d $d/bin ]; then
            MINGWGCC=`echo $d/bin/i[3456]86-mingw*-gcc`
            if [ -x $MINGWGCC ]; then
                MINGW_PREFIX=$d
                TARGET=`echo "$MINGWGCC"|sed -e '1 !d' -e 's/.*\(i[3456]86-mingw[^-]*\).*/\1/g'`
                HB_CCPREFIX="$TARGET-"
            else
                MINGWGCC=`echo $d/i[3456]86-mingw*/bin/gcc`
                if [ -x $MINGWGCC ]; then
                   MINGW_PREFIX=$d
                   TARGET=`echo "$MINGWGCC"|sed -e '1 !d' -e 's!.*\(i[3456]86-mingw[^/]*\).*!\1!g'`
                   HB_CCPREFIX=""
                fi
            fi
        fi
    done
fi

if [ ! -x ${MINGW_PREFIX}/bin/${HB_CCPREFIX}gcc ] && \
   [ ! -x ${MINGW_PREFIX}/${TARGET}/bin/${HB_CCPREFIX}gcc ]; then
    echo "Can't determine the location for the MinGW32 cross-compiler."
    echo "Please install it or add your platform to the $0 script."
    exit 1
fi

HB_CCPATH="$MINGW_PREFIX/bin:$MINGW_PREFIX/$TARGET/bin:"
PATH="$HB_CCPATH$PATH"

export PATH HB_CCPATH HB_CCPREFIX

export HB_TOOLS_PREF="hbw"
export HB_XBUILD="win"
[ "${HB_HOST_BUILD}" = "all" ] || export HB_HOST_BUILD="lib"

export HB_BIN_COMPILE="/tmp/hb-xmingw-$$"
rm -fR "${HB_BIN_COMPILE}"
trap cleanup EXIT >/dev/null 2>&1
mkdir ${HB_BIN_COMPILE}

DIR=`cd $(dirname $0);pwd`
if [ -z "${HB_COMP_PATH}" ]; then
    if which harbour > /dev/null 2>&1; then
        HB_COMP_PATH=`which harbour 2> /dev/null`
    else
        HB_COMP_PATH="$DIR/source/main/$HB_HOST_ARCH/$HB_HOST_CC/harbour"
    fi
fi

if [ -x "${HB_COMP_PATH}" ]; then
    ln -s "${HB_COMP_PATH}" ${HB_BIN_COMPILE}/harbour.exe
else
    echo "You must have a working 'harbour' executable for your platform on your PATH."
    exit 1
fi

if [ -z "${HB_PPGEN_PATH}" ]; then
    if which hbpp &> /dev/null; then
        HB_PPGEN_PATH=`which hbpp 2> /dev/null`
    elif [ -x "${DIR}/source/pp/${HB_HOST_ARCH}/${HB_HOST_CC}/hbpp" ]; then
        HB_PPGEN_PATH="${DIR}/source/pp/${HB_HOST_ARCH}/${HB_HOST_CC}/hbpp"
    else
        DIR=`dirname ${HB_COMP_PATH}`
        if [ -x "${DIR}/hbpp" ]; then
            HB_PPGEN_PATH="${DIR}/hbpp"
        else
            HB_PPGEN_PATH="$DIR/source/pp/$HB_HOST_ARCH/$HB_HOST_CC/hbpp"
        fi
    fi
fi
if [ -d "${HB_PPGEN_PATH}" ]; then
    if [ -x "${HB_PPGEN_PATH}/hbpp" ]; then
        HB_PPGEN_PATH="${HB_PPGEN_PATH}/hbpp"
    fi
fi
if [ -x "${HB_PPGEN_PATH}" ] && [ -f "${HB_PPGEN_PATH}" ]; then
    ln -s ${HB_PPGEN_PATH} ${HB_BIN_COMPILE}/hbpp.exe
    HB_PPGEN_PATH="${HB_BIN_COMPILE}"
else
    echo "You must have a working 'hbpp' executable for your platform on your PATH."
    exit 1
fi
export HB_PPGEN_PATH

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
