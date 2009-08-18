#!/bin/sh
[ "$BASH" ] || exec bash `which $0` ${1+"$@"}
#
# $Id$
#
# This script simplifies cross-compiling Harbour for Windows from Unix systems.
#
# Copyright 2003-2005 by Phil Krylov <phil a t newstar.rinet.ru>
#

UNAME=`uname`
UNAMEL=`echo "$UNAME"|tr A-Z a-z`
UNAMEU=`echo "$UNAME"|tr a-z A-Z`

export HB_ARCHITECTURE=win
export HB_COMPILER=mingw

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

if [ "$HB_HOST_ARCH" != "win" ]; then
    export CC_HB_USER_PRGFLAGS="-D__PLATFORM__WINDOWS -undef:__PLATFORM__UNIX -undef:__PLATFORM__$UNAMEU"
fi

[ -n "$HB_INSTALL_PREFIX" ] || \
export HB_INSTALL_PREFIX="/usr/local/mingw32-harbour"

# try to detect MinGW cross-compiler location
# using some default platform settings
if [ -f /etc/debian_version ]; then
    MINGW_PREFIX=/usr
    [ -n "$TARGET" ] || TARGET=i586-mingw32msvc
    HB_CCPREFIX="$TARGET-"
elif [ -f /etc/gentoo-release ]; then
    if [ -x /opt/xmingw/bin/i386-mingw32msvc-gcc ]; then
        MINGW_PREFIX=/opt/xmingw
        [ -n "$TARGET" ] || TARGET=i386-mingw32msvc
    else
        MINGW_PREFIX=/usr
        [ -n "$TARGET" ] || TARGET=i686-mingw32
    fi
    HB_CCPREFIX="$TARGET-"
elif [ "$UNAME" = "FreeBSD" ]; then
    MINGW_PREFIX=/usr/local/mingw32
    [ -n "$TARGET" ] || TARGET="."
    HB_CCPREFIX=""
    UNAMEL=bsd
elif [ -x /usr/local/bin/i[3456]86-mingw*-gcc ]; then
    MINGW_PREFIX=/usr/local
    [ -n "$TARGET" ] || TARGET=`echo /usr/local/bin/i[3456]86-mingw*-gcc|sed -e '1 !d' -e 's/.*\(i[3456]86-mingw[^-]*\).*/\1/g'`
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
                [ -n "$TARGET" ] || TARGET=`echo "$MINGWGCC"|sed -e '1 !d' -e 's/.*\(i[3456]86-mingw[^-]*\).*/\1/g'`
                HB_CCPREFIX="$TARGET-"
            else
                MINGWGCC=`echo $d/i[3456]86-mingw*/bin/gcc`
                if [ -x $MINGWGCC ]; then
                   MINGW_PREFIX=$d
                   [ -n "$TARGET" ] || TARGET=`echo "$MINGWGCC"|sed -e '1 !d' -e 's!.*\(i[3456]86-mingw[^/]*\).*!\1!g'`
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

export HB_CCPATH="${MINGW_PREFIX}/bin:${MINGW_PREFIX}/${TARGET}/bin:"
export PATH="${HB_CCPATH}${PATH}"
export HB_CCPREFIX
export HB_TOOLS_PREF="hbw"
export HB_XBUILD="win"
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
