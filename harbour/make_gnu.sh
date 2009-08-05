#!/bin/sh
[ "$BASH" ] || exec bash `which $0` ${1+"$@"}
#
# $Id$
#

# ---------------------------------------------------------------
# Copyright 1999-2001 Viktor Szakats (harbour.01 syenar.hu)
# See COPYING for licensing terms.
#
# Harbour Project build script (for *nix systems)
#
# Please read INSTALL for further information.
# ---------------------------------------------------------------

# ---------------------------------------------------------------
# See GNU bash docs here:
#    http://www.gnu.org/software/bash/manual/bashref.html
# See POSIX shell docs here:
#    http://www.opengroup.org/onlinepubs/009695399/utilities/xcu_chap02.html
# ---------------------------------------------------------------

if [ -z "$HB_ARCHITECTURE" ]; then
    if [ "$OSTYPE" = "msdosdjgpp" ]; then
        hb_arch="dos"
    else
        hb_arch=`uname -s | tr -d "[-]" | tr '[A-Z]' '[a-z]' 2>/dev/null`
        case "$hb_arch" in
            *windows*|*mingw32*|msys*) hb_arch="win" ;;
            cygwin*)                   hb_arch="cyg" ;;
            *os/2*)                    hb_arch="os2" ;;
            *dos)                      hb_arch="dos" ;;
            *bsd)                      hb_arch="bsd" ;;
        esac
    fi
    export HB_ARCHITECTURE="$hb_arch"
fi

if [ -z "$HB_COMPILER" ]; then
    case "$HB_ARCHITECTURE" in
        win) HB_COMPILER="mingw" ;;
        cyg) HB_COMPILER="cygwin" ;;
        dos) HB_COMPILER="djgpp" ;;
        *)   HB_COMPILER="gcc" ;;
    esac
    export HB_COMPILER
fi

if [ "$HB_ARCHITECTURE" = "cyg" ]
then
    export HB_ARCHITECTURE=win
fi

if [ -z "$HB_GPM_MOUSE" ]; then
    if [ "$HB_ARCHITECTURE" = "linux" ] && \
       ( [ -f /usr/include/gpm.h ] || [ -f /usr/local/include/gpm.h ]); then
        HB_GPM_MOUSE=yes
    else
        HB_GPM_MOUSE=no
    fi
    export HB_GPM_MOUSE
fi

if [ "$HB_ARCHITECTURE" = "linux" ] && \
   [ "$HB_COMPILER" != "sunpro" ] && \
   [ "$HB_COMPILER" != "sunpro64" ]
then
    if [ "${HB_USER_CFLAGS}" = "${HB_USER_CFLAGS//-fPIC/}" ]
    then
        HB_CPU=`uname -m`
        case "$HB_CPU" in
            *[@_]64)
                export HB_USER_CFLAGS="$HB_USER_CFLAGS -fPIC"
                HB_ARCH64="yes"
                ;;
            *[ix]*86)
                ;;
            *[ix]*86[^0-9]*)
                ;;
            *)
                export HB_USER_CFLAGS="$HB_USER_CFLAGS -fPIC"
                ;;
        esac
    fi
fi

if [ "$HB_ARCHITECTURE" = "win" ] || \
   [ "$HB_ARCHITECTURE" = "wce" ] || \
   [ "$HB_ARCHITECTURE" = "dos" ] || \
   [ "$HB_ARCHITECTURE" = "os2" ]; then
    if [ -n "$HB_INSTALL_PREFIX" ]; then
        export HB_INSTALL_PREFIX="${HB_INSTALL_PREFIX//\\//}"
    fi
    if [ -n "$HB_BIN_INSTALL" ]; then
        export HB_BIN_INSTALL="${HB_BIN_INSTALL//\\//}"
    fi
    if [ -n "$HB_LIB_INSTALL" ]; then
        export HB_LIB_INSTALL="${HB_LIB_INSTALL//\\//}"
    fi
    if [ -n "$HB_INC_INSTALL" ]; then
        export HB_INC_INSTALL="${HB_INC_INSTALL//\\//}"
    fi
    if [ -n "$HB_DOC_INSTALL" ]; then
        export HB_DOC_INSTALL="${HB_DOC_INSTALL//\\//}"
    fi
fi

[ -z "$HB_INSTALL_PREFIX" ] && [ -n "$PREFIX" ] && export HB_INSTALL_PREFIX="$PREFIX"
[ -z "$HB_INSTALL_PREFIX" ] && export HB_INSTALL_PREFIX="/usr/local"

# Set to constant value to be consistent with the non-GNU make files.

case "$HB_INSTALL_PREFIX" in
    /usr|/usr/local|/opt)
        hb_instsubdir="/harbour"
        ;;
    *)
        hb_instsubdir=""
        ;;
esac

if [ -z "$HB_BIN_INSTALL" ]; then export HB_BIN_INSTALL="$HB_INSTALL_PREFIX/bin"; fi
if [ -z "$HB_LIB_INSTALL" ]; then export HB_LIB_INSTALL="$HB_INSTALL_PREFIX/lib$hb_instsubdir"; fi
if [ -z "$HB_INC_INSTALL" ]; then export HB_INC_INSTALL="$HB_INSTALL_PREFIX/include$hb_instsubdir"; fi

if [ "$HB_ARCHITECTURE" = "win" ] || \
   [ "$HB_ARCHITECTURE" = "wce" ] || \
   [ "$HB_ARCHITECTURE" = "dos" ] || \
   [ "$HB_ARCHITECTURE" = "os2" ]; then
    if [ -z "$HB_DOC_INSTALL" ]; then export HB_DOC_INSTALL="$HB_INSTALL_PREFIX/doc"; fi
fi

if [ -z "$HB_ARCHITECTURE" ]; then
    echo "Error: HB_ARCHITECTURE is not set."
fi
if [ -z "$HB_COMPILER" ]; then
    echo "Error: HB_COMPILER is not set."
fi

if [ -z "$HB_ARCHITECTURE" ] || [ -z "$HB_COMPILER" ]; then

    echo
    echo "Usage: make_gnu.sh [command]"
    echo
    echo "The following commands are supported:"
    echo "  - all (default)"
    echo "  - clean"
    echo "  - install"
    echo
    echo "Please read INSTALL for HOWTOs and description"
    echo "of available options."
    exit

else

    export HARBOUR=
    export CLIPPER=

    # ---------------------------------------------------------------
    # Start the GNU make system

    case "$HB_ARCHITECTURE" in
        bsd|hpux|sunos)
            gmake $HB_USER_MAKEFLAGS $*
            ;;
        *)
            make $HB_USER_MAKEFLAGS $*
            ;;
    esac

    if [ "$*" = "clean" ]; then
       find . -type d -name "$HB_ARCHITECTURE" | xargs rmdir 2> /dev/null
    fi
fi
