#!/bin/sh
[ "$BASH" ] || exec bash `which $0` ${1+"$@"}
#
# $Id$
#

# ---------------------------------------------------------------
# Template to initialize the environment before starting
# the GNU make system for Harbour
#
# For further information about the GNU make system please
# check doc/gmake.txt
#
# Copyright 1999-2001 Viktor Szakats (viktor.szakats@syenar.hu)
# See doc/license.txt for licensing terms.
# ---------------------------------------------------------------

if [ -z "$HB_ARCHITECTURE" ]; then
    if [ "$OSTYPE" = "msdosdjgpp" ]; then
        hb_arch="dos"
    else
        hb_arch=`uname -s | tr -d "[-]" | tr '[A-Z]' '[a-z]' 2>/dev/null`
        case "$hb_arch" in
            *windows*|*mingw32*)    hb_arch="w32" ;;
            *dos)   hb_arch="dos" ;;
            *bsd)   hb_arch="bsd" ;;
        esac
    fi
    export HB_ARCHITECTURE="$hb_arch"
fi

if [ -z "$HB_COMPILER" ]; then
    case "$HB_ARCHITECTURE" in
        w32) HB_COMPILER="mingw32" ;;
        dos) HB_COMPILER="djgpp" ;;
        *)   HB_COMPILER="gcc" ;;
    esac
    export HB_COMPILER
fi

if [ -z "$HB_GT_LIB" ]; then
    case "$HB_ARCHITECTURE" in
        w32) HB_GT_LIB="gtwin" ;;
        dos) HB_GT_LIB="gtdos" ;;
        os2) HB_GT_LIB="gtos2" ;;
        *)   HB_GT_LIB="gttrm" ;;
    esac
    export HB_GT_LIB
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

if [ -z "${HB_WITHOUT_GTSLN}" ]; then
    HB_WITHOUT_GTSLN=yes
    case "$HB_ARCHITECTURE" in
        linux|bsd|darwin|hpux|sunos)
            for dir in /usr /usr/local /sw /opt/local
            do
                if [ -f ${dir}/include/slang.h ] || \
                   [ -f ${dir}/include/slang/slang.h ]; then
                    HB_WITHOUT_GTSLN=no
                fi
            done
            ;;
    esac
    export HB_WITHOUT_GTSLN
fi

if [ -z "$HB_COMMERCE" ]; then export HB_COMMERCE=no; fi

if [ "$HB_COMMERCE" = yes ]
then
   export HB_GPM_MOUSE=no
   export HB_WITHOUT_GTSLN=yes
fi

# export PRG_USR=
# export C_USR=
# export L_USR=

if [ "$HB_ARCHITECTURE" = "linux" ]
then
    if [ "${C_USR}" == "${C_USR//-fPIC/}" ]
    then
        HB_CPU=`uname -m`
        case "$HB_CPU" in
            *[@_]64)
                export C_USR="$C_USR -fPIC"
                HB_ARCH64="yes"
                ;;
            *)
                ;;
        esac
    fi
fi

[ -z "$HB_INSTALL_PREFIX" ] && [ -n "$PREFIX" ] && export HB_INSTALL_PREFIX="$PREFIX"
[ -z "$HB_INSTALL_PREFIX" ] && export HB_INSTALL_PREFIX=/usr/local

# Set to constant value to be consistent with the non-GNU make files.

case "$HB_INSTALL_PREFIX" in
    /usr|/usr/local|/opt)
        hb_instsubdir="/harbour"
        ;;
    *)
        hb_instsubdir=""
        ;;
esac

if [ -z "$HB_BIN_INSTALL" ]; then export HB_BIN_INSTALL=$HB_INSTALL_PREFIX/bin; fi
if [ -z "$HB_LIB_INSTALL" ]; then export HB_LIB_INSTALL=$HB_INSTALL_PREFIX/lib$hb_instsubdir; fi
if [ -z "$HB_INC_INSTALL" ]; then export HB_INC_INSTALL=$HB_INSTALL_PREFIX/include$hb_instsubdir; fi


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
   echo "Notes:"
   echo
   echo "  - HB_ARCHITECTURE and HB_COMPILER envvars must be set."
   echo "    The following values are currently supported:"
   echo
   echo "    HB_ARCHITECTURE:"
   echo "      - dos    (HB_GT_LIB=gtdos by default)"
   echo "      - w32    (HB_GT_LIB=gtw32 by default)"
   echo "      - os2    (HB_GT_LIB=gtos2 by default)"
   echo "      - linux  (HB_GT_LIB=gttrm by default)"
   echo "      - bsd    (HB_GT_LIB=gttrm by default)"
   echo "      - darwin (HB_GT_LIB=gttrm by default)"
   echo "      - sunos  (HB_GT_LIB=gttrm by default)"
   echo "      - hpux   (HB_GT_LIB=gttrm by default)"
   echo
   read
   echo "    HB_COMPILER:"
   echo "      - When HB_ARCHITECTURE=dos"
   echo "        - bcc16   (Borland C++ 3.x, 4.x, 5.0x, DOS 16-bit)"
   echo "        - djgpp   (Delorie GNU C, DOS 32-bit)"
   echo "        - rsx32   (EMX/RSXNT/DOS GNU C, DOS 32-bit)"
   echo "        - watcom  (Watcom C++ 9.x, 10.x, 11.x, DOS 32-bit)"
   echo "      - When HB_ARCHITECTURE=w32"
   echo "        - bcc32   (Borland C++ 4.x, 5.x, Windows 32-bit)"
   echo "        - gcc     (Cygnus/Cygwin GNU C, Windows 32-bit)"
   echo "        - mingw32 (MinGW32 GNU C, Windows 32-bit)"
   echo "        - rsxnt   (EMX/RSXNT/Win32 GNU C, Windows 32-bit)"
   echo "        - icc     (IBM Visual Age C++, Windows 32-bit)"
   echo "        - msvc    (Microsoft Visual C++, Windows 32-bit)"
   echo "      - When HB_ARCHITECTURE=linux"
   echo "        - gcc     (GNU C, 32-bit)"
   echo "      - When HB_ARCHITECTURE=os2"
   echo "        - gcc     (EMX GNU C, OS/2 32-bit)"
   echo "        - icc     (IBM Visual Age C++ 3.0, OS/2 32-bit)"
   echo
   read
   echo "    HB_GT_LIB:"
   echo "      - gtstd (TTY streaming)      (for all architectures)"
   echo "      - gtcgi (Standard streaming) (for all architectures)"
   echo "      - gtpca (PC ANSI console)    (for all architectures)"
   echo "      - gtdos (DOS console)        (for dos architecture)"
   echo "      - gtwin (Win32 console)      (for w32 architecture)"
   echo "      - gtwvt (Win32 win console)  (for w32 architecture)"
   echo "      - gtos2 (OS/2 console)       (for os2 architecture)"
   echo "      - gtcrs (Curses console)     (for *nixes, w32 architectures)"
   echo "      - gtsln (Slang console)      (for *nixes, w32 architectures)"
   echo "      - gttrm (Terminal console)   (for *nixes architecture)"
   echo "      - gtxwc (XWindow console)    (for *nixes architecture)"
   echo "      - gtalleg (Allegro console)  (for all architectures)"
   echo
   echo "  - Use these optional envvars to configure the make process"
   echo "    when using the 'all' target:"
   echo
   echo "    PRG_USR - Extra Harbour compiler options"
   echo "    C_USR   - Extra C compiler options"
   echo "    L_USR   - Extra linker options"
   exit

else

   # ---------------------------------------------------------------
   # Start the GNU make system

   if [ "$HB_ARCHITECTURE" = "bsd" ] || [ "$HB_ARCHITECTURE" = "hpux" ]
   then
      gmake $*
   else
      make $*
   fi

   if [ "$*" = "clean" ]; then
      find . -type d -name "$HB_ARCHITECTURE" | xargs rmdir 2> /dev/null
   fi
fi
