#!/bin/sh
#
# $Id$
#

# ---------------------------------------------------------------
# Copyright 2007 Przemyslaw Czerpak (druzus/at/priv.onet.pl),
# See doc/license.txt for licensing terms.
# ---------------------------------------------------------------

if [ -z "$HB_ARCHITECTURE" ]; then
    if [ "$OSTYPE" = "msdosdjgpp" ]; then
        hb_arch="dos"
    else
        hb_arch=`uname -s | tr -d "[-]" | tr '[A-Z]' '[a-z]' 2>/dev/null`
        case "$hb_arch" in
            *windows*|*mingw32*|msys*)   hb_arch="win" ;;
            *cygwin*)                    hb_arch="cyg" ;;
            *os/2*)                      hb_arch="os2" ;;
            *dos)                        hb_arch="dos" ;;
            *bsd)                        hb_arch="bsd" ;;
        esac
    fi
    export HB_ARCHITECTURE="$hb_arch"
fi

if [ -z "$HB_CC_NAME" ]; then
    case "$HB_ARCHITECTURE" in
        win) HB_CC_NAME="mingw" ;;
        dos) HB_CC_NAME="djgpp" ;;
        *)   HB_CC_NAME="gcc" ;;
    esac
    export HB_CC_NAME
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

# default lib dir name
HB_LIBDIRNAME="lib"

HB_ARCH64=""
if [ "$HB_ARCHITECTURE" = "linux" ]
then
    HB_CPU=`uname -m`
    case "$HB_CPU" in
        *[_@]64)
           HB_ARCH64="yes"
           ;;
        *)
           ;;
    esac
fi

[ -z "$CC" ] && CC="gcc"
[ -z "$LD" ] && LD="gcc"

MAKE="make"
CRSLIB="ncurses"
OS_LIBS="-lm"
GT_LIST="TRM"

case "$HB_ARCHITECTURE" in
    win)  GT_LIST="WIN WVT GUI"
          OS_LIBS="-luser32 -lwinspool -lwsock32 -lgdi32"
          ;;
    cyg)  GT_LIST="${GT_LIST} WIN WVT GUI"
          OS_LIBS="-luser32 -lwinspool -lwsock32 -lgdi32"
          ;;
    dos)  GT_LIST="DOS"
          ;;
    os2)  GT_LIST="${GT_LIST} OS2"
          ;;
    linux) OS_LIBS="$OS_LIBS -ldl"
          [ -d "/usr/lib/lib64" ] && [ "${HB_ARCH64}" = yes ] && HB_LIBDIRNAME="lib64"
          ;;
    bsd)  MAKE="gmake"
          ;;
    sunos) MAKE="gmake"
          OS_LIBS="$OS_LIBS -lrt -lsocket -lnsl -lresolv"
          CRSLIB="curses"
          ;;
    hpux) MAKE="gmake"
          OS_LIBS="$OS_LIBS -lrt"
          ;;
esac

#GTSLN=""
#GTCRS=""
#GTXWC=""
for dir in /usr /usr/local /sw /opt/local
do
    if [ "$GTSLN" != yes ]; then
        if [ "$GTSLN" != no ]; then
            if [ -f $dir/include/slang.h ]; then
               [ $dir = /usr ] || HB_USER_CFLAGS="$HB_USER_CFLAGS -I$dir/include"
                GTSLN=yes
            elif [ -f $dir/include/slang/slang.h ]; then
                HB_USER_CFLAGS="$HB_USER_CFLAGS -I$dir/include/slang"
                GTSLN=yes
            fi
        fi
    fi
    if [ "$GTCRS" != yes ]; then
        if [ "$GTCRS" != no ]; then
            if [ -f ${dir}/include/curses.h ]; then
               [ $dir = /usr ] || HB_USER_CFLAGS="$HB_USER_CFLAGS -I$dir/include"
                GTCRS=yes
            elif [ -f ${dir}/include/${CRSLIB}/curses.h ]; then
                HB_USER_CFLAGS="$HB_USER_CFLAGS -I$dir/include/${CRSLIB}"
                GTCRS=yes
            fi
        fi
    fi
    if [ "$GTXWC" != yes ]; then
        if [ "$GTXWC" != no ]; then
            if [ -f ${dir}/include/X11/Xlib.h ] && \
               [ -f ${dir}/include/X11/Xcms.h ] && \
               [ -f ${dir}/include/X11/Xutil.h ] && \
               [ -f ${dir}/include/X11/keysym.h ]; then
               [ $dir = /usr ] || HB_USER_CFLAGS="$HB_USER_CFLAGS -I$dir/include"
                GTXWC=yes
            fi
        fi
    fi
done

[ "${HB_WITHOUT_GTSLN}" != "yes" ] || GTSLN=""
if [ "$HB_COMMERCE" = yes ]; then
    export HB_GPM_MOUSE=no
    GTSLN=""
fi

if [ "$GTCRS" = "yes" ]; then
    GT_LIST="$GT_LIST CRS"
    OS_LIBS="$OS_LIBS -l${CRSLIB}"
fi
if [ "$GTSLN" = "yes" ]; then
    GT_LIST="$GT_LIST SLN"
    OS_LIBS="$OS_LIBS -lslang"
fi
if [ "$GTXWC" = "yes" ]; then
    GT_LIST="$GT_LIST XWC"
    OS_LIBS="$OS_LIBS -lX11 -L/usr/X11R6/$HB_LIBDIRNAME"
fi

for n in $GT_LIST
do
    GT_TEMP="$GT_TEMP \$(GT${n}_LIB)"
    GT_OBJS="$GT_OBJS \$(GT${n}_LIB_OBJS)"
done
GT_LIST=$GT_TEMP

export HB_OS_LIBS="$HB_OS_LIBS $OS_LIBS"
export HB_GT_LIST="$HB_GT_LIST $GT_LIST"
export HB_GT_OBJS="$HB_GT_OBJS $GT_OBJS"
export CC LD

mkdir -p obj/$HB_CC_NAME/mt obj/$HB_CC_NAME/mt_dll obj/$HB_CC_NAME/dll \
         lib/$HB_CC_NAME bin/$HB_CC_NAME

# Convert common.mak (for BCC/VC) to common.cf (GCC)

# Revert Cygwin architecture to 'win'.
# After all it's under Windows OS.
if [ "$HB_ARCHITECTURE" = "cyg" ]
then
    export HB_ARCHITECTURE=win
fi

sed -e 's/;/ /g'             \
    -e 's!\\\\!/!g'          \
    -e 's!\\\(.\)!/\1!g'     \
    -e 's/^!if "\($([A-Za-z0-9_]*)\)" != "\(.*\)"/ifneq (\1,\2)/g'      \
    -e 's/^!if "\($([A-Za-z0-9_]*)\)" == "\(.*\)"/ifeq (\1,\2)/g'       \
    -e 's/^!ifdef /ifdef /g'                                            \
    -e 's/^!ifndef /ifndef /g'                                          \
    -e 's/^!else/else/g'                                                \
    -e 's/^!endif/endif/g'                                              \
    -e 's/^!include/include/g'                                          \
    common.mak > common.cf

$MAKE -r -f make_gcc.mak $HB_USER_MAKEFLAGS $*
rm -f common.cf
