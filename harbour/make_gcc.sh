#!/bin/sh
#
# $Id$
#

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

if [ -z "$HB_CC_NAME" ]; then
    case "$HB_ARCHITECTURE" in
        w32) HB_CC_NAME="mingw" ;;
        dos) HB_CC_NAME="djgpp" ;;
        *)   HB_CC_NAME="gcc" ;;
    esac
    export HB_CC_NAME
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

# default lib dir name
HB_LIBDIRNAME="lib"

HB_ARCH64=""
if [ "$HB_ARCHITECTURE" = "linux" ]
then
    HB_CPU=`uname -m`
    case "$HB_CPU" in
        *[_@]64)
            export C_USR="$C_USR -fPIC"
            HB_ARCH64="yes"
            ;;
        *)
            ;;
    esac
fi

[ -z "$CC" ] && CC="gcc"
[ -z "$LD" ] && LD="gcc"

MAKE="make"
EXEEXT=""
CRSLIB="ncurses"
OS_LIBS="-lm"
GT_LIST="TRM"

case "$HB_ARCHITECTURE" in
    w32)    HB_OS="WIN_32"
            GT_LIST="WIN WVT GUI"
            OS_LIBS="-luser32 -lwinspool -lwsock32"
            EXEEXT=".exe"
            ;;
    dos)    HB_OS="DOS"
            GT_LIST="DOS"
            EXEEXT=".exe"
            ;;
    linux)  HB_OS="LINUX"
            [ -d "/usr/lib/lib64" ] && [ "${HB_ARCH64}" = yes ] && HB_LIBDIRNAME="lib64"
            OS_LIBS="$OS_LIBS -ldl"
            ;;
    bsd)    HB_OS="BSD"
            MAKE="gmake"
            ;;
    darwin) HB_OS="DARWIN"
            ;;
    sunos)  HB_OS="SUNOS"
            OS_LIBS="$OS_LIBS -lrt"
            CRSLIB="curses"
            ;;
    hpux)   HB_OS="HPUX"
            MAKE="gmake"
            OS_LIBS="$OS_LIBS -lrt"
            ;;
    *)      HB_OS=`echo $HB_ARCHITECTURE | tr '[a-z]' '[A-Z]'`
            ;;
esac

GTSLN=""
GTCRS=""
GTXWC=""
for dir in /usr /usr/local /sw /opt/local
do
    if [ "$GTSLN" != yes ]; then
        if [ -f $dir/include/slang.h ]; then
            [ $dir = /usr ] || C_USR="$C_USR -I$dir/include"
            GTSLN=yes
        elif [ -f $dir/include/slang/slang.h ]; then
            C_USR="$C_USR -I$dir/include/slang"
            GTSLN=yes
        fi
    fi
    if [ "$GTCRS" != yes ]; then
        if [ -f ${dir}/include/curses.h ]; then
            [ $dir = /usr ] || C_USR="$C_USR -I$dir/include"
            GTCRS=yes
        elif [ -f ${dir}/include/${CRSLIB}/curses.h ]; then
            C_USR="$C_USR -I$dir/include/${CRSLIB}"
            GTCRS=yes
        fi
    fi
    if [ "$GTXWC" != yes ]; then
        if [ -f ${dir}/include/X11/Xlib.h ] && \
           [ -f ${dir}/include/X11/Xcms.h ] && \
           [ -f ${dir}/include/X11/Xutil.h ] && \
           [ -f ${dir}/include/X11/keysym.h ]; then
            [ $dir = /usr ] || C_USR="$C_USR -I$dir/include"
            GTXWC=yes
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
    #OS_LIBS="$OS_LIBS -lX11 -L/usr/X11R6/$HB_LIBDIRNAME"
fi

export C_USR="$C_USR -DHB_OS_$HB_OS"
export HB_OS_LIBS="$HB_OS_LIBS $OS_LIBS"
export HB_GT_LIST="$HB_GT_LIST $GT_LIST"
export CC LD EXEEXT

#export HB_BUILD_VERBOSE=yes

mkdir -p obj/$HB_CC_NAME lib/$HB_CC_NAME bin/$HB_CC_NAME

# Convert common.mak (for BCC/VC) to common.cf (GCC)

sed -e "s/\.obj/.o/g"         \
    -e "s/\.lib/.a/g"         \
    -e "s/\.dll/.so/g"        \
    -e "s/\.exe//g"           \
    -e "s/;/ /g"              \
    -e 's!\\\(.\)!/\1!g'      \
    -e "s/^HB_LIB_PREFIX=.*$/HB_LIB_PREFIX=lib/g"                       \
    -e 's/^!if "\($([A-Za-z0-9_]*)\)" != "\(.*\)"/ifneq (\1,\2)/g'      \
    -e 's/^!if "\($([A-Za-z0-9_]*)\)" == "\(.*\)"/ifeq (\1,\2)/g'       \
    -e 's/^!ifdef /ifdef /g'                                            \
    -e 's/^!ifndef /ifndef /g'                                          \
    -e 's/^!else/else/g'                                                \
    -e 's/^!endif/endif/g'                                              \
    -e 's/^!include/include/g'                                          \
    -e 's/$(HB_GT_LIST)/$(foreach gt, $(HB_GT_LIST), $(GT$(gt)_LIB))/g' \
   common.mak > common.cf

#$MAKE -n -p -r -f make_gcc.mak 1>EOK 2>ERR
$MAKE -r -f make_gcc.mak $*
rm -f common.cf
