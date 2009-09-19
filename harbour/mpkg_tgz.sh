#!/bin/sh
[ "$BASH" ] || exec bash `which $0` ${1+"$@"}
#
# $Id$
#

# ---------------------------------------------------------------
# Copyright 2003 Przemyslaw Czerpak <druzus@polbox.com>
# simple script to build binaries .tgz from Harbour sources
#
# See COPYING for licensing terms.
# ---------------------------------------------------------------

cd `dirname $0`
. bin/hb-func.sh

name="harbour"
hb_ver=`get_hbver`
hb_verstat=`get_hbverstat`
hb_platform=`get_hbplatform`
[ "${hb_verstat}" = "" ] || hb_ver="${hb_ver}-${hb_verstat}"
[ "${hb_platform}" = "" ] || hb_platform="-${hb_platform}${HB_BUILDSUF}"
[ "${HB_XBUILD}" = "" ] || hb_platform="-${HB_XBUILD}"
hb_archfile="${name}-${hb_ver}${hb_platform}.bin.tar.gz"
# disabled self extracting shell envelop
hb_instfile="${name}-${hb_ver}${hb_platform}.inst.sh"
hb_lnkso="yes"
hb_pref="hb"
hb_contrib=""
hb_sysdir="yes"
hb_exesuf=""

[ -z "$HB_INSTALL_PREFIX" ] && [ -n "$PREFIX" ] && export HB_INSTALL_PREFIX="$PREFIX"

if [ -z "$TMPDIR" ]; then TMPDIR="/tmp"; fi
HB_INST_PREF="$TMPDIR/$name.bin.$USER.$$"

if [ -z "$HB_PLATFORM" ]; then
    if [ "$OSTYPE" = "msdosdjgpp" ]; then
        hb_plat="dos"
    else
        hb_plat=`uname -s | tr -d "[-]" | tr '[A-Z]' '[a-z]' 2>/dev/null`
        case "$hb_plat" in
            *windows*|*mingw32*|msys*) hb_plat="win" ;;
            *os/2*)                    hb_plat="os2" ;;
            *dos)                      hb_plat="dos" ;;
            *bsd)                      hb_plat="bsd" ;;
        esac
    fi
    export HB_PLATFORM="$hb_plat"
fi

if [ -z "$HB_COMPILER" ]; then
    case "$HB_PLATFORM" in
        win) HB_COMPILER="mingw" ;;
        dos) HB_COMPILER="djgpp" ;;
        *)   HB_COMPILER="gcc" ;;
    esac
    export HB_COMPILER
fi

if [ -z "$HB_COMMERCE" ]; then export HB_COMMERCE=no; fi

# default lib dir name
HB_LIBDIRNAME="lib"

ETC="/etc"

HB_ARCH64=""
if [ "$HB_PLATFORM" = "linux" ]
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

# Select the platform-specific installation prefix and ownership
HB_INSTALL_OWNER=root
case "$HB_PLATFORM" in
    darwin)
        [ -z "$HB_INSTALL_PREFIX" ] && HB_INSTALL_PREFIX="/usr/local"
        HB_INSTALL_GROUP=wheel
        ETC="/private/etc"
        hb_lnkso="no"
        ;;
    linux)
        [ -z "$HB_INSTALL_PREFIX" ] && HB_INSTALL_PREFIX="/usr"
        [ -d "$HB_INSTALL_PREFIX/lib64" ] && [ "${HB_ARCH64}" = yes ] && HB_LIBDIRNAME="lib64"
        HB_INSTALL_GROUP=root
        ;;
    sunos)
        [ -z "$HB_INSTALL_PREFIX" ] && HB_INSTALL_PREFIX="/usr"
        [ -d "$HB_INSTALL_PREFIX/lib64" ] && [ "${HB_ARCH64}" = yes ] && HB_LIBDIRNAME="lib64"
        HB_INSTALL_GROUP=root
        ;;
    win)
        [ -z "$HB_INSTALL_PREFIX" ] && HB_INSTALL_PREFIX="/usr/local"
        HB_INSTALL_GROUP=0
        hb_sysdir="no"
        hb_exesuf=".exe"
        hb_instfile=""
        ;;
    dos)
        [ -z "$HB_INSTALL_PREFIX" ] && HB_INSTALL_PREFIX="/${name}"
        HB_INSTALL_GROUP=root
        hb_lnkso="no"
        hb_sysdir="no"
        hb_exesuf=".exe"
        hb_instfile=""
        hb_archfile="${name}.tgz"
        HB_INST_PREF="$TMPDIR/hb-$$"
        ;;
    *)
        [ -z "$HB_INSTALL_PREFIX" ] && HB_INSTALL_PREFIX="/usr/local"
        HB_INSTALL_GROUP=wheel
        ;;
esac

# Select the platform-specific command names
MAKE=make
TAR=tar
hb_gnutar=yes
if gtar --version >/dev/null 2>&1; then
   TAR=gtar
elif ! tar --version >/dev/null 2>&1; then
   hb_gnutar=no
   echo "Warning!!! Cannot find GNU TAR"
fi
if gmake --version >/dev/null 2>&1; then
   MAKE=gmake
elif ! make --version >/dev/null 2>&1; then
   echo "Warning!!! Cannot find GNU MAKE"
fi

# Set other platform-specific build options
if [ -z "$HB_INC_GPM" ]; then
    if [ "$HB_PLATFORM" = "linux" ] && \
       ( [ -f /usr/include/gpm.h ] || [ -f /usr/local/include/gpm.h ]); then
        HB_INC_GPM=yes
    else
        HB_INC_GPM=no
    fi
    export HB_INC_GPM
fi

if [ -z "${HB_INC_SLANG}" ]; then
    HB_INC_SLANG=yes
    case "$HB_PLATFORM" in
        linux|bsd|darwin|hpux|sunos)
            for dir in /usr /usr/local /sw /opt/local
            do
                if [ -f ${dir}/include/slang.h ] || \
                   [ -f ${dir}/include/slang/slang.h ]; then
                    HB_INC_SLANG=no
                fi
            done
            ;;
    esac
    export HB_INC_SLANG
fi

case "$HB_PLATFORM" in
    linux)
        ;;
    darwin)
        [ -z "$HB_INC_X11" ] && export HB_INC_X11=no
        ;;
    dos|win)
        [ -z "$HB_INC_X11" ] && export HB_INC_X11=no
        ;;
    *)
        [ -z "$HB_INC_X11" ] && export HB_INC_X11=no
        ;;
esac

if [ "$HB_COMMERCE" = yes ]
then
   export HB_INC_GPM=no
   export HB_INC_SLANG=no
fi

if [ "${hb_sysdir}" = "yes" ]; then
    export HB_BIN_INSTALL="$HB_INSTALL_PREFIX/bin"
    export HB_INC_INSTALL="$HB_INSTALL_PREFIX/include/${name}"
    export HB_LIB_INSTALL="$HB_INSTALL_PREFIX/$HB_LIBDIRNAME/${name}"
else
    export HB_BIN_INSTALL="$HB_INSTALL_PREFIX/bin"
    export HB_INC_INSTALL="$HB_INSTALL_PREFIX/include"
    export HB_LIB_INSTALL="$HB_INSTALL_PREFIX/$HB_LIBDIRNAME"
fi

# build
umask 022
$MAKE clean
$MAKE
for l in ${hb_contrib}
do
    (cd "contrib/$l"
     $MAKE clean
     $MAKE )
done

# install
rm -fR "${HB_INST_PREF}"

export _DEFAULT_BIN_DIR=$HB_BIN_INSTALL
export _DEFAULT_INC_DIR=$HB_INC_INSTALL
export _DEFAULT_LIB_DIR=$HB_LIB_INSTALL
export HB_BIN_INSTALL="$HB_INST_PREF$HB_BIN_INSTALL"
export HB_INC_INSTALL="$HB_INST_PREF$HB_INC_INSTALL"
export HB_LIB_INSTALL="$HB_INST_PREF$HB_LIB_INSTALL"

mkdir -p $HB_BIN_INSTALL
mkdir -p $HB_INC_INSTALL
mkdir -p $HB_LIB_INSTALL
$MAKE -i install
for l in ${hb_contrib}
do
    (cd "contrib/$l"
     $MAKE -i install)
done

# Keep the size of the binaries to a minimim.
if [ -f $HB_BIN_INSTALL/harbour${hb_exesuf} ]; then
    ${HB_CCPREFIX}strip $HB_BIN_INSTALL/harbour${hb_exesuf}
fi
if [ "$HB_PLATFORM" != "hpux" ]; then
    # Keep the size of the libraries to a minimim, but don't try to strip symlinks.
    ${HB_CCPREFIX}strip -S `find $HB_LIB_INSTALL -type f`
fi

if [ "${hb_sysdir}" = "yes" ]; then

mkdir -p $HB_INST_PREF$ETC/harbour
cp -f source/rtl/gtcrs/hb-charmap.def $HB_INST_PREF$ETC/harbour/hb-charmap.def
chmod 644 $HB_INST_PREF$ETC/harbour/hb-charmap.def

cat > $HB_INST_PREF$ETC/harbour.cfg <<EOF
CC=${HB_CCPREFIX}gcc
CFLAGS=-c -I$_DEFAULT_INC_DIR
VERBOSE=YES
DELTMP=YES
EOF

fi

# check if we should rebuild tools with shared libs
if [ "${hb_lnkso}" = yes ]
then
    case $HB_PLATFORM in
        darwin)     ADD_LIBS="$ADD_LIBS -lncurses -L/opt/local/lib -L/sw/lib" ;;
        dos|win)    ADD_LIBS="" ;;
        sunos)      ADD_LIBS="$ADD_LIBS -lcurses" ;;
        *)          ADD_LIBS="$ADD_LIBS -lncurses" ;;
    esac
    [ "${HB_INC_GPM}" != no ] && ADD_LIBS="$ADD_LIBS -lgpm"
    [ "${HB_INC_SLANG}" != no ] && ADD_LIBS="$ADD_LIBS -lslang"
    [ "${HB_INC_X11}" != no ] && ADD_LIBS="$ADD_LIBS -L/usr/X11R6/$HB_LIBDIRNAME -lX11"

    export HB_USER_LDFLAGS="-L${HB_LIB_INSTALL} -l${name} ${ADD_LIBS} ${HB_USER_LDFLAGS}"
    export HB_USER_PRGFLAGS="\"-D_DEFAULT_INC_DIR='${_DEFAULT_INC_DIR}'\" ${HB_USER_PRGFLAGS}"

    for utl in hbmk2 hbrun hbi18n hbformat hbtest
    do
        (cd "utils/${utl}"
         rm -fR "./${HB_PLATFORM}/${HB_COMPILER}"
         $MAKE install
         ${HB_CCPREFIX}strip "${HB_BIN_INSTALL}/${utl}${hb_exesuf}")
    done
fi

chmod 644 $HB_INC_INSTALL/*

CURDIR=$(pwd)
if [ $hb_gnutar = yes ]; then
    (cd "${HB_INST_PREF}"; $TAR czvf "${CURDIR}/${hb_archfile}" --owner=${HB_INSTALL_OWNER} --group=${HB_INSTALL_GROUP} .)
    UNTAR_OPT=xvpf
else
    (cd "${HB_INST_PREF}"; $TAR covf - . | gzip > "${CURDIR}/${hb_archfile}")
    UNTAR_OPT=xvf
fi
rm -fR "${HB_INST_PREF}"

if [ -n "${hb_instfile}" ]; then

   if [ "${HB_PLATFORM}" = linux ]; then
      DO_LDCONFIG="&& ldconfig"
   else
      DO_LDCONFIG=""
   fi
   # In the generated script use tar instead of $TAR because we can't be sure
   # if $TAR exists in the installation environment
   size=`wc -c "${hb_archfile}"|(read size file; echo $size)`
   cat > "${hb_instfile}" <<EOF
#!/bin/sh
[ "\$BASH" ] || exec bash \`which \$0\` \${1+"\$@"}
if [ "\$1" = "--extract" ]; then
    tail -c $size "\$0" > "${hb_archfile}"
    exit
fi
if [ \`id -u\` != 0 ]; then
    echo "This package has to be installed from root account."
    exit 1
fi
echo "Do you want to install ${name} (y/n)"
read ASK
if [ "\${ASK}" != "y" ] && [ "\${ASK}" != "Y" ]; then
    exit 1
fi
(tail -c $size "\$0" | gzip -cd | (cd /;tar ${UNTAR_OPT} -)) ${DO_LDCONFIG}
exit \$?
HB_INST_EOF
EOF
    cat "${hb_archfile}" >> "${hb_instfile}"
    chmod +x "${hb_instfile}"
    rm -f "${hb_archfile}"

fi
