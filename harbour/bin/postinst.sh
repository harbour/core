#!/bin/sh
[ "$BASH" ] || exec bash `which $0` ${1+"$@"}
#
# $Id$
#

# ---------------------------------------------------------------
# Copyright 2003 Przemyslaw Czerpak <druzus@priv.onet.pl>
# simple script run after Harbour make install to finish install
# process
#
# See doc/license.txt for licensing terms.
# ---------------------------------------------------------------

if [ -z "$HB_ARCHITECTURE" ] || [ -z "$HB_COMPILER" ] || \
   [ -z "$HB_BIN_INSTALL" ] || \
   [ -z "$HB_INC_INSTALL" ] || \
   [ -z "$HB_LIB_INSTALL" ]
then
    echo "The following envvars must be set:"
    echo "   HB_ARCHITECTURE"
    echo "   HB_COMPILER"
    echo "   HB_BIN_INSTALL"
    echo "   HB_INC_INSTALL"
    echo "   HB_LIB_INSTALL"
    exit 1
fi

hb_root=`dirname "$0"`
if [ "${hb_root}" = "." ]
then
    hb_root=".."
else
    hb_root=`dirname "${hb_root}"`
fi
. ${hb_root}/bin/hb-func.sh

if [ "$HB_COMPILER" = "gcc" ] || [ "$HB_COMPILER" = "gpp" ] || \
   [ "$HB_COMPILER" = "mingw32" ] || [ "$HB_COMPILER" = "djgpp" ]
then
    RANLIB=""
    MAKE=make
    AR="${CCPREFIX}ar -cr"
    if [ "${HB_ARCHITECTURE}" = "bsd" ] || [ `uname` = "FreeBSD" ]; then
        MAKE=gmake
    elif [ "${HB_ARCHITECTURE}" = "darwin" ]; then
        # We must build an archive index on Darwin
        AR="${CCPREFIX}ar -crs"
    fi
    if [ "${HB_ARCHITECTURE}" = "sunos" ]; then
        install -m 755 -f "${HB_BIN_INSTALL}" "${hb_root}/bin/hb-mkslib.sh"
    elif [ "${HB_ARCHITECTURE}" != "dos" ]; then
        # Without -c some OSes _move_ the file instead of copying it!
        install -c -m 755 "${hb_root}/bin/hb-mkslib.sh" "${HB_BIN_INSTALL}/hb-mkslib"
    fi
    mk_hbtools "${HB_BIN_INSTALL}" "$@"
    if [ "$HB_COMPILER" = "gcc" ] || [ "$HB_COMPILER" = "gpp" ] || \
       [ "$HB_COMPILER" = "mingw32" ]; then
        mk_hblibso "${hb_root}"
    fi
    # build fm lib with memory statistic
    (cd ${hb_root}/source/vm
    C_USR=${C_USR//-DHB_FM_STATISTICS_OFF/}
    rm -f fm.o
    ${MAKE} -r fm.o
    ${AR} ${HB_LIB_INSTALL}/libfm.a fm.o
    [ -n "${RANLIB}" ] && ${RANLIB} ${HB_LIB_INSTALL}/libfm.a
    rm -f fm.o
    if [ "${HB_MT}" = "MT" ]; then
        ${MAKE} -r fm.o 'HB_LIBCOMP_MT=YES'
        ${AR} ${HB_LIB_INSTALL}/libfmmt.a fm.o
        [ -n "${RANLIB}" ] && ${RANLIB} ${HB_LIB_INSTALL}/libfmmt.a
        rm -f fm.o
    fi
    )
fi
