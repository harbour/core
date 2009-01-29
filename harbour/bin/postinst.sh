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
if [ ! -f ${hb_root}/bin/hb-func.sh ] && [ -f ./bin/hb-func.sh ]
then
   hb_root="."
fi

. ${hb_root}/bin/hb-func.sh

if [ "$HB_COMPILER" = "gcc" ] || [ "$HB_COMPILER" = "gpp" ] || \
   [ "$HB_COMPILER" = "mingw" ] || [ "$HB_COMPILER" = "mingwce" ] || \
   [ "$HB_COMPILER" = "djgpp" ]
then
    RANLIB=""
    MAKE=make
    AR="${CCPREFIX}ar -cr"
    AR_OPT=""
    if [ "${HB_ARCHITECTURE}" = "bsd" ] || \
       [ "${HB_ARCHITECTURE}" = "hpux" ] || \
       [ `uname` = "FreeBSD" ]; then
        MAKE=gmake
    elif [ "${HB_ARCHITECTURE}" = "darwin" ]; then
        # We must build an archive index on Darwin
        #AR="${CCPREFIX}ar -crs"
        AR="libtool"
        AR_OPT="-static ${LIBTOOL_USR} -o "
    fi

    if [ -n "${HB_TOOLS_PREF}" ]; then
        hb_mkslib="${HB_BIN_INSTALL}/${HB_TOOLS_PREF}-mkslib"
        rm -f "${hb_mkslib}"
        sed -e "s/^# HB_ARCHITECTURE=\"\"\$/HB_ARCHITECTURE=\"${HB_ARCHITECTURE}\"/g" \
            -e "s/^# CCPREFIX=\"\"\$/[ -n \"\${CCPREFIX}\" ] || CCPREFIX=\"${CCPREFIX}\"/g" \
            "${hb_root}/bin/hb-mkslib.sh" > "${hb_mkslib}" && \
        chmod 755 "${hb_mkslib}"
    elif [ "${HB_ARCHITECTURE}" = "sunos" ] || \
         [ "${HB_ARCHITECTURE}" = "hpux" ] || \
         ! which install &>/dev/null; then
        rm -f "${HB_BIN_INSTALL}/hb-mkslib"
        cp "${hb_root}/bin/hb-mkslib.sh" "${HB_BIN_INSTALL}/hb-mkslib" && \
        chmod 755 "${HB_BIN_INSTALL}/hb-mkslib"
    elif [ "${HB_ARCHITECTURE}" != "dos" ]; then
        # Without -c some OSes _move_ the file instead of copying it!
        install -c -m 755 "${hb_root}/bin/hb-mkslib.sh" "${HB_BIN_INSTALL}/hb-mkslib"
    fi
    mk_hbtools "${HB_BIN_INSTALL}" "$@"
    if [ "$HB_COMPILER" = "gcc" ] || [ "$HB_COMPILER" = "gpp" ] || \
       [ "$HB_COMPILER" = "mingw" ] || [ "$HB_COMPILER" = "mingwce" ]; then
        mk_hblibso "${hb_root}"
    fi
    # build hbfm lib with memory statistic
    (cd ${hb_root}/source/vm
    export C_USR="${C_USR//-DHB_FM_STATISTICS_OFF/} -DHB_FM_STATISTICS"
    rm -f fm.o
    ${MAKE} -r fm.o
    ${AR} ${AR_OPT} ${HB_LIB_INSTALL}/libhbfm.a fm.o
    [ -n "${RANLIB}" ] && ${RANLIB} ${HB_LIB_INSTALL}/libhbfm.a
    rm -f fm.o
    if [ "${HB_ARCHITECTURE}" != "dos" ]; then
        cd vmmt
        ${MAKE} -r fm.o
        ${AR} ${AR_OPT} ${HB_LIB_INSTALL}/libhbfmmt.a fm.o
        [ -n "${RANLIB}" ] && ${RANLIB} ${HB_LIB_INSTALL}/libhbfmmt.a
        rm -f fm.o
    fi
    )
fi
