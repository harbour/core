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
# See COPYING for licensing terms.
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

if [ "$HB_COMPILER" = "gcc" ] || \
   [ "$HB_COMPILER" = "mingw" ] || \
   [ "$HB_COMPILER" = "mingw64" ] || \
   [ "$HB_COMPILER" = "mingwarm" ] || \
   [ "$HB_COMPILER" = "cygwin" ] || \
   [ "$HB_COMPILER" = "djgpp" ] || \
   [ "$HB_COMPILER" = "icc" ] || \
   [ "$HB_COMPILER" = "sunpro" ]
then
    if [ -n "${HB_TOOLS_PREF}" ]; then
        hb_mkdyn="${HB_BIN_INSTALL}/${HB_TOOLS_PREF}-mkdyn"
        rm -f "${hb_mkdyn}"
        sed -e "s/^# HB_ARCHITECTURE=\"\"\$/HB_ARCHITECTURE=\"${HB_ARCHITECTURE}\"/g" \
            -e "s/^# HB_CCPREFIX=\"\"\$/[ -n \"\${HB_CCPREFIX}\" ] || HB_CCPREFIX=\"${HB_CCPREFIX}\"/g" \
            "${hb_root}/bin/hb-mkdyn.sh" > "${hb_mkdyn}" && \
        chmod 755 "${hb_mkdyn}"
    elif [ "$HB_COMPILER" = "icc" ]; then
        hb_mkdyn="${HB_BIN_INSTALL}/hb-mkdyn"
        rm -f "${hb_mkdyn}"
        sed -e "s/gcc/icc/g" "${hb_root}/bin/hb-mkdyn.sh" > "${hb_mkdyn}" && \
        chmod 755 "${hb_mkdyn}"
    elif [ "$HB_COMPILER" = "sunpro" ]; then
        hb_mkdyn="${HB_BIN_INSTALL}/hb-mkdyn"
        rm -f "${hb_mkdyn}"
        if [ "$HB_ARCHITECTURE" = "sunos" ] && \
           (isalist|grep sparc) &>/dev/null; then
            lnopt="-xcode=pic32"
        else
            lnopt="-KPIC"
        fi
        [ "$HB_BUILD_OPTIM" = "no" ] || lnopt="-fast -xnolibmopt $lnopt"
        sed -e "s/gcc -shared -fPIC/suncc -G ${lnopt} ${HB_ISAOPT}/g" \
            "${hb_root}/bin/hb-mkdyn.sh" > "${hb_mkdyn}" && \
        chmod 755 "${hb_mkdyn}"
    elif [ "${HB_ARCHITECTURE}" = "sunos" ] || \
         [ "${HB_ARCHITECTURE}" = "hpux" ] || \
         ! which install &>/dev/null; then
        hb_mkdyn="${HB_BIN_INSTALL}/hb-mkdyn"
        rm -f "${hb_mkdyn}"
        cp "${hb_root}/bin/hb-mkdyn.sh" "${hb_mkdyn}" && \
        chmod 755 "${hb_mkdyn}"
    elif [ "${HB_ARCHITECTURE}" != "dos" ]; then
        hb_mkdyn="${HB_BIN_INSTALL}/hb-mkdyn"
        # Without -c some OSes _move_ the file instead of copying it!
        install -c -m 755 "${hb_root}/bin/hb-mkdyn.sh" "${hb_mkdyn}"
    fi

    # Compatibility hb-mkslib creation. Please use hb-mkdyn instead.
    if [ -n "${hb_mkdyn}" ] && [ -f "${hb_mkdyn}" ]; then
        hb_mkdyn="${HB_TOOLS_PREF-hb}-mkslib"
        (cd "${HB_BIN_INSTALL}" && rm -f "${hb_mkdyn}" && \
         ln -s "${HB_TOOLS_PREF-hb}-mkdyn" "${hb_mkdyn}")
    fi

    mk_hbtools "${HB_BIN_INSTALL}" "$@"

    if [ "${HB_ARCHITECTURE}" != "dos" ]; then
        mk_hblibso "${hb_root}"
    fi
fi
