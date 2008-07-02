#!/bin/sh
#
# $Id$
#

# ---------------------------------------------------------------
# Copyright 2003 Przemyslaw Czerpak <druzus@polbox.com>
# simple script to build DEBs from Harbour sources
#
# See doc/license.txt for licensing terms.
# ---------------------------------------------------------------

test_reqpkg()
{
    dpkg -l "$1" 2> /dev/null | grep '^ii' &> /dev/null
}

TOINST_LST=""
for i in gcc binutils bash debhelper
do
    test_reqpkg "$i" || TOINST_LST="${TOINST_LST} $i"
done

if [ "$HB_COMMERCE" = yes ]
then
    export HB_GPM_MOUSE=no
    export HB_WITHOUT_GTSLN=yes
else
    if [ -z "$HB_GPM_MOUSE" ] && test_reqpkg libgpmg1-dev
    then
        export HB_GPM_MOUSE=yes
    fi
    if [ -z "$HB_WITHOUT_GTSLN" ] && ! test_reqpkg libslang2-dev
    then
        export HB_WITHOUT_GTSLN=yes
    fi
fi

if [ -z "$HB_WITHOUT_GTCRS" ] && ! test_reqpkg libncurses5-dev
then
    export HB_WITHOUT_GTCRS=yes
fi

if [ -z "$HB_WITHOUT_X11" ] && ! test_reqpkg libx11-dev
then
    export HB_WITHOUT_X11=yes
fi

export HB_CONTRIBLIBS="hbct hbmzip hbtip xhb hbbtree hbmisc hbvpdf hbgt hbbmcdx hbclipsm hbnf"

if [ -z "$HB_WITHOUT_ADS" ] && \
   [ ! -f "/usr/local/ads/acesdk/ace.h" ] && \
   [ ! -f "${HOME}/ads/acesdk/ace.h" ]
then
   export HB_CONTRIBLIBS="${HB_CONTRIBLIBS} rddads"
fi

if [ -z "$HB_WITHOUT_ODBC" ] && ! test_reqpkg unixodbc-dev
then
    export HB_CONTRIBLIBS="${HB_CONTRIBLIBS} hbodbc"
fi

if test_reqpkg libpq-dev
then
    export HB_CONTRIBLIBS="${HB_CONTRIBLIBS} hbpgsql"
fi

if test_reqpkg libmysqlclient15-dev
then
    export HB_CONTRIBLIBS="${HB_CONTRIBLIBS} hbmysql"
fi

if test_reqpkg libgd-xpm-dev || \
   test_reqpkg libgd2-xpm-dev 
then
    export HB_CONTRIBLIBS="${HB_CONTRIBLIBS} hbgd"
fi

if [ -z "${TOINST_LST}" ] || [ "$1" = "--force" ]
then
    . ./bin/pack_src.sh
    stat="$?"
    if [ -z "${hb_filename}" ]
    then
        echo "The script ./bin/pack_src.sh doesn't set archive name to \${hb_filename}"
        exit 1
    elif [ "${stat}" != 0 ]
    then
        echo "Error during packing the sources in ./bin/pack_src.sh"
        exit 1
    elif [ -f ${hb_filename} ]
    then
        dpkg-buildpackage -b
    else
        echo "Cannot find archive file: ${hb_filename}"
        exit 1
    fi
else
    echo "If you want to build Harbour compiler"
    echo "you have to install the folowing packages:"
    echo ""
    echo "${TOINST_LST}"
    echo ""
    echo "you can do that executing:"
    echo "sudo apt-get install ${TOINST_LST}"
    exit 1
fi
