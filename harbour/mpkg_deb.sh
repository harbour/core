#!/bin/sh
[ "$BASH" ] || exec bash `which $0` ${1+"$@"}
#
# $Id$
#

# ---------------------------------------------------------------
# Copyright 2003 Przemyslaw Czerpak <druzus@polbox.com>
# simple script to build DEBs from Harbour sources
#
# See COPYING for licensing terms.
# ---------------------------------------------------------------

test_reqpkg()
{
    dpkg -l "$1" 2> /dev/null | grep '^ii' &> /dev/null
    status="$?"
    if [ -n "$2" ] && [ $status -eq 0 ]
    then
        dpkg --compare-versions `dpkg -s "$1" 2> /dev/null | grep "^Version:" | cut -d' ' -f2` ge $2
        status="$?"
    fi
    return "$status"
}

TOINST_LST=""
for i in gcc binutils bash debhelper
do
    test_reqpkg "$i" || TOINST_LST="${TOINST_LST} $i"
done

if [ "$HB_COMMERCE" = yes ]
then
    export HB_INC_GPM=no
    export HB_INC_SLANG=no
else
    if [ -z "$HB_INC_GPM" ] && ( test_reqpkg libgpmg1-dev || test_reqpkg libgpm-dev )
    then
        export HB_INC_GPM=yes
    fi
    if [ -z "$HB_INC_SLANG" ] && \
       ! test_reqpkg libslang2-dev && \
       ! test_reqpkg libslang1-dev
    then
        export HB_INC_SLANG=no
    fi
fi

if [ -z "$HB_INC_CURSES" ] && ! test_reqpkg libncurses5-dev
then
    export HB_INC_CURSES=no
fi

if [ -z "$HB_INC_X11" ] && ! test_reqpkg libx11-dev
then
    export HB_INC_X11=no
fi

export HB_CONTRIBLIBS="hbbmcdx hbbtree hbclipsm hbct hbgt hbmisc hbmzip hbnetio hbnf hbsqlit3 hbtip hbtpathy hbvpdf hbziparc xhb"

if [ ! -f "/usr/include/hpdf.h" ] && \
   [ ! -f "/usr/local/include/hpdf.h" ]
then
   export HB_CONTRIBLIBS="${HB_CONTRIBLIBS} hbhpdf"
fi

if ( [ -z "$HB_INC_ADS" ] || [ "$HB_INC_ADS" != "no" ] ) && \
   [ ! -f "/usr/local/ads/acesdk/ace.h" ] && \
   [ ! -f "${HOME}/ads/acesdk/ace.h" ]
then
   export HB_CONTRIBLIBS="${HB_CONTRIBLIBS} rddads"
fi

if test_reqpkg libcurl4-gnutls-dev || \
   test_reqpkg libcurl4-openssl-dev
then
    export HB_CONTRIBLIBS="${HB_CONTRIBLIBS} hbcurl"
fi

if test_reqpkg firebird2.0-dev
then
    export HB_CONTRIBLIBS="${HB_CONTRIBLIBS} hbfbird"
fi

if test_reqpkg libfreeimage-dev
then
    export HB_CONTRIBLIBS="${HB_CONTRIBLIBS} hbfimage"
fi

if test_reqpkg libgd-xpm-dev || \
   test_reqpkg libgd2-xpm-dev
then
    export HB_CONTRIBLIBS="${HB_CONTRIBLIBS} hbgd"
fi

if test_reqpkg libgtk2.0-dev
then
    export HB_CONTRIBLIBS="${HB_CONTRIBLIBS} hbgf"
fi

if test_reqpkg libmysqlclient15-dev
then
    export HB_CONTRIBLIBS="${HB_CONTRIBLIBS} hbmysql"
fi

if [ -z "$HB_INC_ODBC" ] || [ "$HB_INC_ODBC" != "no" ] && test_reqpkg unixodbc-dev
then
    export HB_CONTRIBLIBS="${HB_CONTRIBLIBS} hbodbc"
fi

if test_reqpkg libpq-dev
then
    export HB_CONTRIBLIBS="${HB_CONTRIBLIBS} hbpgsql"
fi

if test_reqpkg libqt4-dev "4.5.0"
then
    export HB_CONTRIBLIBS="${HB_CONTRIBLIBS} hbqt"
fi

if [ -z "${TOINST_LST}" ] || [ "$1" = "--force" ]
then
    . ./mpkg_src.sh
    stat="$?"
    if [ -z "${hb_filename}" ]
    then
        echo "The script ./mpkg_src.sh doesn't set archive name to \${hb_filename}"
        exit 1
    elif [ "${stat}" != 0 ]
    then
        echo "Error during packing the sources in ./mpkg_src.sh"
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
