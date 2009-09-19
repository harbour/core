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
