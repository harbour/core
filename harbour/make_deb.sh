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
    dpkg -s "$1" &> /dev/null
}

TOINST_LST=""
for i in gcc binutils bash debmake libncurses5-dev libslang2-dev libgpmg1-dev libx11-dev unixodbc-dev
do
    test_reqpkg "$i" || TOINST_LST="${TOINST_LST} $i"
done

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
