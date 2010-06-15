#!/bin/sh
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
   dpkg-buildpackage -b
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
