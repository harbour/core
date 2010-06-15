#!/bin/sh
#
# $Id$
#

# ---------------------------------------------------------------
# Copyright 2003 Przemyslaw Czerpak <druzus@polbox.com>
#
# This script checks you have all tools to build Harbour binaries
# installed then takes current Harbour sources from SourceForge SVN
# repository and builds binary RPMs at your local host.
#
# See COPYING for licensing terms.
# ---------------------------------------------------------------

export SVNURL="https://harbour-project.svn.sourceforge.net/svnroot/harbour-project/trunk/harbour"
export PROJECT=harbour

test_reqrpm()
{
   rpm -q --whatprovides "$1" >/dev/null 2>&1
}

TOINST_LST=""
for i in subversion make gcc binutils bash
do
   test_reqrpm "$i" || TOINST_LST="${TOINST_LST} $i"
done

if [ -z "${TOINST_LST}" ] || [ "$1" = "--force" ]
then
   cd
   mkdir -p SVN
   cd SVN
   if svn co "${SVNURL}"; then
      cd "${PROJECT}"
      ./mpkg_rpm.sh "$*"
   fi
else
   echo "If you want to build Harbour compilers"
   echo "you have to install the following RPM files:"
   echo "${TOINST_LST}"
   echo ""
   echo "If you want to force installation run this script with --force parameter:"
   echo "$0 --force"
fi
