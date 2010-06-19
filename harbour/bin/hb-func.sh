#!/bin/sh
#
# $Id$
#

# ---------------------------------------------------------------
# Copyright 2003 Przemyslaw Czerpak <druzus@priv.onet.pl>
# small set of functions used by Harbour scripts
# warning: some bash extensions are used
#
# See COPYING for licensing terms.
# ---------------------------------------------------------------

get_hbplatform()
{
   if [ "$OSTYPE" = "msdosdjgpp" ]; then
      id="djgpp"
   else
      # please add your distro suffix if it not belong to the one recognized below
      # and remember that order checking can be important
      [ "${id}" = "" ] && id=`rel=$(rpm -q --queryformat='.%{VERSION}' mandriva-release-One 2>/dev/null) && echo "mdk$rel"|tr -d "."`
      [ "${id}" = "" ] && id=`rel=$(rpm -q --queryformat='.%{VERSION}' mandriva-release 2>/dev/null) && echo "mdk$rel"|tr -d "."`
      [ "${id}" = "" ] && id=`rel=$(rpm -q --queryformat='.%{VERSION}' mandrake-release 2>/dev/null) && echo "mdk$rel"|tr -d "."`
      [ "${id}" = "" ] && id=`rel=$(rpm -q --queryformat='.%{VERSION}' redhat-release 2>/dev/null) && echo "rh$rel"|tr -d "."`
      [ "${id}" = "" ] && id=`rel=$(rpm -q --queryformat='.%{VERSION}' fedora-release 2>/dev/null) && echo "fc$rel"|tr -d "."`
      [ "${id}" = "" ] && id=`rel=$(rpm -q --queryformat='.%{VERSION}' suse-release 2>/dev/null) && echo "sus$rel"|tr -d "."`
      [ "${id}" = "" ] && id=`rel=$(rpm -q --queryformat='.%{VERSION}' openSUSE-release 2>/dev/null) && echo "sus$rel"|tr -d "."`
      [ "${id}" = "" ] && id=`rel=$(rpm -q --queryformat='.%{VERSION}' conectiva-release 2>/dev/null) && echo "cl$rel"|tr -d "."`
      [ "${id}" = "" ] && id=`rel=$(rpm -q --queryformat='.%{VERSION}' aurox-release 2>/dev/null) && echo "cl$rel"|tr -d "."`
      [ "${id}" = "" ] && id=`[ -f /etc/pld-release ] && cat /etc/pld-release|sed -e '/1/ !d' -e 's/[^0-9]//g' -e 's/^/pld/'`
      [ "${id}" = "" ] && id=`uname -s | tr '[ A-Z]' '[_a-z]'`
      case "${id}" in
         mingw*) id="mingw" ;;
         *) ;;
      esac
   fi
   echo "${id}"
}

get_hbver()
{
   hb_rootdir="${1-.}"
   FVER="${hb_rootdir}/include/hbver.h"
   MAJOR=`sed -e '/HB_VER_MAJOR/   !d' -e 's/[^0-9]*\([^ ]*\).*/\1/g' "${FVER}"`
   MINOR=`sed -e '/HB_VER_MINOR/   !d' -e 's/[^0-9]*\([^ ]*\).*/\1/g' "${FVER}"`
   RELEA=`sed -e '/HB_VER_RELEASE/ !d' -e 's/[^0-9]*\([^ ]*\).*/\1/g' "${FVER}"`
   echo "${MAJOR}.${MINOR}.${RELEA}"
}

get_hbver_win()
{
   hb_rootdir="${1-.}"
   FVER="${hb_rootdir}/include/hbver.h"
   MAJOR=`sed -e '/HB_VER_MAJOR/   !d' -e 's/[^0-9]*\([^ ]*\).*/\1/g' "${FVER}"`
   MINOR=`sed -e '/HB_VER_MINOR/   !d' -e 's/[^0-9]*\([^ ]*\).*/\1/g' "${FVER}"`
   echo "${MAJOR}${MINOR}"
}

get_hbverstat()
{
   hb_rootdir="${1-.}"
   FVER="${hb_rootdir}/include/hbver.h"
   VERSTAT=`sed -e '/HB_VER_STATUS/ !d' -e 's/[^\"]*\"\([^\"]*\).*/\1/g' "${FVER}"`
   echo "${VERSTAT}"
}
