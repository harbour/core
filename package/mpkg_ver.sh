#!/bin/sh

# ---------------------------------------------------------------
# Copyright 2003 Przemyslaw Czerpak <druzus@priv.onet.pl>
# small set of functions used by Harbour scripts
#
# See COPYING.txt for licensing terms.
# ---------------------------------------------------------------

get_hbver()
{
   if [ -z "$hb_rootdir" ]; then hb_rootdir=".."; fi
   FVER="${hb_rootdir}/include/hbver.h"
   MAJOR=`sed -e '/HB_VER_MAJOR/   !d' -e 's/[^0-9]*\([^ ]*\).*/\1/g' "${FVER}"`
   MINOR=`sed -e '/HB_VER_MINOR/   !d' -e 's/[^0-9]*\([^ ]*\).*/\1/g' "${FVER}"`
   RELEA=`sed -e '/HB_VER_RELEASE/ !d' -e 's/[^0-9]*\([^ ]*\).*/\1/g' "${FVER}"`
   echo "${MAJOR}.${MINOR}.${RELEA}"
}

get_hbverstat()
{
   if [ -z "$hb_rootdir" ]; then hb_rootdir=".."; fi
   FVER="${hb_rootdir}/include/hbver.h"
   VERSTAT=`sed -e '/HB_VER_STATUS/ !d' -e 's/[^\"]*\"\([^\"]*\).*/\1/g' "${FVER}"`
   echo "${VERSTAT}"
}
