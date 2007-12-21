#!/bin/sh
#
# $Id$
#
# This script requires "TAR" utilities for compression.

if tar --version >/dev/null 2>&1; then
   hb_archbin="tar"
   hb_gnutar="yes"
elif gtar --version >/dev/null 2>&1; then
   hb_archbin="gtar"
   hb_gnutar="yes"
else
   hb_archbin="tar"
   hb_gnutar="no"
   echo "Warning!!! Cannot find GNU TAR"
fi

hb_archopt="-czf"
hb_ext=".tar.gz"
if [ -f bin/hb-func.sh ]; then
  hb_rootdir="."
else
  hb_rootdir=`dirname $0`
  hb_rootdir="${hb_rootdir}/.."
  hb_archopt="-C $hb_rootdir $hb_archopt"
fi
. ${hb_rootdir}/bin/hb-func.sh

hb_ver=`get_hbver ${hb_rootdir}`
hb_filename="harbour-${hb_ver}.src${hb_ext}"
rm -f $hb_filename

#[ -z "$TZ" ] && export TZ=PST8PDT

hb_collect_all()
{
   for d in `find . -name ".svn"`
   do
      for f in `sed -e '/^[ ]*name="..*"$/ !d' -e 's/^[ ]*name="\(.*\)"$/\1/g' $d/entries`
      do
         f="`dirname $d`/$f"
         [ -f "$f" ] && echo "$f"
      done
   done
}

hb_flst="$hb_rootdir/bin/hb_flst.tmp"
(cd "$hb_rootdir";hb_collect_all) > "$hb_flst"
$hb_archbin $hb_archopt $hb_filename --files-from "$hb_flst"
rm -fR "$hb_flst"
