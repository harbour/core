#!/bin/sh
#
# $Id$
#

# This script requires "TAR" utilities for compression.

if [ "$1" = "zip" ] || [ "$1" = "ZIP" ]; then
   hb_archbin="zip"
   hb_ext=".zip"
elif tar --version >/dev/null 2>&1; then
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

hb_currdir=`pwd`

hb_archopt="-czf"
[ -n "${hb_ext}" ] || hb_ext=".tar.gz"

if [ -f mpkg_ver.sh ]; then
   hb_rootdir=".."
else
   hb_rootdir=`dirname $0`
   hb_rootdir=`dirname ${hb_rootdir}`
fi
. ${hb_rootdir}/package/mpkg_ver.sh

hb_ver=`get_hbver ${hb_rootdir}`
hb_filename="${hb_currdir}/harbour-${hb_ver}.src${hb_ext}"
rm -f $hb_filename

#[ -z "$TZ" ] && export TZ=PST8PDT

hb_get_entries()
{
   if [ "$format" = 8 ]; then
      sed -e '/^\f/,+1 !d' -e '/[a-zA-Z0-9_]/ !d' $1/entries
   else
      sed -e '/^[ ]*name="..*"[\r]*$/ !d' -e 's/^[ ]*name="\(.*\)".*$/\1/g' $1/entries
   fi
}

hb_collect_all()
{
   for d in `find . -name ".svn"`
   do
      for f in `hb_get_entries $d`
      do
         f="`dirname $d`/$f"
         [ -f "$f" ] && echo "$f"
      done
   done
}

hb_collect_all_svn()
{
   for f in `svn status -v|sed -e '/^?/ d' -e 's/.* \([^ ]*\)/\1/g'`
   do
      [ -f "$f" ] && echo "$f"
   done
}

hb_collect_all_tree()
{
   exclude="/obj/|/lib/|/bin/.*/|\.tar|\.zip|\.exe|\.log|/linux/|/win|/config/"
   for f in `find -type f | grep -vE ${exclude}`
   do
      echo ${f:2}
   done
   for f in `find config -type f`
   do
      echo ${f}
   done
}

hb_rmflst="yes"
hb_flst="bin/hb_flst.tmp"
#if [ -d "$hb_rootdir/.svn" ] || [ ! -r "$hb_rootdir/$hb_flst" ]; then
if [ -d "$hb_rootdir/.svn" ] ; then
   hb_rmflst="yes"
   #format=`cat $hb_rootdir/.svn/format`
   if [ "$format" = 4 ] || [ "$format" = 8 ]; then
      (cd "$hb_rootdir";hb_collect_all) > "$hb_rootdir/$hb_flst"
   else
      (cd "$hb_rootdir";hb_collect_all_svn) > "$hb_rootdir/$hb_flst"
   fi
   echo "$hb_flst" >> "$hb_rootdir/$hb_flst"
else
   hb_rmflst="yes"
   (cd "$hb_rootdir";hb_collect_all_tree) > "$hb_rootdir/$hb_flst"
fi

if [ "$hb_archbin" = "zip" ]; then
   (cd "$hb_rootdir";$hb_archbin -r -q $hb_filename . "-i@$hb_flst")
else
   (cd "$hb_rootdir";$hb_archbin $hb_archopt $hb_filename --files-from "$hb_flst")
fi
[ "$hb_rmflst" != "yes" ] || rm -fR "$hb_rootdir/$hb_flst"

cd "$hb_currdir"
