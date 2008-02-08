#!/bin/sh

#
# $Id$
#

# ---------------------------------------------------------------
# Partly based on harbour/make_gcc.sh by :
# Copyright 2007 Przemyslaw Czerpak (druzus/at/priv.onet.pl),
# See doc/license.txt for licensing terms.
# ---------------------------------------------------------------

if [ -z "$HB_ARCHITECTURE" ]; then
   if [ "$OSTYPE" = "msdosdjgpp" ]; then
      hb_arch="dos"
   else
      hb_arch=`uname -s | tr -d "[-]" | tr '[A-Z]' '[a-z]' 2>/dev/null`
      case "$hb_arch" in
         *windows*|*mingw32*|msys*)   hb_arch="w32" ;;
         *cygwin*)                    hb_arch="cyg" ;;
         *dos)                        hb_arch="dos" ;;
         *bsd)                        hb_arch="bsd" ;;
      esac
   fi
   export HB_ARCHITECTURE="$hb_arch" _HB_ARCH_SAVED=1
fi

#**************************************************************

if [ -z "$HB_CC_NAME" ]; then
   case "$HB_ARCHITECTURE" in
      w32) HB_CC_NAME="mingw" ;;
      dos) HB_CC_NAME="djgpp" ;;
      *)   HB_CC_NAME="gcc" ;;
   esac
   export HB_CC_NAME _HB_CC_NAME_SAVED=1
fi

#**************************************************************

_HB_DIRS="hbbmcdx hbbtree hbclipsm hbct hbgt hbmisc hbmsql hbnf hbtip hbtpathy hbzlib xhb"

case "$HB_ARCHITECTURE" in
   w32|cyg|os2)
        _HB_DIRS_ADD="gtwvg hbole hbodbc hbw32 hbw32ddr hbwhat32 hbziparch rddado"
        ;;
   *)
        _HB_DIRS_ADD=;;
esac

if [ "${APOLLO_INC}"    != "" ]; then _HB_DIRS="${_HB_DIRS} hbapollo"; fi;
if [ "${FIREBIRD_INC}"  != "" ]; then _HB_DIRS="${_HB_DIRS} hbfbird "; fi;
if [ "${FREEIMAGE_INC}" != "" ]; then _HB_DIRS="${_HB_DIRS} hbfimage"; fi;
if [ "${GD_INC}"        != "" ]; then _HB_DIRS="${_HB_DIRS} hbgd    "; fi;
if [ "${MYSQL_INC}"     != "" ]; then _HB_DIRS="${_HB_DIRS} hbmysql "; fi;
if [ "${PGSQL_INC}"     != "" ]; then _HB_DIRS="${_HB_DIRS} hbpgsql "; fi;
if [ "${ZLIB_INC}"      != "" ]; then _HB_DIRS="${_HB_DIRS} hbzlib  "; fi;
if [ "${ADS_INC}"       != "" ]; then _HB_DIRS="${_HB_DIRS} rddads  "; fi;

_HB_DIRS="${_HB_DIRS} ${_HB_DIRS_ADD}"

#**************************************************************

for n in ${_HB_DIRS}; do
  if [ -d $n ]; then
    echo Entering $n ... \( $1 $2 $3 $4 $5\)
    cd $n
    [ -f ./make_gcc.sh ] && ${SHELL} -c "./make_gcc.sh $1 $2 $3 $4 $5"
    cd ..
  fi
done

#**************************************************************

if [ -n "$_HB_CC_NAME_SAVED" ]; then unset HB_CC_NAME _HB_CC_NAME_SAVED;   fi
if [ -n "$_HB_ARCH_SAVED" ];    then unset HB_ARCHITECTURE _HB_ARCH_SAVED; fi
