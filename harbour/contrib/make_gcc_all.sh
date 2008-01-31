#!/bin/sh

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
   export HB_ARCHITECTURE="$hb_arch" HB_ARCH_SAVED=1
fi

#**************************************************************

if [ -z "$HB_CC_NAME" ]; then
   case "$HB_ARCHITECTURE" in
      w32) HB_CC_NAME="mingw" ;;
      dos) HB_CC_NAME="djgpp" ;;
      *)   HB_CC_NAME="gcc" ;;
   esac
   export HB_CC_NAME HB_CC_NAME_SAVED=1
fi

#**************************************************************

export _HB_DIRS_1="hbbmcdx rddado"
export _HB_DIRS_2="hbbtree"

#echo ${_HB_DIRS_1} ${_HB_DIRS_2}

#**************************************************************

for n in ${_HB_DIRS_1} ${_HB_DIRS_2}; do
  if [ -d $n ]; then
    echo Entering $n ... \( $1 $2 $3 $4 $5\)
    cd $n
    [ -f ./make_gcc.sh ] && ${SHELL} -c "./make_gcc.sh $1 $2 $3 $4 $5"
    cd ..
  fi
done

#**************************************************************

unset _HB_DIRS_1 _HB_DIRS_2

if [ -n "$HB_CC_NAME_SAVED" ]; then unset HB_CC_NAME HB_CC_NAME_SAVED;   fi
if [ -n "$HB_ARCH_SAVED" ];    then unset HB_ARCHITECTURE HB_ARCH_SAVED; fi
