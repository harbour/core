#!/bin/sh
#
# $Id$
#

# ---------------------------------------------------------------
# Copyright 2007 Przemyslaw Czerpak (druzus/at/priv.onet.pl)
# simple script to build Harbour-WinCE cross build RPMs
#
# See COPYING.txt for licensing terms.
# ---------------------------------------------------------------

test_reqrpm()
{
   rpm -q --whatprovides "$1" >/dev/null 2>&1
}

get_rpmmacro()
{
   local R X Y

   R=`rpm --showrc|sed -e "/^-14:.${1}[^a-z0-9A-Z_]/ !d" -e "s/^-14: ${1}.//"`
   X=`echo "${R}"|sed -e "s/.*\(%{\([^}]*\)}\).*/\2/"`
   while [ "${X}" != "${R}" ]
   do
      Y=`get_rpmmacro "$X"`
      if [ -n "${Y}" ]
      then
         R=`echo "${R}"|sed -e "s!%{${X}}!${Y}!g"`
         X=`echo "${R}"|sed -e "s/.*\(%{\([^}]*\)}\).*/\2/"`
      else
         X="${R}"
      fi
   done
   echo -n "${R}"
}

cd `dirname $0`
. ./mpkg_ver.sh
hb_ver=`get_hbver`
hb_verstat=`get_hbverstat`
[ -n "${hb_verstat}" ] || hb_verstat="0"

NEED_RPM="make gcc binutils bash cegcc-mingw32ce"

FORCE=""

LAST=""
while [ $# -gt 0 ]
do
   if [ "$1" = "--force" ]
   then
      FORCE="yes"
   else
      INST_PARAM="${INST_PARAM} $1"
   fi
   LAST="$1"
   shift
done

TOINST_LST=""
for i in ${NEED_RPM}
do
   test_reqrpm "$i" || TOINST_LST="${TOINST_LST} $i"
done

if [ -z "${TOINST_LST}" ] || [ "${FORCE}" = "yes" ]
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
      if [ `id -u` != 0 ] && [ ! -f ${HOME}/.rpmmacros ]
      then
         RPMDIR="${HOME}/RPM"
         mkdir -p ${RPMDIR}/SOURCES ${RPMDIR}/RPMS ${RPMDIR}/SRPMS \
                  ${RPMDIR}/BUILD ${RPMDIR}/SPECS
         echo "%_topdir ${RPMDIR}" > ${HOME}/.rpmmacros
      else
         RPMDIR=`get_rpmmacro "_topdir"`
      fi
      mv ${hb_filename} ${RPMDIR}/SOURCES/
      sed -e "s/^%define version .*$/%define version   ${hb_ver}/g" \
          -e "s/^%define releasen .*$/%define releasen  ${hb_verstat}/g" \
         harbour-wce.spec.in > ${RPMDIR}/SPECS/harbour-wce.spec
      if which rpmbuild >/dev/null 2>&1
      then
         RPMBLD="rpmbuild"
      else
         RPMBLD="rpm"
      fi
      cd ${RPMDIR}/SPECS
      ${RPMBLD} -ba harbour-wce.spec ${INST_PARAM}
   else
      echo "Cannot find archive file: ${hb_filename}"
      exit 1
   fi
else
   echo "If you want to build Harbour compiler"
   echo "you have to install the folowing RPM files:"
   echo "${TOINST_LST}"
   exit 1
fi
