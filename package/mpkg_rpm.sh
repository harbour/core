#!/bin/sh

# ---------------------------------------------------------------
# Copyright 2003 Przemyslaw Czerpak <druzus@polbox.com>
# simple script to build RPMs from Harbour sources
#
# See LICENSE.txt for licensing terms.
# ---------------------------------------------------------------

test_reqrpm()
{
   rpm -q --whatprovides "$1" >/dev/null 2>&1
}

NEED_RPM='make gcc binutils'

FORCE=''

LAST=''
while [ $# -gt 0 ]
do
   if [ "$1" = '--force' ]
   then
      FORCE='yes'
   else
      INST_PARAM="${INST_PARAM} $1"
      if [ "${LAST}" = '--with' ]
      then
         [ "$1" = 'mysql' ] && NEED_RPM="${NEED_RPM} mysql-devel"
         [ "$1" = 'odbc' ] && NEED_RPM="${NEED_RPM} unixodbc-devel"
         [ "$1" = 'pgsql' ] && NEED_RPM="${NEED_RPM} postgresql-devel"
         [ "$1" = 'firebird' ] && NEED_RPM="${NEED_RPM} firebird-devel"
         [ "$1" = 'freeimage' ] && NEED_RPM="${NEED_RPM} freeimage-devel"
         [ "$1" = 'allegro' ] && NEED_RPM="${NEED_RPM} allegro-devel"
      fi
   fi
   LAST="$1"
   shift
done

if [ "$HB_WITH_ADS" != 'no' ]
then
   if [ -f /usr/local/ads/acesdk/ace.h ] || \
      [ -f "${HOME}/ads/acesdk/ace.h" ] || \
      [ -f "${HB_WITH_ADS}/ace.h" ]
   then
      INST_PARAM="${INST_PARAM} --with ads"
   fi
fi
if test_reqrpm 'allegro-devel' && [ "$HB_WITH_ALLEGRO" != 'no' ]
then
   INST_PARAM="${INST_PARAM} --with allegro"
fi
if test_reqrpm 'cairo-devel' && [ "$HB_WITH_CAIRO" != 'no' ]
then
   INST_PARAM="${INST_PARAM} --with cairo"
fi
if test_reqrpm 'libcups2-devel' && [ "$HB_WITH_CUPS" != 'no' ]
then
   INST_PARAM="${INST_PARAM} --with cups"
fi
if test_reqrpm 'curl-devel' && [ "$HB_WITH_CURL" != 'no' ]
then
   INST_PARAM="${INST_PARAM} --with curl"
fi
if test_reqrpm 'openssl' && [ "$HB_WITH_OPENSSL" != 'no' ]
then
   INST_PARAM="${INST_PARAM} --with openssl"
fi
if test_reqrpm 'firebird-devel' && [ "$HB_WITH_FIREBIRD" != 'no' ]
then
   INST_PARAM="${INST_PARAM} --with firebird"
fi
if test_reqrpm 'freeimage-devel' && [ "$HB_WITH_FREEIMAGE" != 'no' ]
then
   INST_PARAM="${INST_PARAM} --with freeimage"
fi
if test_reqrpm 'gd-devel' && [ "$HB_WITH_GD" != 'no' ]
then
   v=$(rpm -q --whatprovides gd-devel --qf "%{VERSION}"|sed -e "s/[^0-9]*\([0-9]*\).*/\1/g")
   [ "$v" -ge 2 ] && INST_PARAM="${INST_PARAM} --with gd"
fi
if ( test_reqrpm 'libmariadbd-devel' || \
     test_reqrpm 'libmysqlclient-devel' || \
     test_reqrpm 'MySQL-devel' || \
     test_reqrpm 'mysql-devel' ) && \
   [ "$HB_WITH_MYSQL" != 'no' ]
then
   INST_PARAM="${INST_PARAM} --with mysql"
fi
if ( test_reqrpm 'unixodbc-devel' || \
     test_reqrpm 'unixODBC-devel' ) && \
   [ "$HB_WITH_ODBC" != 'no' ]
then
   INST_PARAM="${INST_PARAM} --with odbc"
fi
if test_reqrpm 'postgresql-devel' && [ "$HB_WITH_PGSQL" != 'no' ]
then
   INST_PARAM="${INST_PARAM} --with pgsql"
fi

if [ "${HB_BUILD_NOGPLLIB}" = 'yes' ]
then
   INST_PARAM="${INST_PARAM} --without gpllib"
fi
if [ "${HB_BUILD_NOGPLLIB}" = 'yes' ] || \
   [ "${HB_WITH_GPM}" = 'no' ] || \
   ! test_reqrpm 'gpm-devel'
then
   INST_PARAM="${INST_PARAM} --without gpm"
fi
if ! test_reqrpm 'XFree86-devel' && [ "$HB_WITH_X11" != 'no' ]
then
   INST_PARAM="${INST_PARAM} --without X11"
fi
if ! test_reqrpm ncurses || \
   ! test_reqrpm ncurses-devel || \
   [ "$HB_WITH_CURSES" != 'no' ]
then
   INST_PARAM="${INST_PARAM} --without curses"
fi
if ! test_reqrpm slang || \
   ! test_reqrpm slang-devel || \
   [ "$HB_WITH_SLANG" != 'no' ]
then
   INST_PARAM="${INST_PARAM} --without slang"
fi
if ( [ ! -f /usr/include/zlib.h ] && \
     [ ! -f /usr/local/include/zlib.h ] ) || \
   [ "$HB_WITH_ZLIB" = 'local' ]
then
   INST_PARAM="${INST_PARAM} --with localzlib"
fi
if ( [ ! -f /usr/include/pcre.h ] && \
     [ ! -f /usr/local/include/pcre.h ] ) || \
   [ "$HB_WITH_PCRE" = 'local' ]
then
   INST_PARAM="${INST_PARAM} --with localpcre"
fi
if ( [ ! -f /usr/include/pcre2.h ] && \
     [ ! -f /usr/local/include/pcre2.h ] ) || \
   [ "$HB_WITH_PCRE2" = 'local' ]
then
   INST_PARAM="${INST_PARAM} --with localpcre2"
fi

TOINST_LST=''
for i in ${NEED_RPM}
do
   test_reqrpm "$i" || TOINST_LST="${TOINST_LST} $i"
done

if [ -z "${TOINST_LST}" ] || [ "${FORCE}" = 'yes' ]
then
   cd "$(dirname "$0")" || exit
   . ./mpkg_src.sh
   stat="$?"
   if [ -z "${hb_filename}" ]
   then
      echo "The script ./mpkg_src.sh doesn't set archive name to \${hb_filename}"
      exit 1
   elif [ "${stat}" != 0 ]
   then
      echo 'Error during packing the sources in ./mpkg_src.sh'
      exit 1
   elif [ -f "${hb_filename}" ]
   then
      if [ "$(id -u)" != 0 ] && [ ! -f "${HOME}/.rpmmacros" ]
      then
         RPMDIR="${HOME}/RPM"
         mkdir -p "${RPMDIR}/SOURCES" "${RPMDIR}/RPMS" "${RPMDIR}/SRPMS" \
                  "${RPMDIR}/BUILD" "${RPMDIR}/SPECS"
         echo "%_topdir ${RPMDIR}" > "${HOME}/.rpmmacros"
      else
         RPMDIR=$(rpm --eval %_topdir)
      fi

      mv -f "${hb_filename}" "${RPMDIR}/SOURCES/"
      cp harbour.spec "${RPMDIR}/SPECS/"

      if which rpmbuild >/dev/null 2>&1
      then
         RPMBLD='rpmbuild'
      else
         RPMBLD='rpm'
      fi
      cd "${RPMDIR}/SPECS" || exit
      # shellcheck disable=SC2086
      ${RPMBLD} -ba harbour.spec ${INST_PARAM}
   else
      echo "Cannot find archive file: ${hb_filename}"
      exit 1
   fi
else
   echo 'If you want to build Harbour compiler'
   echo 'you have to install the folowing RPM files:'
   echo "${TOINST_LST}"
   exit 1
fi
