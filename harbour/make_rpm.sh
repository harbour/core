#!/bin/sh
#
# $Id$
#

# ---------------------------------------------------------------
# Copyright 2003 Przemyslaw Czerpak <druzus@polbox.com>
# simple script to build RPMs from Harbour sources
#
# See doc/license.txt for licensing terms.
# ---------------------------------------------------------------

######################################################################
# Conditional build:
# --with static      - link all binaries with static libs
# --with adsrdd      - build ads RDD
# --with mysql       - build mysql lib
# --with pgsql       - build pgsql lib
# --with odbc        - build build odbc lib
# --with allegro     - build GTALLEG - Allegro based GT driver
# --without nf       - do not build nanforum lib
# --without x11      - do not build GTXVT
# --without gpm      - build GTSLN and GTCRS without GPM support
# --without gtsln    - do not build GTSLN
######################################################################

test_reqrpm()
{
    rpm -q --whatprovides "$1" &> /dev/null
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

NEED_RPM="gcc binutils bash bison ncurses ncurses-devel"

FORCE=""
BUGGY_RPM=""
if [ -f /etc/conectiva-release ]
then
    BUGGY_RPM="yes"
fi

LAST=""
while [ $# -gt 0 ]
do
    if [ "$1" = "--force" ]
    then
        FORCE="yes"
    else
        INST_PARAM="${INST_PARAM} $1"
        if [ "${LAST}" = "--with" ]
        then
            if  [ -f /etc/conectiva-release ]
            then
                [ "$1" = "mysql" ] && NEED_RPM="${NEED_RPM} MySQL-devel"
                [ "$1" = "odbc" ] && NEED_RPM="${NEED_RPM} unixodbc-devel"
            else
                [ "$1" = "mysql" ] && NEED_RPM="${NEED_RPM} mysql-devel"
                [ "$1" = "odbc" ] && NEED_RPM="${NEED_RPM} unixODBC-devel"
            fi
            [ "$1" = "pgsql" ] && NEED_RPM="${NEED_RPM} postgresql-devel"
            [ "$1" = "allegro" ] && NEED_RPM="${NEED_RPM} allegro-devel"
        fi
    fi
    LAST="$1"
    shift
done

if test_reqrpm "MySQL-devel" || test_reqrpm "mysql-devel"
then 
    INST_PARAM="${INST_PARAM} --with mysql"
fi
if test_reqrpm "postgresql-devel"
then 
    INST_PARAM="${INST_PARAM} --with pgsql"
fi
if test_reqrpm "unixodbc-devel" || test_reqrpm "unixODBC-devel"
then
    INST_PARAM="${INST_PARAM} --with odbc"
fi
if test_reqrpm "allegro-devel"
then
    INST_PARAM="${INST_PARAM} --with allegro"
fi
if ! test_reqrpm "gpm-devel"
then
    INST_PARAM="${INST_PARAM} --without gpm"
fi

TOINST_LST=""
for i in ${NEED_RPM}
do
    test_reqrpm "$i" || TOINST_LST="${TOINST_LST} $i"
done

if [ -z "${TOINST_LST}" ] || [ "${FORCE}" = "yes" ]
then
    . ./bin/pack_src.sh
    stat="$?"
    if [ -z "${hb_filename}" ]
    then
        echo "The script ./bin/pack_src.sh doesn't set archive name to \${hb_filename}"
        exit 1
    elif [ "${stat}" != 0 ]
    then
        echo "Error during packing the sources in ./bin/pack_src.sh"
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
        if [ "${BUGGY_RPM}" = "yes" ]
        then
            cp ${hb_filename} ${RPMDIR}/SOURCES
            cp harbour.spec ${RPMDIR}/SPECS	
        fi
        if [ "${BUGGY_RPM}" = "yes" ]
        then
            rpm -ba harbour.spec ${INST_PARAM}
        elif which rpmbuild &>/dev/null	    
        then
            rpmbuild -ta ${hb_filename} --rmsource ${INST_PARAM}
        else
            rpm -ta ${hb_filename} --rmsource ${INST_PARAM}
        fi
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
