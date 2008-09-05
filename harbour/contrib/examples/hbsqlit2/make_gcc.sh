#!/bin/sh

#
# $Id$
#

if [ "${HB_INC_SQLITE2}" = "" ]
then
    echo "---------------------------------------------------------------"
    echo "IMPORTANT: You will need SQLite 2.8.16 package package installed and this"
    echo "           envvar to be set to successfully build this library:"
    echo "           export HB_INC_SQLITE2=C:/sqlite2"
    echo "           or"
    echo "           export HB_INC_SQLITE2=/usr/include/sqlite2"
    echo "---------------------------------------------------------------"
    exit 1
fi

export HB_ROOT=../../..
export HB_MAKEFILE=../../mtpl_gcc.mak

export CFLAGS=""
for I in ${HB_INC_SQLITE2}; do
    CFLAGS="${CFLAGS} -I${I}"
done
../mtpl_gcc.sh $1 $2 $3 $4 $5 $6 $7 $8 $9

unset CFLAGS

unset HB_ROOT
unset HB_MAKEFILE
