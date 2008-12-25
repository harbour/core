#!/bin/sh

#
# $Id$
#

if [ "${HB_INC_PGSQL}" = "" ]
then
    echo "---------------------------------------------------------------"
    echo "IMPORTANT: You will need Postgre SQL package installed and this"
    echo "           envvar to be set to successfully build this library:"
    echo "           export HB_INC_PGSQL=C:/pgsql/include"
    echo "           or"
    echo "           export HB_INC_PGSQL=/usr/include/postgresql"
    echo "---------------------------------------------------------------"
    exit 1
fi

export CFLAGS=""
export HB_MAKEFILE="../../mtpl_gcc.mak"
for I in ${HB_INC_PGSQL}; do
    CFLAGS="${CFLAGS} -I${I}"
done
../../mtpl_gcc.sh $1 $2 $3 $4 $5 $6 $7 $8 $9
unset CFLAGS
unset HB_MAKEFILE
