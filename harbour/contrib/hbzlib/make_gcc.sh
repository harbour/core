#!/bin/sh

#
# $Id$
#

if [ "${ZLIB_INC}" == "" ]
then
    echo "---------------------------------------------------------------"
    echo "IMPORTANT: You will need ZLib package installed and this"
    echo "           envvar to be set to successfully build this library:"
    echo "           export ZLIB_INC=C:/Zlib/include"
    echo "           or"
    echo "           export ZLIB_INC=/usr/include"
    echo "---------------------------------------------------------------"
    exit 1
fi

export CFLAGS=""
for I in ${ZLIB_INC}; do
    CFLAGS="${CFLAGS} -I${I}"
done
../mtpl_gcc.sh $1 $2 $3 $4 $5 $6 $7 $8 $9
unset CFLAGS
