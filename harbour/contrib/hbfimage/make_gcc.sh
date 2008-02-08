#!/bin/sh

#
# $Id$
#

if [ "${FREEIMAGE_INC}" == "" ]
then
    echo "---------------------------------------------------------------"
    echo "IMPORTANT: You will need FreeImage package installed and this"
    echo "           envvar to be set to successfully build this library:"
    echo "           export FREEIMAGE_INC=C:/FreeImage/source"
    echo "           or"
    echo "           export FREEIMAGE_INC=/usr/include/freeimage"
    echo "---------------------------------------------------------------"
    exit 1
fi

export CFLAGS=""
for I in ${APOLLO_INC}; do
    CFLAGS="${CFLAGS} -I${I}"
done
../mtpl_gcc.sh $1 $2 $3 $4 $5 $6 $7 $8 $9
unset CFLAGS
