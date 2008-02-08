#!/bin/sh

#
# $Id$
#

if [ "${APOLLO_INC}" == "" ]
then
    echo "---------------------------------------------------------------"
    echo "IMPORTANT: You will need Apollo package installed and this"
    echo "           envvar to be set to successfully build this library:"
    echo "           export APOLLO_INC=C:/Apollo/include"
    echo "           or"
    echo "           export APOLLO_INC=/usr/include/apollo"
    echo "---------------------------------------------------------------"
    exit 1
fi

export CFLAGS=""
for I in ${APOLLO_INC}; do
    CFLAGS="${CFLAGS} -I${I}"
done
../mtpl_gcc.sh $1 $2 $3 $4 $5 $6 $7 $8 $9
unset CFLAGS
