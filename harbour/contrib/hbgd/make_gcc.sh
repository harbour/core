#!/bin/sh

#
# $Id$
#

if [ "${GD_INC}" == "" ]
then
    echo "---------------------------------------------------------------"
    echo "IMPORTANT: You will need GD package installed and this"
    echo "           envvar to be set to successfully build this library:"
    echo "           export GD_INC=c:/gd/include"
    echo "           or"
    echo "           export GD_INC=/usr/include/gd"
    echo "---------------------------------------------------------------"
    exit 1
fi

export CFLAGS=
for I in ${GD_INC}; do
    CFLAGS="${CFLAGS} -I${I}"
done
../mtpl_gcc.sh $1 $2 $3 $4 $5 $6 $7 $8 $9
unset CFLAGS
