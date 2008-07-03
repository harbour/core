#!/bin/sh

#
# $Id$
#

if [ "${HB_INC_GD}" = "" ]
then
    echo "---------------------------------------------------------------"
    echo "IMPORTANT: You will need GD package installed and this"
    echo "           envvar to be set to successfully build this library:"
    echo "           export HB_INC_GD=c:/gd/include"
    echo "           or"
    echo "           export HB_INC_GD=/usr/include/gd"
    echo "---------------------------------------------------------------"
    exit 1
fi

export CFLAGS=
for I in ${HB_INC_GD}; do
    CFLAGS="${CFLAGS} -I${I}"
done
../mtpl_gcc.sh $1 $2 $3 $4 $5 $6 $7 $8 $9
unset CFLAGS
