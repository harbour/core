#!/bin/sh

#
# $Id$
#

if [ "${HB_INC_FIREBIRD}" == "" ]
then
    echo "---------------------------------------------------------------"
    echo "IMPORTANT: You'll need Firebird package and this envvar"
    echo "           to be set to successfully build this library:"
    echo "           export HB_INC_FIREBIRD=C:/Firebird/include"
    echo "           or"
    echo "           export HB_INC_FIREBIRD=/usr/include/firebird"
    echo "---------------------------------------------------------------"
    exit 1
fi

export CFLAGS=""
for I in ${HB_INC_FIREBIRD}; do
    CFLAGS="${CFLAGS} -I${I}"
done
../mtpl_gcc.sh $1 $2 $3 $4 $5 $6 $7 $8 $9
unset CFLAGS
