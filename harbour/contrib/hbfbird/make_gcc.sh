#!/bin/sh

#
# $Id$
#

if [ "${FIREBIRD_INC}" == "" ]
then
    echo "---------------------------------------------------------------"
    echo "IMPORTANT: You'll need Firebird package and this envvar"
    echo "           to be set to successfully build this library:"
    echo "           export FIREBIRD_INC=C:/Firebird/include"
    echo "           or"
    echo "           export FIREBIRD_INC=/usr/include/firebird"
    echo "---------------------------------------------------------------"
    exit 1
fi

export CFLAGS=""
for I in ${FIREBIRD_INC}; do
    CFLAGS="${CFLAGS} -I${I}"
done
../mtpl_gcc.sh $1 $2 $3 $4 $5 $6 $7 $8 $9
unset CFLAGS
