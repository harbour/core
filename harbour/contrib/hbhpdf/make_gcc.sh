#!/bin/sh

#
# $Id$
#

if [ "${LIBHARU_INC}" == "" ]
then
    echo "---------------------------------------------------------------"
    echo "IMPORTANT: You will need Haru Free PDF Library (libharu) DLL 
    echo "           package installed and this envvar to be set to"
    echo "           successfully build this library:"
    echo "           export LIBHARU_INC=C:/libharu/include"
    echo "           or"
    echo "           export LIBHARU_INC=/usr/include/libharu"
    echo "---------------------------------------------------------------"
    exit 1
fi

export CFLAGS=
for I in ${LIBHARU_INC}; do
    CFLAGS="${CFLAGS} -I${I}"
done
../mtpl_gcc.sh $1 $2 $3 $4 $5 $6 $7 $8 $9
unset CFLAGS
