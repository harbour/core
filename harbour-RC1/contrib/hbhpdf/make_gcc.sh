#!/bin/sh

#
# $Id$
#

if [ "${HB_INC_LIBHARU}" == "" ]
then
    echo "---------------------------------------------------------------"
    echo "IMPORTANT: You will need Haru Free PDF Library (libharu) DLL 
    echo "           package installed and this envvar to be set to"
    echo "           successfully build this library:"
    echo "           export HB_INC_LIBHARU=C:/libharu/include"
    echo "           or"
    echo "           export HB_INC_LIBHARU=/usr/include/libharu"
    echo "---------------------------------------------------------------"
    exit 1
fi

export CFLAGS=
for I in ${HB_INC_LIBHARU}; do
    CFLAGS="${CFLAGS} -I${I}"
done
../mtpl_gcc.sh $1 $2 $3 $4 $5 $6 $7 $8 $9
unset CFLAGS
