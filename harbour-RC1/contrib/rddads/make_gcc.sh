#!/bin/sh

#
# $Id$
#

if [ "${HB_INC_ADS}" == "" ]
then
    echo "---------------------------------------------------------------"
    echo "IMPORTANT: You will need Advantage Client Engine (5.0 or upper)"
    echo "           installed and this envvar to be set to successfully"
    echo "           build this library:"
    echo "           export HB_INC_ADS=C:/ads/acesdk"
    echo "           or"
    echo "           export HB_INC_ADS=/usr/include/ads"
    echo "---------------------------------------------------------------"
    exit 1
fi

export CFLAGS=""
for I in ${HB_INC_ADS}; do
    CFLAGS="${CFLAGS} -I${I}"
done
../mtpl_gcc.sh $1 $2 $3 $4 $5 $6 $7 $8 $9
unset CFLAGS
