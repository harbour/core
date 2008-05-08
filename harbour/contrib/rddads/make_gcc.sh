#!/bin/sh

#
# $Id$
#

if [ "${ADS_INC}" == "" ]
then
    echo "---------------------------------------------------------------"
    echo "IMPORTANT: You will need Advantage Client Engine (5.0 or upper)"
    echo "           installed and this envvar to be set to successfully"
    echo "           build this library:"
    echo "           export ADS_INC=C:/ads/acesdk"
    echo "           or"
    echo "           export ADS_INC=/usr/include/ads"
    echo "---------------------------------------------------------------"
    exit 1
fi

export CFLAGS=""
for I in ${ADS_INC}; do
    CFLAGS="${CFLAGS} -I${I}"
done
../mtpl_gcc.sh $1 $2 $3 $4 $5 $6 $7 $8 $9
unset CFLAGS
