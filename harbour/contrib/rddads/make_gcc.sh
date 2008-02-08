#!/bin/sh

#
# $Id$
#

if [ "${ADS_INC}" == "" ]
then
    echo "---------------------------------------------------------------"
    echo "IMPORTANT: You will need Advantage package installed and this"
    echo "           envvar to be set to successfully build this library:"
    echo "           export ADS_INC=C:/Ads/acesdk"
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
