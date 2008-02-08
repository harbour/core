#!/bin/sh

#
# $Id$
#

if [ "${MYSQL_INC}" == "" ]
then
    echo "---------------------------------------------------------------"
    echo "IMPORTANT: You will need MYSQL package installed and this"
    echo "           envvar to be set to successfully build this library:"
    echo "           export MYSQL_INC=C:/Mysql/include"
    echo "           or"
    echo "           export MYSQL_INC=/usr/include/mysql"
    echo "---------------------------------------------------------------"
    exit 1
fi

export CFLAGS=""
for I in ${MYSQL_INC}; do
    CFLAGS="${CFLAGS} -I${I}"
done
../mtpl_gcc.sh $1 $2 $3 $4 $5 $6 $7 $8 $9
unset CFLAGS
