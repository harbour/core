#!/bin/sh

#
# $Id$
#

if [ "${PGSQL_INC}" == "" ]
then
    echo "---------------------------------------------------------------"
    echo "IMPORTANT: You will need PostgreSQL package installed and this"
    echo "           envvar to be set to successfully build this library:"
    echo "           export PGSQL_INC=C:/Posgres/include"
    echo "           or"
    echo "           export PGSQL_INC=/usr/include/postgres"
    echo "---------------------------------------------------------------"
    exit 1
fi

export CFLAGS=""
for I in ${PGSQL_INC}; do
    CFLAGS="${CFLAGS} -I${I}"
done
../mtpl_gcc.sh $1 $2 $3 $4 $5 $6 $7 $8 $9
unset CFLAGS
