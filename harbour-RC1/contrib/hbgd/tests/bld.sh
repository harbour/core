#!/bin/sh
#
# $Id$
#

INC_DIR="-I../../include:/usr/include"
MY_EXE=$1
shift
MY_FILES="$*"
MY_LIBS="-L../../lib -lgdlib -L/usr/lib -lhbgd -lpng -lz -ljpeg -lfreetype -lm -lhbct"
MY_GTLIB="-gtstd"

if [ -z ${MY_FILES} ]; then
   MY_FILES=${MY_EXE};
fi

echo "Exe = ${MY_EXE}, Files = ${MY_FILES}"

echo "hbcmp -n -w -es2 -go -I${INC_DIR} ${MY_FILES}"
      hbcmp -n -w -es2 -go -I${INC_DIR} ${MY_FILES}

echo "hblnk ${MY_GTLIB} *.o ${MY_LIBS} -o${MY_EXE}"
      hblnk ${MY_GTLIB} *.o ${MY_LIBS} -o${MY_EXE}

rm -f *.o 
