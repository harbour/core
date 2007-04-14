#!/bin/sh

INC_DIR="-I../../include:/usr/include"
MY_EXE=$1
shift
MY_FILES="$*"
MY_LIBS="-L../../lib -lgdlib -L/usr/lib -lgd -lpng -lz -ljpeg -lfreetype -lm -lct"
MY_GTLIB="-gtstd"

if [ -z ${MY_FILES} ]; then
   MY_FILES=${MY_EXE};
fi

echo "Exe = ${MY_EXE}, Files = ${MY_FILES}"

echo "xhbcmp -n -w -es2 -go ${INC_DIR} ${MY_FILES}"
xhbcmp -n -w -es2 -go -I${INC_DIR} ${MY_FILES}

echo "xhblnk ${MY_GTLIB} *.o ${MY_LIBS} -o${MY_EXE}"
xhblnk ${MY_GTLIB} *.o ${MY_LIBS} -o${MY_EXE}

rm -f *.o 
