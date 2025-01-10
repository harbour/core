#!/bin/bash

#
# Testing GTNAP compiled with MinGW
#

CWD=$(pwd)
rm -rf build
./build.sh -b Debug
./build.sh -b Release

cd tests/cuademo/gtnap_cualib
rm exemplo

if [ "$(uname)" == "Darwin" ]; then
    rm *.dylib
    cp ../../../../hboffice/build/Release/bin/libofficesdk.dylib .
    ../../../../../bin/darwin/clang/hbmk2 exemplo.hbp
    export DYLD_LIBRARY_PATH=$DYLD_LIBRARY_PATH:$(pwd)
else
    rm *.so
    cp ../../../../hboffice/build/Release/bin/libofficesdk.so .
    ../../../../../bin/linux/gcc/hbmk2 exemplo.hbp
    export LIBREOFFICE_HOME=/usr/lib/libreoffice
    export LD_LIBRARY_PATH=.:/usr/lib/libreoffice/program
fi

./exemplo --hb:gtnap
cd ..
cd ..
cd ..
