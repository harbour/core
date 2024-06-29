#!/bin/bash
#
# Testing HBOffice
#
CWD=$(pwd)
rm -rf build
./build.sh -dll -b Debug
./build.sh -lib -b Debug
./build.sh -dll -b Release
./build.sh -lib -b Release
cd tests
rf -rf result
rm -rf doc1 doc2 sheet1 sheet2

if [ "$(uname)" == "Darwin" ]; then
    rm *.dylib
    cp ../build/Release/bin/libofficesdk.dylib .
    ../../../bin/darwin/clang/hbmk2 sheet1.prg hboffice.hbc
    ../../../bin/darwin/clang/hbmk2 sheet2.prg hboffice.hbc
    ../../../bin/darwin/clang/hbmk2 doc1.prg hboffice.hbc
    ../../../bin/darwin/clang/hbmk2 doc2.prg hboffice.hbc
    export DYLD_LIBRARY_PATH=$DYLD_LIBRARY_PATH:$(pwd)
else
    rm *.so
    cp ../build/Release/bin/libofficesdk.so .
    ../../../bin/linux/gcc/hbmk2 sheet1.prg hboffice.hbc
    ../../../bin/linux/gcc/hbmk2 sheet2.prg hboffice.hbc
    ../../../bin/linux/gcc/hbmk2 doc1.prg hboffice.hbc
    ../../../bin/linux/gcc/hbmk2 doc2.prg hboffice.hbc
fi

./sheet1
./sheet2
./doc1
./doc2
cd ..
