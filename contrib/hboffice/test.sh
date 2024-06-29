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
rm *.so
cp ../build/Release/bin/libofficesdk.so .
../../../bin/linux/gcc/hbmk2 sheet1.prg hboffice.hbc
../../../bin/linux/gcc/hbmk2 sheet2.prg hboffice.hbc
../../../bin/linux/gcc/hbmk2 doc1.prg hboffice.hbc
../../../bin/linux/gcc/hbmk2 doc2.prg hboffice.hbc
./sheet1
./sheet2
./doc1
./doc2
cd ..
