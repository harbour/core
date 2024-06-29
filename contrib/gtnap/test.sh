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
rm *.so
cp ../../../../hboffice/build/Release/bin/libofficesdk.so .
../../../../../bin/linux/gcc/hbmk2 exemplo.hbp
./exemplo --hb:gtnap
cd ..
cd ..
cd ..
