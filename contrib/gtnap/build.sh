#!/bin/sh

#
# GTNAP build script
# build -b [Debug/Release]
#

#
# Input parameters
#
HBMK_PATH=../../bin/linux/gcc
BUILD=Debug
CWD=$(pwd)

while [[ $# -gt 0 ]]; do
  case $1 in
    -b)
      BUILD="$2"
      shift
      shift
      ;;
    -*|--*)
      shift
      ;;
  esac
done

#
# Beginning
#
echo ---------------------------
echo Generating GTNAP
echo Main path: $CWD
echo Build type: $BUILD
echo ---------------------------

#
# Build NAppGUI from sources
#
rm -rf build
mkdir build
cd build
cmake  .. -DCMAKE_BUILD_CONFIG=$BUILD || exit 1
make -j 4 || exit 1

#
# Build GTNAP
#
cd $CWD
$HBMK_PATH/hbmk2 ./src/gtnap/gtnap.hbp || exit 1

echo ---------------------------
echo GTNAP build succeed
echo ---------------------------
