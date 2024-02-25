#!/bin/sh

#
# GTNAP build script
# build -b [Debug/Release] [-noliboff]
# [-noliboff] Optional flag to disable the LibreOffice support
#

#
# Input parameters
#
HBMK_PATH=../../bin/linux/gcc
BUILD=Debug
LIBREOFFICE=ON
CWD=$(pwd)

if [ "$(uname)" == "Darwin" ]; then
    # Do something under Mac OS X platform
    HBMK_PATH=../../bin/darwin/clang
# elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
#     # Do something under GNU/Linux platform
# elif [ "$(expr substr $(uname -s) 1 10)" == "MINGW32_NT" ]; then
#     # Do something under 32 bits Windows NT platform
# elif [ "$(expr substr $(uname -s) 1 10)" == "MINGW64_NT" ]; then
#     # Do something under 64 bits Windows NT platform
fi

while [[ $# -gt 0 ]]; do
  case $1 in
    -b)
      BUILD="$2"
      shift
      shift
      ;;
    -noliboff)
      LIBREOFFICE=OFF
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
echo HBMK path: $HBMK_PATH
echo LIBREOFFICE: $LIBREOFFICE
echo ---------------------------

#
# Build NAppGUI from sources
#
mkdir -p build
cd build
if [ "$(uname)" == "Darwin" ]; then
    cmake -G Xcode .. -DCMAKE_OSX_DEPLOYMENT_TARGET=10.13 -DGTNAP_LIBREOFFICE=$LIBREOFFICE || exit 1
    xcodebuild -configuration $BUILD || exit 1
else
    cmake  .. -DCMAKE_BUILD_TYPE=$BUILD -DGTNAP_LIBREOFFICE=$LIBREOFFICE || exit 1
    make -j 4 || exit 1
fi

#
# Build GTNAP
#
cd $CWD

if [ $BUILD == "Debug" ]; then
    $HBMK_PATH/hbmk2 -debug ./src/gtnap/gtnap.hbp || exit 1
else
    $HBMK_PATH/hbmk2 ./src/gtnap/gtnap.hbp || exit 1
fi

echo ---------------------------
echo GTNAP build succeed
echo ---------------------------
