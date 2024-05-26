#!/bin/bash

#
# GTNAP build script
# build -b [Debug/Release]
#

#
# Input parameters
#
HBMK_PATH=../../bin/linux/gcc
BUILD=Release
CWD=$(pwd)

if [ "$(uname)" == "Darwin" ]; then
    # Do something under Mac OS X platform
    HBMK_PATH=../../bin/darwin/clang
    if [[ -z "${MACOSX_DEPLOYMENT_TARGET}" ]]; then
        echo "MACOSX_DEPLOYMENT_TARGET is not set. Please set this environment variable before build."
        exit 1
    fi


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
if [ "$(uname)" == "Darwin" ]; then
echo MACOSX_DEPLOYMENT_TARGET: ${MACOSX_DEPLOYMENT_TARGET}
fi
echo ---------------------------

#
# Build NAppGUI from sources
#
mkdir -p build
cd build
if [ "$(uname)" == "Darwin" ]; then
    cmake -G Xcode .. -DCMAKE_OSX_DEPLOYMENT_TARGET=${MACOSX_DEPLOYMENT_TARGET} || exit 1
    xcodebuild -configuration $BUILD || exit 1
else
    cmake  .. -DCMAKE_BUILD_TYPE=$BUILD || exit 1
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
