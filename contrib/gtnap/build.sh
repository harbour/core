#!/bin/bash

#
# GTNAP build script
# build -comp [gcc|clang] -b [Debug|Release]
#

#
# Input parameters
#
COMPILER=gcc
PLATFORM=linux
BUILD=Release
CWD=$(pwd)

if [ "$(uname)" == "Darwin" ]; then
    # Do something under Mac OS X platform
    PLATFORM=darwin
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
    -comp)
      COMPILER="$2"
      shift
      shift
      ;;
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
echo Compiler: $COMPILER
echo Build type: $BUILD
echo Platform: $PLATFORM
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
    if [ "$COMPILER" == "gcc" ]; then
        cmake .. -DCMAKE_C_COMPILER=gcc -DCMAKE_CXX_COMPILER=g++ -DCMAKE_BUILD_TYPE=$BUILD || exit 1
    fi

    if [ "$COMPILER" == "clang" ]; then
        cmake .. -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++ -DCMAKE_BUILD_TYPE=$BUILD || exit 1
    fi

    make -j 4 || exit 1
fi

#
# Build GTNAP
#
cd $CWD
HBMK_FLAGS=

if [ $BUILD == "Debug" ]; then
    HBMK_FLAGS=-debug
fi

../../bin/$PLATFORM/$COMPILER/hbmk2 -comp=$COMPILER $HBMK_FLAGS ./src/gtnap/gtnap.hbp || exit 1

echo ---------------------------
echo GTNAP build succeed
echo ---------------------------
