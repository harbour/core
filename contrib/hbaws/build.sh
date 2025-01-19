#!/bin/bash

#
# HBAWS build script
#
# Will generate the hbaws.a with the Harbour AWS wrapper.
# build -comp [gcc|clang] -b [Debug|Release]
#

COMPILER=gcc
PLATFORM=linux
BUILD=Release
CWD=$(pwd)

if [ "$(uname)" == "Darwin" ]; then
    # Do something under Mac OS X platform
    if [[ -z "${MACOSX_DEPLOYMENT_TARGET}" ]]; then
        echo "MACOSX_DEPLOYMENT_TARGET is not set. Please set this environment variable before build."
        exit 1
    fi
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
echo Generating HBAWS
echo Main path: $CWD
echo Build type: $BUILD
echo COMPILER: $COMPILER
echo ---------------------------

HBMK_PATH=../../bin/$PLATFORM/$COMPILER
HBMK_FLAGS=

if [ $BUILD == "Debug" ]; then
    HBMK_FLAGS=-debug
fi

$HBMK_PATH/hbmk2 $HBMK_FLAGS -comp=$COMPILER $CWD/hbaws.hbp || exit 1

echo ------------------------
echo HBAWS LIB build succeed
echo ------------------------

cd $CWD
