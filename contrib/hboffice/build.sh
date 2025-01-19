#!/bin/bash

#
# HBOffice build script
#
# Will generate the libhboffice.so with the LibreOffice C-Wrapper.
# build -dll -comp [gcc|clang] -b [Debug|Release]
#
# Will generate the libhboffice.a with the Harbour wrapper and runtime dll loader.
# build -lib -comp [gcc|clang] -b [Debug|Release]

#
# Input parameters
#
OPERATION=dll
PLATFORM=linux
COMPILER=gcc
BUILD=Release
CWD=$(pwd)

if [ "$(uname)" == "Darwin" ]; then
    PLATFORM=darwin
    COMPILER=clang
fi

while [[ $# -gt 0 ]]; do
  case $1 in
    -comp)
      COMPILER="$2"
      shift
      shift
      ;;
    -dll)
      OPERATION=dll
      shift
      ;;
    -lib)
      OPERATION=lib
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
echo Generating LibreOffice
echo Main path: $CWD
echo Build type: $BUILD
echo PLATFORM: $PLATFORM
echo COMPILER: $COMPILER
echo OPERATION: $OPERATION
echo ---------------------------

#
# Generate dynamic library
#
if [ $OPERATION == "dll" ]; then
    if [ "$(uname)" == "Darwin" ]; then
        cmake -G Xcode -S . -B build || exit 1
        cmake --build build --config $BUILD || exit 1
        cd build
    else
        mkdir -p build
        cd build
        if [ "$COMPILER" == "gcc" ]; then
            cmake .. -DCMAKE_C_COMPILER=gcc -DCMAKE_CXX_COMPILER=g++ -DCMAKE_BUILD_TYPE=$BUILD || exit 1
        fi

        if [ "$COMPILER" == "clang" ]; then
            cmake .. -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang -DCMAKE_BUILD_TYPE=$BUILD || exit 1
        fi

        make -j 4 || exit 1
    fi

    rm $BUILD/lib/*core*
    rm $BUILD/lib/*osbs*
    rm $BUILD/lib/*sewer*
    echo ---------------------------
    echo OFFICESDK DLL build succeed
    echo ---------------------------

#
# Generate static library
#
elif [ $OPERATION == "lib" ]; then
    HBMK_PATH=../../bin/$PLATFORM/$COMPILER
    HBMK_FLAGS=

    if [ $BUILD == "Debug" ]; then
        HBMK_FLAGS=-debug
    fi

    $HBMK_PATH/hbmk2 $HBMK_FLAGS -comp=$COMPILER $CWD/src/hboffice/hboffice.hbp || exit 1
    echo ---------------------------
    echo HBOFFICE LIB build succeed
    echo ---------------------------

else
    echo Invalid operation $OPERATION
    exit 1

fi

cd $CWD
