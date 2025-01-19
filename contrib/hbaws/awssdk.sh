#!/bin/bash
#
# AWS-SDK build script
#
# Will download and build the AWS-SDK-CPP libraries.
# This is a prerequisite before build HBAWS and applications that depend on it.
#
# awssdk -comp [gcc|clang] -b [Debug|Release]
#

COMPILER=gcc
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
echo Generating AWS-SDK
echo AWS_SDK_ROOT: $AWS_SDK_ROOT
echo Compiler: $COMPILER
echo Build type: $BUILD
if [ "$(uname)" == "Darwin" ]; then
echo MACOSX_DEPLOYMENT_TARGET: ${MACOSX_DEPLOYMENT_TARGET}
fi
echo ---------------------------

# Check 'AWS_SDK_ROOT'
if [[ -z "${AWS_SDK_ROOT}" ]]; then
    echo 'AWS_SDK_ROOT' is empty.
    exit 1
fi

# clone AWS-SDK repo
if [ ! -d "$AWS_SDK_ROOT/src" ]; then
    git clone --recurse-submodules --depth 1 --branch 1.11.271 https://github.com/aws/aws-sdk-cpp $AWS_SDK_ROOT/src
fi

# Check AWS repo
cd $AWS_SDK_ROOT/src
git status > /dev/null
if [ $? -ne 0 ]; then
    echo $AWS_SDK_ROOT/src is not a git repository. Please remove this folder and try again.
    cd $CWD
    exit 1
fi

# Clean previous build and install folder
if [ -d "$AWS_SDK_ROOT/build" ]; then
    rm -rf $AWS_SDK_ROOT/build
fi

if [ -d "$AWS_SDK_ROOT/$COMPILER/$BUILD" ]; then
    rm -rf $AWS_SDK_ROOT/$COMPILER/$BUILD
fi

# CMake configure process
if [ "$COMPILER" == "gcc" ]; then
    cmake -S $AWS_SDK_ROOT/src -B $AWS_SDK_ROOT/build -DCMAKE_INSTALL_PREFIX=$AWS_SDK_ROOT/$COMPILER/$BUILD -DCMAKE_BUILD_TYPE=$BUILD -DCMAKE_C_COMPILER=gcc -DCMAKE_CXX_COMPILER=g++ -DBUILD_ONLY="s3" -DENABLE_TESTING=OFF -DENABLE_ZLIB_REQUEST_COMPRESSION=OFF -DAWS_SDK_WARNINGS_ARE_ERRORS=OFF -DBUILD_SHARED_LIBS=ON
fi

if [ "$COMPILER" == "clang" ]; then
    cmake -S $AWS_SDK_ROOT/src -B $AWS_SDK_ROOT/build -DCMAKE_INSTALL_PREFIX=$AWS_SDK_ROOT/$COMPILER/$BUILD -DCMAKE_BUILD_TYPE=$BUILD -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++ -DBUILD_ONLY="s3" -DENABLE_TESTING=OFF -DENABLE_ZLIB_REQUEST_COMPRESSION=OFF -DAWS_SDK_WARNINGS_ARE_ERRORS=OFF -DBUILD_SHARED_LIBS=ON
fi

if [ $? -ne 0 ]; then
    echo Error in AWS-SDK CMake configure process
    cd $CWD
    exit 1
fi

# Build AWS-SDK
cmake --build $AWS_SDK_ROOT/build
if [ $? -ne 0 ]; then
    echo Error in AWS-SDK build process
    cd $CWD
    exit 1
fi

# Install AWS-SDK
cmake --install $AWS_SDK_ROOT/build --config $BUILD
if [ $? -ne 0 ]; then
    echo Error in AWS-SDK install process
    cd $CWD
    exit 1
fi

# Successfully exit
echo AWS build and install process successfully
rm -rf $AWS_SDK_ROOT/build
exit 0

