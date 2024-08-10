# Harbour AWS wrapper (hbaws)

## Introduction

**hbaws** is a project that allows us to connect to Amazon Web Services (AWS) directly from Harbour. At the moment, only some features of the S3 service are accessible, but this may be expanded in the future. It provides high-level functions in C that hide the complexity of HTTP requests. This C API is easily portable to Harbour.

## Installation of AWS-SDK-C++

**hbaws** communicates with Amazon servers through the AWS-SDK-CPP, a project maintained by Amazon that encapsulates the complexity of making calls by directly attacking the REST-API. The first step is to download the AWS-SDK code from GitHub and compile it. This task has been automated using two scripts: `awssdk.bat`(Windows) and `awssdk.sh` (Linux/macOS).

- Download and install Git if not present in your development machine. Access to Git from the command line is required.
    ```
    :$ git --version
    git version 2.25.1
    ```

- Download and install CMake if not present in your development machine. Access to CMake from the command line is required.
    ```
    :$ cmake --version
    cmake version 3.25.2
    ```

- In **Linux system** you have to install the development packages of **libcurl** and **libssl**. E.g: In a Debian-based system:
    ```
    sudo apt-get install libcurl-dev
    sudo apt-get install libssl-dev
    ```

- Set the `AWS_SDK_ROOT` environment variable. This path will be used to download and compile the code, as well as to install the headers and binaries once they are generated. E.g: `C:\aws-sdk` (Windows) or `/home/fran/aws-sdk` (Linux).

- Windows:
    ```
    cd contrib\hbaws
    awssdk -b [Debug|Release] -comp [mingw64|msvc64]
    ```

- Linux:
    ```
    cd contrib/hbaws
    awssdk -b [Debug|Release]
    ```

- > **Important:** The first time you run the script, it can be take several minutes. AWS-SDK-CPP is a heavy source project (> 1.5Gb).

- > **Important:** Several different builds can be done from same source code: E.g: `$AWS_SDK_ROOT/mingw64/Debug`, `$AWS_SDK_ROOT/msvc64/Release`, etc.

- > **Important:** The AWS-SDK-CPP source code will be `$AWS_SDK_ROOT/src` and will be keep after compilation. Once the AWS-SDK is successfully generated, you can delete this folder.

- If the `awssdk` script runs successfully, you will have the AWS-SDK headers and libraries in `$AWS_SDK_ROOT/$COMPILER/$BUILD`.

    ![aws-sdk-builds](https://github.com/user-attachments/assets/7dc7dce0-7d34-4095-b137-b372cc941ff7)

## Build HBAWS


"%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Community\VC\Auxiliary\Build\vcvarsall.bat" x64


### Some errors
- The AWS Access Key Id you provided does not exist in our records.
- The request signature we calculated does not match the signature you provided. Check your key and signing method.



# Legacy DOCU

## Download AWS-SDK repo

git clone --recurse-submodules https://github.com/aws/aws-sdk-cpp

## Build AWS-SDK with VisualStudio

cmake -G "Visual Studio 16 2019" -S . -B build -DCMAKE_INSTALL_PREFIX="C:\Users\Fran\Desktop\aws-install" -DBUILD_ONLY="s3" -DENABLE_TESTING=OFF -DENABLE_ZLIB_REQUEST_COMPRESSION=OFF -DAWS_SDK_WARNINGS_ARE_ERRORS=OFF -A x64

cmake -G "Visual Studio 15 2017" -S . -B build -DCMAKE_INSTALL_PREFIX="C:\Users\Fran\Desktop\aws-install" -DBUILD_ONLY="s3" -DENABLE_TESTING=OFF -DENABLE_ZLIB_REQUEST_COMPRESSION=OFF -DAWS_SDK_WARNINGS_ARE_ERRORS=OFF -A x64

cmake -G "Visual Studio 14 2015" -S . -B build -DCMAKE_INSTALL_PREFIX="C:\Users\Fran\Desktop\aws-install" -DBUILD_ONLY="s3" -DENABLE_TESTING=OFF -DENABLE_ZLIB_REQUEST_COMPRESSION=OFF -DAWS_SDK_WARNINGS_ARE_ERRORS=OFF -A x64

cmake --build build --config=Release

cmake --install build --config=Release

## Build AWS-SDK with MinGW

**Compilation fails with MinGW**

cmake -G "MinGW Makefiles" -S . -B ../build -DCMAKE_C_COMPILER=gcc -DCMAKE_CXX_COMPILER=g++ -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX="C:\aws-sdk" -DBUILD_ONLY="s3" -DENABLE_ZLIB_REQUEST_COMPRESSION=OFF -DENABLE_TESTING=OFF -DAWS_SDK_WARNINGS_ARE_ERRORS=OFF -DBUILD_SHARED_LIBS=OFF

cmake --build ../build

cmake --install ../build --config Release


  MSYS2_ARG_CONV_EXCL="-DCMAKE_INSTALL_PREFIX=" \
    ${MINGW_PREFIX}/bin/cmake.exe \
      -G "Ninja" \
      "${_extra_config[@]}" \
      -DBUILD_SHARED_LIBS=OFF \
      -DCMAKE_INSTALL_PREFIX="${MINGW_PREFIX}" \
      -DCMAKE_PREFIX_PATH="${MINGW_PREFIX}" \
      -DCMAKE_MODULE_PATH="${MINGW_PREFIX}"/lib/cmake \
      -DAWS_SDK_WARNINGS_ARE_ERRORS=OFF \
      -DBUILD_ONLY="config;identity-management;lambda;s3;sts;transfer" \
      -DENABLE_UNITY_BUILD=ON \
      -DENABLE_TESTING=OFF \
      -DBUILD_DEPS=OFF \
      -DPYTHON_EXECUTABLE=${MINGW_PREFIX}/bin/python \
      ../${_realname}-${pkgver}






## Build AWS-SDK with GCC Linux

**libcurl is required**

sudo apt-get install libcurl-dev
sudo apt-get install libssl-dev

cmake -S . -B build -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX="/home/fran/Desktop/aws-install" -DBUILD_ONLY="s3" -DENABLE_ZLIB_REQUEST_COMPRESSION=OFF -DENABLE_TESTING=OFF -DAWS_SDK_WARNINGS_ARE_ERRORS=OFF

cmake --build build

cmake --install build --config Release

## AWS hello with Visual Studio

cmake -G "Visual Studio 15 2017" -S . -B build -A x64

## AWS hello with MinGW

cmake -G "MinGW Makefiles" -S . -B build -DCMAKE_C_COMPILER=gcc -DCMAKE_CXX_COMPILER=g++ -DCMAKE_BUILD_TYPE=Release

## AWS hello with GCC Linux

cmake -S . -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build

