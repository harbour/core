# Harbour AWS wrapper (hbaws)

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

