# Harbour AWS wrapper (hbaws)

* [Introduction](#introduction)
* [Installation of AWS-SDK-C++](#installation-of-aws-sdk-c++)
    - [AWS-SDK with MinGW](#aws-sdk-with-mingw)
    - [AWS-SDK with Clang](#aws-sdk-with-clang)
    - [AWS-SDK with MSVC](#aws-sdk-with-msvc)
    - [AWS-SDK with GCC Linux](#aws-sdk-with-gcc-linux)
    - [AWS-SDK with CLANG Linux](#aws-sdk-with-clang-linux)
    - [AWS-SDK result](#aws-sdk-result)

* [Build HBAWS](#build-hbaws)
    - [Build HBAWS with MinGW](#build-hbaws-with-mingw)
    - [Build HBAWS with Clang](#build-hbaws-with-clang)
    - [Build HBAWS with MSVC](#build-hbaws-with-msvc)
    - [Build HBAWS with GCC Linux](#build-hbaws-with-gcc-linux)
    - [Build HBAWS with CLANG Linux](#build-hbaws-with-clang-linux)
* [HBAWS examples](#hbaws-examples)
* [Reference guide](#reference-guide)
    - [HBAWS_INIT](#hbaws_init)
    - [HBAWS_FINISH](#hbaws_finish)
    - [HBAWS_S3_LIST_ALL](#hbaws_s3_list_all)
    - [HBAWS_S3_LIST_PAGINATED](#hbaws_s3_list_paginated)
    - [HBAWS_S3_UPLOAD_SIMPLE](#hbaws_s3_upload_simple)
    - [HBAWS_S3_UPLOAD_MULTIPART](#hbaws_s3_upload_multipart)
    - [HBAWS_S3_COPY_SIMPLE](#hbaws_s3_copy_simple)
    - [HBAWS_S3_COPY_MULTIPART](#hbaws_s3_copy_multipart)
    - [HBAWS_S3_DOWNLOAD](#hbaws_s3_download)
    - [HBAWS_S3_DELETE](#hbaws_s3_delete)
    - [HBAWS_S3_RESTORE](#hbaws_s3_restore)

## Introduction

**hbaws** is a project that allows us to connect to Amazon Web Services (AWS) directly from Harbour. At the moment, only some features of the S3 service are accessible, but this may be expanded in the future. It provides high-level functions in C that hide the complexity of HTTP requests. This C API is easily portable to Harbour.

## Installation of AWS-SDK-C++

**hbaws** communicates with Amazon servers through the AWS-SDK-CPP, a project maintained by Amazon that encapsulates the complexity of making calls using the REST-API. The first step is to download the AWS-SDK code from GitHub and compile it. This task has been automated using two scripts: `awssdk.bat` (Windows) and `awssdk.sh` (Linux/macOS).

- Download and install Git if not present in your machine. Access to Git from the command line is required.
    ```
    :$ git --version
    git version 2.25.1
    ```

- Download and install CMake if not present in your machine. Access to CMake from the command line is required.
    ```
    :$ cmake --version
    cmake version 3.25.2
    ```

- In **Linux system** you have to install the development packages of **libcurl** and **libssl**. E.g: In a Debian-based system:
    ```
    sudo apt-get install libcurl-dev
    sudo apt-get install libssl-dev
    ```

- Set the `AWS_SDK_ROOT` environment variable. This path will be used to download and compile the code, as well as to install the headers and binaries once they are generated. E.g: `C:\aws-sdk` (Windows) or `/home/user/aws-sdk` (Linux).

### AWS-SDK with MinGW

```
cd contrib\hbaws
set AWS_SDK_ROOT=C:\aws-sdk
awssdk -b [Debug|Release] -comp mingw64
```

### AWS-SDK with Clang

```
cd contrib\hbaws
set AWS_SDK_ROOT=C:\aws-sdk
awssdk -b [Debug|Release] -comp clang
```

### AWS-SDK with MSVC

At least Visual Studio 2015 is required. First, you need to set the `CMAKE_GENERATOR` environment variable to the version you plan to use.

```
set CMAKE_GENERATOR=Visual Studio 17 2022
set CMAKE_GENERATOR=Visual Studio 16 2019
set CMAKE_GENERATOR=Visual Studio 15 2017
set CMAKE_GENERATOR=Visual Studio 14 2015
```
```
cd contrib\hbaws
set AWS_SDK_ROOT=C:\aws-sdk
awssdk -b [Debug|Release] -comp msvc64
```

### AWS-SDK with GCC Linux

```
AWS_SDK_ROOT=/home/user/aws-sdk
cd contrib/hbaws
./awssdk -comp gcc -b [Debug|Release]
```

### AWS-SDK with CLANG Linux

```
AWS_SDK_ROOT=/home/user/aws-sdk
cd contrib/hbaws
./awssdk -comp clang -b [Debug|Release]
```

### AWS-SDK result

If the `awssdk` script runs successfully, you will have the AWS-SDK headers and libraries in `$AWS_SDK_ROOT/$COMPILER/$BUILD`.

> **Important:** The first time you run the `awssdk` script, it can be take several minutes. AWS-SDK-CPP is a heavy source project (> 1.5Gb).

> **Important:** Several different builds can be done from same source code: E.g: `$AWS_SDK_ROOT/mingw64/Debug`, `$AWS_SDK_ROOT/msvc64/Release`, etc.

> **Important:** The AWS-SDK-CPP source code will be `$AWS_SDK_ROOT/src` and will not be deleted after compilation. Once the AWS-SDK is successfully generated, you can remove this folder.

> **Important:** The AWS-SDK dynamic libraries must be re-distributed with the Harbour-based executables. You will find them in the `/bin` folder of each installation. See the [HBAWS examples](#hbaws-examples) section.

**Windows redistributables DLLs** in `/bin` folder.

![awssdk_dlls](https://github.com/user-attachments/assets/697e5470-a848-412d-a31b-119633cc4e56)

**Linux redistributables .so** in `/lib` folder.

![linux_aws_so](https://github.com/user-attachments/assets/d8529946-c9a0-4ee1-b576-fa05a26c6514)

## Build HBAWS

To build the `hbaws` library, just run the `build.bat` or `build.sh` scripts.

### Build HBAWS with MinGW

```
cd contrib\hbaws
set AWS_SDK_ROOT=C:\aws-sdk
build -b [Debug|Release] -comp mingw64
```

The `libhbaws.a` will be generated in `hbaws\build\[Debug|Release]\lib`.

### Build HBAWS with Clang

```
cd contrib\hbaws
set AWS_SDK_ROOT=C:\aws-sdk
build -b [Debug|Release] -comp clang
```

The `libhbaws.a` will be generated in `hbaws\build\[Debug|Release]\lib`.

### Build HBAWS with MSVC

The HBMK2 system needs access to MSVC compilers (cl). To do this, you need to run the corresponding script according to the version of VisualStudio installed. For example, for VS 2017.

```
:: Set Visual Studio 2017 64bit compiler for hbmk2 (msvc)
"%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Community\VC\Auxiliary\Build\vcvarsall.bat" x64
```
```
cd contrib\hbaws
set AWS_SDK_ROOT=C:\aws-sdk
build -b [Debug|Release] -comp msvc64
```

The `hbaws.lib` will be generated in `hbaws\build\[Debug|Release]\lib`.

### Build HBAWS with GCC Linux

```
cd contrib/hbaws
export AWS_SDK_ROOT=/home/user/aws-sdk
build -comp gcc -b [Debug|Release]
```

The `libhbaws.a` will be generated in `hbaws/build/[Debug|Release]/lib`.

### Build HBAWS with CLANG Linux

```
cd contrib/hbaws
export AWS_SDK_ROOT=/home/user/aws-sdk
build -comp clang -b [Debug|Release]
```

The `libhbaws.a` will be generated in `hbaws/build/[Debug|Release]/lib`.

## HBAWS examples

Some examples have been provided in `contrib/hbaws/tests/harbour`.

> **Important:** Before running examples, open the `credentials.prg` and fill the required data.

> **Important:** AWS-SDK DLLs must be accesible by the executables.

    # Windows MinGW
    set PATH=%AWS_SDK_ROOT%\mingw64\Release\bin;%PATH%

    # Windows MSVC
    set PATH=%AWS_SDK_ROOT%\msvc64\Release\bin;%PATH%

    # Linux GCC
    export LD_LIBRARY_PATH=$AWS_SDK_ROOT/gcc/Release/lib:$LD_LIBRARY_PATH

* `listall`: Use of `HBAWS_S3_LIST_ALL` function.
    ```
    cd contrib\hbaws\tests\harbour
    ..\..\..\..\bin\win\mingw64\hbmk2 listall.prg credentials.prg hbaws.hbc -comp=mingw64
    ..\..\..\..\bin\win\msvc64\hbmk2 listall.prg credentials.prg hbaws.hbc -comp=msvc64
    ../../../../bin/linux/gcc/hbmk2 listall.prg credentials.prg hbaws.hbc
    ```

* `listpage`: Use of `HBAWS_S3_LIST_PAGINATED` function.
    ```
    cd contrib\hbaws\tests\harbour
    ..\..\..\..\bin\win\mingw64\hbmk2 listpage.prg credentials.prg hbaws.hbc -comp=mingw64
    ..\..\..\..\bin\win\msvc64\hbmk2 listpage.prg credentials.prg hbaws.hbc -comp=msvc64
    ../../../../bin/linux/gcc/hbmk2 listpage.prg credentials.prg hbaws.hbc
    ```

* `upload`: Use of `HBAWS_S3_UPLOAD_SIMPLE` function.
    ```
    cd contrib\hbaws\tests\harbour
    ..\..\..\..\bin\win\mingw64\hbmk2 upload.prg credentials.prg hbaws.hbc -comp=mingw64
    ..\..\..\..\bin\win\msvc64\hbmk2 upload.prg credentials.prg hbaws.hbc -comp=msvc64
    ../../../../bin/linux/gcc/hbmk2 upload.prg credentials.prg hbaws.hbc
    ```

* `uploadm`: Use of `HBAWS_S3_UPLOAD_MULTIPART` function.
    ```
    cd contrib\hbaws\tests\harbour
    ..\..\..\..\bin\win\mingw64\hbmk2 uploadm.prg credentials.prg hbaws.hbc -comp=mingw64
    ..\..\..\..\bin\win\msvc64\hbmk2 uploadm.prg credentials.prg hbaws.hbc -comp=msvc64
    ../../../../bin/linux/gcc/hbmk2 uploadm.prg credentials.prg hbaws.hbc
    ```

* `copy`: Use of `HBAWS_S3_COPY_SIMPLE` function.
    ```
    cd contrib\hbaws\tests\harbour
    ..\..\..\..\bin\win\mingw64\hbmk2 copy.prg credentials.prg hbaws.hbc -comp=mingw64
    ..\..\..\..\bin\win\msvc64\hbmk2 copy.prg credentials.prg hbaws.hbc -comp=msvc64
    ../../../../bin/linux/gcc/hbmk2 copy.prg credentials.prg hbaws.hbc
    ```

* `copym`: Use of `HBAWS_S3_COPY_MULTIPART` function.
    ```
    cd contrib\hbaws\tests\harbour
    ..\..\..\..\bin\win\mingw64\hbmk2 copym.prg credentials.prg hbaws.hbc -comp=mingw64
    ..\..\..\..\bin\win\msvc64\hbmk2 copym.prg credentials.prg hbaws.hbc -comp=msvc64
    ../../../../bin/linux/gcc/hbmk2 copym.prg credentials.prg hbaws.hbc
    ```

* `download`: Use of `HBAWS_S3_DOWNLOAD` function.
    ```
    cd contrib\hbaws\tests\harbour
    ..\..\..\..\bin\win\mingw64\hbmk2 download.prg credentials.prg hbaws.hbc -comp=mingw64
    ..\..\..\..\bin\win\msvc64\hbmk2 download.prg credentials.prg hbaws.hbc -comp=msvc64
    ../../../../bin/linux/gcc/hbmk2 download.prg credentials.prg hbaws.hbc
    ```

* `delete`: Use of `HBAWS_S3_DELETE` function.
    ```
    cd contrib\hbaws\tests\harbour
    ..\..\..\..\bin\win\mingw64\hbmk2 delete.prg credentials.prg hbaws.hbc -comp=mingw64
    ..\..\..\..\bin\win\msvc64\hbmk2 delete.prg credentials.prg hbaws.hbc -comp=msvc64
    ../../../../bin/linux/gcc/hbmk2 delete.prg credentials.prg hbaws.hbc
    ```

* `restore`: Use of `HBAWS_S3_RESTORE` function.
    ```
    cd contrib\hbaws\tests\harbour
    ..\..\..\..\bin\win\mingw64\hbmk2 restore.prg credentials.prg hbaws.hbc -comp=mingw64
    ..\..\..\..\bin\win\msvc64\hbmk2 restore.prg credentials.prg hbaws.hbc -comp=msvc64
    ../../../../bin/linux/gcc/hbmk2 restore.prg credentials.prg hbaws.hbc
    ```

## Reference guide

### HBAWS_INIT

This function must be call before any other. It remember the session. Only one call is required.

```
LOCAL L_OK := HBAWS_INIT(@C_ERR, C_AccessKey, C_Secret)

PAR1: Reference string to store the error message (if any).
PAR2: Access key.
PAR3: Secret.
RET: .T. if connection with AWS can be performed. If .F., in C_ERR will be stored the error.
```

### HBAWS_FINISH

This function must be called before the program terminates or calls to AWS are no longer needed.

```
HBAWS_FINISH()

No parameters neither return value.
```

### HBAWS_S3_LIST_ALL

Return a list of ALL files in the bucket, breaking the AWS 1000 items limit.

```
LOCAL V_OBJS := HBAWS_S3_LIST_ALL(@C_ERR, C_BUCKET, C_PREFIX)

PAR1: Reference string to store the error message (if any).
PAR2: Bucket where the list will be performed.
PAR3: Filter prefix (optional, allow NIL or empty string).
RET: A vector of vectors with all objects. { {Obj1}, {Obj2}, ..., {ObjN} }.

* If error, will return an empty vector {} and C_ERR will contain the error message.

* Every inner {Obj} vector has this info:
  - S3Key: Obj[OBJ_S3KEY]
  - ContentSize: hb_ntos(Obj[OBJ_CONTENT_SIZE])
  - ContentType: Obj[OBJ_CONTENT_TYPE]
  - Date: DToC(Obj[OBJ_DATE])
  - Time: Obj[OBJ_TIME]
  - TimeZone: Obj[OBJ_TIMEZONE]
  - StorageClass: Obj[OBJ_STORAGE_CLASS]
  - IsRestore: hb_ValToStr(Obj[OBJ_IS_RESTORE])
  - RestoreDate: DToC(Obj[OBJ_RESTORE_DATE])
  - RestoreTime: Obj[OBJ_RESTORE_TIME]
  - RestoreTimeZone: Obj[OBJ_RESTORE_TIMEZONE]
  - ChecksumAlgorithm: Obj[OBJ_CHECKSUM_ALGORITHM]
  - ETag: Obj[OBJ_ETAG]
```

### HBAWS_S3_LIST_PAGINATED

Return a single page of the list files in the bucket, returning a continuation token for a possible next call.

```
LOCAL V_OBJS := HBAWS_S3_LIST_PAGINATED(@C_ERR, C_BUCKET, C_PREFIX, C_START_AFTER, N_MAX_KEYS, C_CONTINUATION_TOKEN, @C_NEXT_CONTINUATION_TOKEN)

PAR1: Reference string to store the error message (if any).
PAR2: Bucket where the list will be performed.
PAR3: Filter prefix (optional, allow NIL or empty string).
PAR4: Object key for begin the search (optional, allow NIL or empty string).
PAR5: Maximum number of objects returned. If >1000, the page will be limited to 1000.
PAR6: Previous continuation token. If NIL of empty the search will start by the beginning.
PAR6: Reference string to store the next continuation token for possible next call. If no more pages, NIL will be stored.
RET: A vector of vectors with all objects in the page. { {Obj1}, {Obj2}, ..., {ObjN} }.

* If error, will return an empty vector {} and C_ERR will contain the error message.
* Every inner {Obj} vector has the same info as HBAWS_S3_LIST_ALL.
```

### HBAWS_S3_UPLOAD_SIMPLE

Upload a file to AWS-S3 bucket in a single request. Use this function for relative small sized files.

```
LOCAL L_OK := HBAWS_S3_UPLOAD_SIMPLE(@C_ERR, C_BUCKET, C_LOCAL_FILE, C_KEY, C_CONTENT_TYPE, N_STORAGE)

PAR1: Reference string to store the error message (if any).
PAR2: Bucket name to upload.
PAR3: The local path with the file to be uploaded.
PAR4: The key of the uploaded file in AWS-S3.
PAR5: The mime content-type. E.g: "image/svg+xml"
PAR6: Storage class numeric value.
RET: .T. if upload is success or .F. if error.

The Storage class values are defined in 'hbaws.ch'
#define STORAGE_STANDARD 1
#define STORAGE_REDUCED_REDUNDANCY 2
#define STORAGE_STANDARD_IA 3
#define STORAGE_ONEZONE_IA 4
#define STORAGE_INTELLIGENT_TIERING 5
#define STORAGE_GLACIER 6
#define STORAGE_DEEP_ARCHIVE 7
#define STORAGE_OUTPOSTS 8
#define STORAGE_GLACIER_IR 9
#define STORAGE_SNOW 10
#define STORAGE_EXPRESS_ONEZONE 11
```

### HBAWS_S3_UPLOAD_MULTIPART

Upload a file to AWS-S3 bucket using a multipart request model. Use this function for really big files.

```
LOCAL L_OK := HBAWS_S3_UPLOAD_MULTIPART(@C_ERR, C_BUCKET, C_LOCAL_FILE, C_KEY, C_CONTENT_TYPE, N_STORAGE, N_CHUNK_SIZE, N_REQUESTS)

PAR1: Reference string to store the error message (if any).
PAR2: Bucket name to upload.
PAR3: The local path with the file to be uploaded.
PAR4: The key of the uploaded file in AWS-S3.
PAR5: The mime content-type. E.g: "image/svg+xml"
PAR6: Storage class numeric value.
PAR7: Amount of BYTES to upload in each single request. AWS establish, at least, 5Mb (5 * 1024 * 1024). If this parameter is smaller, will be omitted and 5Mb will be set.
PAR8: Number of retries if any single part upload fails.
RET: .T. if upload is success or .F. if error.

IMPORTANT: If upload fail, 'AbortMultipartUpload()' will be called to clean any part of file in AWS-S3 caches.
```

### HBAWS_S3_COPY_SIMPLE

Copy a file from a AWS-S3 bucket to another bucket. Use user registered in `HBAWS_INIT()` should have read permissions for `source` file and write permissions for `destiny` bucket.

```
LOCAL L_OK := HBAWS_S3_COPY_SIMPLE(@C_ERR, C_SRC_BUCKET, C_SRC_KEY, C_DEST_BUCKET, C_DEST_KEY, C_DEST_CONTENT_TYPE, N_DEST_STORAGE)

PAR1: Reference string to store the error message (if any).
PAR2: Source bucket name.
PAR3: Source key.
PAR4: Destiny bucket name.
PAR5: Destiny key.
PAR6: Content type for destiny file.
PAR7: Storage class for destiny file.
RET: .T. if copy is success or .F. if error.
```

### HBAWS_S3_COPY_MULTIPART

Copy a file from a AWS-S3 bucket to another bucket using a multipart request model. Use user registered in `HBAWS_INIT()` should have read permissions for `source` file and write permissions for `destiny` bucket.

```
LOCAL L_OK := HBAWS_S3_COPY_MULTIPART(@C_ERR, C_SRC_BUCKET, C_SRC_KEY, C_DEST_BUCKET, C_DEST_KEY, C_DEST_CONTENT_TYPE, N_DEST_STORAGE, N_CHUNK_SIZE, N_REQUESTS)

PAR1: Reference string to store the error message (if any).
PAR2: Source bucket name.
PAR3: Source key.
PAR4: Destiny bucket name.
PAR5: Destiny key.
PAR6: Content type for destiny file.
PAR7: Storage class for destiny file.
PAR8: Amount of BYTES to upload in each single request. AWS establish, at least, 5Mb (5 * 1024 * 1024). If this parameter is smaller, will be omitted and 5Mb will be set.
PAR9: Number of retries if any single part upload fails.
RET: .T. if upload is success or .F. if error.

IMPORTANT: If copy fail, 'AbortMultipartUpload()' will be called to clean any part of file in AWS-S3 caches.
```

### HBAWS_S3_DOWNLOAD

Download a file from a AWS-S3 bucket.

```
LOCAL L_OK := HBAWS_S3_DOWNLOAD(@C_ERR, C_BUCKET, C_KEY, C_LOCAL_FILE)

PAR1: Reference string to store the error message (if any).
PAR2: Bucket to download from.
PAR3: The key of the file to download.
PAR4: The local path where the file will be saved.
RET: .T. if download is success or .F. if error.
```

### HBAWS_S3_DELETE

Delete a file from a AWS-S3 bucket.

```
LOCAL L_OK := HBAWS_S3_DELETE(@C_ERR, C_BUCKET, C_KEY)

PAR1: Reference string to store the error message (if any).
PAR2: Bucket to delete from.
PAR3: The key of the file to delete.
RET: .T. if delete is success or .F. if error.
```

IMPORTANT: If the file doesn't exists in the bucket, will return .T.

### HBAWS_S3_RESTORE

Restore a file from a AWS-S3 bucket.

```
LOCAL L_OK := HBAWS_S3_RESTORE(@C_ERR, C_BUCKET, C_KEY, N_NUM_DAYS, N_TIER)

PAR1: Reference string to store the error message (if any).
PAR2: Bucket to restore from.
PAR3: The key of the file to restore.
PAR4: Number of days to restore.
PAR5: Tier.
RET: .T. if restore is success or .F. if error.
```

The Tier values are defined in 'hbaws.ch'
#define TIER_STANDARD 1
#define TIER_BULK 2
#define TIER_EXPEDITED 3

# Legacy DOCU (To be removed)

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

### Some errors
- The AWS Access Key Id you provided does not exist in our records.
- The request signature we calculated does not match the signature you provided. Check your key and signing method.

