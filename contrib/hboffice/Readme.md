# Harbour LibreOffice-SDK wrapper (hboffice)

* [Introduction](#introduction)
* [Installation of LibreOffice](#installation-of-libreoffice)
    - [Installation in Windows](#installation-in-windows)
    - [Installation in Linux](#installation-in-linux)
    - [Installation in macOS](#installation-in-macos)
    - [About LibreOffice-SDK](#about-libreoffice-sdk)
* [Installation of CMake](#installation-of-cmake)
* [Build hboffice](#build-hboffice)
    - [Build hboffice in Windows](#build-hboffice-in-windows)
    - [More about build hboffice in Windows](#more-about-build-hboffice-in-windows)
    - [Build hboffice in Linux](#build-hboffice-in-linux)
    - [Build hboffice in macOS](#build-hboffice-in-macos)

* [hboffice examples](#hboffice-examples)

## Introduction

**hboffice** is a project to use the LibreOffice-SDK in Harbour projects. It is an incomplete API, since the LibreOffice SDK is very extensive. It provides high-level functions in C that hide the complexity of using the SDK directly in C++. This C API is easily portable to Harbour.

Compiling the project generates two or three binaries.

* **officesdk.dll**/**libofficesdk.so**/**libofficesdk.dylib**: Dynamic library that contains the C API and the linkage to LibreOffice. In this way the links with LibreOffice-SDK are not propagated. On Windows, it must be compiled with Visual Studio (MinGW is not supported).

* **officesdk.lib**: (Only in Windows) Static library with the .dll exported symbols.

* **hboffice.lib**/**libhboffice.a**: Static library that contains the Harbour wrapper. You can use any compiler (MSVC, MingGW, GCC, Clang, etc).

## Installation of LibreOffice

It is necessary to **correctly install the LibreOffice package**, both on the development machines and on the final user machines.

### Installation in Windows

* Install the LibreOffice package. This installation is **required on both development machines and final user machines**.
    ![download_libreoffice](https://github.com/frang75/harbour_nappgui/assets/42999199/c410187b-3f27-473e-b756-4dce9b91fecd)

* Install the LibreOffice-SDK package. This installation is **required ONLY for compile hboffice in development machines**.
    > **Important:** LibreOffice-SDK is available in 32-bit and 64-bit versions. You will need to compile hboffice in 32 or 64 bits depending on the version of LibreOffice. It is not possible to mix 32 and 64 binaries. **By default, in Windows, hboffice will be compiled in 64bit**.

    > **Important:** The SDK version must be the same as LibreOffice application.

    ![download_libreoffice_sdk](https://github.com/frang75/harbour_nappgui/assets/42999199/4821de74-7e38-486a-94f6-ffd59d0f14a0)

* Set the `LIBREOFFICE_HOME` environment variable with the path to the LibreOffice home directory (usually `C:\Program Files\LibreOffice`). This environment variable is required both to compile the program and to run it on the user's machines. hboffice will connect to the LibreOffice program at runtime.

    ![envvar_libreoffice](https://github.com/frang75/harbour_nappgui/assets/42999199/3ad38b78-9214-4567-94b8-94dcf926848f)

* Add `%LIBREOFFICE_HOME%/program` path to `PATH` environment variable. In order to run any hboffice-based application, LibreOffice .DLLs must be accesible and located.

    ![path_envvar](https://github.com/frang75/harbour_nappgui/assets/42999199/d0215a5e-8569-4dca-a313-f765ada84080)

### Installation in Linux

* Install the LibreOffice package. This installation is **required on both development machines and final user machines**.
    ```
    sudo apt-get install libreoffice
    ```
* Install the LibreOffice development libraries. This installation is **required ONLY for compile hboffice in development machines**.
    ```
    sudo apt-get install libreoffice-dev

    # Optional, not mandatory for compile
    sudo apt-get install libreoffice-dev-doc
    ```
* Set the `LIBREOFFICE_HOME` environment variable with the path to the LibreOffice home directory (usually `/usr/lib/libreoffice`). This environment variable is required both to compile the program and to run it on the user's machines. hboffice will connect to the LibreOffice program at runtime. It is recommended to define this variable in the `.bashrc` so that it is always present.
    ```
    nano .bashrc
    # Add at the end
    export LIBREOFFICE_HOME=/usr/lib/libreoffice
    # Ctrl+X to save
    source .bashrc
    cd $LIBREOFFICE_HOME
    ls
    CREDITS.fodt  NOTICE  presets  program  sdk  share
    ```

* Add `$LIBREOFFICE_HOME/program` path to `LD_LIBRARY_PATH` environment variable. In order to run any hboffice-based application, LibreOffice shared libraries `.so` must be accesible and located.
    ```
    # Add at the end of .bashrc
    export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$LIBREOFFICE_HOME/program
    ```

### Installation in macOS

* Download and install the [LibreOffice.app](https://www.libreoffice.org/download/download-libreoffice/) bundle, typically in the `/Applications` directory. This installation is **required on both development machines and final user machines**.
    ![libreoffice_download_macos](https://github.com/frang75/harbour_nappgui/assets/42999199/1c0bae9a-6751-4ebc-9cc1-463bee184492)
    ![libreoffice_macos_app](https://github.com/frang75/harbour_nappgui/assets/42999199/5b04ca49-d54c-43a9-9db6-b72c9f7dfc1c)

* Download the LibreOffice-SDK package. This installation is **required ONLY for compile hboffice in development machines**.
    The `LibreOffice-SDK.dmg` just include a folder. Move this folder wherever you want. In our case, to `/Applications/libreoffice-sdk`.
    ![libreoffice_sdk_download_macos](https://github.com/frang75/harbour_nappgui/assets/42999199/756e5a57-446f-46c4-b53c-c87739a1f599)
    ![libreoffice_sdk_install_macos](https://github.com/frang75/harbour_nappgui/assets/42999199/6d332b5b-862b-4730-809a-afbffcb3942a)

* Set the `LIBREOFFICE_HOME` environment variable with the path to the `LibreOffice.app` bundle (usually `/Applications/LibreOffice.app`). This environment variable is required both to compile the program and to run it on the user's machines. hboffice will connect to the LibreOffice program at runtime.

* Add `$LIBREOFFICE_HOME$/Contents/Frameworks` path to `DYLD_LIBRARY_PATH` environment variable. In order to run any hboffice-based application, LibreOffice shared libraries `.dylib` must be accesible and located.

* Set the `LIBREOFFICE_SDK` environment variable with the path to the LibreOffice-SDK folder (`/Applications/libreoffice-sdk` in this example). It is recommended to define all these environment variables in the `.zshrc` so that it is always present.
    ```
    nano .zshrc

    # All machines (dev and production)
    export LIBREOFFICE_HOME=/Applications/LibreOffice.app
    export DYLD_LIBRARY_PATH=$DYLD_LIBRARY_PATH:$LIBREOFFICE_HOME/Contents/Frameworks
    # Only in dev machines
    export LIBREOFFICE_SDK=/Applications/libreoffice-sdk
    ```

* Go to `$LIBREOFFICE_HOME$/Contents/MacOS` and create a symbolic link for `soffice` executable.  **required on both development machines and final user machines**. The bootstrap process looks for `libreoffice` executable.
    ```
    /Applications/LibreOffice.app/Contents/MacOS

    ln -s soffice libreoffice
    ```

* Go to `$LIBREOFFICE_HOME$/Contents/Frameworks` and create the symbolic links for these libraries. The final version of library `.3` can differ in your installation **required ONLY for compile hboffice in development machines**.
    ```
    /Applications/LibreOffice.app/Contents/Frameworks

    ln -s libuno_cppu.dylib.3 libuno_cppu.dylib
    ln -s libuno_cppuhelpergcc3.dylib.3 libuno_cppuhelpergcc3.dylib
    ln -s libuno_purpenvhelpergcc3.dylib.3 libuno_purpenvhelpergcc3.dylib
    ln -s libuno_sal.dylib.3 libuno_sal.dylib
    ln -s libuno_salhelpergcc3.dylib.3 libuno_salhelpergcc3.dylib
    ```

### About LibreOffice-SDK

> **Important:** The `LIBREOFFICE_HOME` environment variable must be set and pointing to the LibreOffice home directory. e.g. `C:\Program Files\LibreOffice`, `/usr/lib/libreoffice`, `/Applications/LibreOffice.app`.

> **Important:** The `LIBREOFFICE_SDK` environment variable only is required on macOS development machines.

> **Important:** The first time a hboffice program uses a LibreOffice function, an instance of the LibreOffice application will be started invisibly (`soffice.bin` process). This first call will have a small delay due to the initialization of the process. It is imperative that LibreOffice is running in order to use the SDK from Harbour.

## Installation of CMake

In order to compile HBOFFICE you need to have [CMake](https://cmake.org/download) installed and accessible from the command line.

```
:~/$ cmake --version
cmake version 3.10.2
```

## Build hboffice

### Build hboffice in Windows

First step generate **officesdk.dll**:

```
cd contrib/hboffice
set CMAKE_GENERATOR=Visual Studio 15 2017
build.bat -dll -b Release

:: Full command
build.bat -dll -b [Release|Debug]
```

Then, generate the **hboffice.lib**:

```
build.bat -lib -comp mingw64 -b Release

:: Full command
build.bat -lib -comp [mingw64|clang|msvc64] -b [Release|Debug]

:: Required for VisualStudio
"%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Community\VC\Auxiliary\Build\vcvarsall.bat" x64
```

After these two steps, you will have:
* `officesdk.dll` in `/build/Release/bin` folder.
* `officesdk.lib` and `hboffice.lib/libhboffice.a` in `/build/Release/lib` folder.

### More about build hboffice in Windows

> **Important:** To build the dynamic library `officesdk.dll` is **imperative to use Visual Studio**.

To build `hboffice.lib` you can use Visual Studio, MinGW or Clang, depending on the compiler you use to generate the final executables.

If you have installed other Visual Studio version change the `CMAKE_GENERATOR` value:
```
set CMAKE_GENERATOR=Visual Studio 17 2022
set CMAKE_GENERATOR=Visual Studio 16 2019
set CMAKE_GENERATOR=Visual Studio 15 2017
set CMAKE_GENERATOR=Visual Studio 14 2015
set CMAKE_GENERATOR=Visual Studio 12 2013
set CMAKE_GENERATOR=Visual Studio 11 2012
```

> **Important:** Because LibreOffice-SDK requires C++11, you need, at least, Visual Studio 2012.

> **Important:** If you want compile `hboffice.lib` with Visual Studio, you have to set up the compiler. Harbour cannot find it automatically.
```
:: Setup Visual Studio accesible by Harbour
:: This command depends on Visual Studio specific version/installation.
"%ProgramFiles(x86)%\Microsoft Visual Studio 11.0\VC\vcvarsall.bat" x64
```

### Build hboffice in Linux

First step generate **libofficesdk.so**:

```
cd contrib/hboffice
./build.sh -dll -b Release

:: Full command
./build.sh -dll -b [Release|Debug]
```

Then, generate the **libhboffice.a**:

```
./build.sh -lib -comp gcc -b Release

:: Full command
./build.sh -lib -comp [gcc|clang] -b [Release|Debug]
```

After these two steps, you will have:
* `libofficesdk.so` in `/build/Release/bin` folder.
* `libhboffice.a` in `/build/Release/lib` folder.

### Build hboffice in macOS

> **Important:** Xcode is required to build in macOS.
```
% xcodebuild -version
Xcode 14.3.1
Build version 14E300c
```

First step generate **libofficesdk.dylib**:

> **Important:** In order to avoid linker warnings, use the same `MACOSX_DEPLOYMENT_TARGET` as when building Harbour. More [here](../gtnap/Readme.md#build-harbour-in-macos).

```
cd contrib/hboffice
export MACOSX_DEPLOYMENT_TARGET=10.13
./build.sh -dll -b Release

:: Full command
./build.sh -dll -b [Release|Debug]
```

Then, generate the **libhboffice.a**:

```
./build.sh -lib -b Release

:: Full command
./build.sh -lib -b [Release|Debug]
```

After these two steps, you will have:
* `libofficesdk.dylib` in `/build/Release/bin` folder.
* `libhboffice.a` in `/build/Release/lib` folder.


## hboffice examples

In the `/hboffice/tests` folder there are different examples of use. To run them:

* Windows: Copy `/build/Release/bin/officesdk.dll` into `/tests`.
* Linux: Copy `/build/Release/bin/libofficesdk.so` into `/tests`.
* macOS: Copy `/build/Release/bin/libofficesdk.dylib` into `/tests`.
* Compile and run one of the examples (e.g. `sheet1.prg`)
* The results documents will be saved in `/tests/result`.

```
:: In Windows
cd \contrib\hboffice\tests
copy ..\build\Release\bin\officesdk.dll
..\..\..\bin\win\mingw64\hbmk2 sheet1.prg hboffice.hbc -comp=mingw64

:: Just run
sheet1.exe
```

```
# In Linux
cd /contrib/hboffice/tests
cp ../build/Release/bin/libofficesdk.so .
../../../bin/linux/gcc/hbmk2 sheet1.prg hboffice.hbc

# Just run
./sheet1
```

```
# In macOS
cd /contrib/hboffice/tests
cp ../build/Release/bin/libofficesdk.dylib .
../../../bin/darwin/clang/hbmk2 sheet1.prg hboffice.hbc
export DYLD_LIBRARY_PATH=$DYLD_LIBRARY_PATH:$(pwd)
# Just run
./sheet1
```

> **Important:** Dynamic libraries `officesdk.dll`, `libofficesdk.so` or `libofficesdk.dylib` must be distributed with final executables and must be installed in the same folder.
