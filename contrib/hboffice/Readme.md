# Harbour LibreOffice-SDK wrapper (hboffice)

* [Introduction](#introduction)
* [Installation of LibreOffice](#installation-of-libreoffice)
    - [Installation in Windows](#installation-in-windows)
    - [Installation in Linux](#installation-in-linux)
    - [Installation in macOS](#installation-in-macos)
    - [About LibreOffice-SDK](#about-libreoffice-sdk)

## Introduction

**hboffice** is a project to use the LibreOffice-SDK in Harbour projects. It is an incomplete API, since the LibreOffice SDK is very extensive. It provides high-level functions in C that hide the complexity of using the SDK directly in C++. This C API is easily portable to Harbour.

Compiling the project generates two binaries.

* **officesdk.dll**/**libofficesdk.so**: Dynamic library that contains the C API and the link to LibreOffice. In this way the links with LibreOffice-SDK are not propagated. On Windows, it must be compiled with Visual Studio (MinGW is not supported).

* **officesdk.lib**/**libofficesdk.a**: Static library that contains the Harbour wrapper and symbol loading at runtime. You can use any compiler (MSVC, MingGW, GCC, etc).

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

* Add `$LIBREOFFICE_HOME$/program` path to `LD_LIBRARY_PATH` environment variable. In order to run any hboffice-based application, LibreOffice shared libraries `.so` must be accesible and located.
    ```
    # Add at the end of .bashrc
    export LD_LIBRARY_PATH=$LD_LIBRARY_PATH$:$LIBREOFFICE_HOME$/program
    ```

### Installation in macOS

**TODO**

### About LibreOffice-SDK

> **Important:** The `LIBREOFFICE_HOME` environment variable must be set and pointing to the LibreOffice home directory. e.g. `/usr/lib/libreoffice`, `C:\Program Files\LibreOffice`

> **Important:** The first time a hboffice program uses a LibreOffice function, an instance of the LibreOffice application will be started invisibly (`soffice.bin` process). This first call will have a small delay due to the initialization of the process. It is imperative that LibreOffice is running in order to use the SDK from Harbour.

## Build hboffice

### Build hboffice in Windows

First step generate **officesdk.dll**

```
cd contrib/hboffice
set CMAKE_GENERATOR=Visual Studio 17 2022
build.dat -dll -b Release
```

This script will generate the dll in `/build/Release/bin` folder.

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

> **Important:** The **officesdk.dll** does not support other Windows compiler (like MinGW).

