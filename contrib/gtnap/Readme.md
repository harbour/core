# GTNap

Harbour cross-platform video subsystem using NAppGUI-SDK
https://github.com/frang75/nappgui_src

* [Installing CMake](#installing-cmake)
* [Installing Visual Studio](#installing-visual-studio)
* [Installing MinGW GCC](#installing-mingw-gcc)
* [Installing MinGW CLANG](#installing-mingw-clang)
* [Installing GCC in Linux](#installing-gcc-in-linux)
* [Installing CLANG in Linux](#installing-clang-in-linux)

* [Installing Xcode in macOS](#installing-xcode-in-macos)
* [Build Harbour](#build-harbour)
    - [Build Harbour in Windows Visual Studio](#build-harbour-in-windows-visual-studio)
    - [Build Harbour in Windows MinGW](#build-harbour-in-windows-mingw)
    - [Build Harbour in Windows Clang](#build-harbour-in-windows-clang)
    - [Build Harbour in Linux GCC](#build-harbour-in-linux-gcc)
    - [Build Harbour in Linux Clang](#build-harbour-in-linux-clang)
    - [Build Harbour in macOS](#build-harbour-in-macos)
* [Build GTNap](#build-gtnap)
    - [In Windows with MinGW](#in-windows-with-mingw)
    - [In Windows with Clang](#in-windows-with-clang)
    - [In Windows with VisualStudio](#in-windows-with-visualstudio)
    - [In Linux with GCC](#in-linux-with-gcc)
    - [In Linux with Clang](#in-linux-with-clang)
    - [In macOS with Xcode](#in-macos-with-xcode)
* [Using GTNap](#using-gtnap)
* [Compile and run CUADEMO example](#compile-and-run-cuademo-example)
* [Harbour debugging](#harbour-debugging)
* [Application ICON](#application-icon)
   - [Icon in Windows (and manifest)](#icon-in-windows-and-manifest)
   - [Icon in Linux](#icon-in-linux)
* [GTNap developer mode](#gtnap-developer-mode)
   - [Windows developer mode](#windows-developer-mode)
   - [Linux developer mode](#linux-developer-mode)
   - [macOS developer mode](#macos-developer-mode)

* [GTNap design](#gtnap-design)

## Installing CMake

For building GTNap CMake tool is necessary:

### CMake in Windows

* Download from https://cmake.org/download/

* Select **Add CMake to the system PATH for all users** when installing.
    ![cmake_win](https://user-images.githubusercontent.com/42999199/235419286-0a6101f4-b43b-4e40-a3cb-c585fe908185.png)

### CMake in Linux

   * `sudo apt-get install cmake cmake-gui`

   * Open a terminal/cmd and check if cmake works:
      ```
      :~/$ cmake --version
      cmake version 3.10.2
      ```

### CMake in macOS

* Download from https://cmake.org/download/

* Move `CMake.app` to `/Applications` folder.

* By default, CMake does not configure command line access on macOS. You can create symbolic links with `sudo "/Applications/CMake.app/Contents/bin/cmake-gui" --install`.
    ![cmake_macos](https://github.com/frang75/harbour_nappgui/assets/42999199/0c25cc32-faf9-4e81-a949-6c5ac1b67bb5)

* Open a terminal/cmd and check if cmake works:
    ```
    % cmake --version
    cmake version 3.21.4
    ```

## Installing Visual Studio

To use the `msvc`/`msvc64` compilers we need to install Visual Studio environment. Microsoft offers the free [Community](https://visualstudio.microsoft.com/vs/) version since VS2017.

Once installed, `msvc` compilers are not directly accesible by command line. We need to run `vcvarsall.bat` script, installed in different locations depending on each version. For example, in Visual Studio 2012.

```
:: Set the Visual Studio 64bit compiler
"%ProgramFiles(x86)%\Microsoft Visual Studio 11.0\VC\vcvarsall.bat" x64

:: Check compiler is working
C:>cl
Microsoft (R) C/C++ Optimizing Compiler Version 17.00.61030 for x64
Copyright (C) Microsoft Corporation.  All rights reserved.

:: MSBuild tools is working
C:>msbuild
Microsoft (R) Build Engine version 4.8.9037.0
[Microsoft .NET Framework, version 4.0.30319.42000]
Copyright (C) Microsoft Corporation. All rights reserved.
```

## Installing MinGW GCC

MinGW is a project that provides a native Windows version of the GCC compiler. There are different ways to install it, but the most direct is through the MYSYS2 environment.

* Download and install MSYS2 from [here](https://www.msys2.org). By default, is installed in `C:\msys64`.

* Open a MSYS2 terminal and write `pacman -S --needed base-devel mingw-w64-ucrt-x86_64-toolchain` to install the MinGW/GCC compiler.

* Open the **Environment Variables** editor and add: `C:\msys64\mingw64\bin` and `\msys64\ucrt64\bin` folders to PATH environment variable.

* If everything went well, open a `cmd` and type these commands to see if MinGW is active.

    ```
    :: MinGW Make is working
    C:>mingw32-make
    mingw32-make: *** No targets specified and no makefile found.  Stop.

    :: MinGW gcc compiler is working
    C:>gcc --version
    gcc (Rev6, Built by MSYS2 project) 13.2.0
    Copyright (C) 2023 Free Software Foundation, Inc.
    This is free software; see the source for copying conditions.  There is NO
    warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.```
    ```

## Installing MinGW CLANG

Using MSYS2, you can also install the Clang compiler using `pacman -S mingw-w64-x86_64-clang`. If everything went well, open a `cmd` and check the compiler is working.

    ```
    :: Clang compiler is working in Windows
    C:>clang --version
    clang version 18.1.4
    Target: x86_64-w64-windows-gnu
    Thread model: posix
    InstalledDir: C:/msys64/mingw64/bin
    ```

## Installing GCC in Linux

```
# Install GCC compiler and build tools
:~$ sudo apt-get install build-essential

# Check make tool is working
:~$ make --version
GNU Make 4.3
Built for x86_64-pc-linux-gnu

# Check gcc compiler is working
~$ gcc --version
gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
Copyright (C) 2023 Free Software Foundation, Inc.
```

## Installing CLANG in Linux

```
# Install CLANG compiler and build tools
:~$ sudo apt-get install clang

# Check clang compiler is working
:~$ clang --version
clang version 10.0.0-4ubuntu1
Target: x86_64-pc-linux-gnu
Thread model: posix
InstalledDir: /usr/bin
```

## Installing Xcode in macOS

Xcode provides the `AppleClang` compiler and build tools for macOS. Download and install it from Apple website [here](https://developer.apple.com/xcode/).
![xcode_macos](https://github.com/frang75/harbour_nappgui/assets/42999199/cf140563-49df-4c3f-acef-24149fda9382)

```
# Check build tools is working
user@host% xcodebuild -version
Xcode 12.5.1
Build version 12E507

# Check clang compiler is working
user@host% clang --version
Apple clang version 12.0.5 (clang-1205.0.22.11)
Target: x86_64-apple-darwin20.6.0
```

## Build Harbour

```
:: Remove the /bin/win folder
:: Remove the /lib/win folder
```

### Build Harbour in Windows Visual Studio

```
:: Important: Use cmd and not PowerShell
:: Set Visual Studio 2017 64bit compiler for hbmk2 (msvc64)
"%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Community\VC\Auxiliary\Build\vcvarsall.bat" x64

:: Go to main folder of harbour working copy
cd harbour_nappgui

win-make HB_COMPILER=msvc64

! Building Harbour 3.2.0dev from source - https://harbour.github.io
! MAKE: win-make 4.1 sh.exe
! HB_HOST_PLAT: win (x86_64)  HB_SHELL: nt
! HB_PLATFORM: win (x86_64) (auto-detected)
! HB_COMPILER: msvc64 (v1700)
! Component: 'zlib' found in C:/harbour_nappgui/src/3rd/zlib (local)
! Component: 'pcre' found in C:/harbour_nappgui/src/3rd/pcre (local)
...
```

### Build Harbour in Windows MinGW

```
:: Go to main folder of harbour working copy
cd harbour_nappgui

mingw32-make.exe -j2 HB_CPU=x86_64 HB_COMPILER=mingw64

C:\harbour_nappgui>win-make HB_COMPILER=mingw64
! Building Harbour 3.2.0dev from source - https://harbour.github.io
! MAKE: win-make 4.1 sh.exe
! HB_HOST_PLAT: win (x86_64)  HB_SHELL: nt
! HB_PLATFORM: win (x86_64) (auto-detected)
! HB_COMPILER: mingw64
! Component: 'zlib' found in C:/harbour_nappgui/src/3rd/zlib (local)
! Component: 'pcre' found in C:/harbour_nappgui/src/3rd/pcre (local)
...
```

### Build Harbour in Windows Clang

```
:: Go to main folder of harbour working copy
cd harbour_nappgui

mingw32-make.exe -j2 HB_CPU=x86_64 HB_COMPILER=clang

C:\harbour_nappgui>mingw32-make.exe -j2 HB_CPU=x86_64 HB_BUILD_CONTRIBS=no HB_COMPILER=clang
! Building Harbour 3.2.0dev from source - https://harbour.github.io
! MAKE: mingw32-make.exe 4.4.1 sh.exe   -j2 -- HB_COMPILER=clang HB_BUILD_CONTRIBS=no HB_CPU=x86_64
! HB_HOST_PLAT: win (x86_64)  HB_SHELL: nt
! HB_PLATFORM: win (x86_64) (auto-detected)
! HB_COMPILER: clang
! Component: 'zlib' found in C:/harbour_nappgui/src/3rd/zlib (local)
! Component: 'pcre' found in C:/harbour_nappgui/src/3rd/pcre (local)
...
```

### Build Harbour in Linux GCC

```
# Go to main folder of harbour working copy
cd harbour_nappgui

make -j4 HB_CPU=x86_64 HB_COMPILER=gcc

! Building Harbour 3.2.0dev from source - https://harbour.github.io
! MAKE: make 4.1 /bin/sh
! HB_HOST_PLAT: linux (x86_64)  HB_SHELL: sh
! LD_LIBRARY_PATH: /home/fran/harbour_nappgui/lib/linux/gcc:
! HB_PLATFORM: linux (x86_64) (auto-detected)
! HB_COMPILER: gcc (auto-detected: /usr/bin/)
...
```

### Build Harbour in Linux Clang

```
# Go to main folder of harbour working copy
cd harbour_nappgui

make -j4 HB_CPU=x86_64 HB_COMPILER=clang

! Building Harbour 3.2.0dev from source - https://harbour.github.io
! MAKE: make 4.2.1 /bin/sh
! HB_HOST_PLAT: linux (x86_64)  HB_SHELL: sh
! LD_LIBRARY_PATH: /home/fran/harbour_nappgui/lib/linux/clang::/usr/lib/libreoffice/program
! HB_PLATFORM: linux (x86_64) (auto-detected)
! HB_COMPILER: clang
...
```

### Build Harbour in macOS

```
# Go to main folder of harbour working copy
cd harbour_nappgui

# Set the minimum macOS version
export MACOSX_DEPLOYMENT_TARGET=10.13

make

! Building Harbour 3.2.0dev from source - https://harbour.github.io
! MAKE: /Applications/Xcode.app/Contents/Developer/usr/bin/make 3.81 /bin/sh
! HB_HOST_PLAT: darwin (x86_64)  HB_SHELL: sh
! LD_LIBRARY_PATH: /Users/fran/harbour_nappgui/lib/darwin/clang:
! HB_PLATFORM: darwin (x86_64) (auto-detected)
! HB_COMPILER: clang (auto-detected: /usr/bin/)
! HB_HOST_PKGM: homebrew
! Component: 'zlib' found in /Users/fran/harbour_nappgui/src/3rd/zlib (local)
! Component: 'pcre' found in /Users/fran/harbour_nappgui/src/3rd/pcre (local)
! Component: 'gpm' not supported on darwin platform
! Component: 'slang' not found
! Component: 'curses' not found
! Component: 'x11' not found
! Component: 'wattcp/watt-32' not supported on darwin platform
! HB_INSTALL_PREFIX automatically set to: /opt/harbour
! GIT_REVISION: 0a1c79204d
...
```

## Build GTNap

The `/contrib/gtnap` project consists of several files and folders:

```
/prj                NAppGUI CMake-based build system.
/resources          Files used in development debug.
/src                Source code.
/src/gtnap          GTNAP terminal source.
/src/**             NAppGUI libraries source.
/tests              Testing and examples.
/tests/cuademo      Cuademo example application.
/tools              Build tools required by NAppGUI.
build.bat           GTNap build script for Windows.
build.sh            GTNap build script for Linux/macOS.
CMakeLists.txt      Main script for CMake build system.
CMakeTargets.cmake  NAppGUI/GTNap build targets for CMake.
gtnap.ch            Predefined constants for GTNAp.
gtnap.hbc           Options for projects that use GTNap.
gtnap.hbp           GTNap project for hbmk2.
gtnap.hbx           GTNap symbol file, autogenerated by hbmk2.
Readme.md           This documentation.
```

### In Windows with MinGW

`build.bat` script allows to compile GTNAP using VisualStudio `msvc64` compiler, MinGW `mingw64` compiler or `clang` compiler.

```
:: Goto gtnap folder
cd contrib\gtnap

:: Just build
build.bat -b [Debug|Release] -comp mingw64
```

This will generate several static libraries:

* The GT library: `libgtnap.a` in `build/[Debug|Release]/lib` folder.
* The NAppGUI libraries: `libsewer.a`, `libosbs.a`, `libcore.a`, `libgeom2d.a`, `libdraw2d.a`, `libosgui.a`, `libgui.a`, `libosapp.a`, `libinet.a` in `build/[Debug|Release]/lib` folder.
* The GTNAP Debugger lib: `libdeblib.a` in `build/[Debug|Release]/lib` folder.
* The GTNAP Forms libs: `libnflib.a`, `libnforms.a` in `build/[Debug|Release]/lib` folder.
* The GTNAP utilities executables: `gtnapdeb`, `napdesign` in `build/[Debug|Release]/bin` folder.

### In Windows with Clang

```
:: Goto gtnap folder
cd contrib\gtnap

:: Just build
build.bat -b [Debug|Release] -comp clang
```

This will generate several static libraries:

* The GT library: `libgtnap.a` in `build/[Debug|Release]/lib` folder.
* The NAppGUI libraries: `libsewer.a`, `libosbs.a`, `libcore.a`, `libgeom2d.a`, `libdraw2d.a`, `libosgui.a`, `libgui.a`, `libosapp.a`, `libinet.a` in `build/[Debug|Release]/lib` folder.
* The GTNAP Debugger lib: `libdeblib.a` in `build/[Debug|Release]/lib` folder.
* The GTNAP Forms libs: `libnflib.a`, `libnforms.a` in `build/[Debug|Release]/lib` folder.
* The GTNAP utilities executables: `gtnapdeb`, `napdesign` in `build/[Debug|Release]/bin` folder.

### In Windows with VisualStudio

> **Important:** The MSVC compiler used by Harbour `hbmk2` is configured using the `vcvarsall.bat` script of the specific version of Visual Studio we are going to use. To configure the compiler in CMake, we must set the `CMAKE_GENERATOR` environment variable to the same version as `vcvarsall.bat`.
```
set CMAKE_GENERATOR=Visual Studio 17 2022
set CMAKE_GENERATOR=Visual Studio 16 2019
set CMAKE_GENERATOR=Visual Studio 15 2017
set CMAKE_GENERATOR=Visual Studio 14 2015
set CMAKE_GENERATOR=Visual Studio 12 2013
set CMAKE_GENERATOR=Visual Studio 11 2012
set CMAKE_GENERATOR=Visual Studio 10 2010
set CMAKE_GENERATOR=Visual Studio 9 2008
set CMAKE_GENERATOR=Visual Studio 8 2005
```

> **Important:** On Windows 64bit machines we can force 32bit compilation using the `-a Win32` flag in `build.bat` script. By default, it will compile to 64bit.

```
:: Goto gtnap folder
cd contrib\gtnap

:: Set Visual Studio 2017 CMake generator
set CMAKE_GENERATOR=Visual Studio 15 2017
:: Set Visual Studio 2017 64bit compiler for hbmk2 (msvc64)
"%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Community\VC\Auxiliary\Build\vcvarsall.bat" x64

:: Just build
build.bat -b [Debug|Release] -comp msvc64
```
This will generate several static libraries:

* The GT library: `gtnap.lib` in `build/[Debug|Release]/lib` folder.
* The NAppGUI libraries: `sewer.lib`, `osbs.lib`, `core.lib`, `geom2d.lib`, `draw2d.lib`, `osgui.lib`, `gui.lib`, `osapp.lib`, `inet.lib` in `build/[Debug|Release]/lib` folder.
* The GTNAP Debugger lib: `deblib.lib` in `build/[Debug|Release]/lib` folder.
* The GTNAP Forms libs: `nflib.lib`, `nforms.lib` in `build/[Debug|Release]/lib` folder.
* The GTNAP utilities executables: `gtnapdeb`, `napdesign` in `build/[Debug|Release]/bin` folder.

### In Linux with GCC

First of all, install the required development libraries:

```
sudo apt-get install libgtk-3-dev
sudo apt-get install libglu1-mesa-dev freeglut3-dev mesa-common-dev
sudo apt-get install libcurl4-openssl-dev
```

Then, compile gtnap
```
# Goto gtnap folder
cd contrib/gtnap

# Just build
bash ./build.sh -comp gcc -b [Debug|Release]
```

This will generate several static libraries:

* The GT library: `libgtnap.a` in `build/[Debug|Release]/lib` folder.
* The NAppGUI libraries: `libsewer.a`, `libosbs.a`, `libcore.a`, `libgeom2d.a`, `libdraw2d.a`, `libosgui.a`, `libgui.a`, `libosapp.a`, `libinet.a` in `build/[Debug|Release]/lib` folder.
* The GTNAP Debugger lib: `deblib.lib` in `build/[Debug|Release]/lib` folder.
* The GTNAP Forms libs: `nflib.lib`, `nforms.lib` in `build/[Debug|Release]/lib` folder.
* The GTNAP utilities executables: `gtnapdeb`, `napdesign` in `build/[Debug|Release]/bin` folder.

### In Linux with CLANG

```
# Goto gtnap folder
cd contrib/gtnap

# Just build
bash ./build.sh -comp clang -b [Debug|Release]
```

This will generate several static libraries:

* The GT library: `libgtnap.a` in `build/[Debug|Release]/lib` folder.
* The NAppGUI libraries: `libsewer.a`, `libosbs.a`, `libcore.a`, `libgeom2d.a`, `libdraw2d.a`, `libosgui.a`, `libgui.a`, `libosapp.a`, `libinet.a` in `build/[Debug|Release]/lib` folder.
* The GTNAP Debugger lib: `deblib.lib` in `build/[Debug|Release]/lib` folder.
* The GTNAP Forms libs: `nflib.lib`, `nforms.lib` in `build/[Debug|Release]/lib` folder.
* The GTNAP utilities executables: `gtnapdeb`, `napdesign` in `build/[Debug|Release]/bin` folder.

### In macOS with Xcode

> **Important:** On macOS, before compile Harbour and GTNap, **we must** set the `MACOSX_DEPLOYMENT_TARGET` environment variable, in order to establish the minimum version of macOS where our GTNAP applications will run.

```
export MACOSX_DEPLOYMENT_TARGET=14.0      # Sonoma
export MACOSX_DEPLOYMENT_TARGET=13.6      # Ventura
export MACOSX_DEPLOYMENT_TARGET=13.5
export MACOSX_DEPLOYMENT_TARGET=13.4
export MACOSX_DEPLOYMENT_TARGET=13.3
export MACOSX_DEPLOYMENT_TARGET=13.2
export MACOSX_DEPLOYMENT_TARGET=13.1
export MACOSX_DEPLOYMENT_TARGET=13.0
export MACOSX_DEPLOYMENT_TARGET=12.4      # Monterey
export MACOSX_DEPLOYMENT_TARGET=12.3
export MACOSX_DEPLOYMENT_TARGET=12.2
export MACOSX_DEPLOYMENT_TARGET=12.1
export MACOSX_DEPLOYMENT_TARGET=12.0
export MACOSX_DEPLOYMENT_TARGET=11.5      # Big Sur
export MACOSX_DEPLOYMENT_TARGET=11.4
export MACOSX_DEPLOYMENT_TARGET=11.3
export MACOSX_DEPLOYMENT_TARGET=11.2
export MACOSX_DEPLOYMENT_TARGET=11.1
export MACOSX_DEPLOYMENT_TARGET=11.0
export MACOSX_DEPLOYMENT_TARGET=10.15     # Catalina
export MACOSX_DEPLOYMENT_TARGET=10.14     # Mojave
export MACOSX_DEPLOYMENT_TARGET=10.13     # High Sierra
export MACOSX_DEPLOYMENT_TARGET=10.12     # Sierra
...
It is not recommended to compile for lower systems.
```

> **Important:** GTNap, has been tested with `MACOSX_DEPLOYMENT_TARGET=10.13`.

Then, compile Harbour and gtnap
```
# Set the minimum macOS
export MACOSX_DEPLOYMENT_TARGET=10.13

# Goto gtnap folder
cd contrib/gtnap

# Just build
bash ./build.sh -b [Debug|Release]
```

This will generate several static libraries:

* The GT library: `libgtnap.a` in `/build` folder.
* The NAppGUI libraries: `libsewer.a`, `libosbs.a`, `libcore.a`, `libgeom2d.a`, `libdraw2d.a`, `libosgui.a`, `libgui.a`, `libosapp.a`, `libinet.a` in `build/[Debug|Release]/lib` folder.
* The GTNAP Debugger lib: `deblib.lib` in `build/[Debug|Release]/lib` folder.
* The GTNAP Forms libs: `nflib.lib`, `nforms.lib` in `build/[Debug|Release]/lib` folder.
* The GTNAP utilities executables: `gtnapdeb`, `napdesign` in `build/[Debug|Release]/bin` folder.

## Using GTNap

Just adding `-gtnap` flag into your `.hbp` project file.

## Compile and run CUADEMO example

- To compile in Windows with MinGW:
   ```
   :: Use -debug option or omit for release version
   cd contrib\gtnap\tests\cuademo\gtnap_cualib
   ..\..\..\..\..\bin\win\mingw64\hbmk2.exe [-debug] -comp=mingw64 exemplo.hbp
   exemplo --hb:gtnap
   exemplo --hb:gtwin
   ```

- To compile in Windows with VisualStudio:
   ```
   :: Set 64bit compiler
   "%ProgramFiles(x86)%\Microsoft Visual Studio 11.0\VC\vcvarsall.bat" x64

   :: Use -debug option or omit for release version
   cd contrib\gtnap\tests\cuademo\gtnap_cualib
   ..\..\..\..\..\bin\win\msvc64\hbmk2.exe [-debug] -comp=msvc64 exemplo.hbp
   exemplo --hb:gtnap
   exemplo --hb:gtwin
   ```

- To compile in Linux:
   ```
   cd contrib/gtnap/tests/cuademo/gtnap_cualib
   # Use -debug option or omit for release version
   ../../../../../bin/linux/gcc/hbmk2 [-debug] exemplo.hbp
   ./exemplo --hb:gtnap
   ./exemplo --hb:gttrm
   ```

- To compile in macOS:
   ```
   cd contrib/gtnap/tests/cuademo/gtnap_cualib
   ../../../../../bin/darwin/clang/hbmk2 exemplo.hbp
   ./exemplo --hb:gtnap
   ./exemplo --hb:gttrm
   ```

![gtnap_win](https://user-images.githubusercontent.com/42999199/235419502-456ce304-62cf-4559-b84f-964e20a763d3.png)
![gtnap_linux](https://user-images.githubusercontent.com/42999199/235419548-0ef049d8-6c51-45e2-be4d-393a58c1f07a.png)
![gtnap_macos](https://github.com/frang75/harbour_nappgui/assets/42999199/99a37caf-4167-4b75-87c5-8c5ca4756e5a)

## Harbour debugging

GTNap provides connection to Harbour's built-in debugger in text mode. To use it, you don't have to do anything special, just compile GTNap in Debug mode as we already indicated in the previous section.

```
# Windows
build.bat -b Debug -a x64

# Linux or macOS
bash ./build.sh -b Debug
```

In addition to `gtnap.lib`, the build script will create the `/build/Debug/bin/gtnapdeb` executable that will be invoked by GTNap when the time comes. To use the debugger, just add the `AltD()` command anywhere in the Harbour code:

```
CUA20 @ 18,40,26,90 JANELA V_JANELA ;
     TITULO "Menu com rolamento" SUBTITULO "%T";
     AJUDA "T?????"
*
AltD()
ESPECIALIZE V_JANELA MENU ROLAVERTICAL
ADDOPCAO V_JANELA TEXTO "#Opção 1" ;
    ACAO MOSTRAR("M?????","Foi escolhida a opção 1") AJUDA "P06723"
ADDOPCAO V_JANELA TEXTO "Opção #2" ;
    ACAO MOSTRAR("M?????","Foi escolhida a opção 2") AJUDA "P06725"
```

GTNap will stop the execution at this point and launch the Debugger. Because GTNap provides a bypass to the built-in debugger, the debugging commands will be the same as in text mode.

![debugger](https://github.com/frang75/harbour_nappgui/assets/42999199/3c080fd6-4e71-4c02-b02b-31e79d490ef1)

![debugger_process](https://github.com/frang75/harbour_nappgui/assets/42999199/6718de3b-1b14-48cc-b22c-715fdb2104e7)

## Application ICON

Create two `.ico` files with transparent backgrounds. One of 48x48 (Linux) and another 256x256 (Windows). You can refer to the files in `/contrib/gtnap/tests/cuademo`.

![image](https://github.com/frang75/harbour_nappgui/assets/42999199/bbe22a2f-6c86-43fc-9bca-64ddf6ee2532)

### Icon in Windows (and manifest)

Create a `*.rc` file with the application name in the same code directory `example.rc`. The content of this file should point to the location of the icon:

```
APPLICATION_ICON ICON "..\\icon256.ico"
```

> **Important:** Using the MinGW compiler is also necessary add the application manifest to the `*.rc` file.

* Create a `Application.manifest` file with this content into `/contrib/gtnap/tests/cuademo`
    ```
    <?xml version="1.0" encoding="UTF-8" standalone="yes"?>
    <assembly xmlns="urn:schemas-microsoft-com:asm.v1" manifestVersion="1.0">
        <assemblyIdentity version="1.0.0.0" processorArchitecture="*" name="exemplo.application" type="win32" />
        <description>Your application description here.</description>
        <dependency>
            <dependentAssembly>
                <assemblyIdentity type="win32" name="Microsoft.Windows.Common-Controls" version="6.0.0.0" processorArchitecture="*" publicKeyToken="6595b64144ccf1df" language="*" />
            </dependentAssembly>
        </dependency>
    </assembly>
    ```

* Add this line to `exemplo.rc`
    ```
    APPLICATION_ICON ICON "..\\icon256.ico"
    1 24 "..\\Application.manifest"
    ```

Add the `*.rc` to your `*.hbp` so that it compiles with the rest of the source files (for MSVC only).
```
...
stubs.prg
inkey_.prg
texto.prg

# Icon
{allmsvc|allmingw}exemplo.rc
```
![application_icon_win](https://github.com/frang75/harbour_nappgui/assets/42999199/7455ccb8-08f9-4e59-93cc-d1fca0806ea5)

### Icon in Linux

Copy `icon48.ico` to the same directory as the executable and rename it the same as the executable (`example.ico`). NAppGUI looks for this file at runtime and will pass it to the GTK toolkit. No action is required at compile time.

![image](https://github.com/frang75/harbour_nappgui/assets/42999199/d136e7d4-072c-45c9-af02-49a112fb7ed2)

![application_icon_xfce](https://github.com/frang75/harbour_nappgui/assets/42999199/af3528c8-b232-437c-b307-cae9b401815c)

## GTNap developer mode

**(Only if you are working on gtnap development)**. Otherwise, you can skip this section.

Using the NAppGUI build system, based on CMake, it is possible to create a single Visual Studio/VSCode solution containing the source code and internal dependencies of: NAppGUI, GTNap, HBOffice and cuademo-example. Thanks to this, it is extremely easy to debug the entire stack and develop a use case that requires all the technologies.

A `CMakeLists.txt` and `nap-dev` folder have been provided to generate the debugging solution. Just go to Harbour root path and write:

### Windows developer mode

```
# In /harbour_nappgui
set CMAKE_GENERATOR=Visual Studio 15 2017
cmake -S . -B build -A x64
```

* It will generate a Visual Studio solution in `/build` folder. Open it.

* Go to `exemplo` project, right click Properties. In `Debugging`:
    * Command arguments: `--hb:gtnap`
    * Working directory: `..\..\..\..\..\contrib\gtnap\tests\cuademo\gtnap_cualib`

* Right click in `exemplo` project: `Set as startup project`.

* `Build->Build Solution`.

* `Debug->Start Debugging`.

   ![debug_visual_studio](https://user-images.githubusercontent.com/42999199/235441869-da3fd867-8a35-4554-a526-4775c0b84f0b.png)


### Linux developer mode

```
# In /harbour_nappgui
cmake -S . -B build
cmake --build build
```

* Open VSCode and `/habour_nappgui` folder (main path).

* Go to `Run and Debug` and select `Exemplo`:

    ![debug_vscode_linux](https://github.com/frang75/harbour_nappgui/assets/42999199/20c7ce47-a6eb-4132-8c45-be8c2b9543a2)


### macOS developer mode

```
:: In /harbour_nappgui
cmake -G Xcode -S . -B build
```

* It will generate a Xcode project in `/build` folder. Open it.

* Select `exemplo` project.

  ![xcode_select_project](https://github.com/frang75/harbour_nappgui/assets/42999199/63df349c-3c17-402f-ad26-27e1c943cb44)

* Then `Product->Scheme->Edit Scheme`. In `Debugging`:
    * Arguments Passed On Launch: `--hb:gtnap`
    * Environment variables:
       - `LIBREOFFICE_HOME` --> `/Applications/LibreOffice.app`
       - `DYLD_LIBRARY_PATH` --> `/Applications/LibreOffice.app/Contents/Frameworks` (or the path of your LibreOffice installation).
    * Options, Working Directory: `/Users/fran/harbour_nappgui/contrib/gtnap/tests/cuademo/gtnap_cualib` (the full path of `gtnap_cualib` in your system).

      ![xcode_scheme](https://github.com/frang75/harbour_nappgui/assets/42999199/fe244c0c-d95e-4df6-b20a-8080a37757c1)

      ![xcode_scheme_2](https://github.com/frang75/harbour_nappgui/assets/42999199/3e962c68-9532-4749-9235-0aeeb4c37c72)

* Press `[PLAY]` button or `Product->Run`.

  ![xcode_debugging](https://github.com/frang75/harbour_nappgui/assets/42999199/f0f4af86-5241-44bf-8ad7-711e47deb541)


## GTNap design

GTNAP has been designed with two modes of operation in mind:
   * **Full-Graphic:** Cross-platform "modern" look&feel applications. Not compatible with Harbour/Clipper text command neither CUALIB.
   * **Semi-Graphic:** Cross-platform "cualib-compatible" graphics layer. Improve the appearance of GTWVW solution and runs in Windows/Linux.

![GTNAP_Full_graphic](https://user-images.githubusercontent.com/42999199/202912012-ecc6d479-7455-4cca-85f6-05cd57730407.png)

![GTNAP_Semil_graphic](https://user-images.githubusercontent.com/42999199/202912021-e6ffcd8b-08b5-494f-ae1a-f78b6403bddb.png)

