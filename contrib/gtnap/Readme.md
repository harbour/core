# GTNap

Harbour cross-platform video subsystem using NAppGUI-SDK
https://github.com/frang75/nappgui_src

* [Installing CMake](#installing-cmake)
* [Installing Visual Studio](#installing-visual-studio)
* [Installing MinGW GCC](#installing-mingw-gcc)
* [Installing GCC in Linux](#installing-gcc-in-linux)
* [Installing Xcode in macOS](#installing-xcode-in-macos)
* [Build Harbour](#build-harbour)
    - [Build Harbour in Windows Visual Studio](#build-harbour-in-windows-visual-studio)
    - [Build Harbour in Windows MinGW](#build-harbour-in-windows-mingw)
    - [Build Harbour in Linux](#build-harbour-in-linux)
    - [Build Harbour in macOS](#build-harbour-in-macos)
* [Build GTNap](#build-gtnap)
    - [In Windows with MinGW](#in-windows-with-mingw)
    - [In Windows with VisualStudio](#in-windows-with-visualstudio)
    - [In Linux with GCC](#in-linux-with-gcc)
    - [In macOS with Xcode](#in-macos-with-xcode)
* [Using GTNap](#using-gtnap)
* [Compile and run CUADEMO example](#compile-and-run-cuademo-example)
* [Application ICON](#application-icon)
   - [In Windows](#icon-in-windows)
   - [In Linux](#icon-in-linux)
* [GTNap developer mode](#gtnap-developer-mode)
   - [Step 1. Compile Harbour](#step-1-compile-harbour)
   - [Step 2. Generate exemplo C files](#step-2-generate-exemplo-c-files)
   - [Step 3. Debugging using Visual Studio (Windows)](#step-3-debugging-using-visual-studio-windows)
   - [Step 4. Debugging using VSCode (Linux)](#step-4-debugging-using-vscode-linux)
   - [Step 5. Debugging using Xcode (macOS)](#step-5-debugging-using-xcode-macos)
* [GTNap design](#gtnap-design)
* [GTNap Log](#gtnap-log)

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

### Build Harbour in Windows Visual Studio

```
:: Set the Visual Studio 64bit compiler
"%ProgramFiles(x86)%\Microsoft Visual Studio 11.0\VC\vcvarsall.bat" x64

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

win-make HB_COMPILER=mingw64

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

### Build Harbour in Linux

```
# Go to main folder of harbour working copy
cd harbour_nappgui

make

! Building Harbour 3.2.0dev from source - https://harbour.github.io
! MAKE: make 4.1 /bin/sh
! HB_HOST_PLAT: linux (x86_64)  HB_SHELL: sh
! LD_LIBRARY_PATH: /home/fran/harbour_nappgui/lib/linux/gcc:
! HB_PLATFORM: linux (x86_64) (auto-detected)
! HB_COMPILER: gcc (auto-detected: /usr/bin/)
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
gtnap.hbc           Options for proyects that use GTNap.
gtnap.hbp           GTNap project for hbmk2.
gtnap.hbx           GTNap symbol file, autogenerated by hbmk2.
Readme.md           This documentation.
```

### In Windows with MinGW

`build.bat` script allows to compile GTNAP using VisualStudio `msvc64` compiler or MinGW `mingw64` compiler.

```
:: Goto gtnap folder
cd contrib\gtnap

:: Just build
build.bat -b [Debug|Release] -comp mingw64
```

This will generate several static libraries:

* The GT library: `libgtnap.a` in `build/[Debug|Release]/lib` folder.
* The NAppGUI libraries: `libsewer.a`, `libosbs.a`, `libcore.a`, `libgeom2d.a`, `libdraw2d.a`, `libosgui.a`, `libgui.a`, `libosapp.a`, `libinet.a` in `build/[Debug|Release]/lib` folder.

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

:: Set Visual Studio 2012 CMake generator
set CMAKE_GENERATOR=Visual Studio 11 2012

:: Set Visual Studio 2012 32bit compiler for hbmk2 (msvc)
"%ProgramFiles(x86)%\Microsoft Visual Studio 11.0\VC\vcvarsall.bat"

:: Set Visual Studio 2012 64bit compiler for hbmk2 (msvc64)
"%ProgramFiles(x86)%\Microsoft Visual Studio 11.0\VC\vcvarsall.bat" x64

:: Just build
build.bat -b [Debug|Release] -comp msvc64
```

This will generate several static libraries:

* The GT library: `gtnap.lib` in `build/[Debug|Release]/lib` folder.
* The NAppGUI libraries: `sewer.lib`, `osbs.lib`, `core.lib`, `geom2d.lib`, `draw2d.lib`, `osgui.lib`, `gui.lib`, `osapp.lib`, `inet.lib` in `build/[Debug|Release]/lib` folder.

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
bash ./build.sh -b [Debug|Release]
```

This will generate several static libraries:

* The GT library: `libgtnap.a` in `build/[Debug|Release]/lib` folder.
* The NAppGUI libraries: `libsewer.a`, `libosbs.a`, `libcore.a`, `libgeom2d.a`, `libdraw2d.a`, `libosgui.a`, `libgui.a`, `libosapp.a`, `libinet.a` in `build/[Debug|Release]/lib` folder.

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

## Application ICON

Create two `.ico` files with transparent backgrounds. One of 48x48 (Linux) and another 256x256 (Windows). You can refer to the files in `/contrib/gtnap/tests/cuademo`.

![image](https://github.com/frang75/harbour_nappgui/assets/42999199/bbe22a2f-6c86-43fc-9bca-64ddf6ee2532)

### Icon in Windows

Create a `*.rc` file with the application name in the same code directory `example.rc`. The content of this file should point to the location of the icon:
```
APPLICATION_ICON ICON "..\\icon256.ico"
```

Add the `*.rc` to your `*.hbp` so that it compiles with the rest of the source files (for MSVC only).
```
...
stubs.prg
inkey_.prg
texto.prg

# Icon
{allmsvc}exemplo.rc
```
![application_icon_win](https://github.com/frang75/harbour_nappgui/assets/42999199/7455ccb8-08f9-4e59-93cc-d1fca0806ea5)

### Icon in Linux

Copy `icon48.ico` to the same directory as the executable and rename it the same as the executable (`example.ico`). NAppGUI looks for this file at runtime and will pass it to the GTK toolkit. No action is required at compile time.

![image](https://github.com/frang75/harbour_nappgui/assets/42999199/d136e7d4-072c-45c9-af02-49a112fb7ed2)

![application_icon_xfce](https://github.com/frang75/harbour_nappgui/assets/42999199/af3528c8-b232-437c-b307-cae9b401815c)

## GTNap developer mode

**(Only if you are working on gtnap development)**. Otherwise, you can skip this section.

Using the NAppGUI build system, based on CMake, it is possible to create a single Visual Studio/VSCode solution containing the source code and internal dependencies of: NAppGUI, GTNap and cuademo-example. Thanks to this, it is extremely easy to debug the entire stack and develop a use case that requires all the technologies.

### Step 1. Compile Harbour

Compile Harbour as we seen in previos sections.

**To use developer mode, harbour needs to be recompiled, as a new -keepc option has been introduced in hbmk2.**

### Step 2. Generate exemplo C files

cuademo-example is a Harbour application. We need the `*.c` files that `hbmk2` generates for the build/debug project. The files will be generated in `/src/example`.

* In Windows:

    ```
    :: Go to exemplo folder
    cd contrib\gtnap\tests\cuademo\gtnap_cualib

    :: Create exemplo folder in /src
    mkdir ..\..\..\src\exemplo

    :: Build exemplo preserving hbmk2 generated C files
    ..\..\..\..\..\bin\win\msvc\hbmk2 exemplo.hbp -debug -trace -keepc -workdir=..\..\..\src\exemplo -o..\..\..\build\exemplo exemplo.hbp
    ```

* In Linux:

    ```
    # Go to exemplo folder
    cd contrib/gtnap/tests/cuademo/gtnap_cualib

    # Create exemplo folder in /src
    mkdir ../../../src/exemplo

    # Build exemplo preserving hbmk2 generated C files
    ../../../../../bin/linux/gcc/hbmk2 exemplo.hbp -debug -trace -keepc -workdir=../../../src/exemplo -o../../../build/exemplo exemplo.hbp
    ```

* In macOS:

    ```
    # Go to exemplo folder
    cd contrib/gtnap/tests/cuademo/gtnap_cualib

    # Create exemplo folder in /src
    mkdir ../../../src/exemplo

    # Build exemplo preserving hbmk2 generated C files
    ../../../../../bin/darwin/clang/hbmk2 exemplo.hbp -debug -trace -keepc -workdir=../../../src/exemplo -o../../../build/exemplo exemplo.hbp
    ```

### Step 3. Debugging using Visual Studio (Windows)

* Open a terminal

    ```
    :: Goto gtnap folder
    cd contrib\gtnap

    :: Set the Visual Studio version
    "%ProgramFiles(x86)%\Microsoft Visual Studio 11.0\VC\vcvarsall.bat"

    :: Generate the solution
    cmake -S . -B build-dev -DCMAKE_WARN_VS11=OFF -DGTNAP_DEVELOPER_MODE=ON -DGTNAP_LIBREOFFICE=YES


    -- Building for: Visual Studio 11 2012
    -- The C compiler identification is MSVC 17.0.61030.0
    -- The CXX compiler identification is MSVC 17.0.61030.0
    -- Detecting C compiler ABI info
    -- Detecting C compiler ABI info - done
    -- Check for working C compiler: C:/Program Files (x86)/Microsoft Visual Studio 11.0/VC/bin/cl.exe - skipped
    -- Detecting C compile features
    -- Detecting C compile features - done
    -- Detecting CXX compiler ABI info
    -- Detecting CXX compiler ABI info - done
    -- Check for working CXX compiler: C:/Program Files (x86)/Microsoft Visual Studio 11.0/VC/bin/cl.exe - skipped
    -- Detecting CXX compile features
    -- Detecting CXX compile features - done
    -- ---------------------------------------------
    -- NAppGUI Cross-Platform SDK 1.3.1.4310
    -- 2015-2023 Francisco Garcia Collado
    -- MIT License
    -- ---------------------------------------------
    -- Visual Studio 11 2012
    -- - Platform Toolset: v110/x86
    -- - GTNAP Developer mode
    ...
    ```

* Go to `gtnap\build-dev` in explorer and double click in `Cuademo.sln`. This will open VS IDE.

   ![cuademo_solution](https://user-images.githubusercontent.com/42999199/235441790-76a70f65-2178-4d9f-8253-cef90c23f21e.png)

* Build->Build Solution

   ![build_solution](https://user-images.githubusercontent.com/42999199/235441823-26ad4413-43ec-4e23-a291-1fec9c4ad75d.png)

* In Solution Explorer, right click over `exemplo` project, then `Properties`, `Debugging`:
    - `Command Arguments`: `--hb:gtnap`.
    - `Working Directory`: `C:\harbour_nappgui\contrib\gtnap\tests\cuademo\gtnap_cualib`.

* Right click again over `exemplo`: `Set as Statup Project`.

* Debug->Start Debugging: This will launch the exemplo executable in Debug mode. You will be able to set breakpoints in any NAppGUI, GTNap or example source file, inspect variables, step by step, etc.

   ![debug_visual_studio](https://user-images.githubusercontent.com/42999199/235441869-da3fd867-8a35-4554-a526-4775c0b84f0b.png)

### Step 4. Debugging using VSCode (Linux)

* Install VSCode in Linux

    `sudo apt-get install code`

* Install C/C++ Extension Pack

    https://marketplace.visualstudio.com/items?itemName=ms-vscode.cpptools-extension-pack
    ![vscode_cpp_extension_pack](https://user-images.githubusercontent.com/42999199/235436515-4637b500-164a-49e1-a05a-7f2e32f2f2b9.png)

* **File->Open folder.** `gtnap` folder in `/contrib`. You will see a `.vscode` folder in `gtnap` with the config files to run and debug `exemplo`.

   ![gtnap_openfolder](https://user-images.githubusercontent.com/42999199/235436558-a89c0a7d-f1b5-4bd3-836b-2abd7f25c52e.png)
   ![gtnap_project_vscode](https://user-images.githubusercontent.com/42999199/235436626-91d0c0bb-515a-4db1-a5d9-db36adb69582.png)

* **[F1] CMake:Configure.** Will run CMake over `/src/CMakeLists.txt` and generate `build-dev` folder.

   ```
   [main] Configuring project: gtnap
   [cmake] ---------------------------------------------
   [cmake] NAppGUI Cross-Platform SDK 1.3.1.4310
   [cmake] 2015-2023 Francisco Garcia Collado
   [cmake] MIT License
   [cmake] ---------------------------------------------
   [cmake] Unix Makefiles
   [cmake] - Linux Platform: Ubuntu 18.04
   [cmake] - GCC compiler: 7.5.0
   [cmake] - Build architecture: x64
   [cmake] - Host architecture: x86_64
   [cmake] - Build Config: Debug
   [cmake] - Toolkit: Gtk+3
   [cmake] - GTNAP Developer mode
   ...
   ```

* **[F1] CMake:Build Target.** Select exemplo executable.

   ![cmake_build_target](https://user-images.githubusercontent.com/42999199/235436681-fd242bcd-c9c4-450b-8716-3cb030e6ce02.png)

* **Run and Debug:** button at VSCode left panel and Exemplo. This will launch the exemplo executable in Debug mode. You will be able to set breakpoints in any NAppGUI, GTNap or example source file, inspect variables, step by step, etc.

   ![debug_vscode](https://user-images.githubusercontent.com/42999199/235436730-8df78930-7aad-487a-a79a-f79d00363931.png)

### Step 5. Debugging using Xcode (macOS)

* Open a terminal

    ```
    # Goto gtnap folder
    cd contrib/gtnap

    # Generate the solution
    cmake -G Xcode -S . -B build-dev -DGTNAP_DEVELOPER_MODE=ON -DGTNAP_LIBREOFFICE=NO


    -- The C compiler identification is AppleClang 14.0.3.14030022
    -- The CXX compiler identification is AppleClang 14.0.3.14030022
    -- Detecting C compiler ABI info
    -- Detecting C compiler ABI info - done
    -- Check for working C compiler: /Volumes/SSD/Xcode14/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/clang - skipped
    -- Detecting C compile features
    -- Detecting C compile features - done
    -- Detecting CXX compiler ABI info
    -- Detecting CXX compiler ABI info - done
    -- Check for working CXX compiler: /Volumes/SSD/Xcode14/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/clang++ - skipped
    -- Detecting CXX compile features
    -- Detecting CXX compile features - done
    -- ---------------------------------------------
    -- NAppGUI Cross-Platform SDK
    -- 2015-2024 Francisco Garcia Collado
    -- MIT License
    -- ---------------------------------------------
    -- * Version: 1.4.1.4920
    -- * Generator: Xcode
    -- * Build shared: False
    -- * Base OSX: 13.3-Ventura
    -- * Deployment target OSX: 10.13-High Sierra
    ...
    ```
* Go to the generated `build-dev` folder and open the `NAppGUI.xcodeproj` with the Xcode solution.

   ![build_dev_macos](https://github.com/frang75/harbour_nappgui/assets/42999199/59ff03f7-40a3-4142-a479-09270d6ad598)

* Go to Product->Scheme->Edit scheme:
    - Arguments (+): `--hb:gtnap`
    - Options->Working directory: `/Users/fran/harbour_nappgui/contrib/gtnap/tests/cuademo/gtnap_cualib`

    ![arguments](https://github.com/frang75/harbour_nappgui/assets/42999199/8b836c01-93c8-4265-bd67-fc20324e3419)
    ![working_dir](https://github.com/frang75/harbour_nappgui/assets/42999199/0e176163-0e8c-44b7-99e6-9c6fc4eb772e)

* Select `exemplo` in target selector and press the Play button. This will launch the exemplo executable in Debug mode. You will be able to set breakpoints in any NAppGUI, GTNap or example source file, inspect variables, step by step, etc.

   ![debug_xcode](https://github.com/frang75/harbour_nappgui/assets/42999199/6a20067b-bcd7-4d16-ba5f-c3c18b177fb0)

## GTNap design

GTNAP has been designed with two modes of operation in mind:
   * **Full-Graphic:** Cross-platform "modern" look&feel applications. Not compatible with Harbour/Clipper text command neither CUALIB.
   * **Semi-Graphic:** Cross-platform "cualib-compatible" graphics layer. Improve the appearance of GTWVW solution and runs in Windows/Linux.

![GTNAP_Full_graphic](https://user-images.githubusercontent.com/42999199/202912012-ecc6d479-7455-4cca-85f6-05cd57730407.png)
![GTNAP_Semil_graphic](https://user-images.githubusercontent.com/42999199/202912021-e6ffcd8b-08b5-494f-ae1a-f78b6403bddb.png)

## GTNap Log

GTNAP generates a log file.

```
[17:39:13] Starting log for 'exemplo'
[17:39:13] i_gtnap_cualib_create(Exemplo das rotinas de janelamento, 35, 110)
[17:39:13] hb_gtnap_GetCursorStyle()
[17:39:13] hb_gtnap_SetCursorStyle(0)
[17:39:13] hb_gtnap_DispCount()
[17:39:13] hb_gtnap_GetBlink()
[17:39:13] hb_gtnap_SetBlink(0)
[17:39:13] hb_gtnap_MaxRow()
[17:39:13] hb_gtnap_MaxCol()
[17:39:13] hb_gtnap_MaxRow()
[17:39:13] hb_gtnap_MaxCol()
[17:39:13] hb_gtnap_Version(0)
[17:39:13] hb_gtnap_MaxRow()
[17:39:13] hb_gtnap_MaxRow()
[17:39:13] hb_gtnap_MaxCol()
[17:39:13] hb_gtnap_Version(0)
[17:39:13] hb_gtnap_GetCursorStyle()
[17:39:13] hb_gtnap_SetCursorStyle(0)
[17:39:13] hb_gtnap_DispCount()
[17:39:13] hb_gtnap_GetPos(0, 0). NO GTNAP Window!
[17:39:13] hb_gtnap_GetPos(0, 0). NO GTNAP Window!
[17:39:13] hb_gtnap_Version(0)
[17:39:14] hb_gtnap_MaxRow()
[17:39:14] hb_gtnap_MaxCol()
[17:39:14] hb_gt_wvw_Scroll(0, 0, 35, 110): Rows:(0) Cols:(0) Color(112)
[17:39:14] hb_gtnap_DispCount()
[17:39:14] hb_gtnap_Version(0)
[17:39:14] hb_gtnap_DispBegin()
[17:39:14] hb_gtnap_SetPos(0, 1)
[17:39:14] hb_gtnap_DispCount()
[17:39:14] hb_gtnap_GetPos(0, 1)
[17:39:14] hb_gtnap_WriteAt(0, 1, 109):                                           Escolha o tipo de janela
[17:39:14] hb_gtnap_DispCount()
[17:39:14] hb_gtnap_SetPos(0, 0)
[17:39:14] hb_gtnap_DispCount()
[17:39:14] hb_gtnap_GetPos(0, 0)
[17:39:14] hb_gtnap_WriteAt(0, 0, 10): SAY in 0,0
[17:39:14] hb_gtnap_DispCount()
[17:39:14] hb_gtnap_SetPos(1, 1)
[17:39:14] hb_gtnap_DispCount()
[17:39:14] hb_gtnap_GetPos(1, 1)
[17:39:14] hb_gtnap_WriteAt(1, 1, 10): SAY in 1,1
[17:39:14] hb_gtnap_DispCount()
[17:39:14] hb_gtnap_SetPos(2, 2)
[17:39:14] hb_gtnap_DispCount()
[17:39:14] hb_gtnap_GetPos(2, 2)
[17:39:14] hb_gtnap_WriteAt(2, 2, 10): SAY in 2,2
[17:39:14] hb_gtnap_DispCount()
[17:39:14] hb_gtnap_SetPos(3, 3)
[17:39:14] hb_gtnap_DispCount()
[17:39:14] hb_gtnap_GetPos(3, 3)
[17:39:14] hb_gtnap_WriteAt(3, 3, 10): SAY in 3,3
[17:39:14] hb_gtnap_DispCount()
[17:39:14] hb_gtnap_SetPos(4, 4)
[17:39:14] hb_gtnap_DispCount()
[17:39:14] hb_gtnap_GetPos(4, 4)
[17:39:14] hb_gtnap_WriteAt(4, 4, 10): SAY in 4,4
[17:39:14] hb_gtnap_DispCount()
[17:39:14] hb_gtnap_SetPos(5, 5)
[17:39:14] hb_gtnap_DispCount()
[17:39:14] hb_gtnap_GetPos(5, 5)
[17:39:14] hb_gtnap_WriteAt(5, 5, 10): SAY in 5,5
[17:39:14] hb_gtnap_DispCount()
[17:39:14] hb_gtnap_SetPos(6, 6)
[17:39:14] hb_gtnap_DispCount()
[17:39:14] hb_gtnap_GetPos(6, 6)
[17:39:14] hb_gtnap_WriteAt(6, 6, 10): SAY in 6,6
[17:39:14] hb_gtnap_DispCount()
[17:39:14] hb_gtnap_SetPos(7, 7)
[17:39:14] hb_gtnap_DispCount()
[17:39:14] hb_gtnap_GetPos(7, 7)
[17:39:14] hb_gtnap_WriteAt(7, 7, 10): SAY in 7,7
[17:39:14] hb_gtnap_DispCount()
[17:39:14] hb_gtnap_SetPos(8, 8)
[17:39:14] hb_gtnap_DispCount()
[17:39:14] hb_gtnap_GetPos(8, 8)
[17:39:14] hb_gtnap_WriteAt(8, 8, 10): SAY in 8,8
[17:39:14] hb_gtnap_DispCount()
[17:39:14] hb_gtnap_SetPos(9, 9)
[17:39:14] hb_gtnap_DispCount()
[17:39:14] hb_gtnap_GetPos(9, 9)
[17:39:14] hb_gtnap_WriteAt(9, 9, 10): SAY in 9,9
[17:39:14] hb_gtnap_DispCount()
[17:39:14] hb_gtnap_SetPos(10, 10)
[17:39:14] hb_gtnap_DispCount()
[17:39:14] hb_gtnap_GetPos(10, 10)
[17:39:14] hb_gtnap_WriteAt(10, 10, 12): SAY in 10,10
[17:39:14] hb_gtnap_DispCount()
[17:39:14] hb_gtnap_SetPos(11, 11)
[17:39:14] hb_gtnap_DispCount()
[17:39:14] hb_gtnap_GetPos(11, 11)
[17:39:14] hb_gtnap_WriteAt(11, 11, 12): SAY in 11,11
[17:39:14] hb_gtnap_DispCount()
[17:39:14] hb_gtnap_SetPos(12, 12)
[17:39:14] hb_gtnap_DispCount()
[17:39:14] hb_gtnap_GetPos(12, 12)
[17:39:14] hb_gtnap_WriteAt(12, 12, 12): SAY in 12,12
[17:39:14] hb_gtnap_DispCount()
[17:39:14] hb_gtnap_SetPos(13, 13)
[17:39:14] hb_gtnap_DispCount()
[17:39:14] hb_gtnap_GetPos(13, 13)
[17:39:14] hb_gtnap_WriteAt(13, 13, 12): SAY in 13,13
[17:39:14] hb_gtnap_DispCount()
[17:39:14] hb_gtnap_SetPos(14, 14)
[17:39:14] hb_gtnap_DispCount()
[17:39:14] hb_gtnap_GetPos(14, 14)
[17:39:14] hb_gtnap_WriteAt(14, 14, 12): SAY in 14,14
[17:39:14] hb_gtnap_DispCount()
[17:39:14] hb_gtnap_SetPos(15, 15)
[17:39:14] hb_gtnap_DispCount()
[17:39:14] hb_gtnap_GetPos(15, 15)
[17:39:14] hb_gtnap_WriteAt(15, 15, 12): SAY in 15,15
[17:39:14] hb_gtnap_DispCount()
[17:39:14] hb_gtnap_SetPos(16, 16)
[17:39:14] hb_gtnap_DispCount()
[17:39:14] hb_gtnap_GetPos(16, 16)
[17:39:14] hb_gtnap_WriteAt(16, 16, 12): SAY in 16,16
[17:39:14] hb_gtnap_DispCount()
[17:39:14] hb_gtnap_SetPos(17, 17)
[17:39:14] hb_gtnap_DispCount()
[17:39:14] hb_gtnap_GetPos(17, 17)
[17:39:14] hb_gtnap_WriteAt(17, 17, 12): SAY in 17,17
[17:39:14] hb_gtnap_DispCount()
[17:39:14] hb_gtnap_SetPos(18, 18)
[17:39:14] hb_gtnap_DispCount()
[17:39:14] hb_gtnap_GetPos(18, 18)
[17:39:14] hb_gtnap_WriteAt(18, 18, 12): SAY in 18,18
[17:39:14] hb_gtnap_DispCount()
[17:39:14] hb_gtnap_SetPos(19, 19)
[17:39:14] hb_gtnap_DispCount()
[17:39:14] hb_gtnap_GetPos(19, 19)
[17:39:14] hb_gtnap_WriteAt(19, 19, 12): SAY in 19,19
[17:39:14] hb_gtnap_DispCount()
[17:39:14] hb_gtnap_SetPos(20, 20)
[17:39:14] hb_gtnap_DispCount()
[17:39:14] hb_gtnap_GetPos(20, 20)
[17:39:14] hb_gtnap_WriteAt(20, 20, 12): SAY in 20,20
[17:39:14] hb_gtnap_DispCount()
[17:39:14] hb_gtnap_Version(0)
[17:40:08] hb_gtnap_GetCursorStyle()
[17:40:08] hb_gtnap_SetCursorStyle(1)
[17:40:08] hb_gtnap_DispCount()
[17:40:08] hb_gtnap_Version(0)
[17:40:08] hb_gtnap_SetPos(0, 0). NO GTNAP Window!
[17:40:08] hb_gtnap_DispCount()
[17:40:08] hb_gtnap_GetCursorStyle()
[17:40:08] hb_gtnap_SetCursorStyle(1)
[17:40:08] hb_gtnap_DispCount()
[17:40:08] hb_gtnap_Version(0)
[17:40:08] i_gtnap_cualib_destroy()
[17:40:08] [OK] Heap Memory Staticstics
[17:40:08] ============================
[17:40:08] Total a/dellocations: 190, 190
[17:40:08] Total bytes a/dellocated: 16991, 16991
[17:40:08] Max bytes allocated: 16099
[17:40:08] Effective reallocations: (0/4)
[17:40:08] Real allocations: 1 pages of 65536 bytes
[17:40:08] ============================
[17:40:08] Config: Debug
[17:40:08] You have an execution log in: 'C:\Users\USUARIO\AppData\Roaming\exemplo\log.txt'
[17:40:08] hb_gtnap_DispCount()
[17:40:08] hb_gtnap_DispCount()
[17:40:08] hb_gtnap_Exit()
```



