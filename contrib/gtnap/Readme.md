# GTNap

Harbour cross-platform video subsystem using NAppGUI-SDK
https://github.com/frang75/nappgui_src

## Installing CMake

For building GTNap CMake tool is necessary:

* In Windows:
    * Download from https://cmake.org/download/
    * Select **Add CMake to the system PATH for all users** when installing.
    ![cmake_win](https://user-images.githubusercontent.com/42999199/235419286-0a6101f4-b43b-4e40-a3cb-c585fe908185.png)

* In Linux:
    * `sudo apt-get install cmake cmake-gui`

   * Open a terminal/cmd and check if cmake works:
      ```
      :~/$ cmake --version
      cmake version 3.10.2
      ```

* In macOS:
    * Download from https://cmake.org/download/
    * Move `CMake.app` to `/Applications` folder.
    * By default, CMake does not configure command line access on macOS. You can create symbolic links with `sudo "/Applications/CMake.app/Contents/bin/cmake-gui" --install`.
    ![cmake_macos](https://github.com/frang75/harbour_nappgui/assets/42999199/0c25cc32-faf9-4e81-a949-6c5ac1b67bb5)

   * Open a terminal/cmd and check if cmake works:
      ```
      % cmake --version
      cmake version 3.21.4
      ```

## LibreOffice-SDK support

As of November 23, GTNap adds support for the LibreOffice-SDK, in order to add capabilities for editing documents from Harbour. It is necessary to **correctly install the LibreOffice package**, both on the development machines and on the client machines.

* In Windows:
    * Install the LibreOffice package. This installation is **required on both development machines and user machines**.
      ![download_libreoffice](https://github.com/frang75/harbour_nappgui/assets/42999199/c410187b-3f27-473e-b756-4dce9b91fecd)

    * Install the LibreOffice development libraries. This installation is **required ONLY for compile GTNAP in development machines**.
      > **Important:** LibreOffice-SDK is available in 32-bit and 64-bit versions. You will need to compile GTNap in 32 or 64 bits depending on the version of LibreOffice. It is not possible to mix 32 and 64 libraries in the same executable. In this tutorial we use the **64bit** version.

      ![download_libreoffice_sdk](https://github.com/frang75/harbour_nappgui/assets/42999199/4821de74-7e38-486a-94f6-ffd59d0f14a0)

    * Set the `LIBREOFFICE_HOME` environment variable with the path to the LibreOffice home directory (usually `C:\Program Files\LibreOffice`). This environment variable is required both to compile the program and to run it on the user's machines. GTNAP will connect to the LibreOffice program at runtime.

      ![envvar_libreoffice](https://github.com/frang75/harbour_nappgui/assets/42999199/3ad38b78-9214-4567-94b8-94dcf926848f)

   * Add `%LIBREOFFICE_HOME%/program` path to `PATH` environment variable. In order to run `exemplo` or any GTNAP-based application, LibreOffice .DLLs must be accesible and located.

      ![path_envvar](https://github.com/frang75/harbour_nappgui/assets/42999199/d0215a5e-8569-4dca-a313-f765ada84080)


* In Linux:
    * Install the LibreOffice package. This installation is **required on both development machines and user machines**.
        ```
        sudo apt-get install libreoffice
        ```
    * Install the LibreOffice development libraries. This installation is **required ONLY for compile GTNAP in development machines**.
        ```
        sudo apt-get install libreoffice-dev

        # Optional, not mandatory for compile
        sudo apt-get install libreoffice-dev-doc
        ```
    * Set the `LIBREOFFICE_HOME` environment variable with the path to the LibreOffice home directory (usually `/usr/lib/libreoffice`). This environment variable is required both to compile the program and to run it on the user's machines. GTNAP will connect to the LibreOffice program at runtime. It is recommended to define this variable in the `.bashrc` so that it is always present.
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

   * Add `$LIBREOFFICE_HOME$/program` path to `LD_LIBRARY_PATH` environment variable. In order to run `exemplo` or any GTNAP-based application, LibreOffice shared libraries `.so` must be accesible and located.
        ```
        # Add at the end of .bashrc
        export LD_LIBRARY_PATH=$LD_LIBRARY_PATH$:$LIBREOFFICE_HOME$/program
        ```

> **Important:** GTNAP-based programs will not be able to compile or run if LibreOffice is not correctly installed.

> **Important:** The `LIBREOFFICE_HOME` environment variable must be set and pointing to the LibreOffice home directory. e.g. `/usr/lib/libreoffice`, `C:\Program Files\LibreOffice`

> **Important:** The first time a GTNAP program uses a LibreOffice function, an instance of the LibreOffice application will be started invisibly (`soffice.bin` process). This first call will have a small delay due to the initialization of the process. It is imperative that LibreOffice is running in order to use the SDK from C++/Harbour/GTNAP.

> **Important:** In order to run `exemplo` or any GTNAP-based application, the `LD_LIBRARY_PATH` (Linux) or `PATH` (Windows) environment variable must point to `{LIBREOFFICE_HOME}/program`. e.g.
`/usr/lib/libreoffice/program`, `C:\Program Files\LibreOffice\program`.

> **Important:** A new directory has been created for the LibreOffice example files (read/write) `/tests/cuademo/office`.

## Build GTNap

The `/contrib/gtnap` project consists of several files and folders:

```
/prj                NAppGUI CMake-based build system.
/src                Source code of NAppGUI libraries and gtnap terminal.
/tests              Testing and examples.
build.bat           GTNap build script for Windows.
build.sh            GTNap build script for Linux.
CMakeLists.txt      Main script for CMake build system.
CMakeTargets.cmake  NAppGUI/GTNap build targets for CMake.
gtnap.ch            Predefined constants for GTNAp.
gtnap.hbc           Options for proyects that use GTNap.
gtnap.hbp           GTNap project for hbmk2.
gtnap.hbx           GTNap symbol file, autogenerated by hbmk2.
Readme.md           This documentation.
```

### In Windows

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
:: default Debug
:: default x64 (on x64 Windows SO)
:: default Win32 (on x86 Windows SO)
build.bat -b [Debug|Release] -a [x64|Win32]
```

This will generate several static libraries:

* The GT library: `gtnap.lib` in `/build` folder.
* The NAppGUI libraries: `sewer.lib`, `osbs.lib`, `core.lib`, `geom2d.lib`, `draw2d.lib`, `osgui.lib`, `gui.lib`, `osapp.lib`, `inet.lib` in `build/[Debug|Release]/lib` folder.
* The LibreOffice wrapper library `officesdk.lib` in `build/[Debug|Release]/lib` folder.

### In Linux
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
bash ./build.sh -b Debug
bash ./build.sh -b Release
```

This will generate several static libraries:

* The GT library: `libgtnap.a` in `/build` folder.
* The NAppGUI libraries: `libsewer.a`, `libosbs.a`, `libcore.a`, `libgeom2d.a`, `libdraw2d.a`, `libosgui.a`, `libgui.a`, `libosapp.a`, `libinet.a` in `build/[Debug|Release]/lib` folder.
* The LibreOffice wrapper library `libofficesdk.a` in `build/[Debug|Release]/lib` folder.


### In macOS

First of all, install Xcode.

![xcode_macos](https://github.com/frang75/harbour_nappgui/assets/42999199/cf140563-49df-4c3f-acef-24149fda9382)

Check Xcode is working:
```
% xcode-select -p
/Applications/Xcode.app/Contents/Developer
```

Then, compile gtnap (same as Linux)
```
# Goto gtnap folder
cd contrib/gtnap

# Just build
bash ./build.sh -b Debug
bash ./build.sh -b Release
```

This will generate several static libraries:

* The GT library: `libgtnap.a` in `/build` folder.
* The NAppGUI libraries: `libsewer.a`, `libosbs.a`, `libcore.a`, `libgeom2d.a`, `libdraw2d.a`, `libosgui.a`, `libgui.a`, `libosapp.a`, `libinet.a` in `build/[Debug|Release]/lib` folder.

## Using GTNap

Just adding `-gtnap` flag into your `.hbp` project file.

## Compile and run CUADEMO example

- To compile in Windows:
   ```
   :: Set 64bit compiler
   "%ProgramFiles(x86)%\Microsoft Visual Studio 11.0\VC\vcvarsall.bat" x64

   cd contrib\gtnap\tests\cuademo\gtnap_cualib
   ..\..\..\..\..\bin\win\msvc64\hbmk2.exe exemplo.hbp
   exemplo --hb:gtnap
   exemplo --hb:gtwin
   ```

- To compile in Linux:
   ```
   cd contrib/gtnap/tests/cuademo/gtnap_cualib
   ../../../../../bin/linux/gcc/hbmk2 exemplo.hbp
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
![gt_win](https://user-images.githubusercontent.com/42999199/235419608-10188d35-8283-4419-89e2-76def4a13cb4.png)
![gtrtrm_linux](https://user-images.githubusercontent.com/42999199/235419619-9dd0093e-bdd5-4ae8-aac1-1a3b55f96125.png)

## Application ICON

Create two `.ico` files with transparent backgrounds. One of 48x48 (Linux) and another 256x256 (Windows). You can refer to the files in `/contrib/gtnap/tests/cuademo`.

![image](https://github.com/frang75/harbour_nappgui/assets/42999199/bbe22a2f-6c86-43fc-9bca-64ddf6ee2532)

### In Windows

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

### In Linux

Copy `icon48.ico` to the same directory as the executable and rename it the same as the executable (`example.ico`). NAppGUI looks for this file at runtime and will pass it to the GTK toolkit. No action is required at compile time.

![image](https://github.com/frang75/harbour_nappgui/assets/42999199/d136e7d4-072c-45c9-af02-49a112fb7ed2)

![application_icon_xfce](https://github.com/frang75/harbour_nappgui/assets/42999199/af3528c8-b232-437c-b307-cae9b401815c)

## GTNap developer mode

**(Only if you are working on gtnap development)**. Otherwise, you can skip this section.

Using the NAppGUI build system, based on CMake, it is possible to create a single Visual Studio/VSCode solution containing the source code and internal dependencies of: NAppGUI, GTNap and cuademo-example. Thanks to this, it is extremely easy to debug the entire stack and develop a use case that requires all the technologies.

### Step 1. Compile Harbour

**To use developer mode, harbour needs to be recompiled, as a new -keepc option has been introduced in hbmk2.**

* In Windows:
    ```
    :: Set the Visual Studio 64bit compiler
    "%ProgramFiles(x86)%\Microsoft Visual Studio 11.0\VC\vcvarsall.bat" x64

    :: Go to main folder of harbour working copy
    cd harbour_nappgui

    win-make

    ! Building Harbour 3.2.0dev from source - https://harbour.github.io
    ! MAKE: win-make 4.1 sh.exe
    ! HB_HOST_PLAT: win (x86_64)  HB_SHELL: nt
    ! HB_PLATFORM: win (x86_64) (auto-detected)
    ! HB_COMPILER: msvc64 (v1700) (auto-detected: C:/Program Files (x86)/Microsoft Visual Studio 11.0/VC/BIN/amd64/)
    ! Component: 'zlib' found in C:/harbour_nappgui/src/3rd/zlib (local)
    ! Component: 'pcre' found in C:/harbour_nappgui/src/3rd/pcre (local)
    ...
    ```

* In Linux:
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

* In macOS:
    ```
    # Go to main folder of harbour working copy
    cd harbour_nappgui

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

### Step 3. Debugging using Visual Studio (Windows)

* Open a terminal

    ```
    :: Goto gtnap folder
    cd contrib\gtnap

    :: Set the Visual Studio version
    "%ProgramFiles(x86)%\Microsoft Visual Studio 11.0\VC\vcvarsall.bat"

    :: Generate the solution
    cmake -S src -B build-dev -DCMAKE_WARN_VS11=OFF -DGTNAP_DEVELOPER_MODE=ON


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

* In Solution Explorer, right click over `exemplo` project, then `Properties`, `Debugging`, `Command Arguments`: `--hb:gtnap`.

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

### GTNap design

GTNAP has been designed with two modes of operation in mind:
   * **Full-Graphic:** Cross-platform "modern" look&feel applications. Not compatible with Harbour/Clipper text command neither CUALIB.
   * **Semi-Graphic:** Cross-platform "cualib-compatible" graphics layer. Improve the appearance of GTWVW solution and runs in Windows/Linux.

![GTNAP_Full_graphic](https://user-images.githubusercontent.com/42999199/202912012-ecc6d479-7455-4cca-85f6-05cd57730407.png)
![GTNAP_Semil_graphic](https://user-images.githubusercontent.com/42999199/202912021-e6ffcd8b-08b5-494f-ae1a-f78b6403bddb.png)

# GTNAP Log

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



