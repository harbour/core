# GTNAP

Harbour cross-platform video subsystem using NAppGUI-SDK
https://github.com/frang75/nappgui_src

## Installing CMake

For building gtnap CMake tool is necessary:

* In Windows:
    * Download from https://cmake.org/download/
    * Select **Add CMake to the system PATH for all users** when installing.

* In Linux:
    * `sudo apt-get install cmake cmake-gui`

* Open a terminal/cmd and check if cmake works:
    ```
    :~/$ cmake --version
    cmake version 3.10.2
    ```
## Build gtnap

### In Windows
```
:: Goto gtnap folder
cd contrib/gtnap

:: Set the Visual Studio compiler
"%ProgramFiles(x86)%\Microsoft Visual Studio 11.0\VC\vcvarsall.bat"

:: Just build
build.bat -b Debug
build.bat -b Release
```

This will generate several static libraries:

* The GT library: `gtnap.lib` in `/build` folder.
* The NAppGUI libraries: `sewer.lib`, `osbs.lib`, `core.lib`, `geom2d.lib`, `draw2d.lib`, `osgui.lib`, `gui.lib`, `osapp.lib`, `inet.lib` in `build/[Debug|Release]` folder.

### In Linux
```
# Goto gtnap folder
cd contrib/gtnap

# Just build
bash ./build.sh -b Debug
bash ./build.sh -b Release
```

This will generate several static libraries:

* The GT library: `libgtnap.a` in `/build` folder.
* The NAppGUI libraries: `libsewer.a`, `libosbs.a`, `libcore.a`, `libgeom2d.a`, `libdraw2d.a`, `libosgui.a`, `libgui.a`, `libosapp.a`, `libinet.a` in `build/[Debug|Release]` folder.

## Using gtnap

Just adding `-gtnap` flag into your `.hbp` project file.

## Compile and run an CUADEMO example

- To compile the semi-graphics (Windows):
   * `cd contrib\gtnap\tests\cuademo\gtnap_cualib`
   * `..\..\..\..\..\bin\win\msvc\hbmk2.exe exemplo.hbp`
   * `exemplo --hb:gtnap`
   * `exemplo --hb:gtwin`

- To compile the semi-graphics (Linux):

   * `cd contrib/gtnap/tests/cuademo/gtnap_cualib`
   * `../../../../../bin/linux/gcc/hbmk2 exemplo.hbp`
   * `./exemplo --hb:gtnap`
   * `./exemplo --hb:gttrm`

![gtnap_cualib_semigraph_windows](https://user-images.githubusercontent.com/42999199/202914436-e1cc6c7e-e28a-4ba0-bb0e-169cc03b78fc.png)
![gtnap_cualib_gtwin](https://user-images.githubusercontent.com/42999199/202914447-1722dfe4-f340-4bd8-91a9-00824c0eb46c.png)

![gtnap_cualib_semigraph_linux](https://user-images.githubusercontent.com/42999199/202914463-76905df1-0e83-4e9d-b321-23c09dd8bbb0.png)

![gtnap_cualib_gttrm](https://user-images.githubusercontent.com/42999199/202914496-ae868c1c-460a-4be8-997a-f559ce5b7d18.png)



Just adding `-gtnap` flag into your `.hbp` project file.

For build the exemplo application:

```
cd ./tests/cuademo/gtnap_cualib

hbmk2 exemplo.hbp

./exemplo --hb:gtnap
```

../../bin/linux/gcc/hbmk2 exemplo.hbp

../../bin/linux/gcc/hbmk2 exemplo.hbp




:: Set the compiler
call "%ProgramFiles(x86)%\Microsoft Visual Studio 11.0\VC\vcvarsall.bat"

* At the moment the project compile in:
   * VisualStudio 2012 (windows)
   * Ubuntu 18 GCC 7.5.0 (linux)

GTNAP has been designed with two modes of operation in mind:
   * **Full-Graphic:** Cross-platform "modern" look&feel applications. Not compatible with Harbour/Clipper text command neither CUALIB.
   * **Semi-Graphic:** Cross-platform "cualib-compatible" graphics layer. Improve the appearance of GTWVW solution and runs in Windows/Linux.

![GTNAP_Full_graphic](https://user-images.githubusercontent.com/42999199/202912012-ecc6d479-7455-4cca-85f6-05cd57730407.png)
![GTNAP_Semil_graphic](https://user-images.githubusercontent.com/42999199/202912021-e6ffcd8b-08b5-494f-ae1a-f78b6403bddb.png)

## Compile Harbour

- Clone the harbour repository

```
git clone https://github.com/frang75/harbour_nappgui.git
```

- Compile in VS 2012:

    * Open Developer Command Prompt for VS2012

    * Go to ``harbour_nappgui`` working copy

    * ``win-make``

    ```
    ! Building Harbour 3.2.0dev from source - https://harbour.github.io
    ! MAKE: win-make 4.1 sh.exe
    ! HB_HOST_PLAT: win (x86_64)  HB_SHELL: nt
    ! HB_PLATFORM: win (x86) (auto-detected)
    ! HB_COMPILER: msvc (v1700) (auto-detected: C:/Program Files (x86)/Microsoft Visual Studio 11.0/VC/BIN/)
    ! Component: 'zlib' found in c:/harbour_gtnap/src/3rd/zlib (local)
    ...
    ```

- Compile in Ubuntu 18:

   * Open a Terminal

   * Go to ``harbour_nappgui`` working copy

   * ``make``

## Compile **GTNAP**

- Compile in VS 2012:

   * Open Developer Command Prompt for VS2012

   * Go to `gtnap` working copy `(cd C:\harbour_nappgui\contrib\gtnap)`

   * `..\..\bin\win\msvc\hbmk2.exe gtnap.hbp`

   * This will generate: `hbmk2: Target up to date: build\msvc\gtnap.lib`

- Compile in Ubuntu 18:

   * Open a Terminal

   * Go to `gtnap` working copy `(cd harbour_nappgui/contrib/gtnap)`

   * `../../bin/linux/gcc/hbmk2 gtnap.hbp`

   * This will generate: `hbmk2: Target up to date: build/gcc/libgtnap.a`


## Compile and run an CUADEMO example FULL GRAPHICS

- To compile the full-graphics (Windows):
   * `cd C:\harbour_nappgui\contrib\gtnap\tests\cuademo\gtnap_pure`
   * `..\..\..\..\..\bin\win\msvc\hbmk2.exe exemplo.hbp`
   * `exemplo`

- To compile the full-graphics (Linux):

   * `cd harbour_nappgui/contrib/gtnap/tests/cuademo/gtnap_pure`
   * `../../../../../bin/linux/gcc/hbmk2 exemplo.hbp`
   * `./exemplo`

![exemplo_full_graphics_windows](https://user-images.githubusercontent.com/42999199/202913860-fdb93eb1-08bd-42c1-ae8b-95687dd8b7fd.png)
![exemplo_full_graphics_linux](https://user-images.githubusercontent.com/42999199/202913872-9700e5c1-1763-4453-8907-d8531c829c1e.png)




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



