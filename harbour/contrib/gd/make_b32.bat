@echo off

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN
:BUILD

if not exist bgd.dll goto NODLL

   make -fmakefile.bc %1 %2 %3 > make_b32.log
   if errorlevel 1 goto BUILD_ERR

   impdef -a ..\..\lib\b32\libbgd.def bgd.dll > make_b32.log
   if errorlevel 1 goto BUILD_ERR

   implib -a ..\..\lib\b32\libbgd.lib ..\..\lib\b32\libbgd.def > make_b32.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   copy ..\..\lib\b32\libbgd.lib ..\..\lib > nul
   copy ..\..\lib\b32\hbgd.lib ..\..\lib > nul
   goto EXIT

:BUILD_ERR

   notepad make_b32.log
   goto EXIT

:CLEAN

   if exist ..\..\lib\b32\hbgd.lib      del ..\..\lib\b32\hbgd.lib
   if exist ..\..\lib\b32\hbgd.bak      del ..\..\lib\b32\hbgd.bak

   if exist ..\..\obj\b32\gdwrp.obj     del ..\..\obj\b32\gdwrp.obj

   if exist ..\..\obj\b32\gd.c          del ..\..\obj\b32\gd.c
   if exist ..\..\obj\b32\gdimage.c     del ..\..\obj\b32\gdimage.c
   if exist ..\..\obj\b32\gdchart.c     del ..\..\obj\b32\gdchart.c
   if exist ..\..\obj\b32\gdbar.c       del ..\..\obj\b32\gdbar.c
   if exist ..\..\obj\b32\gdbarcod.c    del ..\..\obj\b32\gdbarcod.c

   if exist ..\..\obj\b32\gd.obj        del ..\..\obj\b32\gd.obj
   if exist ..\..\obj\b32\gdimage.obj   del ..\..\obj\b32\gdimage.obj
   if exist ..\..\obj\b32\gdchart.obj   del ..\..\obj\b32\gdchart.obj
   if exist ..\..\obj\b32\gdbar.obj     del ..\..\obj\b32\gdbar.obj
   if exist ..\..\obj\b32\gdbarcod.obj  del ..\..\obj\b32\gdbarcod.obj
   goto EXIT


:NODLL

echo.
echo.Missing bgd.dll, please download it from:
echo.http://www.libgd.org/Downloads (Windows.DLL)
echo.
echo.Make aborted.
echo.

:EXIT

