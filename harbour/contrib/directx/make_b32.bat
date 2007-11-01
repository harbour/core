@echo off
rem
rem $Id$
rem

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD

   make -fmakefile.bc %1 %2 %3 > make_b32.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   copy ..\..\lib\b32\hbwin32ddrw.lib ..\..\lib\*.* > nul
   if exist ..\..\lib\b32\hbwin32ddrw.bak del ..\..\lib\b32\hbwin32ddrw.bak
   goto EXIT

:BUILD_ERR

   notepad make_b32.log
   goto EXIT

:CLEAN
   if exist ..\..\lib\b32\hbwin32ddrw.lib del ..\..\lib\b32\hbwin32ddrw.lib
   if exist ..\..\lib\b32\hbwin32ddrw.bak del ..\..\lib\b32\hbwin32ddrw.bak
   if exist ..\..\obj\b32\w32_ddrw.obj    del ..\..\obj\b32\w32_ddrw.obj

   goto EXIT

:EXIT
