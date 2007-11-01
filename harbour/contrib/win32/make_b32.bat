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

   copy ..\..\lib\b32\hbwin32.lib ..\..\lib\*.* > nul
   if exist ..\..\lib\b32\hbwin32.bak del ..\..\lib\b32\hbwin32.bak
   goto EXIT

:BUILD_ERR

   notepad make_b32.log
   goto EXIT

:CLEAN
   if exist ..\..\lib\b32\hbwin32.lib del ..\..\lib\b32\hbwin32.lib
   if exist ..\..\lib\b32\hbwin32.bak del ..\..\lib\b32\hbwin32.bak
   if exist ..\..\obj\b32\tprinter.obj   del ..\..\obj\b32\tprinter.obj
   if exist ..\..\obj\b32\w32_ole.obj    del ..\..\obj\b32\w32_ole.obj 
   if exist ..\..\obj\b32\w32_prn.obj    del ..\..\obj\b32\w32_prn.obj 
   if exist ..\..\obj\b32\w32_tole.obj   del ..\..\obj\b32\w32_tole.obj
   if exist ..\..\obj\b32\w32_tprn.obj   del ..\..\obj\b32\w32_tprn.obj

   goto EXIT

:EXIT
