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

   copy ..\..\lib\b32\hbwin32prn.lib ..\..\lib\*.* > nul
   if exist ..\..\lib\b32\hbwin32prn.bak del ..\..\lib\b32\hbwin32prn.bak
   goto EXIT

:BUILD_ERR

   notepad make_b32.log
   goto EXIT

:CLEAN
   if exist ..\..\lib\b32\hbwin32prn.lib del ..\..\lib\b32\hbwin32prn.lib
   if exist ..\..\lib\b32\hbwin32prn.bak del ..\..\lib\b32\hbwin32prn.bak
   if exist ..\..\obj\b32\w32_papi.obj   del ..\..\obj\b32\w32_papi.obj
   if exist ..\..\obj\b32\w32_pcls.obj   del ..\..\obj\b32\w32_pcls.obj
   if exist ..\..\obj\b32\tprinter.obj   del ..\..\obj\b32\tprinter.obj

   goto EXIT

:EXIT
