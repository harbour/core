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

   copy ..\..\lib\b32\rddads.lib ..\..\lib\*.* > nul
   goto EXIT

:BUILD_ERR

   notepad make_b32.log
   goto EXIT

:CLEAN
   if exist ..\..\lib\b32\rddads.lib   del ..\..\lib\b32\rddads.lib
   if exist ..\..\lib\b32\rddads.bak   del ..\..\lib\b32\rddads.bak
   if exist ..\..\obj\b32\ads1.obj     del ..\..\obj\b32\ads1.obj
   if exist ..\..\obj\b32\adsfunc.obj  del ..\..\obj\b32\adsfunc.obj
   if exist ..\..\obj\b32\adsmgmnt.obj del ..\..\obj\b32\adsmgmnt.obj

   goto EXIT

:EXIT

