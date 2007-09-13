@echo off
rem
rem $Id$
rem

if not exist obj md obj
if not exist obj\b32 md obj\b32

:BUILD

   make -fmakefile.bc %1 %2 %3 > make_b32.log
   if "%1" == "clean" goto CLEAN
   if "%1" == "CLEAN" goto CLEAN
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK
   copy ..\..\lib\b32\hbzip.lib ..\..\lib > nul
   goto EXIT

:BUILD_ERR

   notepad make_b32.log
   goto EXIT

:CLEAN
   if exist make_b32.log del make_b32.log

:EXIT

