@echo off
rem
rem $Id$
rem

:BUILD

   make -fmakefile.bc %1 %2 %3 > make_b32.log
   if "%1" == "clean" goto CLEAN
   if "%1" == "CLEAN" goto CLEAN
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK
   goto EXIT

:BUILD_ERR
   notepad make_b32.log
   goto EXIT

:CLEAN
   if exist make_b32.log del make_b32.log

:EXIT

