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

   copy bin\b32\*.exe bin\ > nul
   copy lib\b32\*.lib lib\ > nul
   goto EXIT

:BUILD_ERR

   notepad make_b32.log
   goto EXIT

:CLEAN

   echo Y | del bin\b32\*.* > nul
   echo Y | del obj\b32\*.* > nul
   echo Y | del lib\b32\*.* > nul
   echo Y | del make_b32.log > nul
   goto EXIT

:EXIT

