@echo off
rem 
rem $Id$
rem 

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD

   nmake /f makefile.vc %1 %2 %3 > make_vc.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   copy bin\vc\*.exe bin\ > nul
   copy lib\vc\*.lib lib\ > nul
   goto EXIT

:BUILD_ERR

   notepad make_vc.log
   goto EXIT

:CLEAN

   echo Y | del bin\vc\*.* > nul
   echo Y | del obj\vc\*.* > nul
   echo Y | del lib\vc\*.* > nul
   echo Y | del make_vc.log > nul
   goto EXIT

:EXIT

