@echo off
rem 
rem $Id$
rem 

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD

   nmake /f makefile.vc %1 %2 %3 > make_vc.log
   if errorlevel 1 notepad make_vc.log
   goto EXIT

:CLEAN

   echo Y | del bin\*.exe > nul
   echo Y | del obj\vc\*.* > nul
   echo Y | del lib\vc\*.* > nul
   goto EXIT

:EXIT

