@echo off
rem 
rem $Id$
rem 

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD

   nmake /f makefile.vc %1 %2 %3 > make_vc.log
   notepad make_vc.log
   goto EXIT

:CLEAN

   echo Y | del bin\*.exe > NUL
   echo Y | del obj\vc\*.* > NUL
   echo Y | del lib\vc\*.* > NUL
   goto EXIT

:EXIT

