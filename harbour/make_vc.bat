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

   if exist bin\vc\*.dll copy bin\vc\*.dll bin\*.* > nul
   copy bin\vc\*.exe bin\*.* > nul
   copy lib\vc\*.lib lib\*.* > nul
   goto EXIT

:BUILD_ERR

   notepad make_vc.log
   goto EXIT

:CLEAN

   if exist bin\vc\*.exe del bin\vc\*.exe
   if exist bin\vc\*.dll del bin\vc\*.dll
   if exist bin\vc\*.pdb del bin\vc\*.pdb
   if exist bin\vc\*.ilk del bin\vc\*.ilk
   if exist bin\vc\*.map del bin\vc\*.map
   if exist lib\vc\*.lib del lib\vc\*.lib
   if exist obj\vc\*.obj del obj\vc\*.obj
   if exist obj\vc\*.c   del obj\vc\*.c
   if exist obj\vc\*.h   del obj\vc\*.h
   if exist obj\vc\*.pch del obj\vc\*.pch
   if exist make_vc.log  del make_vc.log
   goto EXIT

:EXIT

