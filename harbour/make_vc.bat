@echo off
rem 
rem $Id$
rem 

rem ---------------------------------------------------------------
rem This is a generic template file, if it doesn't fit your own needs 
rem please DON'T MODIFY IT.
rem
rem Instead, make a local copy and modify that one, or make a call to 
rem this batch file from your customized one. [vszakats]
rem ---------------------------------------------------------------

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD

   nmake /f makefile.vc %1 %2 %3 > make_vc.log
   if errorlevel 1 goto BUILD_ERR
   cd contrib\libmisc
   nmake /f makefile.vc %1 %2 %3 >> make_vc.log
   if errorlevel 1 goto BUILD_ERR
    cd..\..

:BUILD_OK

   copy bin\vc\*.exe bin\*.* > nul
   copy lib\vc\*.lib lib\*.* > nul
   goto EXIT

:BUILD_ERR

   notepad make_vc.log
   goto EXIT

:CLEAN

   if exist bin\vc\*.exe del bin\vc\*.exe
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

