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

set rem=rem
if "%1"=="/?" set rem=echo.
%rem% ---------------------------------------------------------------
%rem% Usage: make_vc [/y] [/a or CLEAN or other specific target]
%rem% Call with nothing, /Y, /A, or CLEAN
%rem% nothing - compiles what needs it.
%rem% CLEAN, delete targets.
%rem% /A clean, then compile all
%rem% /Y non batch mode (forces makefile.vc)
%rem% ---------------------------------------------------------------
set rem=
if "%1"=="/?" goto exit

if not exist obj md obj
if not exist obj\vc md obj\vc
if not exist lib md lib
if not exist lib\vc md lib\vc
if not exist bin md bin
if not exist bin\vc md bin\vc

set MK_FILE=makefile.vc
if "%OS%" == "Windows_NT" set MK_FILE=makefile.nt
if "%1" == "/Y" set MK_FILE=makefile.vc
if "%1" == "/y" set MK_FILE=makefile.vc
if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD

   nmake /f %MK_FILE% %1 %2 %3 > make_vc.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   copy bin\vc\*.exe bin\*.* > nul
   copy lib\vc\*.lib lib\*.* > nul
   goto EXIT

:BUILD_ERR

   notepad make_vc.log
   goto EXIT

:CLEAN

   nmake /f %MK_FILE% %1
   rem in this case, the makefile handles most cleanup. Add what you need here
   if exist make_vc.log del make_vc.log
   rem etc.

:EXIT
SET MK_FILE=
