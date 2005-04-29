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
%rem% Usage: make_vc64 [/y] [/a or CLEAN or other specific target]
%rem% Call with nothing, /Y, /A, or CLEAN
%rem% nothing - compiles what needs it.
%rem% CLEAN, delete targets.
%rem% /A clean, then compile all
%rem% /Y non batch mode (forces makefile.vc)
%rem% ---------------------------------------------------------------
set rem=
if "%1"=="/?" goto exit

if not exist obj md obj
if not exist obj\vc64 md obj\vc64
if not exist lib md lib
if not exist lib\vc64 md lib\vc64
if not exist bin md bin
if not exist bin\vc64 md bin\vc64

set MK_FILE=makefile.vc
if "%OS%" == "Windows_NT" set MK_FILE=makefile64.nt
if "%1" == "/Y" set MK_FILE=makefile64.vc
if "%1" == "/y" set MK_FILE=makefile64.vc
if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD

   nmake /f %MK_FILE% %1 %2 %3 > make_vc64.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   copy bin\vc64\*.exe bin\*.* > nul
   copy lib\vc64\*.lib lib\*.* > nul
   goto EXIT

:BUILD_ERR

   notepad make_vc64.log
   goto EXIT

:CLEAN

   nmake /f %MK_FILE% %1
   rem in this case, the makefile handles most cleanup. Add what you need here
   if exist make_vc64.log del make_vc64.log
   rem etc.

:EXIT
SET MK_FILE=
