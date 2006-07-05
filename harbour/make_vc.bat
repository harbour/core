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
%rem% Usage: make_vc [CLEAN or <TARGET>]
%rem% Call with nothing or CLEAN
%rem% nothing - compiles what needs it.
%rem% CLEAN, delete targets.
%rem% ---------------------------------------------------------------
set rem=
if "%1"=="/?" goto EXIT

rem ---------------------------------------------------------------

if "%MAKE_PROGRAM%" == "" set MAKE_PROGRAM=nmake.exe

rem ---------------------------------------------------------------

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

rem ---------------------------------------------------------------

rem Unblock any of the below settings
rem if you want to costumize your build
rem process

rem set HB_BUILD_MODE=P
rem set HB_BUILD_DLL=yes
rem set HB_DEBUG_BUILD=yes
rem set SHOW_COMMANDS=yes

rem ---------------------------------------------------------------

:BUILD
   if not exist obj\nul         md obj
   if not exist obj\vc\nul      md obj\vc
   if not exist obj\dll\nul     md obj\dll
   if not exist obj\dll\vc\nul  md obj\dll\vc
   if not exist lib\nul         md lib
   if not exist lib\vc\nul      md lib\vc
   if not exist bin\nul         md bin
   if not exist bin\vc\nul      md bin\vc

   %MAKE_PROGRAM% %MKFLAGS% /f makefile.vc %1 %2 %3 >make_vc.log
   if errorlevel 1 goto BUILD_ERR

rem ---------------------------------------------------------------

:BUILD_OK
   copy bin\vc\*.exe bin\*.* > nul
   copy lib\vc\*.lib lib\*.* > nul
   goto EXIT

rem ---------------------------------------------------------------

:BUILD_ERR
   notepad make_vc.log
   goto EXIT

rem ---------------------------------------------------------------

:CLEAN
   %MAKE_PROGRAM% %MKFLAGS% /f makefile.vc clean

   rem in this case, the makefile handles
   rem most cleanup. Add what you need here

   if exist make_vc.log del make_vc.log
   goto EXIT

rem ---------------------------------------------------------------

:EXIT
   set HB_BUILD_MODE=
   set HB_BUILD_DLL=
   set HB_DEBUG_BUILD=
   set SHOW_COMMANDS=
   set MAKE_PROGRAM=
