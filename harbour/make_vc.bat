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
rem
rem Set any of the below settings to customize your build process:
rem    set HB_BUILD_MODE=C
rem    set HB_BUILD_DLL=yes
rem    set HB_BUILD_DEBUG=yes
rem    set HB_BUILD_VERBOSE=yes
rem    set HB_REBUILD_PARSER=yes
rem    set HB_MAKE_PROGRAM=
rem    set HB_SHOW_ERRORS=
rem    set HB_MAKE_FLAGS=
rem ---------------------------------------------------------------

set _HB_CC_NAME=%HB_CC_NAME%
set _HB_MAKE_PROGRAM=%HB_MAKE_PROGRAM%

if "%_HB_CC_NAME%"      == "" set _HB_CC_NAME=vc
if "%_HB_MAKE_PROGRAM%" == "" set _HB_MAKE_PROGRAM=nmake.exe

set _HB_MAKEFILE=make_%_HB_CC_NAME%.mak

rem ---------------------------------------------------------------

rem Save the user value, force silent file overwrite with COPY
rem (not all Windows versions support the COPY /Y flag)
set HB_ORGENV_COPYCMD=%COPYCMD%
set COPYCMD=/Y

rem ---------------------------------------------------------------

if "%1" == "clean" goto CLEAN
if "%1" == "Clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN
if "%1" == "install" goto INSTALL
if "%1" == "Install" goto INSTALL
if "%1" == "INSTALL" goto INSTALL

:BUILD

   %_HB_MAKE_PROGRAM% %HB_MAKE_FLAGS% -f %_HB_MAKEFILE% %1 %2 %3 > make_%_HB_CC_NAME%.log
   if errorlevel 1 set HB_EXIT_LEVEL=1
   if errorlevel 1 if not "%HB_SHOW_ERRORS%" == "no" notepad make_%_HB_CC_NAME%.log
   goto EXIT

:CLEAN

   %_HB_MAKE_PROGRAM% %HB_MAKE_FLAGS% -f %_HB_MAKEFILE% CLEAN > make_%_HB_CC_NAME%.log
   if errorlevel 1 set HB_EXIT_LEVEL=1
   if errorlevel 1 goto EXIT
   if exist make_%_HB_CC_NAME%.log del make_%_HB_CC_NAME%.log > nul
   if exist inst_%_HB_CC_NAME%.log del inst_%_HB_CC_NAME%.log > nul
   goto EXIT

:INSTALL

   %_HB_MAKE_PROGRAM% %HB_MAKE_FLAGS% -f %_HB_MAKEFILE% INSTALL > nul
   if errorlevel 1 set HB_EXIT_LEVEL=1
   goto EXIT

:EXIT

rem ---------------------------------------------------------------

rem Restore user value
set COPYCMD=%HB_ORGENV_COPYCMD%

set _HB_CC_NAME=
set _HB_MAKE_PROGRAM=
set _HB_MAKEFILE=
