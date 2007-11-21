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
rem    set HB_MAKE_PROGRAM=
rem    set HB_MAKE_FLAGS=
rem ---------------------------------------------------------------

if "%HB_CC_NAME%"      == "" set HB_CC_NAME=vc
if "%HB_MAKE_PROGRAM%" == "" set HB_MAKE_PROGRAM=nmake.exe
if "%HB_SHOW_ERRORS%"  == "" set HB_SHOW_ERRORS=yes
set HB_MAKEFILE=..\mtpl_%HB_CC_NAME%.mak

rem ---------------------------------------------------------------

rem Save the user value, force silent file overwrite with COPY
rem (not all Windows versions support the COPY /Y flag)
set HB_ORGENV_COPYCMD=%COPYCMD%
set COPYCMD=/Y

rem ---------------------------------------------------------------

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN
if "%1" == "install" goto INSTALL
if "%1" == "INSTALL" goto INSTALL

:BUILD

   %HB_MAKE_PROGRAM% %HB_MAKE_FLAGS% -f %HB_MAKEFILE% %1 %2 %3 > make_%HB_CC_NAME%.log
   if errorlevel 1 if "%HB_SHOW_ERRORS%" == "yes" notepad make_%HB_CC_NAME%.log
   if errorlevel 1 set HB_EXIT_LEVEL=1
   goto EXIT

:CLEAN

   %HB_MAKE_PROGRAM% %HB_MAKE_FLAGS% -f %HB_MAKEFILE% CLEAN > make_%HB_CC_NAME%.log
   if errorlevel 1 set HB_EXIT_LEVEL=1
   if errorlevel 1 goto EXIT

   if exist make_%HB_CC_NAME%.log del make_%HB_CC_NAME%.log > nul
   if exist inst_%HB_CC_NAME%.log del inst_%HB_CC_NAME%.log > nul
   goto EXIT

:INSTALL

   %HB_MAKE_PROGRAM% %HB_MAKE_FLAGS% -f %HB_MAKEFILE% INSTALL > nul
   if errorlevel 1 set HB_EXIT_LEVEL=1
   if errorlevel 1 goto EXIT
   goto EXIT

:EXIT

rem ---------------------------------------------------------------

rem Restore user value
set COPYCMD=%HB_ORGENV_COPYCMD%

if "%HB_EXIT_LEVEL%" == "1" exit 1
