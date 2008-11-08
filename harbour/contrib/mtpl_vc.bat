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
rem    set MK_USR=
rem ---------------------------------------------------------------

set _HB_CC_NAME=%HB_CC_NAME%
set _HB_MAKE_PROGRAM=%HB_MAKE_PROGRAM%
set _HB_MAKEFILE=%HB_MAKEFILE%

if "%_HB_CC_NAME%"      == "" set _HB_CC_NAME=vc
if "%_HB_MAKE_PROGRAM%" == "" set _HB_MAKE_PROGRAM=nmake.exe
if "%_HB_MAKEFILE%"     == "" set _HB_MAKEFILE=..\mtpl_vc.mak

set _HB_MAKELOG=make_%_HB_CC_NAME%.log
set HB_EXIT_LEVEL=

rem ---------------------------------------------------------------

if "%1" == "clean" goto CLEAN
if "%1" == "Clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN
if "%1" == "install" goto INSTALL
if "%1" == "Install" goto INSTALL
if "%1" == "INSTALL" goto INSTALL

:BUILD

   %_HB_MAKE_PROGRAM% %MK_USR% -nologo -f %_HB_MAKEFILE% %1 %2 %3 > %_HB_MAKELOG%
   if errorlevel 1 set HB_EXIT_LEVEL=1
   if errorlevel 1 if not "%HB_SHOW_ERRORS%" == "no" notepad %_HB_MAKELOG%
   goto EXIT

:CLEAN

   %_HB_MAKE_PROGRAM% %MK_USR% -nologo -f %_HB_MAKEFILE% CLEAN > %_HB_MAKELOG%
   if errorlevel 1 set HB_EXIT_LEVEL=1
   if errorlevel 1 goto EXIT

   if exist %_HB_MAKELOG% del %_HB_MAKELOG% > nul
   goto EXIT

:INSTALL

   %_HB_MAKE_PROGRAM% %MK_USR% -nologo -f %_HB_MAKEFILE% INSTALL > nul
   if errorlevel 1 set HB_EXIT_LEVEL=1
   goto EXIT

:EXIT
