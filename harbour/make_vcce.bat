@echo off
rem
rem $Id: make_b32.bat 7949 2007-11-11 17:08:43Z vszakats $
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
rem    set HB_MAKE_PROGRAM=
rem    set HB_MAKE_FLAGS=
rem    set HB_CC_NAME=
rem ---------------------------------------------------------------

if "%HB_CC_NAME%"      == "" set HB_CC_NAME=vcce
if "%HB_GT_LIB%"       == "" set HB_GT_LIB=gtwvt
if "%HB_GT_DEFAULT%"   == "" set HB_GT_DEFAULT=wvt
if "%HB_MAKE_PROGRAM%" == "" set HB_MAKE_PROGRAM=nmake.exe

set C_USR=%C_USR% -DHB_NO_WIN_CONSOLE

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

   %HB_MAKE_PROGRAM% %HB_MAKE_FLAGS% /f make_%HB_CC_NAME%.mak %1 %2 %3 > make_ce.log
   if errorlevel 1 notepad make_%HB_CC_NAME%.log
   goto EXIT

:CLEAN

   %HB_MAKE_PROGRAM% %HB_MAKE_FLAGS% /f make_%HB_CC_NAME%.mak CLEAN > make_ce.log
   if exist make_%HB_CC_NAME%.log del make_%HB_CC_NAME%.log > nul
   if exist inst_%HB_CC_NAME%.log del inst_%HB_CC_NAME%.log > nul
   goto EXIT

:INSTALL

   %HB_MAKE_PROGRAM% %HB_MAKE_FLAGS% /f make_%HB_CC_NAME%.mak INSTALL > nul
   goto EXIT

:EXIT

rem ---------------------------------------------------------------

rem Restore user value
set COPYCMD=%HB_ORGENV_COPYCMD%
