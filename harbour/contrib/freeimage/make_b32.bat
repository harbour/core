@echo off
rem
rem $Id$
rem

rem ---------------------------------------------------------------
rem IMPORTANT: You'll need Freeimage headers and binary from here:
rem            http://freeimage.sourceforge.net/download.html
rem            and this envvar to be set to successfully build this library:
rem            set FREEIMAGE_DIR=C:\FreeImage
rem ---------------------------------------------------------------

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

if "%HB_CC_NAME%" == "" set HB_CC_NAME=b32
if "%HB_MAKE_PROGRAM%" == "" set HB_MAKE_PROGRAM=make.exe
if "%HB_SHOW_ERRORS%"  == "" set HB_SHOW_ERRORS=yes
set HB_MAKEFILE=..\mtpl_%HB_CC_NAME%.mak

set C_USR=%C_USR% -I%FREEIMAGE_DIR%\source -DHB_OS_WIN_32_USED

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

   implib ..\..\lib\%HB_CC_NAME%\FreeImage.lib %FREEIMAGE_DIR%\Dist\FreeImage.dll

   %HB_MAKE_PROGRAM% %HB_MAKE_FLAGS% -f %HB_MAKEFILE% %1 %2 %3 > make_%HB_CC_NAME%.log
   if errorlevel 1 if "%HB_SHOW_ERRORS%" == "yes" notepad make_%HB_CC_NAME%.log
   goto EXIT

:CLEAN

   %HB_MAKE_PROGRAM% %HB_MAKE_FLAGS% -f %HB_MAKEFILE% CLEAN > make_%HB_CC_NAME%.log
   if errorlevel 1 goto EXIT
   if exist make_%HB_CC_NAME%.log del make_%HB_CC_NAME%.log > nul
   if exist inst_%HB_CC_NAME%.log del inst_%HB_CC_NAME%.log > nul
   goto EXIT

:INSTALL

   set _HB_INSTALL_PREFIX=%HB_INSTALL_PREFIX%
   if "%_HB_INSTALL_PREFIX%" == "" set _HB_INSTALL_PREFIX=..\..
   set _HB_LIB_INSTALL=%HB_LIB_INSTALL%
   if "%_HB_LIB_INSTALL%" == "" set _HB_LIB_INSTALL=%_HB_INSTALL_PREFIX%\lib

   copy ..\..\lib\%HB_CC_NAME%\FreeImage.lib %_HB_LIB_INSTALL%

   %HB_MAKE_PROGRAM% %HB_MAKE_FLAGS% -f %HB_MAKEFILE% INSTALL > nul
   goto EXIT

:EXIT

rem ---------------------------------------------------------------

rem Restore user value
set COPYCMD=%HB_ORGENV_COPYCMD%
