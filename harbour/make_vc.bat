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
rem    set HB_MAKE_PROGRAM=
rem    set HB_MAKE_FLAGS=
rem ---------------------------------------------------------------

if "%HB_MAKE_PROGRAM%" == "" set HB_MAKE_PROGRAM=nmake.exe

rem Save the user value, force silent file overwrite with COPY
rem (not all Windows versions support the COPY /Y flag)
set HB_ORGENV_COPYCMD=%COPYCMD%
set COPYCMD=/Y

rem ---------------------------------------------------------------

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

if "%1" == "install" goto INSTALL
if "%1" == "INSTALL" goto INSTALL

rem ---------------------------------------------------------------

:BUILD

   %HB_MAKE_PROGRAM% %HB_MAKE_FLAGS% /f makefile.vc %1 %2 %3 > make_vc.log
   if errorlevel 1 goto BUILD_ERR
   goto EXIT

rem ---------------------------------------------------------------

:BUILD_ERR

   notepad make_vc.log
   goto EXIT

rem ---------------------------------------------------------------

:CLEAN

   %HB_MAKE_PROGRAM% %HB_MAKE_FLAGS% /f makefile.vc CLEAN > make_vc.log

   rem In this case, the makefile handles most cleanup.

   if exist make_vc.log del make_vc.log
   goto EXIT

rem ---------------------------------------------------------------

:INSTALL

   if "%HB_INSTALL_PREFIX%" == "" set HB_INSTALL_PREFIX=.

   if "%HB_BIN_INSTALL%" == "" set HB_BIN_INSTALL=%HB_INSTALL_PREFIX%\bin
   if "%HB_INC_INSTALL%" == "" set HB_INC_INSTALL=%HB_INSTALL_PREFIX%\include
   if "%HB_LIB_INSTALL%" == "" set HB_LIB_INSTALL=%HB_INSTALL_PREFIX%\lib

   %HB_MAKE_PROGRAM% %HB_MAKE_FLAGS% /f makefile.vc INSTALL > nul
   goto EXIT

rem ---------------------------------------------------------------

:EXIT
rem Restore user value
set COPYCMD=%HB_ORGENV_COPYCMD%
set HB_ORGENV_COPYCMD=
