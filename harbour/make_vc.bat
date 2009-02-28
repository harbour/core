@rem
@rem $Id$
@rem

rem ---------------------------------------------------------------
rem This file is kept for compatibility with old non-GNU make
rem system. Please read INSTALL how to migrate to the GNU make
rem based one.
rem
rem ATTENTION: For this to work, you will need the GNU make.exe
rem            (MinGW build is fine) in your PATH _before_ the
rem            compiler tools.
rem ---------------------------------------------------------------

@echo off

if "%HB_BUILD_WINCE%" == "yes" set HB_COMPILER=msvcce

if "%HB_INSTALL_PREFIX%" == "" set HB_INSTALL_PREFIX=%~dp0
set _HB_CC_NAME=%HB_CC_NAME%
if "%_HB_CC_NAME%" == "" set _HB_CC_NAME=vc

rem ; NOTE: For complete compatibility please uncomment line below:
rem if "%HB_BUILD_DLL%" == "" set HB_BUILD_DLL=yes

call make_gnu.bat > make_%_HB_CC_NAME%.log

set _HB_CC_NAME=

if exist hbpostmk.bat call hbpostmk.bat
