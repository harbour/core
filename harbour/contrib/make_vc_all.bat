@echo off
rem
rem $Id$
rem

rem ---------------------------------------------------------------

if "%HB_CC_NAME%"      == "" set HB_CC_NAME=vc
if "%HB_MAKE_PROGRAM%" == "" set HB_MAKE_PROGRAM=nmake.exe
if "%HB_SHOW_ERRORS%"  == "" set HB_SHOW_ERRORS=no

set HB_MAKE_FLAGS=/NOLOGO /S /C %HB_MAKE_FLAGS%

rem ---------------------------------------------------------------

%HB_MAKE_PROGRAM% /f make_all.mak %1 %2 %3 %4 %5 %6 %7 %8 %9
