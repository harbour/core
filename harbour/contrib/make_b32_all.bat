@echo off
rem
rem $Id$
rem

rem ---------------------------------------------------------------

if "%HB_CC_NAME%"      == "" set HB_CC_NAME=b32
if "%HB_MAKE_PROGRAM%" == "" set HB_MAKE_PROGRAM=make.exe
if "%HB_SHOW_ERRORS%"  == "" set HB_SHOW_ERRORS=no

rem ---------------------------------------------------------------

%HB_MAKE_PROGRAM% -N -r -f make_all.mak %1 %2 %3 %4 %5 %6 %7 %8 %9
