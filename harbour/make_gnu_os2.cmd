@echo off
rem 
rem $Id$
rem 

rem Minimal initialization of environment variables for OS2 GCC build
rem for further information about see make_gnu.bat

if "%HB_ARCHITECTURE%" == "" set HB_ARCHITECTURE=os2
if "%HB_COMPILER%" == "" set HB_COMPILER=gcc
if "%HB_GT_LIB%" == "" set HB_GT_LIB=

make -r %1 %2 %3 %4 %5 %6 %7 %8 %9 > make_gnu.log
