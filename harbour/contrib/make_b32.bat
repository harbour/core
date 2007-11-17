@echo off
rem
rem $Id$
rem

if "%1" == "" goto ERROR

set ECHO=echo
cd %1
call make_b32.bat %2 %3 %4 %5 %6 %7 %8 %9
cd ..
set ECHO=

goto EXIT

:ERROR
echo ----------------------------------------
echo RUN : make_b32.bat DIRNAME  !!!
echo ----------------------------------------

:EXIT
