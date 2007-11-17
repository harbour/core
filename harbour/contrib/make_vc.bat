@echo off
rem
rem $Id$
rem

if "%1" == "" goto ERROR

cd %1
call make_vc.bat %2 %3 %4 %5 %6 %7 %8 %9
cd ..

goto EXIT

:ERROR
echo ----------------------------------------
echo RUN : make_vc.bat DIRNAME  !!!
echo ----------------------------------------

:EXIT
