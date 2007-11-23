@echo off
rem
rem $Id: make_b32.bat 7991 2007-11-17 11:42:49Z vszakats $
rem

if "%1" == "" goto ERROR

echo Entering: %1

cd %1
call make_b32.bat %2 %3 %4 %5 %6 %7 %8 %9
cd ..

goto EXIT

:ERROR
echo ----------------------------------------
echo Usage: make_b32.bat <dirname>
echo ----------------------------------------

:EXIT
