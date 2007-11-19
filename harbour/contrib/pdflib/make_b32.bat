@echo off
rem
rem $Id$
rem

if "%PDFLIB_DIR%" == "" goto HELP

set C_USR=%C_USR% -I%PDFLIB_DIR%\src\pdflib -DHB_OS_WIN_32_USED

call ..\mtpl_b32.bat %1 %2 %3 %4 %5 %6 %7 %8 %9

goto EXIT

:HELP

echo ---------------------------------------------------------------
echo IMPORTANT: You'll need PDFLIB sources and this envvar
echo            to be set to successfully build this library:
echo            set PDFLIB_DIR=C:\pdflib
echo ---------------------------------------------------------------

:EXIT
