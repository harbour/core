@echo off
rem
rem $Id$
rem

if NOT "%PDFLIB_DIR%" == "" goto EXEC
echo PDFLIB_DIR environment variable has to be defined !
echo ---------------------------------------------------------------
echo IMPORTANT: You'll need PDFLIB sources and this envvar
echo            to be set to successfully build this library:
echo            set PDFLIB_DIR=C:\pdflib
echo ---------------------------------------------------------------
exit 1

:EXEC
set C_USR=%C_USR% -I%PDFLIB_DIR%\src\pdflib -DHB_OS_WIN_32_USED

call ..\mtpl_vc.bat %1 %2 %3 %4 %5 %6 %7 %8 %9

goto EXIT

:EXIT
