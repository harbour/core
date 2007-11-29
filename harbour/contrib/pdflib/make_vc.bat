@echo off
rem
rem $Id$
rem

if not "%PDFLIB_DIR%" == "" goto DIR_OK

echo ---------------------------------------------------------------
echo IMPORTANT: You'll need PDFLIB sources and this envvar
echo            to be set to successfully build this library:
echo            set PDFLIB_DIR=C:\pdflib
echo ---------------------------------------------------------------
exit 1

:DIR_OK

set CFLAGS=-I%PDFLIB_DIR%\src\pdflib

call ..\mtpl_vc.bat %1 %2 %3 %4 %5 %6 %7 %8 %9

set CFLAGS=
