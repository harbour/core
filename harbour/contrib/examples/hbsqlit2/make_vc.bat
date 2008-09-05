@echo off
rem
rem $Id$
rem

if not "%HB_INC_SQLITE2% == "" goto DIR_OK

echo ---------------------------------------------------------------
echo IMPORTANT: You'll need SQLite 2.8.16 package and this envvar
echo            to be set to successfully build this library:
echo            set HB_INC_SQLITE2=C:\sqlite2
echo ---------------------------------------------------------------
goto POST_EXIT

:DIR_OK

set CFLAGS=-I"%HB_INC_SQLITE2%"

set HB_ROOT=..\..\..
set HB_MAKEFILE=..\..\mtpl_vc.mak

call ..\mtpl_vc.bat %1 %2 %3 %4 %5 %6 %7 %8 %9

set HB_ROOT=
set HB_MAKEFILE=

set CFLAGS=

:POST_EXIT
