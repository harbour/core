@echo off
rem
rem $Id$
rem

rem The compilation is done in three steps. PLEASE DO NOT MODIFY
rem IT or you will break Win9x command.com line length limit !!!

set HB_SHOW_ERRORS=no

set DO_NOT_COMPILE=examples hbclip hgf

set DIRS=adordd bmdbfcdx btree libct libgt libmisc libnf msql
for %%n in ( %DIRS% ) do %COMSPEC% /c make_b32.bat %%n %1 %2 %3 %4 %5 %6 %7 %8 %9

set DIRS=ole hbzlib htmllib odbc telepath tip win32 xhb
for %%n in ( %DIRS% ) do %COMSPEC% /c make_b32.bat %%n %1 %2 %3 %4 %5 %6 %7 %8 %9

set DIRS=samples directx
if not "%APOLLO_DIR%"    == "" set DIRS=%DIRS% apollo
if not "%FIREBIRD_DIR%"  == "" set DIRS=%DIRS% firebird
if not "%FREEIMAGE_DIR%" == "" set DIRS=%DIRS% freeimage
if not "%GD_DIR%"        == "" set DIRS=%DIRS% gd
if not "%MYSQL_DIR%"     == "" set DIRS=%DIRS% mysql
if not "%PDFLIB_DIR%"    == "" set DIRS=%DIRS% pdflib
if not "%PGSQL_DIR%"     == "" set DIRS=%DIRS% pgsql
if not "%ADS_DIR%"       == "" set DIRS=%DIRS% rdd_ads
for %%n in ( %DIRS% ) do %COMSPEC% /c make_b32.bat %%n %1 %2 %3 %4 %5 %6 %7 %8 %9
