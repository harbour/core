@echo off
rem
rem $Id$
rem

rem The compilation is done in three steps. PLEASE DO NOT MODIFY
rem IT or you will break Win98 command.com line length limit !!!

set HB_SHOW_ERRORS=no

set DO_NOT_COMPILE=examples hbclip hgf msql

set DIRS=adordd bmdbfcdx btree libct libgt libmisc libnf ole

FOR %%n IN ( %DIRS% ) DO %COMSPEC% /c make_b32.bat %%n %1 %2 %3 %4 %5 %6 %7 %8 %9

set DIRS=hbzlib htmllib odbc rdd_ads telepath tip win32 xhb

FOR %%n IN ( %DIRS% ) DO %COMSPEC% /c make_b32.bat %%n %1 %2 %3 %4 %5 %6 %7 %8 %9

set DIRS=samples directx
if not "%APOLLO_DIR%"    == "" set DIRS=%DIRS% apollo
if not "%FIREBIRD_DIR%"  == "" set DIRS=%DIRS% firebird
if not "%FREEIMAGE_DIR%" == "" set DIRS=%DIRS% freeimage
if not "%GD_DIR%"        == "" set DIRS=%DIRS% gd
if not "%MYSQL_DIR%"     == "" set DIRS=%DIRS% mysql
if not "%PDFLIB_DIR%"    == "" set DIRS=%DIRS% pdflib
if not "%PGSQL_DIR%"     == "" set DIRS=%DIRS% pgsql

FOR %%n IN ( %DIRS% ) DO %COMSPEC% /c make_b32.bat %%n %1 %2 %3 %4 %5 %6 %7 %8 %9

