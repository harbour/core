@echo off
rem
rem $Id$
rem

rem The compilation is done in three steps. PLEASE DO NOT MODIFY
rem IT or you will break Win9x command.com line length limit !!!

set HB_SHOW_ERRORS=no

rem set DO_NOT_COMPILE=examples hbclip hgf

set _HB_DIRS=adordd bmdbfcdx hbbtree gtwvg libct libgt libmisc libnf hbmsql
for %%n in ( %_HB_DIRS% ) do %COMSPEC% /c make_vc.bat %%n %1 %2 %3 %4 %5 %6 %7 %8 %9

set _HB_DIRS=ole hbziparch odbc telepath tip what32 win32 xhb
for %%n in ( %_HB_DIRS% ) do %COMSPEC% /c make_vc.bat %%n %1 %2 %3 %4 %5 %6 %7 %8 %9

set _HB_DIRS=samples hbw32ddr
if not "%APOLLO_DIR%"    == "" set _HB_DIRS=%_HB_DIRS% hbapollo
if not "%FIREBIRD_DIR%"  == "" set _HB_DIRS=%_HB_DIRS% hbfbird
if not "%FREEIMAGE_DIR%" == "" set _HB_DIRS=%_HB_DIRS% hbfimage
if not "%GD_DIR%"        == "" set _HB_DIRS=%_HB_DIRS% hbgd
if not "%MYSQL_DIR%"     == "" set _HB_DIRS=%_HB_DIRS% hbmysql
if not "%PGSQL_DIR%"     == "" set _HB_DIRS=%_HB_DIRS% hbpgsql
if not "%ADS_DIR%"       == "" set _HB_DIRS=%_HB_DIRS% rdd_ads
for %%n in ( %_HB_DIRS% ) do %COMSPEC% /c make_vc.bat %%n %1 %2 %3 %4 %5 %6 %7 %8 %9

set HB_SHOW_ERRORS=
set _HB_DIRS=
