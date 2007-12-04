@echo off
rem
rem $Id$
rem

rem *******************************************************
rem The compilation is done in three steps. PLEASE DO NOT MODIFY IT
rem or you will break a Windows9x command.com line length limit !!!
rem *******************************************************

set HB_SHOW_ERRORS=no

rem *******************************************************
rem Creating a worker bat file ...
rem *******************************************************

set __BATWORKER__=_hbwrk_.bat

echo @echo off                                         >%__BATWORKER__%
echo if not exist %%1\make_vc.bat goto EXIT           >>%__BATWORKER__%
echo echo Entering: %%1                               >>%__BATWORKER__%
echo cd %%1                                           >>%__BATWORKER__%
echo call make_vc.bat %%2 %%3 %%4 %%5 %%6 %%7 %%8 %%9 >>%__BATWORKER__%
echo cd ..                                            >>%__BATWORKER__%

rem *******************************************************
rem Compiling contrib dirs ...
rem *******************************************************

set _HB_DIRS=hbrddado hbbmcdx hbbtree hbgtwvg hbct hbgt hbmisc hbnf hbmsql
for %%n in ( %_HB_DIRS% ) do %COMSPEC% /c %__BATWORKER__% %%n %1 %2 %3 %4 %5 %6 %7 %8 %9

set _HB_DIRS=hbole hbziparch hbodbc hbtpathy hbtip hbw32 xhb
for %%n in ( %_HB_DIRS% ) do %COMSPEC% /c %__BATWORKER__% %%n %1 %2 %3 %4 %5 %6 %7 %8 %9

set _HB_DIRS=hbclipsm hbw32ddr
if not "%APOLLO_DIR%"    == "" set _HB_DIRS=%_HB_DIRS% hbapollo
if not "%FIREBIRD_DIR%"  == "" set _HB_DIRS=%_HB_DIRS% hbfbird
if not "%FREEIMAGE_DIR%" == "" set _HB_DIRS=%_HB_DIRS% hbfimage
if not "%GD_DIR%"        == "" set _HB_DIRS=%_HB_DIRS% hbgd
if not "%MYSQL_DIR%"     == "" set _HB_DIRS=%_HB_DIRS% hbmysql
if not "%PGSQL_DIR%"     == "" set _HB_DIRS=%_HB_DIRS% hbpgsql
if not "%ADS_DIR%"       == "" set _HB_DIRS=%_HB_DIRS% hbrddads
for %%n in ( %_HB_DIRS% ) do %COMSPEC% /c %__BATWORKER__% %%n %1 %2 %3 %4 %5 %6 %7 %8 %9

rem *******************************************************
rem Cleaning ...
rem *******************************************************

del %__BATWORKER__% > nul

set _HB_DIRS=
set __BATWORKER__=
set HB_SHOW_ERRORS=
