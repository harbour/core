@rem
@rem $Id$
@rem

@echo off

rem *******************************************************
rem Bat file for creating (almost all) contrib libs
rem for Harbour Project for Microsoft Visual C/C++
rem *******************************************************

rem *******************************************************
rem Copyright 2007 Marek Paliwoda (mpaliwoda "at" interia "dot" pl)
rem See doc/license.txt for licensing terms.
rem *******************************************************

rem *******************************************************
rem The compilation is done in three steps. PLEASE DO NOT MODIFY IT
rem or you will break a Windows9x command.com line length limit !!!
rem *******************************************************

set HB_SHOW_ERRORS=no

rem *******************************************************
rem Creating a worker bat file ...
rem *******************************************************

set _HB_BATWORKER=_hbwrk_.bat

echo @echo off                                          > %_HB_BATWORKER%
echo if %%1x == x goto EXIT                            >> %_HB_BATWORKER%
echo if not exist %%1\make_vc.bat goto EXIT            >> %_HB_BATWORKER%
echo echo Entering: %%1                                >> %_HB_BATWORKER%
echo cd %%1                                            >> %_HB_BATWORKER%
echo call make_vc.bat %%2 %%3 %%4 %%5 %%6 %%7 %%8 %%9  >> %_HB_BATWORKER%
echo cd ..                                             >> %_HB_BATWORKER%
echo :EXIT                                             >> %_HB_BATWORKER%

rem *******************************************************
rem Compiling contrib dirs ...
rem *******************************************************

set _HB_DIRS=%HB_CONTRIBLIBS%
if not "%_HB_DIRS%" == "" goto :OVERRIDE

set _HB_DIRS=gtwvg hbbmcdx hbbtree hbclipsm hbcrypt hbct hbgt hbmisc
for %%n in ( %_HB_DIRS% ) do %COMSPEC% /c %_HB_BATWORKER% %%n %1 %2 %3 %4 %5 %6 %7 %8 %9

set _HB_DIRS=hbmsql hbmzip hbnf hbodbc hbole hbsqlit3
for %%n in ( %_HB_DIRS% ) do %COMSPEC% /c %_HB_BATWORKER% %%n %1 %2 %3 %4 %5 %6 %7 %8 %9

set _HB_DIRS=hbtip hbtpathy hbvpdf hbwin hbziparc
for %%n in ( %_HB_DIRS% ) do %COMSPEC% /c %_HB_BATWORKER% %%n %1 %2 %3 %4 %5 %6 %7 %8 %9

set _HB_DIRS=rddado rddsql xhb
if not "%HB_INC_ALLEGRO%%HB_DIR_ALLEGRO%"     == "" set _HB_DIRS=%_HB_DIRS% gtalleg
if not "%HB_INC_APOLLO%%HB_DIR_APOLLO%"       == "" set _HB_DIRS=%_HB_DIRS% hbapollo
if not "%HB_INC_BLAT%%HB_DIR_BLAT%"           == "" set _HB_DIRS=%_HB_DIRS% hbblat
if not "%HB_INC_CURL%%HB_DIR_CURL%"           == "" set _HB_DIRS=%_HB_DIRS% hbcurl
if not "%HB_INC_FIREBIRD%%HB_DIR_FIREBIRD%"   == "" set _HB_DIRS=%_HB_DIRS% hbfbird
if not "%HB_INC_FREEIMAGE%%HB_DIR_FREEIMAGE%" == "" set _HB_DIRS=%_HB_DIRS% hbfimage
if not "%HB_INC_GD%%HB_DIR_GD%"               == "" set _HB_DIRS=%_HB_DIRS% hbgd
if not "%HB_INC_LIBHARU%%HB_DIR_LIBHARU%"     == "" set _HB_DIRS=%_HB_DIRS% hbhpdf
if not "%HB_INC_MYSQL%%HB_DIR_MYSQL%"         == "" set _HB_DIRS=%_HB_DIRS% hbmysql
if not "%HB_INC_PGSQL%%HB_DIR_PGSQL%"         == "" set _HB_DIRS=%_HB_DIRS% hbpgsql
if not "%HB_INC_OPENSSL%%HB_DIR_OPENSSL%"     == "" set _HB_DIRS=%_HB_DIRS% hbssl
if not "%HB_INC_ADS%%HB_DIR_ADS%"             == "" set _HB_DIRS=%_HB_DIRS% rddads
:OVERRIDE
for %%n in ( %_HB_DIRS% ) do %COMSPEC% /c %_HB_BATWORKER% %%n %1 %2 %3 %4 %5 %6 %7 %8 %9

if not "%HB_CONTRIB_ADDONS%" == "" goto :NO_ADDONS
for %%n in ( %HB_CONTRIB_ADDONS% ) do %COMSPEC% /c %_HB_BATWORKER% %%n %1 %2 %3 %4 %5 %6 %7 %8 %9
:NO_ADDONS

rem *******************************************************
rem Cleaning ...
rem *******************************************************

del %_HB_BATWORKER% > nul

set _HB_DIRS=
set _HB_BATWORKER=
set HB_SHOW_ERRORS=
