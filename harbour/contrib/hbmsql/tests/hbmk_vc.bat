@echo off
rem
rem $Id: bld_vc.bat 8411 2008-05-14 06:41:01Z vszakats $
rem

if "%HB_BIN_INSTALL%" == "" set HB_BIN_INSTALL=..\..\..\bin
if "%HB_LIB_INSTALL%" == "" set HB_LIB_INSTALL=..\..\..\lib
if "%HB_INC_INSTALL%" == "" set HB_INC_INSTALL=..\..\..\include

set HB_ARCHITECTURE=w32
set HB_COMPILER=msvc
set HB_USER_LIBS=%HB_USER_LIBS% hbmsql.lib

call %HB_BIN_INSTALL%\hbmk.bat %1 %2 %3 %4 %5 %6 %7 %8 %9
