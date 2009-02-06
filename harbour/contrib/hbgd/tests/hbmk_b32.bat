@echo off
rem
rem $Id$
rem

if "%HB_BIN_INSTALL%" == "" set HB_BIN_INSTALL=..\..\..\bin
if "%HB_LIB_INSTALL%" == "" set HB_LIB_INSTALL=..\..\..\lib
if "%HB_INC_INSTALL%" == "" set HB_INC_INSTALL=..\..\..\include

set HB_ARCHITECTURE=win
set HB_COMPILER=bcc32
set HB_USER_LIBS=hbgd.lib bgd.lib hbct.lib
rem ; uncomment if needed
rem set HB_GUI=yes

call %HB_BIN_INSTALL%\hbmk.bat %1 %2 %3 %4 %5 %6 %7 %8 %9
