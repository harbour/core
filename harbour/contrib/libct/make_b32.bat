@echo off
rem
rem $Id$
rem

if "%CC_NAME%" == "" set CC_NAME=b32
if "%HB_MAKE_PROGRAM%" == "" set HB_MAKE_PROGRAM=make.exe

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

if "%1" == "install" goto INSTALL
if "%1" == "INSTALL" goto INSTALL

:BUILD

   %HB_MAKE_PROGRAM% -f ..\maketpl.%CC_NAME% %1 %2 %3 > make_%CC_NAME%.log
   if errorlevel 1 notepad make_%CC_NAME%.log
   goto EXIT

:CLEAN

   %HB_MAKE_PROGRAM% %HB_MAKE_FLAGS% -f ..\maketpl.%CC_NAME% CLEAN > make_%CC_NAME%.log
   goto EXIT

:INSTALL

   if "%HB_INSTALL_PREFIX%" == "" set HB_INSTALL_PREFIX=..\..

   if "%HB_BIN_INSTALL%"    == "" set HB_BIN_INSTALL=%HB_INSTALL_PREFIX%\bin
   if "%HB_INC_INSTALL%"    == "" set HB_INC_INSTALL=%HB_INSTALL_PREFIX%\include
   if "%HB_LIB_INSTALL%"    == "" set HB_LIB_INSTALL=%HB_INSTALL_PREFIX%\lib

   %HB_MAKE_PROGRAM% %HB_MAKE_FLAGS% -f ..\maketpl.%CC_NAME% INSTALL > nul
   goto EXIT

:EXIT
