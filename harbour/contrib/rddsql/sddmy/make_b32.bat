@echo off
rem
rem $Id$
rem

if not "%HB_INC_MYSQL%%HB_DIR_MYSQL%" == "" goto DIR_OK

echo ---------------------------------------------------------------
echo IMPORTANT: You'll need MySQL package and this envvar
echo            to be set to successfully build this library:
echo            set HB_INC_MYSQL=C:\mysql\include
echo            or
echo            set HB_DIR_MYSQL=C:\mysql
echo            if you want to generate .lib for the .dll.
echo ---------------------------------------------------------------
goto POST_EXIT

:DIR_OK

if "%HB_INC_MYSQL%" == "" set HB_INC_MYSQL=%HB_DIR_MYSQL%\include
set _HB_DLL_NAME=libmySQL
set _HB_DLL_DIR=%HB_DIR_MYSQL%\bin
set HB_ROOT=..\..\..
set HB_MAKEFILE=..\..\mtpl_b32.mak
set CFLAGS=-I"%HB_INC_MYSQL%";..\..\..\include

rem ---------------------------------------------------------------

call ..\..\mtpl_b32.bat %1 %2 %3 %4 %5 %6 %7 %8 %9

rem ---------------------------------------------------------------

if "%HB_DIR_MYSQL%" == "" goto POST_EXIT

set _HB_INSTALL_PREFIX=%HB_INSTALL_PREFIX%
if "%_HB_INSTALL_PREFIX%" == "" set _HB_INSTALL_PREFIX=..\..\..
set _HB_LIB_INSTALL=%HB_LIB_INSTALL%
if "%_HB_LIB_INSTALL%" == "" set _HB_LIB_INSTALL=%_HB_INSTALL_PREFIX%\lib

if "%1" == "clean" goto POST_CLEAN
if "%1" == "Clean" goto POST_CLEAN
if "%1" == "CLEAN" goto POST_CLEAN
if "%1" == "install" goto POST_INSTALL
if "%1" == "Install" goto POST_INSTALL
if "%1" == "INSTALL" goto POST_INSTALL

:POST_BUILD

   implib ..\..\..\lib\%_HB_CC_NAME%\%_HB_DLL_NAME%.lib "%_HB_DLL_DIR%\%_HB_DLL_NAME%.dll" >> %_HB_MAKELOG%
   goto POST_EXIT

:POST_CLEAN

   if exist ..\..\..\lib\%_HB_CC_NAME%\%_HB_DLL_NAME%.lib del ..\..\..\lib\%_HB_CC_NAME%\%_HB_DLL_NAME%.lib > nul
   if exist ..\..\..\lib\%_HB_CC_NAME%\%_HB_DLL_NAME%.exp del ..\..\..\lib\%_HB_CC_NAME%\%_HB_DLL_NAME%.exp > nul
   if exist %_HB_LIB_INSTALL%\%_HB_DLL_NAME%.lib       del %_HB_LIB_INSTALL%\%_HB_DLL_NAME%.lib       > nul
   goto POST_EXIT

:POST_INSTALL

   if exist %_HB_LIB_INSTALL%\%_HB_DLL_NAME%.lib del %_HB_LIB_INSTALL%\%_HB_DLL_NAME%.lib
   if exist ..\..\..\lib\%_HB_CC_NAME%\%_HB_DLL_NAME%.lib copy ..\..\..\lib\%_HB_CC_NAME%\%_HB_DLL_NAME%.lib %_HB_LIB_INSTALL%
   goto POST_EXIT

:POST_EXIT

set CFLAGS=
set _HB_DLL_NAME=
set _HB_DLL_DIR=
set _HB_INSTALL_PREFIX=
set _HB_LIB_INSTALL=
