@echo off
rem
rem $Id$
rem

if NOT "%PGSQL_DIR%" == "" goto EXEC
echo PGSQL_DIR environment variable has to be defined !
echo ---------------------------------------------------------------
echo IMPORTANT: You'll need PostreSQL package and this envvar
echo            to be set to successfully build this library:
echo            set PGSQL_DIR=C:\pgsql
echo ---------------------------------------------------------------
exit 1

:EXEC
set C_USR=%C_USR% -I%PGSQL_DIR%\include -DHB_OS_WIN_32_USED
rem .exe
set HB_DLL_NAME=postgres
if "%HB_DLL_DIR%" == "" set HB_DLL_DIR=%PGSQL_DIR%\bin

rem ---------------------------------------------------------------

call ..\mtpl_b32.bat %1 %2 %3 %4 %5 %6 %7 %8 %9

rem ---------------------------------------------------------------

set _HB_INSTALL_PREFIX=%HB_INSTALL_PREFIX%
if "%_HB_INSTALL_PREFIX%" == "" set _HB_INSTALL_PREFIX=..\..
set _HB_LIB_INSTALL=%HB_LIB_INSTALL%
if "%_HB_LIB_INSTALL%" == "" set _HB_LIB_INSTALL=%_HB_INSTALL_PREFIX%\lib

if "%1" == "clean" goto POST_CLEAN
if "%1" == "CLEAN" goto POST_CLEAN
if "%1" == "install" goto POST_INSTALL
if "%1" == "INSTALL" goto POST_INSTALL

:POST_BUILD

   implib ..\..\lib\%HB_CC_NAME%\%HB_DLL_NAME%.lib %HB_DLL_DIR%\%HB_DLL_NAME%.exe
   goto POST_EXIT

:POST_CLEAN

   if exist ..\..\lib\%HB_CC_NAME%\%HB_DLL_NAME%.lib del ..\..\lib\%HB_CC_NAME%\%HB_DLL_NAME%.lib > nul
   if exist ..\..\lib\%HB_CC_NAME%\%HB_DLL_NAME%.exp del ..\..\lib\%HB_CC_NAME%\%HB_DLL_NAME%.exp > nul
   if exist %_HB_LIB_INSTALL%\%HB_DLL_NAME%.lib      del %_HB_LIB_INSTALL%\%HB_DLL_NAME%.lib      > nul
   goto POST_EXIT

:POST_INSTALL

   if exist %_HB_LIB_INSTALL%\%HB_DLL_NAME%.lib del %_HB_LIB_INSTALL%\%HB_DLL_NAME%.lib
   if exist ..\..\lib\%HB_CC_NAME%\%HB_DLL_NAME%.lib copy ..\..\lib\%HB_CC_NAME%\%HB_DLL_NAME%.lib %_HB_LIB_INSTALL%
   goto POST_EXIT

:POST_EXIT
