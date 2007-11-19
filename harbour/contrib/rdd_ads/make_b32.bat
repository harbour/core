@echo off
rem
rem $Id$
rem

if "%ADS_DIR%" == "" goto HELP

set C_USR=%C_USR% -I%ADS_DIR%
set HB_DLL_NAME=ace32
if not "%HB_DLL_DIR%" == "" goto DIR_OK
if exist "%ADS_DIR%\32bit\ace32.dll"        set HB_DLL_DIR=%ADS_DIR%\32bit
if exist "%ADS_DIR%\Redistribute\ace32.dll" set HB_DLL_DIR=%ADS_DIR%\Redistribute
if exist "%ADS_DIR%\ace32.dll"              set HB_DLL_DIR=%ADS_DIR%

:DIR_OK

echo Using this .dll: %HB_DLL_DIR%\%HB_DLL_NAME%.dll

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

   implib ..\..\lib\%HB_CC_NAME%\%HB_DLL_NAME%.lib %HB_DLL_DIR%\%HB_DLL_NAME%.dll
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

:HELP

echo ---------------------------------------------------------------
echo IMPORTANT: You'll need Advantage Client Engine installed and
echo            these envvars set to successfully build this library:
echo            set ACE_DIR=C:\ads\acesdk
echo            set ACE_VER=810
echo ---------------------------------------------------------------

:POST_EXIT
