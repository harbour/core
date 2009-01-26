@echo off
rem
rem $Id$
rem

if not "%HB_INC_OPENSSL%%HB_DIR_OPENSSL%" == "" goto DIR_OK

echo ---------------------------------------------------------------
echo IMPORTANT: You'll need the 'OpenSSL' package and this envvar
echo            to be set to successfully build this library:
echo            set HB_INC_OPENSSL=C:\openssl
echo            or
echo            set HB_DIR_OPENSSL=C:\openssl
echo            if you want to generate .lib for the .dll.
echo ---------------------------------------------------------------
goto POST_EXIT

:DIR_OK

if "%HB_INC_OPENSSL%" == "" set HB_INC_OPENSSL=%HB_DIR_OPENSSL%\inc32
set CFLAGS=-I"%HB_INC_OPENSSL%"
set _HB_DLL_NAME1=libeay32
set _HB_DLL_NAME2=ssleay32
if exist "%HB_DIR_OPENSSL%\out32dll\%_HB_DLL_NAME1%.dll" set _HB_DLL_DIR=%HB_DIR_OPENSSL%\out32dll
if exist "%HB_DIR_OPENSSL%\dll\%_HB_DLL_NAME1%.dll"      set _HB_DLL_DIR=%HB_DIR_OPENSSL%\dll
if exist "%HB_DIR_OPENSSL%\%_HB_DLL_NAME1%.dll"          set _HB_DLL_DIR=%HB_DIR_OPENSSL%

if not "%HB_DIR_OPENSSL%" == "" echo Using .dll: "%_HB_DLL_DIR%\%_HB_DLL_NAME1%.dll"

rem ---------------------------------------------------------------

call ..\mtpl_vc.bat %1 %2 %3 %4 %5 %6 %7 %8 %9

rem ---------------------------------------------------------------

if "%HB_DIR_OPENSSL%" == "" goto POST_EXIT

set _HB_INSTALL_PREFIX=%HB_INSTALL_PREFIX%
if "%_HB_INSTALL_PREFIX%" == "" set _HB_INSTALL_PREFIX=..\..
set _HB_LIB_INSTALL=%HB_LIB_INSTALL%
if "%_HB_LIB_INSTALL%" == "" set _HB_LIB_INSTALL=%_HB_INSTALL_PREFIX%\lib

if "%1" == "clean" goto POST_CLEAN
if "%1" == "Clean" goto POST_CLEAN
if "%1" == "CLEAN" goto POST_CLEAN
if "%1" == "install" goto POST_INSTALL
if "%1" == "Install" goto POST_INSTALL
if "%1" == "INSTALL" goto POST_INSTALL

:POST_BUILD

   rem Use supplied .lib file.
   if not exist ..\..\lib\%_HB_CC_NAME%\%_HB_DLL_NAME1%.lib copy "%_HB_DLL_DIR%\%_HB_DLL_NAME1%.lib" ..\..\lib\%_HB_CC_NAME%\%_HB_DLL_NAME1%.lib > nul
   if not exist ..\..\lib\%_HB_CC_NAME%\%_HB_DLL_NAME2%.lib copy "%_HB_DLL_DIR%\%_HB_DLL_NAME2%.lib" ..\..\lib\%_HB_CC_NAME%\%_HB_DLL_NAME2%.lib > nul
   goto POST_EXIT

:POST_CLEAN

   if exist ..\..\lib\%_HB_CC_NAME%\%_HB_DLL_NAME1%.lib del ..\..\lib\%_HB_CC_NAME%\%_HB_DLL_NAME1%.lib > nul
   if exist ..\..\lib\%_HB_CC_NAME%\%_HB_DLL_NAME1%.exp del ..\..\lib\%_HB_CC_NAME%\%_HB_DLL_NAME1%.exp > nul
   if exist %_HB_LIB_INSTALL%\%_HB_DLL_NAME1%.lib       del %_HB_LIB_INSTALL%\%_HB_DLL_NAME1%.lib       > nul
   if exist ..\..\lib\%_HB_CC_NAME%\%_HB_DLL_NAME2%.lib del ..\..\lib\%_HB_CC_NAME%\%_HB_DLL_NAME2%.lib > nul
   if exist ..\..\lib\%_HB_CC_NAME%\%_HB_DLL_NAME2%.exp del ..\..\lib\%_HB_CC_NAME%\%_HB_DLL_NAME2%.exp > nul
   if exist %_HB_LIB_INSTALL%\%_HB_DLL_NAME2%.lib       del %_HB_LIB_INSTALL%\%_HB_DLL_NAME2%.lib       > nul
   goto POST_EXIT

:POST_INSTALL

   if exist %_HB_LIB_INSTALL%\%_HB_DLL_NAME1%.lib del %_HB_LIB_INSTALL%\%_HB_DLL_NAME1%.lib
   if exist ..\..\lib\%_HB_CC_NAME%\%_HB_DLL_NAME1%.lib copy ..\..\lib\%_HB_CC_NAME%\%_HB_DLL_NAME1%.lib %_HB_LIB_INSTALL%
   if exist %_HB_LIB_INSTALL%\%_HB_DLL_NAME2%.lib del %_HB_LIB_INSTALL%\%_HB_DLL_NAME2%.lib
   if exist ..\..\lib\%_HB_CC_NAME%\%_HB_DLL_NAME2%.lib copy ..\..\lib\%_HB_CC_NAME%\%_HB_DLL_NAME2%.lib %_HB_LIB_INSTALL%
   goto POST_EXIT

:POST_EXIT

set CFLAGS=
set _HB_DLL_NAME1=
set _HB_DLL_NAME2=
set _HB_DLL_DIR=
set _HB_INSTALL_PREFIX=
set _HB_LIB_INSTALL=
