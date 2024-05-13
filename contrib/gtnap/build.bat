::
:: GTNAP build script
::
:: build -b [Debug|Release] -comp [mingw64|msvc64] [-noliboff]
:: Debug is default configuration
:: mingw64 is default compiler
:: [-noliboff] Optional flag to disable the LibreOffice support
::

@echo off

::
:: Input parameters
::
set COMPILER=mingw64
set BUILD=Debug
set "CWD=%cd%"
set LIBREOFFICE=ON

:parse
IF "%~1"=="" GOTO endparse
IF "%~1"=="-b" GOTO build
IF "%~1"=="-comp" GOTO compiler
IF "%~1"=="-noliboff" GOTO noliboff
SHIFT
GOTO parse

:build
SHIFT
set BUILD=%~1
GOTO parse

:compiler
SHIFT
set COMPILER=%~1
GOTO parse

:noliboff
set LIBREOFFICE=OFF
SHIFT
GOTO parse

:endparse

::
:: Beginning
::
echo ---------------------------
echo Generating GTNAP
echo Main path: %CWD%
echo Build type: %BUILD%
echo COMPILER: %COMPILER%
echo LIBREOFFICE: %LIBREOFFICE%
echo ---------------------------

::
:: Configure compiler and cmake
::
set CMAKE_ARGS=
set CMAKE_BUILD=
IF "%COMPILER%"=="mingw64" GOTO config_mingw64
IF "%COMPILER%"=="msvc64" GOTO config_msvc64
goto error_compiler

:config_mingw64
:: Mono-configuration build system
set CMAKE_ARGS=-G "MinGW Makefiles" -DCMAKE_BUILD_TYPE=%BUILD%
set CMAKE_BUILD=-j 4
goto cmake

:config_msvc64:
:: Multi-configuration build system
set CMAKE_ARGS=-Ax64
set CMAKE_BUILD=--config %BUILD%
goto cmake

::
:: Build NAppGUI from sources
::
:cmake
call cmake %CMAKE_ARGS% -S %CWD% -B %CWD%\build -DGTNAP_LIBREOFFICE=%LIBREOFFICE% || goto error_cmake
call cmake --build %CWD%\build %CMAKE_BUILD% || goto error_build

::
:: Build GTNAP
::
set HBMK_PATH=..\\..\\bin\\win\\%COMPILER%
set HBMK_FLAGS=

IF "%BUILD%"=="Debug" GOTO hbmk2_debug
goto hbmk2

:hbmk2_debug
set HBMK_FLAGS=-debug

:hbmk2
echo HBMK HOME: %HBMK_PATH%
call %HBMK_PATH%\\hbmk2.exe -comp=%COMPILER% %HBMK_FLAGS% %CWD%\src\gtnap\gtnap.hbp || goto error_gtnap

echo ---------------------------
echo GTNAP build succeed
echo ---------------------------
goto end

::
:: Errors
::
:error_compiler
echo Unknown compiler
exit 1

:error_cmake
echo Error in NAppGUI CMake generate
exit 1

:error_build
echo Error building NAppGUI
exit 1

:error_gtnap
echo Error building GTNAP
exit 1

:end
