::
:: GTNAP build script
::
:: build -b [Debug/Release] -a [x64|Win32] [-noliboff]
::
:: [-noliboff] Optional flag to disable the LibreOffice support
::

@echo off

::
:: Input parameters
::
set HBMK_PATH=..\\..\\bin\\win\\msvc64
set BUILD=Debug
set ARCH=x64
set "CWD=%cd%"
set LIBREOFFICE=ON

:: AMD64 IA64 x86
IF "%PROCESSOR_ARCHITECTURE%"=="AMD64" GOTO endprocessor
set HBMK_PATH=..\\..\\bin\\win\\msvc
set ARCH=Win32
:endprocessor

:parse
IF "%~1"=="" GOTO endparse
IF "%~1"=="-b" GOTO build
IF "%~1"=="-a" GOTO arch
IF "%~1"=="-noliboff" GOTO noliboff
SHIFT
GOTO parse

:build
SHIFT
set BUILD=%~1
GOTO parse

:arch
SHIFT
set ARCH=%~1
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
echo Architecture: %ARCH%
echo Build type: %BUILD%
echo HBMK_PATH: %HBMK_PATH%
echo LIBREOFFICE: %LIBREOFFICE%
echo ---------------------------

::
:: Build NAppGUI from sources
::
call cmake -S %CWD% -B %CWD%\build -A%ARCH% -DGTNAP_LIBREOFFICE=%LIBREOFFICE% || goto error_cmake
call cmake --build %CWD%\build --config %BUILD% || goto error_build

::
:: Build GTNAP
::
IF "%BUILD%"=="Debug" GOTO hbmk2_debug
call %HBMK_PATH%\\hbmk2.exe %CWD%\src\gtnap\gtnap.hbp || goto error_gtnap
GOTO hbmk2_end

:hbmk2_debug
call %HBMK_PATH%\\hbmk2.exe -debug %CWD%\src\gtnap\gtnap.hbp || goto error_gtnap
:hbmk2_end

echo ---------------------------
echo GTNAP build succeed
echo ---------------------------
goto end

::
:: Errors
::
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
