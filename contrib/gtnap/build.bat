::
:: GTNAP build script
:: build -b [Debug/Release]
::

@echo off

::
:: Input parameters
::
set HBMK_PATH=..\\..\\bin\\win\\msvc64
set ARCH=x64
set BUILD=Debug
set "CWD=%cd%"

:: AMD64 IA64 x86
IF "%PROCESSOR_ARCHITECTURE%"=="AMD64" GOTO endprocessor
set HBMK_PATH=..\\..\\bin\\win\\msvc
set ARCH=Win32
:endprocessor

:parse
IF "%~1"=="" GOTO endparse
IF "%~1"=="-b" GOTO build
IF "%~1"=="-a" GOTO arch
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
echo ---------------------------

::
:: Build NAppGUI from sources
::
call cmake -S %CWD% -B %CWD%\build -A%ARCH% || goto error_cmake
call cmake --build %CWD%\build --config %BUILD%  || goto error_build

::
:: Build GTNAP
::
call %HBMK_PATH%\\hbmk2.exe %CWD%\src\gtnap\gtnap.hbp || goto error_gtnap

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
