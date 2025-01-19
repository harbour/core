::
:: GTNAP build script
::
:: build -b [Debug|Release] -comp [msvc64|mingw64|clang]
:: Release is default configuration
:: mingw64 is default compiler
::

@echo off

::
:: Input parameters
::
set COMPILER=mingw64
set BUILD=Release
set "CWD=%cd%"

:parse
IF "%~1"=="" GOTO endparse
IF "%~1"=="-b" GOTO build
IF "%~1"=="-comp" GOTO compiler
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

:endparse

::
:: Beginning
::
set HBMK_PATH=..\\..\\bin\\win\\%COMPILER%
echo ---------------------------
echo Generating GTNAP
echo Main path: %CWD%
echo Build type: %BUILD%
echo COMPILER: %COMPILER%
echo HBMK path: %HBMK_PATH%
echo ---------------------------

::
:: Configure compiler and cmake
::
set CMAKE_ARGS=
set CMAKE_BUILD=
IF "%COMPILER%"=="msvc64" GOTO config_msvc64
IF "%COMPILER%"=="mingw64" GOTO config_mingw64
IF "%COMPILER%"=="clang" GOTO config_clang
goto error_compiler

:config_msvc64:
:: Multi-configuration build system
set CMAKE_ARGS=-Ax64 -DCMAKE_C_COMPILER=cl -DCMAKE_CXX_COMPILER=cl
set CMAKE_BUILD=--config %BUILD%
goto cmake

:config_mingw64
:: Mono-configuration build system
set CMAKE_ARGS=-G "MinGW Makefiles" -DCMAKE_C_COMPILER=gcc -DCMAKE_CXX_COMPILER=g++ -DCMAKE_BUILD_TYPE=%BUILD%
set CMAKE_BUILD=-j 4
goto cmake

:config_clang
:: Mono-configuration build system
set CMAKE_ARGS=-G "MinGW Makefiles" -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++ -DCMAKE_BUILD_TYPE=%BUILD%
set CMAKE_BUILD=-j 4
goto cmake

::
:: Build NAppGUI from sources
::
:cmake
call cmake %CMAKE_ARGS% -S %CWD% -B %CWD%\build || goto error_cmake
call cmake --build %CWD%\build %CMAKE_BUILD% || goto error_build

::
:: Build GTNAP
::
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
goto end

:error_cmake
echo Error in NAppGUI CMake generate
goto end

:error_build
echo Error building NAppGUI
goto end

:error_gtnap
echo Error building GTNAP
goto end

:end
