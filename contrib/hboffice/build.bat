::
:: HBOffice build script
::
:: Will generate the hboffice.dll with the LibreOffice C-Wrapper. Visual Studio required
:: build -dll -b [Debug|Release]
::
:: Will generate the hboffice.lib with the Harbour wrapper and runtime dll loader.
:: Visual Studio (msvc64) or MinGW (mingw64) allowed
:: build -lib -b [Debug|Release] -comp [mingw64|msvc64]

@echo off

::
:: Input parameters
::
set OPERATION=dll
set COMPILER=mingw64
set BUILD=Debug
set "CWD=%cd%"

:parse
IF "%~1"=="" GOTO endparse
IF "%~1"=="-dll" GOTO dll
IF "%~1"=="-lib" GOTO lib
IF "%~1"=="-comp" GOTO compiler
IF "%~1"=="-b" GOTO build
SHIFT
GOTO parse

:dll
set OPERATION=dll
SHIFT
GOTO parse

:lib
set OPERATION=lib
SHIFT
GOTO parse

:build
SHIFT
set BUILD=%~1
SHIFT
GOTO parse

:compiler
SHIFT
set COMPILER=%~1
SHIFT
GOTO parse

:endparse

::
:: Beginning
::
echo ---------------------------
echo Generating LibreOffice
echo Main path: %CWD%
echo Build type: %BUILD%
echo COMPILER: %COMPILER%
echo OPERATION: %OPERATION%
echo ---------------------------

IF "%OPERATION%"=="dll" GOTO generate_dll
IF "%OPERATION%"=="lib" GOTO generate_lib
goto error_operation

:generate_dll
set CMAKE_ARGS=-Ax64 -DCMAKE_C_COMPILER=cl -DCMAKE_CXX_COMPILER=cl -DLIBREOFFICE_DLL=YES
set CMAKE_BUILD=--config %BUILD%
call cmake %CMAKE_ARGS% -S %CWD% -B %CWD%\build-dll || goto error_cmake
call cmake --build %CWD%\build-dll %CMAKE_BUILD% || goto error_build
goto end

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
set CMAKE_ARGS=-G "MinGW Makefiles" -DCMAKE_C_COMPILER=gcc -DCMAKE_CXX_COMPILER=g++ -DCMAKE_BUILD_TYPE=%BUILD%
set CMAKE_BUILD=-j 4
goto cmake

:config_msvc64:
:: Multi-configuration build system
set CMAKE_ARGS=-Ax64 -DCMAKE_C_COMPILER=cl -DCMAKE_CXX_COMPILER=cl
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
goto end
:: exit 1

:error_cmake
echo Error in NAppGUI CMake generate
goto end
:: exit 1

:error_build
echo Error building NAppGUI
goto end
:: exit 1

:error_gtnap
echo Error building GTNAP
goto end
:: exit 1

:error_operation
echo Invalid operation '%OPERATION%'
goto end
:: exit 1

:end
