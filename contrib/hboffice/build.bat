::
:: HBOffice build script
::
:: Will generate the hboffice.dll with the LibreOffice C-Wrapper. Visual Studio required
:: build -dll -b [Debug|Release]
::
:: Will generate the hboffice.lib with the Harbour wrapper and runtime dll loader.
:: Visual Studio (msvc64) or MinGW (mingw64) or Clang allowed
:: build -lib -b [Debug|Release] -comp [msvc64|mingw64|clang]

@echo off

::
:: Input parameters
::
set OPERATION=dll
set COMPILER=mingw64
set BUILD=Release
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
set COMPILER=msvc64
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

::
:: Generate dynamic library
::
:generate_dll
set CMAKE_ARGS=-Ax64 -DCMAKE_C_COMPILER=cl -DCMAKE_CXX_COMPILER=cl -DCMAKE_WARN_VS11=OFF
set CMAKE_BUILD=--config %BUILD%
call cmake %CMAKE_ARGS% -S %CWD% -B %CWD%\build || goto error_cmake
call cmake --build %CWD%\build %CMAKE_BUILD% || goto error_build
del %CWD%\build\%BUILD%\lib\core.*
del %CWD%\build\%BUILD%\lib\osbs.*
del %CWD%\build\%BUILD%\lib\sewer.*

echo ---------------------------
echo OFFICESDK DLL build succeed
echo ---------------------------
goto end

::
:: Generate static library
::
:generate_lib
set HBMK_PATH=..\\..\\bin\\win\\%COMPILER%
set HBMK_FLAGS=

IF "%BUILD%"=="Debug" GOTO hbmk2_debug
goto hbmk2

:hbmk2_debug
set HBMK_FLAGS=-debug

:hbmk2
echo HBMK HOME: %HBMK_PATH%
call %HBMK_PATH%\\hbmk2.exe %HBMK_FLAGS% -comp=%COMPILER% %CWD%\src\hboffice\hboffice.hbp || goto error_hboffice

echo ---------------------------
echo HBOFFICE LIB build succeed
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

:error_hboffice
echo Error building HBOFFICE
goto end

:error_operation
echo Invalid operation '%OPERATION%'
goto end

:end
