@echo off

::
:: Input parameters
::
set HBMK_PATH=..\\..\\bin\\win\\msvc
set BUILD=Debug
set COMPILER=msvc12
set "CWD=%cd%"

:parse
IF "%~1"=="" GOTO endparse
IF "%~1"=="-b" GOTO build
IF "%~1"=="-msvc12" set COMPILER=msvc12
IF "%~1"=="-msvc12_x64" set COMPILER=msvc12_x64
SHIFT
GOTO parse

:build
SHIFT
set BUILD=%~1
GOTO parse

:endparse

::
:: Beginning
::
echo ---------------------------
echo Generating GTNAP
echo Main path: %CWD%
echo Compiler: %COMPILER%
echo Build type: %BUILD%
echo ---------------------------

::
:: Setup MSVC compiler (uncomment your option)
::
:: MSVC 2012 (32 bits)
IF %COMPILER% == "msvc12" call "%ProgramFiles(x86)%\Microsoft Visual Studio 11.0\VC\vcvarsall.bat"

:: MSVC 2012 (64 bits)
IF %COMPILER% == "msvc12_x64" call "%ProgramFiles%\Microsoft Visual Studio 11.0\VC\vcvarsall.bat" x86_amd64

::
:: Build NAppGUI from sources
::
call cmake -S %CWD%\nappgui\src -B %CWD%\nappgui\build -DCMAKE_WARN_VS11=OFF
call cmake --build %CWD%\nappgui\build --config %BUILD%

::
:: Build GTNAP
::
call %HBMK_PATH%\\hbmk2.exe gtnap.hbp
