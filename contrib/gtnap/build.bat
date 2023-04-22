@echo off

::
:: Input parameters
::
set HBMK_PATH=..\\..\\bin\\win\\msvc
set BUILD=Debug
set "CWD=%cd%"

:parse
IF "%~1"=="" GOTO endparse
IF "%~1"=="-b" GOTO build
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
:: Build NAppGUI from sources
::
call cmake -S %CWD%\nappgui\src -B %CWD%\build -DCMAKE_WARN_VS11=OFF
call cmake --build %CWD%\build --config %BUILD%

::
:: Build GTNAP
::
call %HBMK_PATH%\\hbmk2.exe gtnap.hbp
