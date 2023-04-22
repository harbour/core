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
echo Build type: %BUILD%
echo ---------------------------

::
:: Build NAppGUI from sources
::
call cmake -S %CWD%\src -B %CWD%\build -DCMAKE_WARN_VS11=OFF || goto error_cmake
call cmake --build %CWD%\build --config %BUILD%  || goto error_build

::
:: Build GTNAP
::
call %HBMK_PATH%\\hbmk2.exe %CWD%\src\gtnap\gtnap.hbp || goto error_gtnap

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
