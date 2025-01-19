::
:: HBAWS build script
::
:: Will generate the hbaws.lib with the Harbour AWS wrapper.
:: Visual Studio (msvc64) or MinGW (mingw64) or Clang allowed
:: build -b [Debug|Release] -comp [msvc64|mingw64|clang]

@echo off

::
:: Input parameters
::
set COMPILER=mingw64
set BUILD=Release
set "CWD=%cd%"

:parse
IF "%~1"=="" GOTO endparse
IF "%~1"=="-comp" GOTO compiler
IF "%~1"=="-b" GOTO build
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
echo Generating HBAWS
echo Main path: %CWD%
echo Build type: %BUILD%
echo COMPILER: %COMPILER%
echo AWS_SDK_ROOT: %AWS_SDK_ROOT%
echo ---------------------------

set HBMK_PATH=..\\..\\bin\\win\\%COMPILER%
set HBMK_FLAGS=

IF "%AWS_SDK_ROOT%"=="" GOTO error_no_aws_root

IF "%BUILD%"=="Debug" GOTO hbmk2_debug
goto hbmk2

:hbmk2_debug
set HBMK_FLAGS=-debug

:hbmk2
echo HBMK HOME: %HBMK_PATH%
call %HBMK_PATH%\\hbmk2.exe -comp=%COMPILER% %HBMK_FLAGS% %CWD%\hbaws.hbp || goto error_hbaws

echo ------------------------
echo HBAWS LIB build succeed
echo ------------------------
goto end

::
:: Errors
::

:error_no_aws_root
echo No 'AWS_SDK_ROOT' variable defined
goto end

:error_hbaws
echo Error building HBAWS
goto end

:end
