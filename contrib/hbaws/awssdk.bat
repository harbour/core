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
set BUILD=Release
set "CWD=%cd%"

:parse
IF "%~1"=="" GOTO endparse
IF "%~1"=="-b" GOTO build
SHIFT
GOTO parse

:build
SHIFT
set BUILD=%~1
SHIFT
GOTO parse

:endparse

::
:: Beginning
::
echo ---------------------------
echo Generating AWS-SDK
echo AWS_SDK_ROOT: %AWS_SDK_ROOT%
echo Build type: %BUILD%
echo COMPILER: %COMPILER%
echo OPERATION: %OPERATION%
echo ---------------------------

IF "%AWS_SDK_ROOT%"=="" GOTO error_no_root

:download_aws
IF exist %AWS_SDK_ROOT%\src goto check_aws
git clone --recurse-submodules --depth 1 --branch 1.11.271 https://github.com/aws/aws-sdk-cpp %AWS_SDK_ROOT%\src

:check_aws
cd %AWS_SDK_ROOT%\src
call git status > NUL || goto error_no_git

:build
@REM IF exist %AWS_SDK_ROOT%\build rmdir /s /q %AWS_SDK_ROOT%\build

@REM call cmake -S %AWS_SDK_ROOT%\src -B %AWS_SDK_ROOT%\build -DCMAKE_INSTALL_PREFIX=%AWS_SDK_ROOT% -DBUILD_ONLY="s3" -DENABLE_TESTING=OFF -DENABLE_ZLIB_REQUEST_COMPRESSION=OFF -DAWS_SDK_WARNINGS_ARE_ERRORS=OFF -DBUILD_SHARED_LIBS=OFF -A x64 || goto error_cmake

@REM call cmake --build %AWS_SDK_ROOT%\build --config %BUILD% || goto error_build

@REM call cmake --install %AWS_SDK_ROOT%\build --config %BUILD% || goto error_install

@REM echo INSTALL SUCCESS


goto end

::
:: Error section
::
:error_no_root
echo 'AWS_SDK_ROOT' is empty.
goto end

:error_no_git
echo %AWS_SDK_ROOT%\src is not a git repository. Please remove this folder and try again.
goto end

:error_cmake
echo Error in AWS-SDK CMake configure process
goto end

:error_build
echo Error in AWS-SDK build process
goto end

:error_install
echo Error in AWS-SDK install process
goto end

:end
cd %CWD%

@REM ::
@REM :: Input parameters
@REM ::
@REM set OPERATION=dll
@REM set COMPILER=mingw64
@REM set BUILD=Release
@REM set "CWD=%cd%"

@REM :parse
@REM IF "%~1"=="" GOTO endparse
@REM IF "%~1"=="-dll" GOTO dll
@REM IF "%~1"=="-lib" GOTO lib
@REM IF "%~1"=="-comp" GOTO compiler
@REM IF "%~1"=="-b" GOTO build
@REM SHIFT
@REM GOTO parse

@REM :dll
@REM set OPERATION=dll
@REM set COMPILER=msvc64
@REM SHIFT
@REM GOTO parse

@REM :lib
@REM set OPERATION=lib
@REM SHIFT
@REM GOTO parse

@REM :build
@REM SHIFT
@REM set BUILD=%~1
@REM SHIFT
@REM GOTO parse

@REM :compiler
@REM SHIFT
@REM set COMPILER=%~1
@REM SHIFT
@REM GOTO parse

@REM :endparse

@REM ::
@REM :: Beginning
@REM ::
@REM echo ---------------------------
@REM echo Generating LibreOffice
@REM echo Main path: %CWD%
@REM echo Build type: %BUILD%
@REM echo COMPILER: %COMPILER%
@REM echo OPERATION: %OPERATION%
@REM echo ---------------------------

@REM IF "%OPERATION%"=="dll" GOTO generate_dll
@REM IF "%OPERATION%"=="lib" GOTO generate_lib
@REM goto error_operation

@REM ::
@REM :: Generate dynamic library
@REM ::
@REM :generate_dll
@REM set CMAKE_ARGS=-Ax64 -DCMAKE_C_COMPILER=cl -DCMAKE_CXX_COMPILER=cl -DCMAKE_WARN_VS11=OFF
@REM set CMAKE_BUILD=--config %BUILD%
@REM call cmake %CMAKE_ARGS% -S %CWD% -B %CWD%\build || goto error_cmake
@REM call cmake --build %CWD%\build %CMAKE_BUILD% || goto error_build
@REM del %CWD%\build\%BUILD%\lib\core.*
@REM del %CWD%\build\%BUILD%\lib\osbs.*
@REM del %CWD%\build\%BUILD%\lib\sewer.*

@REM echo ---------------------------
@REM echo OFFICESDK DLL build succeed
@REM echo ---------------------------
@REM goto end

@REM ::
@REM :: Generate static library
@REM ::
@REM :generate_lib
@REM set HBMK_PATH=..\\..\\bin\\win\\%COMPILER%
@REM set HBMK_FLAGS=

@REM IF "%BUILD%"=="Debug" GOTO hbmk2_debug
@REM goto hbmk2

@REM :hbmk2_debug
@REM set HBMK_FLAGS=-debug

@REM :hbmk2
@REM echo HBMK HOME: %HBMK_PATH%
@REM call %HBMK_PATH%\\hbmk2.exe -comp=%COMPILER% %HBMK_FLAGS% %CWD%\src\hboffice\hboffice.hbp || goto error_hboffice

@REM echo ---------------------------
@REM echo HBOFFICE LIB build succeed
@REM echo ---------------------------
@REM goto end

@REM ::
@REM :: Errors
@REM ::
@REM :error_compiler
@REM echo Unknown compiler
@REM goto end

@REM :error_cmake
@REM echo Error in NAppGUI CMake generate
@REM goto end

@REM :error_build
@REM echo Error building NAppGUI
@REM goto end

@REM :error_hboffice
@REM echo Error building HBOFFICE
@REM goto end

@REM :error_operation
@REM echo Invalid operation '%OPERATION%'
@REM goto end

@REM :end
