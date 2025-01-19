::
:: AWS-SDK build script
::
:: Will download and build the AWS-SDK-CPP libraries.
:: This is a prerequisite before build HBAWS and applications that depend on it.
::
:: awssdk -b [Debug|Release] -comp [mingw64|msvc64]
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
IF "%~1"=="-comp" GOTO compiler
IF "%~1"=="-b" GOTO build
SHIFT
GOTO parse

:compiler
SHIFT
set COMPILER=%~1
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
echo ---------------------------

:: Check 'AWS_SDK_ROOT'
IF "%AWS_SDK_ROOT%"=="" GOTO error_no_root

:download_aws
:: clone AWS-SDK repo and apply MinGW patch
IF exist %AWS_SDK_ROOT%\src goto check_aws
git clone --recurse-submodules --depth 1 --branch 1.11.271 https://github.com/aws/aws-sdk-cpp %AWS_SDK_ROOT%\src
cd %AWS_SDK_ROOT%\src
git apply %CWD%\prj\mingw.patch

:check_aws
cd %AWS_SDK_ROOT%\src
call git status > NUL || goto error_no_git

:clean
:: Clean previous build and install folder
IF exist %AWS_SDK_ROOT%\build rmdir /s /q %AWS_SDK_ROOT%\build
IF exist %AWS_SDK_ROOT%\%COMPILER%\%BUILD% rmdir /s /q %AWS_SDK_ROOT%\%COMPILER%\%BUILD%

:build
IF "%COMPILER%"=="mingw64" GOTO build_mingw
IF "%COMPILER%"=="clang" GOTO build_clang
IF "%COMPILER%"=="msvc64" GOTO build_msvc
goto error_compiler

:build_mingw
:: IMPORTANT!! MinGW static link build is broken.
:: At the moment MinGW based HBAWS apps must to redistribute AWS Dlls
call cmake -G "MinGW Makefiles" -S %AWS_SDK_ROOT%\src -B %AWS_SDK_ROOT%\build -DCMAKE_INSTALL_PREFIX=%AWS_SDK_ROOT%\%COMPILER%\%BUILD% -DCMAKE_BUILD_TYPE=%BUILD% -DCMAKE_C_COMPILER=gcc -DCMAKE_CXX_COMPILER=g++ -DBUILD_ONLY="s3" -DENABLE_TESTING=OFF -DENABLE_ZLIB_REQUEST_COMPRESSION=OFF -DAWS_SDK_WARNINGS_ARE_ERRORS=OFF -DBUILD_SHARED_LIBS=ON || goto error_cmake
call cmake --build %AWS_SDK_ROOT%\build || goto error_build
call cmake --install %AWS_SDK_ROOT%\build --config %BUILD% || goto error_install
goto build_ok

:build_clang
:: IMPORTANT!! Clang static link build is broken.
:: At the moment Clang based HBAWS apps must to redistribute AWS Dlls
call cmake -G "MinGW Makefiles" -S %AWS_SDK_ROOT%\src -B %AWS_SDK_ROOT%\build -DCMAKE_INSTALL_PREFIX=%AWS_SDK_ROOT%\%COMPILER%\%BUILD% -DCMAKE_BUILD_TYPE=%BUILD% -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++ -DBUILD_ONLY="s3" -DENABLE_TESTING=OFF -DENABLE_ZLIB_REQUEST_COMPRESSION=OFF -DAWS_SDK_WARNINGS_ARE_ERRORS=OFF -DBUILD_SHARED_LIBS=ON || goto error_cmake
call cmake --build %AWS_SDK_ROOT%\build || goto error_build
call cmake --install %AWS_SDK_ROOT%\build --config %BUILD% || goto error_install
goto build_ok

:build_msvc
call cmake -S %AWS_SDK_ROOT%\src -B %AWS_SDK_ROOT%\build -DCMAKE_INSTALL_PREFIX=%AWS_SDK_ROOT%\%COMPILER%\%BUILD% -DCMAKE_C_COMPILER=cl -DCMAKE_CXX_COMPILER=cl -DBUILD_ONLY="s3" -DENABLE_TESTING=OFF -DENABLE_ZLIB_REQUEST_COMPRESSION=OFF -DAWS_SDK_WARNINGS_ARE_ERRORS=OFF -DBUILD_SHARED_LIBS=ON -A x64 || goto error_cmake
call cmake --build %AWS_SDK_ROOT%\build --config %BUILD% || goto error_build
call cmake --install %AWS_SDK_ROOT%\build --config %BUILD% || goto error_install
goto build_ok

:build_ok
echo AWS build and install process successfully
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

:error_compiler
echo %COMPILER% is not a valid value [mingw64|msvc64]
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
IF exist %AWS_SDK_ROOT%\build rmdir /s /q %AWS_SDK_ROOT%\build
cd %CWD%
