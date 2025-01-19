::
:: Build all script
:: Harbour/GTNAP/HBAWS/HBOFFICE
:: Using different compilers (VS, MinGW-GCC, MinGW-Clang)
:: Important! This script will remove previous Harbour compilations
:: It can take several minutes to finish all tasks
::
:: Options: -comp [msvc64|mingw64|clang]  (mingw64 default)
::          -b [Debug|Release]            (Release default)
::
:: build_all.bat -comp msvc64 -b Release
::
@echo off
set ALL_BUILD_COMPILER=mingw64
set BUILD=Release
set HBMK_FLAGS=

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
set ALL_BUILD_COMPILER=%~1
SHIFT
GOTO parse

:endparse

cd ..

:: Remove previous compilations
rmdir /s /q bin\win
rmdir /s /q lib\win
win-make clean

:build_harbour

IF "%ALL_BUILD_COMPILER%"=="msvc64" GOTO harbour_vs
IF "%ALL_BUILD_COMPILER%"=="mingw64" GOTO harbour_mingw
IF "%ALL_BUILD_COMPILER%"=="clang" GOTO harbour_clang
goto error_unknown_compiler

IF "%BUILD%"=="Debug" SET HBMK_FLAGS=-debug

:: Compile Harbour using Visual Studio
:harbour_vs
:: Use of Visual Studio 2017. Change two next commands to use another version
:: This command allow all MSVC tools available for Harbour compiler.
call "%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Community\VC\Auxiliary\Build\vcvarsall.bat" x64
:: Set generator for all CMake-based build scripts
set CMAKE_GENERATOR=Visual Studio 15 2017

call win-make -j4 HB_CPU=x86_64 HB_COMPILER=msvc64 || goto error_harbour_vs
echo ----------------------------------
echo Harbour msvc64 build successfully
echo ----------------------------------
goto hboffice

:: Compile Harbour Mingw-64
:harbour_mingw
call mingw32-make.exe -j4 HB_CPU=x86_64 HB_COMPILER=mingw64
echo -----------------------------------
echo Harbour mingw64 build successfully
echo -----------------------------------
goto hboffice

:: Compile Harbour clang
:harbour_clang
call mingw32-make.exe -j4 HB_CPU=x86_64 HB_COMPILER=clang
echo ---------------------------------
echo Harbour clang build successfully
echo ---------------------------------
goto hboffice

:hboffice
:hboffice_dll_build
cd contrib\hboffice
rmdir /s /q build

:: The LibreOffice dll MUST to be compiled with Visual Studio (no MinGW/Clang support from LibreOffice)
:: Use of Visual Studio 2017. Change two next commands to use another version
:: This command allow all MSVC tools available for Harbour compiler.
call "%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Community\VC\Auxiliary\Build\vcvarsall.bat" x64
:: Set generator for all CMake-based build scripts
set CMAKE_GENERATOR=Visual Studio 15 2017
call build.bat -dll -b %BUILD% || goto error_hboffice_dll
echo --------------------------------
echo hboffice dll build successfully
echo --------------------------------

:hboffice_lib_build
call build.bat -lib -comp %ALL_BUILD_COMPILER% -b %BUILD% || goto error_hboffice_lib
echo --------------------------------
echo hboffice lib build successfully
echo --------------------------------

:: Return to harbour main path after hboffice
cd ..
cd ..

:hbaws_build
cd contrib\hbaws
rmdir /s /q build
call build.bat -comp %ALL_BUILD_COMPILER% -b %BUILD% || goto error_hbaws
echo -------------------------
echo hbaws build successfully
echo -------------------------

:: Return to harbour main path after hboffice
cd ..
cd ..

:gtnap_build
cd contrib\gtnap
rmdir /s /q build
call build.bat -comp %ALL_BUILD_COMPILER% -b %BUILD% || goto error_gtnap
echo -------------------------
echo gtnap build successfully
echo -------------------------

:gtnap_exemplo_test
cd tests/cuademo/gtnap_cualib
del /q *.exe
del /q *.dll
copy ..\..\..\..\hboffice\build\%BUILD%\bin\officesdk.dll
..\..\..\..\..\bin\win\%ALL_BUILD_COMPILER%\hbmk2.exe %HBMK_FLAGS% -comp=%ALL_BUILD_COMPILER% exemplo.hbp
exemplo --hb:gtnap

:: Return to gtnap path
cd ..
cd ..
cd ..

:: Return to harbour main path after hboffice
cd ..
cd ..


echo ---------------------------------------
echo All build jobs generated successfully
echo ---------------------------------------

cd nap-dev
goto end

::
:: Errors
::
:error_unknown_compiler
echo Error Unknown compiler %ALL_BUILD_COMPILER%
goto end

:error_harbour_vs
echo Error building Harbour using VisualStudio (msvc64)
goto end

:error_harbour_gcc
echo Error building Harbour using MinGW-GCC (mingw64)
goto end

:error_harbour_clang
echo Error building Harbour using Clang (clang)
goto end

:error_hboffice_dll
echo Error building HBOFFICE dll
goto end

:error_hboffice_lib
echo Error building HBOFFICE lib
goto end

:error_hbaws
echo Error building HBAWS
goto end

:error_hbaws
echo Error building GTNAP
goto end

:end
