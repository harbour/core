:: Build HBOFFICE development solution (Windows)
:: WIP More things to do
@echo off
echo ------------------------
echo HBOFFICE Developer mode
echo ------------------------

:: Set the debug solution in VisualStudio
call "%ProgramFiles(x86)%\Microsoft Visual Studio 11.0\VC\vcvarsall.bat" x64
set CMAKE_GENERATOR=Visual Studio 11 2012
set DEBUG_COMPILER=msvc64
set CMAKE_ARGS=-Ax64 -DCMAKE_C_COMPILER=cl -DCMAKE_CXX_COMPILER=cl -DCMAKE_WARN_VS11=OFF

:: Begin
set hbpath=%cd%\\..\\..
set hbofficepath=%cd%
echo Main HBOFFICE path: %hbofficepath%

:: Generate HBOFFICE in Debug
call build.bat -dll -b Debug || goto error_build
call build.bat -lib -b Debug -comp %DEBUG_COMPILER% || goto error_build

:: Generate test sources
cd %hbofficepath%\\tests
echo sheet1: %hbofficepath%\src\sheet1
if not exist %hbofficepath%\src\sheet1 mkdir %hbofficepath%\src\sheet1
%hbpath%\\bin\\win\\%DEBUG_COMPILER%\\hbmk2.exe -debug -comp=%DEBUG_COMPILER% -keepc -workdir=%hbofficepath%\src\sheet1 -o%hbofficepath%\build\sheet1 sheet1.hbp || goto error_test

:: Generate the DEBUG solution
cd %hbofficepath%
cmake %CMAKE_ARGS% -S . -B build-dev -DHBOFFICE_DEVELOPER_MODE=ON || goto error_cmake

echo ----------------------------------
echo HBOFFICE DEVELOPER build succeed
echo ----------------------------------
goto end

:error_build
echo Error building hboffice debug
goto end

:error_test
echo Error building test sources
goto end

:error_cmake
echo Error generating CMake DEBUG solution
goto end

:end
cd %hbofficepath%
