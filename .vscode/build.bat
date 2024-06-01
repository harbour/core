:: Generate 'exemplo' in developer mode (Windows)
:: This script will generate a full debug solution that include:
:: 'gtnap', 'hboffice' and 'exemplo'
@echo off
echo -----------------------
echo Exemplo Developer mode
echo -----------------------

:: Set the debug solution VisualStudio
call "%ProgramFiles(x86)%\Microsoft Visual Studio 11.0\VC\vcvarsall.bat" x64
set DEBUG_COMPILER=msvc64
set CMAKE_GENERATOR=Visual Studio 11 2012
set CMAKE_ARGS=-Ax64 -DCMAKE_C_COMPILER=cl -DCMAKE_CXX_COMPILER=cl -DCMAKE_WARN_VS11=OFF

:: Set the debug solution MinGW
@REM set DEBUG_COMPILER=mingw64
@REM set CMAKE_GENERATOR=MinGW Makefiles
@REM set CMAKE_ARGS=-G "MinGW Makefiles" -DCMAKE_C_COMPILER=gcc -DCMAKE_CXX_COMPILER=g++ -DCMAKE_BUILD_TYPE=Debug

@REM :: Generate
@REM :: Generate GTNAP
@REM set mpath=%cd%
@REM echo Main path: %mpath%
@REM cd .\\contrib\\gtnap
@REM set gtpath=%cd%
@REM echo GTNAP path: %gtpath%
@REM call build.bat -b Debug -comp %DEBUG_COMPILER% || goto error_build

@REM :: Generate exemplo sources
@REM cd %gtpath%\\tests\\cuademo\\gtnap_cualib
@REM echo Exemplo: %gtpath%\src\exemplo
@REM if not exist %gtpath%\src\exemplo mkdir %gtpath%\src\exemplo
@REM ..\\..\\..\\..\\..\\bin\\win\\%DEBUG_COMPILER%\\hbmk2.exe -debug -comp=%DEBUG_COMPILER% -keepc -workdir=%gtpath%\src\exemplo -o%gtpath%\build\exemplo exemplo.hbp || goto error_exemplo

@REM :: Generate hello sources
@REM cd %gtpath%\\tests\\hello
@REM echo Hello: %gtpath%\src\hello
@REM if not exist %gtpath%\src\hello mkdir %gtpath%\src\hello
@REM ..\\..\\..\\..\\bin\\win\\%DEBUG_COMPILER%\\hbmk2.exe -debug -comp=%DEBUG_COMPILER% -keepc -workdir=%gtpath%\src\hello -o%gtpath%\build\hello hello.hbp || goto error_hello

:: Generate the DEBUG solution
cd %gtpath%
cmake %CMAKE_ARGS% -S . -B build-dev -DGTNAP_DEVELOPER_MODE=ON -DGTNAP_LIBREOFFICE=ON || goto error_cmake
goto end

echo -------------------------------
echo GTNAP DEVELOPER build succeed
echo -------------------------------
goto end

:error_build
echo Error building GTNAP
exit 1

:error_exemplo
echo Error building Exemplo
exit 1

:error_hello
echo Error building Hello
exit 1

:error_cmake
echo Error generating CMake DEBUG solution
exit 1

:end

