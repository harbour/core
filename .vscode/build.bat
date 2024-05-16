:: Build GTNAP and generate developer mode examples (Windows)
@echo off
echo -----------------------
echo GTNAP Developer mode
echo -----------------------

:: Set the debug solution VisualStudio
@REM call "%ProgramFiles(x86)%\Microsoft Visual Studio 11.0\VC\vcvarsall.bat" x64
@REM set DEBUG_COMPILER=msvc64
@REM set CMAKE_GENERATOR=Visual Studio 11 2012
@REM set CMAKE_ARGS=-Ax64 -DCMAKE_C_COMPILER=cl -DCMAKE_CXX_COMPILER=cl -DCMAKE_WARN_VS11=OFF

:: Set the debug solution MinGW
set DEBUG_COMPILER=mingw64
set CMAKE_GENERATOR=MinGW Makefiles
set CMAKE_ARGS=-G "MinGW Makefiles" -DCMAKE_C_COMPILER=gcc -DCMAKE_CXX_COMPILER=g++ -DCMAKE_BUILD_TYPE=Debug

:: Generate GTNAP
set mpath=%cd%
echo Main path: %mpath%
cd .\\contrib\\gtnap
set gtpath=%cd%
echo GTNAP path: %gtpath%
call build.bat -b Debug -comp %DEBUG_COMPILER% || goto error_build

:: Generate exemplo sources
cd %gtpath%\\tests\\cuademo\\gtnap_cualib
echo Exemplo: %gtpath%\src\exemplo
if not exist %gtpath%\src\exemplo mkdir %gtpath%\src\exemplo
..\\..\\..\\..\\..\\bin\\win\\%DEBUG_COMPILER%\\hbmk2.exe -debug -comp=%DEBUG_COMPILER% -keepc -workdir=%gtpath%\src\exemplo -o%gtpath%\build\exemplo exemplo.hbp || goto error_exemplo

:: Generate hello sources
cd %gtpath%\\tests\\hello
echo Hello: %gtpath%\src\hello
if not exist %gtpath%\src\hello mkdir %gtpath%\src\hello
..\\..\\..\\..\\bin\\win\\%DEBUG_COMPILER%\\hbmk2.exe -debug -comp=%DEBUG_COMPILER% -keepc -workdir=%gtpath%\src\hello -o%gtpath%\build\hello hello.hbp || goto error_hello

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

