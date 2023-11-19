:: Build GTNAP and generate developer mode examples (Windows)

:: Set the compiler
call "%ProgramFiles(x86)%\Microsoft Visual Studio 11.0\VC\vcvarsall.bat" x64
set CMAKE_GENERATOR=Visual Studio 11 2012
echo %LIBREOFFICE_HOME%

:: Generate GTNAP
set mpath=%cd%
echo Main path: %mpath%
cd .\\contrib\\gtnap
set gtpath=%cd%
echo GTNAP path: %gtpath%
call build -b Debug

:: Generate exemplo sources
cd %gtpath%\\tests\\cuademo\\gtnap_cualib
echo Exemplo: %gtpath%\src\exemplo
if not exist %gtpath%\src\exemplo mkdir %gtpath%\src\exemplo
..\\..\\..\\..\\..\\bin\\win\\msvc64\\hbmk2.exe exemplo.hbp -debug -trace -keepc -workdir=%gtpath%\src\exemplo -o%gtpath%\build\exemplo || goto error

:: Generate hello sources
cd %gtpath%\\tests\\hello
echo Hello: %gtpath%\src\hello
if not exist %gtpath%\src\hello mkdir %gtpath%\src\hello
..\\..\\..\\..\\bin\\win\\msvc64\\hbmk2 hello.hbp -debug -trace -keepc -workdir=%gtpath%\src\hello -o%gtpath%\build\hello

:: Generate VS solution
cd %gtpath%
cmake -S . -B build-dev -Ax64 -DCMAKE_WARN_VS11=OFF -DGTNAP_DEVELOPER_MODE=ON
goto end

@REM :: Run exemplo
@REM cd %gtpath%\\build
@REM START /B exemplo --hb:gtnap
@REM ::START /B exemplo --hb:gtwin
@REM goto end

:error
exit 1

:end

