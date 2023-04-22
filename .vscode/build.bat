:: Build and run cuademo in Windows (Developer mode)

:: Set the compiler
call "%ProgramFiles(x86)%\Microsoft Visual Studio 11.0\VC\vcvarsall.bat"

:: Generate GTNAP
set "cwd=%cd%"
echo %cd%
cd .\\contrib\\gtnap
echo %cd%
call build -b Debug
cd %cwd%

:: Generate cuademo
echo %cd%
cd .\\tests\\cuademo\\gtnap_cualib
echo %cd%
..\\..\\..\\..\\..\\bin\\win\\msvc\\hbmk2.exe -debug -trace -workdir=./build exemplo.hbp || goto error

START /B exemplo --hb:gtnap
::START /B exemplo --hb:gtwin
goto end

:error
exit 1

:end

