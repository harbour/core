:: Build GTNAP and run exemplo (cuademo) in Windows (Developer mode)

:: Set the compiler
call "%ProgramFiles(x86)%\Microsoft Visual Studio 11.0\VC\vcvarsall.bat"

:: Generate GTNAP
set "cwd=%cd%"
echo %cd%
cd .\\contrib\\gtnap
echo %cd%
call build -b Debug
cd %cwd%

:: Generate exemplo
echo %cd%
cd .\\tests\\cuademo\\gtnap_cualib
echo %cd%
if not exist %cwd%\src\exemplo mkdir %cwd%\src\exemplo
..\\..\\..\\..\\..\\bin\\win\\msvc\\hbmk2.exe -debug -trace -keepc -workdir=%cwd%\src\exemplo -o%cwd%\build\exemplo exemplo.hbp || goto error

:: Generate exemplo debug project
cd %cwd%
cmake -S src -B build_cuademo -DCMAKE_WARN_VS11=OFF -DGTNAP_DEVELOPER_MODE=ON

:: Run exemplo
cd %cwd%\\build
::START /B exemplo --hb:gtnap
::START /B exemplo --hb:gtwin
goto end

:error
exit 1

:end

