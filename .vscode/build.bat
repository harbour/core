:: Build GTNAP and run exemplo (cuademo) in Windows (Developer mode)

:: Set the compiler
call "%ProgramFiles(x86)%\Microsoft Visual Studio 11.0\VC\vcvarsall.bat"

:: Generate GTNAP
set mpath=%cd%
echo Main path: %mpath%
cd .\\contrib\\gtnap
set gtpath=%cd%
echo GTNAP path: %gtpath%
call build -b Debug

:: Generate exemplo
cd %gtpath%\\tests\\cuademo\\gtnap_cualib
echo Exemplo: %gtpath%\src\exemplo
if not exist %gtpath%\src\exemplo mkdir %gtpath%\src\exemplo
..\\..\\..\\..\\..\\bin\\win\\msvc\\hbmk2.exe -debug -trace -keepc -workdir=%gtpath%\src\exemplo -o%gtpath%\build\exemplo exemplo.hbp || goto error

:: Generate exemplo debug project
cd %gtpath%
cmake -S src -B build_cuademo -DCMAKE_WARN_VS11=OFF -DGTNAP_DEVELOPER_MODE=ON

:: Run exemplo
cd %gtpath%\\build
START /B exemplo --hb:gtnap
::START /B exemplo --hb:gtwin
goto end

:error
exit 1

:end

