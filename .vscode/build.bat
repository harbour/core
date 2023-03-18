call "%ProgramFiles(x86)%\Microsoft Visual Studio 11.0\VC\vcvarsall.bat"
set "cwd=%cd%"
echo %cd%
cd .\\contrib\\gtnap
echo %cd%
..\\..\\bin\\win\\msvc\\hbmk2.exe gtnap.hbp
cd %cwd%
echo %cd%
cd .\\contrib\\gtnap\\tests\\cuademo\\gtnap_cualib
echo %cd%
..\\..\\..\\..\\..\\bin\\win\\msvc\\hbmk2.exe exemplo.hbp
START /B exemplo --hb:gtnap
START /B exemplo --hb:gtwin
