@echo off
PUSHD
:: Cambiare questi set con i valori del proprio PC
set STARTDRIVE=%CD%
set HB_INSTALL_PREFIX=%~dp0
set HB_COMP_VER=64
set HB_BUILD_PARTS=compiler
CALL "c:\Program Files (x86)\Microsoft Visual Studio\2019\Community\VC\Auxiliary\Build\vcvars64.bat"
set path=%PATH%;%HB_INSTALL_PREFIX%\bin\win\msvc;"C:\Program Files\7-Zip";"c:\Program Files\Notepad++"
POPD
harbour -q
