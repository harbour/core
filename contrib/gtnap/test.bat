::
:: Testing GTNAP compiled with MinGW
::

@echo off
set "CWD=%cd%"
rmdir /s /q build
call build.bat -comp mingw64 -b Debug
call build.bat -comp mingw64 -b Release

cd tests\cuademo\gtnap_cualib
del /q *.exe
del /q *.dll
copy ..\..\..\..\hboffice\build\Release\bin\officesdk.dll
..\..\..\..\..\bin\win\mingw64\hbmk2.exe -comp=mingw64 exemplo.hbp
exemplo --hb:gtnap
cd ..
cd ..
cd ..