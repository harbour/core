::
:: Testing HBOffice compiled with MinGW
::

@echo off
set "CWD=%cd%"
rmdir /s /q build
call build.bat -dll -b Debug
call build.bat -lib -comp mingw64 -b Debug
call build.bat -dll -b Release
call build.bat -lib -comp mingw64 -b Release
cd tests
del /q result\*
del /q *.exe
del /q *.dll
copy ..\build\Release\bin\officesdk.dll
..\..\..\bin\win\mingw64\hbmk2 sheet1.prg hboffice.hbc -comp=mingw64
..\..\..\bin\win\mingw64\hbmk2 sheet2.prg hboffice.hbc -comp=mingw64
..\..\..\bin\win\mingw64\hbmk2 doc1.prg hboffice.hbc -comp=mingw64
..\..\..\bin\win\mingw64\hbmk2 doc2.prg hboffice.hbc -comp=mingw64
sheet1
sheet2
doc1
doc2
cd ..