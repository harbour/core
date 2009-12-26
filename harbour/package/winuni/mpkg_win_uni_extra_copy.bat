@rem
@rem $Id$
@rem

@echo off

@rem - Adjust target dir, mingw dirs, set HB_DIR_UPX, create required packages beforehand.
@rem - Run this from vanilla official source tree only.

xcopy /y RELNOTES                                                               F:\hb\hb20\
xcopy /y /s ..\..\examples\*.*                                                  F:\hb\hb20\examples\
xcopy /y /s ..\..\tests\*.*                                                     F:\hb\hb20\tests\
xcopy /y ..\..\bin\hb-mkimp.bat                                                 F:\hb\hb20\bin\
xcopy /y HARBOUR_README_ADDONS                                                  F:\hb\hb20\addons\
xcopy /y HARBOUR_README_DJGPP                                                   F:\hb\hb20\comp\djgpp\
xcopy /y HARBOUR_README_MINGW                                                   F:\hb\hb20\comp\mingw\
xcopy /y HARBOUR_README_MINGW64                                                 F:\hb\hb20\comp\mingw64\
xcopy /y HARBOUR_README_MINGWARM                                                F:\hb\hb20\comp\mingwarm\
xcopy /y HARBOUR_README_POCC                                                    F:\hb\hb20\comp\pocc\
xcopy /y HARBOUR_README_WATCOM                                                  F:\hb\hb20\comp\watcom\

xcopy /y /s ..\..\contrib\hbide\*.*                                             F:\hb\hb20\contrib\hbide\

xcopy /y /s ..\..\pkg\win\mingw\harbour-2.0.0-win-mingw                         F:\hb\hb20\

xcopy /y /s ..\..\pkg\linux\watcom\harbour-2.0.0-linux-watcom\lib               F:\hb\hb20\lib\linux\watcom\
xcopy /y /s ..\..\pkg\dos\watcom\hb200wa\lib                                    F:\hb\hb20\lib\
xcopy /y /s ..\..\pkg\os2\watcom\harbour-2.0.0-os2-watcom\lib                   F:\hb\hb20\lib\
xcopy /y /s ..\..\pkg\wce\mingwarm\harbour-2.0.0-wce-mingwarm\lib               F:\hb\hb20\lib\
xcopy /y /s ..\..\pkg\win\bcc\harbour-2.0.0-win-bcc\lib                         F:\hb\hb20\lib\
xcopy /y /s ..\..\pkg\win\mingw\harbour-2.0.0-win-mingw\lib                     F:\hb\hb20\lib\
xcopy /y /s ..\..\pkg\win\mingw64\harbour-2.0.0-win-mingw64\lib                 F:\hb\hb20\lib\
xcopy /y /s ..\..\pkg\win\msvc\harbour-2.0.0-win-msvc\lib                       F:\hb\hb20\lib\
xcopy /y /s ..\..\pkg\win\msvc64\harbour-2.0.0-win-msvc64\lib                   F:\hb\hb20\lib\
xcopy /y /s ..\..\pkg\win\watcom\harbour-2.0.0-win-watcom\lib                   F:\hb\hb20\lib\

xcopy /y    ..\..\pkg\wce\mingwarm\harbour-2.0.0-wce-mingwarm\bin\*.dll         F:\hb\hb20\bin\
xcopy /y    ..\..\pkg\win\bcc\harbour-2.0.0-win-bcc\bin\*.dll                   F:\hb\hb20\bin\
xcopy /y    ..\..\pkg\win\msvc64\harbour-2.0.0-win-msvc64\bin\*.dll             F:\hb\hb20\bin\

rem ; Using msvc64 because mingw64 .dll handling is broken.
 copy /y    ..\..\pkg\win\msvc64\harbour-2.0.0-win-msvc64\bin\harbour.exe       F:\hb\hb20\bin\harbour-x64.exe
 copy /y    ..\..\pkg\win\msvc64\harbour-2.0.0-win-msvc64\bin\hbpp.exe          F:\hb\hb20\bin\hbpp-x64.exe
 copy /y    ..\..\pkg\win\msvc64\harbour-2.0.0-win-msvc64\bin\hbformat-dll.exe  F:\hb\hb20\bin\hbformat-x64.exe
 copy /y    ..\..\pkg\win\msvc64\harbour-2.0.0-win-msvc64\bin\hbi18n-dll.exe    F:\hb\hb20\bin\hbi18n-x64.exe
 copy /y    ..\..\pkg\win\msvc64\harbour-2.0.0-win-msvc64\bin\hbmk2-dll.exe     F:\hb\hb20\bin\hbmk2-x64.exe
 copy /y    ..\..\pkg\win\msvc64\harbour-2.0.0-win-msvc64\bin\hbrun-dll.exe     F:\hb\hb20\bin\hbrun-x64.exe
 copy /y    ..\..\pkg\win\msvc64\harbour-2.0.0-win-msvc64\bin\hbtest-dll.exe    F:\hb\hb20\bin\hbtest-x64.exe

xcopy /y    "%HB_DIR_UPX%upx.exe"                                               F:\hb\hb20\bin\
 copy /y    "%HB_DIR_UPX%LICENSE"                                               F:\hb\hb20\bin\upx_LICENSE.txt

xcopy /y /s /e F:\devl\MinGW-441-tdm                                            F:\hb\hb20\comp\mingw\
del F:\hb\hb20\comp\mingw\tdm-mingw-1.908.0-4.4.1-2.exe

pushd

cd ..\..\contrib

for /F %%a in ( 'dir /b /ad' ) do (
   echo %%a
   xcopy /y /s %%a\*.hbc     F:\hb\hb20\contrib\%%a\
   xcopy /y /s %%a\tests\*.* F:\hb\hb20\contrib\%%a\tests\
   xcopy /y /s %%a\utils\*.* F:\hb\hb20\contrib\%%a\utils\
)

for /F %%a in ( 'dir /b /ad rddsql' ) do (
   echo %%a
   xcopy /y /s rddsql\%%a\*.hbc     F:\hb\hb20\contrib\rddsql\%%a\
   xcopy /y /s rddsql\%%a\tests\*.* F:\hb\hb20\contrib\rddsql\%%a\tests\
)

popd
