@echo off

:: ---------------------------------------------------------------
:: Copyright 2009-2015 Viktor Szakats (vszakats.net/harbour)
:: See COPYING.txt for licensing terms.
:: ---------------------------------------------------------------

:: - Adjust target dir, mingw dirs, set HB_DIR_UPX, HB_DIR_7Z, HB_DIR_MINGW,
::   create required packages beforehand.
:: - Requires BCC in PATH or HB_DIR_BCC_IMPLIB (for implib).
:: - Run this from vanilla official source tree only.
:: - Requires GNU sed tool in PATH

echo ! Self: %0

if "%HB_VS%" == "" set HB_VS=34
if "%HB_VL%" == "" set HB_VL=340
if "%HB_VM%" == "" set HB_VM=3.4
if "%HB_VF%" == "" set HB_VF=3.4.0dev
if "%HB_RT%" == "" set HB_RT=C:\hb\

set HB_DR=hb%HB_VS%\
set HB_ABSROOT=%HB_RT%%HB_DR%

:: Autodetect the base bitness, by default it will be 32-bit,
:: and 64-bit if it's the only one available.

if exist "%~dp0..\..\pkg\win\mingw\harbour-%HB_VF%-win-mingw" (
   :: mingw 32-bit base system
   set BASE=mingw
) else if exist "%~dp0..\..\pkg\win\mingw64\harbour-%HB_VF%-win-mingw64" (
   :: mingw 64-bit base system
   set BASE=mingw64
)

:: Assemble unified package from per-target builds

if exist "%HB_ABSROOT%" rd /q /s "%HB_ABSROOT%"

xcopy /y /s /q "%~dp0..\..\extras\*.*"                                                      "%HB_ABSROOT%extras\"
xcopy /y /s /q "%~dp0..\..\tests\*.*"                                                       "%HB_ABSROOT%tests\"
xcopy /y       "%~dp0HARBOUR_README_ADDONS.txt"                                             "%HB_ABSROOT%addons\"
xcopy /y       "%~dp0HARBOUR_README_DJGPP.txt"                                              "%HB_ABSROOT%comp\djgpp\"
xcopy /y       "%~dp0HARBOUR_README_MINGW.txt"                                              "%HB_ABSROOT%comp\mingw\"
xcopy /y       "%~dp0HARBOUR_README_MINGWARM.txt"                                           "%HB_ABSROOT%comp\mingwarm\"
xcopy /y       "%~dp0HARBOUR_README_POCC.txt"                                               "%HB_ABSROOT%comp\pocc\"
xcopy /y       "%~dp0HARBOUR_README_WATCOM.txt"                                             "%HB_ABSROOT%comp\watcom\"

if "%BASE%" == "32" xcopy /y /s /q "%~dp0..\..\pkg\win\mingw\harbour-%HB_VF%-win-mingw" "%HB_ABSROOT%"
if "%BASE%" == "64" xcopy /y /s /q "%~dp0..\..\pkg\win\mingw64\harbour-%HB_VF%-win-mingw64" "%HB_ABSROOT%"

xcopy /y /s    "%~dp0..\..\pkg\linux\watcom\harbour-%HB_VF%-linux-watcom\lib"               "%HB_ABSROOT%lib\linux\watcom\" 2> nul
xcopy /y /s    "%~dp0..\..\pkg\dos\watcom\hb%HB_VL%wa\lib"                                  "%HB_ABSROOT%lib\" 2> nul
xcopy /y /s    "%~dp0..\..\pkg\os2\watcom\harbour-%HB_VF%-os2-watcom\lib"                   "%HB_ABSROOT%lib\" 2> nul
xcopy /y /s    "%~dp0..\..\pkg\wce\mingwarm\harbour-%HB_VF%-wce-mingwarm\lib"               "%HB_ABSROOT%lib\" 2> nul
xcopy /y /s    "%~dp0..\..\pkg\win\bcc\harbour-%HB_VF%-win-bcc\lib"                         "%HB_ABSROOT%lib\" 2> nul
xcopy /y /s    "%~dp0..\..\pkg\win\bcc64\harbour-%HB_VF%-win-bcc64\lib"                     "%HB_ABSROOT%lib\" 2> nul
xcopy /y /s    "%~dp0..\..\pkg\win\mingw\harbour-%HB_VF%-win-mingw\lib"                     "%HB_ABSROOT%lib\" 2> nul
xcopy /y /s    "%~dp0..\..\pkg\win\mingw64\harbour-%HB_VF%-win-mingw64\lib"                 "%HB_ABSROOT%lib\" 2> nul
xcopy /y /s    "%~dp0..\..\pkg\win\msvc\harbour-%HB_VF%-win-msvc\lib"                       "%HB_ABSROOT%lib\" 2> nul
xcopy /y /s    "%~dp0..\..\pkg\win\msvc64\harbour-%HB_VF%-win-msvc64\lib"                   "%HB_ABSROOT%lib\" 2> nul
xcopy /y /s    "%~dp0..\..\pkg\win\watcom\harbour-%HB_VF%-win-watcom\lib"                   "%HB_ABSROOT%lib\" 2> nul

xcopy /y       "%~dp0..\..\pkg\win\mingw64\harbour-%HB_VF%-win-mingw64\bin\*.dll"           "%HB_ABSROOT%bin\" 2> nul
xcopy /y       "%~dp0..\..\pkg\wce\mingwarm\harbour-%HB_VF%-wce-mingwarm\bin\*.dll"         "%HB_ABSROOT%bin\" 2> nul

:: Create special implibs for Borland (requires BCC in PATH)
:: NOTE: Using intermediate .def files, because direct .dll to .lib conversion
::       is buggy in BCC55 and BCC58 (no other versions tested), leaving off
::       leading underscore from certain ("random") symbols, resulting in
::       unresolved externals, when trying to use it. [vszakats]
for %%a in ( "%HB_ABSROOT%bin\*-%HB_VS%.dll" ) do (
   "%HB_DIR_BCC_IMPLIB%impdef.exe" -a "%HB_ABSROOT%lib\win\bcc\%%~na-bcc.defraw" "%%a"
   echo s/LIBRARY     %%~na.DLL/LIBRARY     "%%~na.dll"/Ig> _hbtemp.sed
   sed -f _hbtemp.sed < "%HB_ABSROOT%lib\win\bcc\%%~na-bcc.defraw" > "%HB_ABSROOT%lib\win\bcc\%%~na-bcc.def"
   "%HB_DIR_BCC_IMPLIB%implib.exe" -c -a "%HB_ABSROOT%lib\win\bcc\%%~na-bcc.lib" "%HB_ABSROOT%lib\win\bcc\%%~na-bcc.def"
   del "%HB_ABSROOT%lib\win\bcc\%%~na-bcc.defraw"
   del "%HB_ABSROOT%lib\win\bcc\%%~na-bcc.def"
)
if exist _hbtemp.sed del _hbtemp.sed

 copy /y       "%~dp0..\..\pkg\win\mingw64\harbour-%HB_VF%-win-mingw64\bin\hbmk2.exe"     "%HB_ABSROOT%bin\hbmk2-x64.exe"
 copy /y       "%~dp0..\..\pkg\win\mingw64\harbour-%HB_VF%-win-mingw64\bin\hbnetio.exe"   "%HB_ABSROOT%bin\hbnetio-x64.exe"
 copy /y       "%~dp0..\..\pkg\win\mingw64\harbour-%HB_VF%-win-mingw64\bin\hbrun.exe"     "%HB_ABSROOT%bin\hbrun-x64.exe"
 copy /y       "%~dp0..\..\pkg\win\mingw64\harbour-%HB_VF%-win-mingw64\bin\hbspeed.exe"   "%HB_ABSROOT%bin\hbspeed-x64.exe"
 copy /y       "%~dp0..\..\pkg\win\mingw64\harbour-%HB_VF%-win-mingw64\bin\hbtest.exe"    "%HB_ABSROOT%bin\hbtest-x64.exe"

xcopy /y       "%HB_DIR_UPX%upx.exe"                                                      "%HB_ABSROOT%bin\"
 copy /y       "%HB_DIR_UPX%LICENSE"                                                      "%HB_ABSROOT%bin\upx_LICENSE.txt"

xcopy /y /s /q /e "%HB_DIR_MINGW%"                                                        "%HB_ABSROOT%comp\mingw\"

:: Copy mingw runtime .dlls

:: Pick the ones from a multi-target mingw distro
:: that match the bitness of our base.
set _MINGW_DLL_DIR=%HB_DIR_MINGW%\bin
if "%BASE%" == "32" if exist "%HB_DIR_MINGW%\x86_64-w64-mingw32\lib32" set _MINGW_DLL_DIR=%HB_DIR_MINGW%\x86_64-w64-mingw32\lib32
if "%BASE%" == "64" if exist "%HB_DIR_MINGW%\i686-w64-mingw32\lib64"   set _MINGW_DLL_DIR=%HB_DIR_MINGW%\i686-w64-mingw32\lib64

if exist "%_MINGW_DLL_DIR%\libgcc_s_*.dll" xcopy /y "%HB_DIR_MINGW%\bin\libgcc_s_*.dll" "%HB_ABSROOT%bin\"
if exist "%_MINGW_DLL_DIR%\mingwm10.dll"   xcopy /y "%HB_DIR_MINGW%\bin\mingwm10.dll"   "%HB_ABSROOT%bin\"

:: Delete stuff from compiler folder we don't need

rd /q /s  "%HB_ABSROOT%comp\mingw\etc\" 2> nul
rd /q /s  "%HB_ABSROOT%comp\mingw\opt\" 2> nul
rd /q /s  "%HB_ABSROOT%comp\mingw\share\" 2> nul
del /q /f "%HB_ABSROOT%comp\mingw\bin\gdb*.exe" 2> nul
del /q /f "%HB_ABSROOT%comp\mingw\bin\gfortran.exe" 2> nul
del /q /f "%HB_ABSROOT%comp\mingw\bin\gnat*.exe" 2> nul
del /q /f "%HB_ABSROOT%comp\mingw\bin\libgfortran-*.dll" 2> nul
del /q /f "%HB_ABSROOT%comp\mingw\bin\libgnarl-*.dll" 2> nul
del /q /f "%HB_ABSROOT%comp\mingw\bin\libgnat-*.dll" 2> nul

:: 32-bit hosted mingw
for /f %%i in ('dir /b "%HB_ABSROOT%comp\mingw\lib\gcc\i686-w64-mingw32\?.*"') do set _GCCVER=%%i
rd /q /s  "%HB_ABSROOT%comp\mingw\i686-w64-mingw32\lib64\" 2> nul
rd /q /s  "%HB_ABSROOT%comp\mingw\lib\gcc\i686-w64-mingw32\%_GCCVER%\64\" 2> nul
rd /q /s  "%HB_ABSROOT%comp\mingw\lib\gcc\i686-w64-mingw32\%_GCCVER%\adainclude\" 2> nul
rd /q /s  "%HB_ABSROOT%comp\mingw\lib\gcc\i686-w64-mingw32\%_GCCVER%\adalib\" 2> nul
rd /q /s  "%HB_ABSROOT%comp\mingw\lib\gcc\i686-w64-mingw32\lib64\" 2> nul
del /q /f "%HB_ABSROOT%comp\mingw\bin\i686-w64-mingw32-gfortran.exe" 2> nul
del /q /f "%HB_ABSROOT%comp\mingw\i686-w64-mingw32\lib\libgfortran-*.dll" 2> nul
del /q /f "%HB_ABSROOT%comp\mingw\i686-w64-mingw32\lib\libgnarl-*.dll" 2> nul
del /q /f "%HB_ABSROOT%comp\mingw\i686-w64-mingw32\lib\libgnat-*.dll" 2> nul
del /q /f "%HB_ABSROOT%comp\mingw\libexec\gcc\i686-w64-mingw32\%_GCCVER%\f951.exe" 2> nul
del /q /f "%HB_ABSROOT%comp\mingw\libexec\gcc\i686-w64-mingw32\%_GCCVER%\gnat1.exe" 2> nul

if not "%_GCCVER%" == "" set MINGW_VER=%_GCCVER%

:: 64-bit hosted mingw
for /f %%i in ('dir /b "%HB_ABSROOT%comp\mingw\lib\gcc\x86_64-w64-mingw32\?.*"') do set _GCCVER=%%i
rd /q /s  "%HB_ABSROOT%comp\mingw\x86_64-w64-mingw32\lib32\" 2> nul
rd /q /s  "%HB_ABSROOT%comp\mingw\lib\gcc\x86_64-w64-mingw32\%_GCCVER%\32\" 2> nul
rd /q /s  "%HB_ABSROOT%comp\mingw\lib\gcc\x86_64-w64-mingw32\%_GCCVER%\adainclude\" 2> nul
rd /q /s  "%HB_ABSROOT%comp\mingw\lib\gcc\x86_64-w64-mingw32\%_GCCVER%\adalib\" 2> nul
rd /q /s  "%HB_ABSROOT%comp\mingw\lib\gcc\x86_64-w64-mingw32\lib32\" 2> nul
del /q /f "%HB_ABSROOT%comp\mingw\bin\x86_64-w64-mingw32-gfortran.exe" 2> nul
del /q /f "%HB_ABSROOT%comp\mingw\x86_64-w64-mingw32\lib\libgfortran-*.dll" 2> nul
del /q /f "%HB_ABSROOT%comp\mingw\x86_64-w64-mingw32\lib\libgnarl-*.dll" 2> nul
del /q /f "%HB_ABSROOT%comp\mingw\x86_64-w64-mingw32\lib\libgnat-*.dll" 2> nul
del /q /f "%HB_ABSROOT%comp\mingw\libexec\gcc\x86_64-w64-mingw32\%_GCCVER%\f951.exe" 2> nul
del /q /f "%HB_ABSROOT%comp\mingw\libexec\gcc\x86_64-w64-mingw32\%_GCCVER%\gnat1.exe" 2> nul

if not "%_GCCVER%" == "" set MINGW_VER=%_GCCVER%

:: Burn build information into RELNOTES.txt

for /f %%i in ('git rev-parse --short HEAD') do set VCS_ID=%%i
sed -e "s/_VCS_ID_/%VCS_ID%/g" -e "s/_HB_VF_/%HB_VF%/g" -e "s/_MINGW_VER_/%MINGW_VER%/g" "%~dp0RELNOTES.txt" > "%HB_ABSROOT%RELNOTES.txt"

:: Create unified installer

pushd "%HB_RT%"

if exist "%HB_RT%harbour-%HB_VF%-win-log.txt" del "%HB_RT%harbour-%HB_VF%-win-log.txt"
if exist "%HB_RT%harbour-%HB_VF%-win.exe" del "%HB_RT%harbour-%HB_VF%-win.exe"

"%HB_DIR_NSIS%makensis.exe" %HB_OPT_NSIS% "%~dp0mpkg_win_uni.nsi"

:: Create unified archive

echo.> _hbfiles

    echo "%HB_DR%ChangeLog*.txt"                            >> _hbfiles
    echo "%HB_DR%CONTRIBUTING.md"                           >> _hbfiles
    echo "%HB_DR%COPYING.txt"                               >> _hbfiles
    echo "%HB_DR%README.md"                                 >> _hbfiles
    echo "%HB_DR%RELNOTES.txt"                              >> _hbfiles
    echo "%HB_DR%bin\*-%HB_VS%.dll"                         >> _hbfiles
    echo "%HB_DR%bin\harbour.exe"                           >> _hbfiles
    echo "%HB_DR%bin\hbformat.exe"                          >> _hbfiles
    echo "%HB_DR%bin\hbi18n.exe"                            >> _hbfiles
    echo "%HB_DR%bin\hbmk2.exe"                             >> _hbfiles
    echo "%HB_DR%bin\hbnetio.exe"                           >> _hbfiles
    echo "%HB_DR%bin\hbpp.exe"                              >> _hbfiles
    echo "%HB_DR%bin\hbrun.exe"                             >> _hbfiles
    echo "%HB_DR%bin\hbspeed.exe"                           >> _hbfiles
    echo "%HB_DR%bin\hbtest.exe"                            >> _hbfiles
if exist "%HB_DR%bin\*.hb"     echo "%HB_DR%bin\*.hb"       >> _hbfiles
    echo "%HB_DR%bin\upx*.*"                                >> _hbfiles
    echo "%HB_DR%include\*.*"                               >> _hbfiles
    echo "%HB_DR%bin\hbmk2-x64.exe"                         >> _hbfiles
    echo "%HB_DR%bin\hbnetio-x64.exe"                       >> _hbfiles
    echo "%HB_DR%bin\hbrun-x64.exe"                         >> _hbfiles
    echo "%HB_DR%bin\hbspeed-x64.exe"                       >> _hbfiles
    echo "%HB_DR%bin\hbtest-x64.exe"                        >> _hbfiles
    echo "%HB_DR%lib\win\mingw\*.*"                         >> _hbfiles
    echo "%HB_DR%lib\win\mingw64\*.*"                       >> _hbfiles
    echo "%HB_DR%lib\wce\mingwarm\*.*"                      >> _hbfiles
    echo "%HB_DR%addons\HARBOUR_README_ADDONS.txt"          >> _hbfiles
rem echo "%HB_DR%comp\djgpp\HARBOUR_README_DJGPP.txt"       >> _hbfiles
    echo "%HB_DR%comp\watcom\HARBOUR_README_WATCOM.txt"     >> _hbfiles
    echo "%HB_DR%comp\pocc\HARBOUR_README_POCC.txt"         >> _hbfiles
    echo "%HB_DR%comp\mingw\HARBOUR_README_MINGW.txt"       >> _hbfiles
    echo "%HB_DR%comp\mingwarm\HARBOUR_README_MINGWARM.txt" >> _hbfiles
rem echo "%HB_DR%lib\dos\djgpp\*.*"                         >> _hbfiles
    echo "%HB_DR%lib\dos\watcom\*.*"                        >> _hbfiles
    echo "%HB_DR%lib\linux\watcom\*.*"                      >> _hbfiles
    echo "%HB_DR%lib\os2\watcom\*.*"                        >> _hbfiles
    echo "%HB_DR%lib\win\msvc\*.*"                          >> _hbfiles
    echo "%HB_DR%lib\win\msvc64\*.*"                        >> _hbfiles
rem echo "%HB_DR%bin\harbour-%HB_VS%-bcc.dll"               >> _hbfiles
    echo "%HB_DR%lib\win\bcc\*.*"                           >> _hbfiles
    echo "%HB_DR%lib\win\bcc64\*.*"                         >> _hbfiles
    echo "%HB_DR%lib\win\watcom\*.*"                        >> _hbfiles
rem echo "%HB_DR%lib\win\pocc\*.*"                          >> _hbfiles
rem echo "%HB_DR%lib\win\pocc64\*.*"                        >> _hbfiles
rem echo "%HB_DR%lib\wce\poccarm\*.*"                       >> _hbfiles
    echo "%HB_DR%bin\*-%HB_VS%-x64.dll"                     >> _hbfiles
    echo "%HB_DR%bin\harbour-%HB_VS%-wce-arm.dll"           >> _hbfiles
rem echo "%HB_DR%bin\harbour-%HB_VS%-os2.dll"               >> _hbfiles
    echo "%HB_DR%tests\*.*"                                 >> _hbfiles
    echo "%HB_DR%doc\*.*"                                   >> _hbfiles
    echo "%HB_DR%comp\mingw\*"                              >> _hbfiles
    echo "%HB_DR%extras\*.*"                                >> _hbfiles
    echo "%HB_DR%contrib\*.*"                               >> _hbfiles

if exist "%HB_RT%harbour-%HB_VF%-win.7z" del "%HB_RT%harbour-%HB_VF%-win.7z"
"%HB_DIR_7Z%7z" a -r -mx "%HB_RT%harbour-%HB_VF%-win.7z" @_hbfiles > nul

del _hbfiles

popd
