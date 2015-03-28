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

set HB_VS_DEF=34
set HB_VL_DEF=340
set HB_VM_DEF=3.4
set HB_VF_DEF=3.4.0dev
set HB_RT_DEF=C:\hb\

if "%HB_VS%" == "" set HB_VS=%HB_VS_DEF%
if "%HB_VL%" == "" set HB_VL=%HB_VL_DEF%
if "%HB_VM%" == "" set HB_VM=%HB_VM_DEF%
if "%HB_VF%" == "" set HB_VF=%HB_VF_DEF%
if "%HB_RT%" == "" set HB_RT=%HB_RT_DEF%

set HB_DR=hb%HB_VS%\
set HB_ABSROOT=%HB_RT%%HB_DR%

:: Autodetect the base bitness, by default it will be 32-bit,
:: and 64-bit if it's the only one available.

if exist "%~dp0..\..\pkg\win\mingw\harbour-%HB_VF%-win-mingw" (
   :: mingw 32-bit base system
   set LIB_TARGET=32
) else if exist "%~dp0..\..\pkg\win\mingw64\harbour-%HB_VF%-win-mingw64" (
   :: mingw 64-bit base system
   set LIB_TARGET=64
)

:: Assemble unified package from per-target builds

if exist "%HB_ABSROOT%" rd /q /s "%HB_ABSROOT%"

xcopy /y /s /q "%~dp0..\..\extras\*.*"                                                      "%HB_ABSROOT%extras\"
xcopy /y /s /q "%~dp0..\..\tests\*.*"                                                       "%HB_ABSROOT%tests\"

:: Using xcopy to create the destination subdir in the same step
xcopy /y /q    "%~dp0ADDONS.txt"                                                            "%HB_ABSROOT%addons\"
mv "%HB_ABSROOT%addons\ADDONS.txt" "%HB_ABSROOT%addons\README.txt"

if "%LIB_TARGET%" == "32" xcopy /y /s /q "%~dp0..\..\pkg\win\mingw\harbour-%HB_VF%-win-mingw" "%HB_ABSROOT%"
if "%LIB_TARGET%" == "64" xcopy /y /s /q "%~dp0..\..\pkg\win\mingw64\harbour-%HB_VF%-win-mingw64" "%HB_ABSROOT%"

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

:: Copy upx

if not "%HB_DIR_UPX%" == "" (
   xcopy /y "%HB_DIR_UPX%upx.exe" "%HB_ABSROOT%bin\"
    copy /y "%HB_DIR_UPX%LICENSE" "%HB_ABSROOT%bin\upx_LICENSE.txt"
)

:: Copy C compiler

set MINGW_HOST=32
if exist "%HB_DIR_MINGW%\x86_64-w64-mingw32" set MINGW_HOST=64

if "%MINGW_HOST%" == "32" set MINGW_ROOT=comp\mingw\
if "%MINGW_HOST%" == "64" set MINGW_ROOT=comp\mingw64\

if not "%_HB_PKG_WINUNI_BUNDLE_C%" == "no" xcopy /y /s /q /e "%HB_DIR_MINGW%" "%HB_ABSROOT%%MINGW_ROOT%"

:: Copy mingw runtime .dlls

:: Pick the ones from a multi-target mingw distro
:: that match the bitness of our base target.
set _MINGW_DLL_DIR=%HB_DIR_MINGW%\bin
if "%LIB_TARGET%" == "32" if exist "%HB_DIR_MINGW%\x86_64-w64-mingw32\lib32" set _MINGW_DLL_DIR=%HB_DIR_MINGW%\x86_64-w64-mingw32\lib32
if "%LIB_TARGET%" == "64" if exist "%HB_DIR_MINGW%\i686-w64-mingw32\lib64"   set _MINGW_DLL_DIR=%HB_DIR_MINGW%\i686-w64-mingw32\lib64

if exist "%_MINGW_DLL_DIR%\libgcc_s_*.dll" xcopy /y "%HB_DIR_MINGW%\bin\libgcc_s_*.dll" "%HB_ABSROOT%bin\"
if exist "%_MINGW_DLL_DIR%\mingwm*.dll"    xcopy /y "%HB_DIR_MINGW%\bin\mingwm*.dll"    "%HB_ABSROOT%bin\"
:: for posix cc1.exe to run without putting mingw\bin into PATH
rem if exist "%_MINGW_DLL_DIR%\libwinpthread-*.dll" xcopy /y "%HB_DIR_MINGW%\bin\libwinpthread-*.dll" "%HB_ABSROOT%bin\"

:: Delete stuff from C compiler folder we don't need:
:: - secondary target from multi-target mingws
:: - non-C/C++ language support
:: - gdb (along with Python)
:: - 3rd party libraries

rd /q /s  "%HB_ABSROOT%%MINGW_ROOT%etc\" 2> nul
rd /q /s  "%HB_ABSROOT%%MINGW_ROOT%opt\" 2> nul
rd /q /s  "%HB_ABSROOT%%MINGW_ROOT%share\" 2> nul
del /q /f "%HB_ABSROOT%%MINGW_ROOT%bin\gdb*.exe" 2> nul
del /q /f "%HB_ABSROOT%%MINGW_ROOT%bin\gfortran.exe" 2> nul
del /q /f "%HB_ABSROOT%%MINGW_ROOT%bin\gnat*.exe" 2> nul
del /q /f "%HB_ABSROOT%%MINGW_ROOT%bin\libgfortran-*.dll" 2> nul
del /q /f "%HB_ABSROOT%%MINGW_ROOT%bin\libgnarl-*.dll" 2> nul
del /q /f "%HB_ABSROOT%%MINGW_ROOT%bin\libgnat-*.dll" 2> nul

if "%MINGW_HOST%" == "32" (

   for /f %%a in ('dir /b "%HB_DIR_MINGW%\lib\gcc\i686-w64-mingw32\?.*"') do set _GCCVER=%%a
   if not "%_GCCVER%" == "" set MINGW_VER=%_GCCVER%

   rd /q /s  "%HB_ABSROOT%%MINGW_ROOT%i686-w64-mingw32\lib64\" 2> nul
   rd /q /s  "%HB_ABSROOT%%MINGW_ROOT%lib\gcc\i686-w64-mingw32\%_GCCVER%\64\" 2> nul
   rd /q /s  "%HB_ABSROOT%%MINGW_ROOT%lib\gcc\i686-w64-mingw32\%_GCCVER%\adainclude\" 2> nul
   rd /q /s  "%HB_ABSROOT%%MINGW_ROOT%lib\gcc\i686-w64-mingw32\%_GCCVER%\adalib\" 2> nul
   rd /q /s  "%HB_ABSROOT%%MINGW_ROOT%lib\gcc\i686-w64-mingw32\lib64\" 2> nul
   del /q /f "%HB_ABSROOT%%MINGW_ROOT%bin\i686-w64-mingw32-gfortran.exe" 2> nul
   del /q /f "%HB_ABSROOT%%MINGW_ROOT%i686-w64-mingw32\lib\libgfortran-*.dll" 2> nul
   del /q /f "%HB_ABSROOT%%MINGW_ROOT%i686-w64-mingw32\lib\libgnarl-*.dll" 2> nul
   del /q /f "%HB_ABSROOT%%MINGW_ROOT%i686-w64-mingw32\lib\libgnat-*.dll" 2> nul
   del /q /f "%HB_ABSROOT%%MINGW_ROOT%libexec\gcc\i686-w64-mingw32\%_GCCVER%\f951.exe" 2> nul
   del /q /f "%HB_ABSROOT%%MINGW_ROOT%libexec\gcc\i686-w64-mingw32\%_GCCVER%\gnat1.exe" 2> nul

) else if "%MINGW_HOST%" == "64" (

   for /f %%a in ('dir /b "%HB_DIR_MINGW%\lib\gcc\x86_64-w64-mingw32\?.*"') do set _GCCVER=%%a
   if not "%_GCCVER%" == "" set MINGW_VER=%_GCCVER%

   rd /q /s  "%HB_ABSROOT%%MINGW_ROOT%x86_64-w64-mingw32\lib32\" 2> nul
   rd /q /s  "%HB_ABSROOT%%MINGW_ROOT%lib\gcc\x86_64-w64-mingw32\%_GCCVER%\32\" 2> nul
   rd /q /s  "%HB_ABSROOT%%MINGW_ROOT%lib\gcc\x86_64-w64-mingw32\%_GCCVER%\adainclude\" 2> nul
   rd /q /s  "%HB_ABSROOT%%MINGW_ROOT%lib\gcc\x86_64-w64-mingw32\%_GCCVER%\adalib\" 2> nul
   rd /q /s  "%HB_ABSROOT%%MINGW_ROOT%lib\gcc\x86_64-w64-mingw32\lib32\" 2> nul
   del /q /f "%HB_ABSROOT%%MINGW_ROOT%bin\x86_64-w64-mingw32-gfortran.exe" 2> nul
   del /q /f "%HB_ABSROOT%%MINGW_ROOT%x86_64-w64-mingw32\lib\libgfortran-*.dll" 2> nul
   del /q /f "%HB_ABSROOT%%MINGW_ROOT%x86_64-w64-mingw32\lib\libgnarl-*.dll" 2> nul
   del /q /f "%HB_ABSROOT%%MINGW_ROOT%x86_64-w64-mingw32\lib\libgnat-*.dll" 2> nul
   del /q /f "%HB_ABSROOT%%MINGW_ROOT%libexec\gcc\x86_64-w64-mingw32\%_GCCVER%\f951.exe" 2> nul
   del /q /f "%HB_ABSROOT%%MINGW_ROOT%libexec\gcc\x86_64-w64-mingw32\%_GCCVER%\gnat1.exe" 2> nul
)

echo ! mingw version: %MINGW_VER% %MINGW_HOST%-bit hosted

:: Burn build information into RELNOTES.txt

set _HB_VER=%HB_VF%
if not "%HB_VF%" == "%HB_VF_DEF%" set _HB_VER=%HB_VF_DEF% %_HB_VER%

for /f %%a in ('git rev-parse --short HEAD') do set VCS_ID=%%a
sed -e "s/_VCS_ID_/%VCS_ID%/g"^
    -e "s/_HB_VERSION_/%_HB_VER%/g"^
    -e "s/_MINGW_VER_/%MINGW_VER%/g" "%~dp0RELNOTES.txt" > "%HB_ABSROOT%RELNOTES.txt"

:: Create unified installer

pushd "%HB_RT%"

if exist "harbour-%HB_VF%-win-log.txt" del "harbour-%HB_VF%-win-log.txt"
if exist "harbour-%HB_VF%-win.exe" del "harbour-%HB_VF%-win.exe"

"%HB_DIR_NSIS%makensis.exe" "%~dp0mpkg_win_uni.nsi"

:: Create unified archive

echo.> _hbfiles

if exist "%HB_DR%bin\hbformat.exe" echo "%HB_DR%bin\hbformat.exe" >> _hbfiles
if exist "%HB_DR%bin\hbnetio.exe"  echo "%HB_DR%bin\hbnetio.exe"  >> _hbfiles

if exist "%HB_ABSROOT%%MINGW_ROOT%*" (
   echo "%HB_DR%bin\libgcc_s_*.dll"      >> _hbfiles
   echo "%HB_DR%bin\mingwm*.dll"         >> _hbfiles
   echo "%HB_DR%bin\libwinpthread-*.dll" >> _hbfiles
)

echo "%HB_DR%ChangeLog*.txt"       >> _hbfiles
echo "%HB_DR%CONTRIBUTING.md"      >> _hbfiles
echo "%HB_DR%COPYING.txt"          >> _hbfiles
echo "%HB_DR%README.md"            >> _hbfiles
echo "%HB_DR%RELNOTES.txt"         >> _hbfiles
echo "%HB_DR%bin\*-%HB_VS%*.dll"   >> _hbfiles
echo "%HB_DR%bin\harbour.exe"      >> _hbfiles
echo "%HB_DR%bin\hbi18n.exe"       >> _hbfiles
echo "%HB_DR%bin\hbmk2.exe"        >> _hbfiles
echo "%HB_DR%bin\hbpp.exe"         >> _hbfiles
echo "%HB_DR%bin\hbrun.exe"        >> _hbfiles
echo "%HB_DR%bin\hbspeed.exe"      >> _hbfiles
echo "%HB_DR%bin\hbtest.exe"       >> _hbfiles
echo "%HB_DR%bin\*.hb"             >> _hbfiles
echo "%HB_DR%bin\upx*.*"           >> _hbfiles
echo "%HB_DR%include\*.*"          >> _hbfiles
echo "%HB_DR%lib\win\mingw\*.*"    >> _hbfiles
echo "%HB_DR%lib\win\mingw64\*.*"  >> _hbfiles
echo "%HB_DR%lib\wce\mingwarm\*.*" >> _hbfiles
echo "%HB_DR%lib\dos\djgpp\*.*"    >> _hbfiles
echo "%HB_DR%lib\dos\watcom\*.*"   >> _hbfiles
echo "%HB_DR%lib\linux\watcom\*.*" >> _hbfiles
echo "%HB_DR%lib\os2\watcom\*.*"   >> _hbfiles
echo "%HB_DR%lib\win\msvc\*.*"     >> _hbfiles
echo "%HB_DR%lib\win\msvc64\*.*"   >> _hbfiles
echo "%HB_DR%lib\win\bcc\*.*"      >> _hbfiles
echo "%HB_DR%lib\win\bcc64\*.*"    >> _hbfiles
echo "%HB_DR%lib\win\watcom\*.*"   >> _hbfiles
echo "%HB_DR%lib\win\pocc\*.*"     >> _hbfiles
echo "%HB_DR%lib\win\pocc64\*.*"   >> _hbfiles
echo "%HB_DR%lib\wce\poccarm\*.*"  >> _hbfiles
echo "%HB_DR%tests\*.*"            >> _hbfiles
echo "%HB_DR%doc\*.*"              >> _hbfiles
echo "%HB_DR%comp\mingw\*"         >> _hbfiles
echo "%HB_DR%comp\mingw64\*"       >> _hbfiles
echo "%HB_DR%comp\djgpp\*"         >> _hbfiles
echo "%HB_DR%comp\pocc\*"          >> _hbfiles
echo "%HB_DR%comp\watcom\*"        >> _hbfiles
echo "%HB_DR%extras\*.*"           >> _hbfiles
echo "%HB_DR%contrib\*.*"          >> _hbfiles
echo "%HB_DR%addons\README.txt"    >> _hbfiles

if exist "harbour-%HB_VF%-win.7z" del "harbour-%HB_VF%-win.7z"
"%HB_DIR_7Z%7z" a -r -mx "harbour-%HB_VF%-win.7z" @_hbfiles > nul

del _hbfiles

popd
