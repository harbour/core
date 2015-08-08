@if not "%_HB_PKG_DEBUG%" == "yes" echo off

:: ---------------------------------------------------------------
:: Copyright 2009-2015 Viktor Szakats (vszakats.net/harbour)
:: See LICENSE.txt for licensing terms.
:: ---------------------------------------------------------------

:: - Adjust target dir, MinGW dirs, set HB_DIR_UPX, HB_DIR_7Z, HB_DIR_MINGW,
::   create required packages beforehand.
:: - Requires BCC in PATH or HB_DIR_BCC_IMPLIB (for implib).
:: - Run this from vanilla official source tree only.
:: - Requires GNU sed, touch and OpenSSL tools in PATH
:: - Optional HB_SFX_7Z envvar pointed to 7z SFX module
::   found in: http://7zsfx.info/files/7zsd_150_2712.7z

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

set _SCRIPT=%~dp0mpkg.hb
set _ROOT=%~dp0..

:: Auto-detect the base bitness, by default it will be 32-bit,
:: and 64-bit if it's the only one available.

if exist "%~dp0..\pkg\win\mingw\harbour-%HB_VF%-win-mingw" (
   :: MinGW 32-bit base system
   set LIB_TARGET=32
) else if exist "%~dp0..\pkg\win\mingw64\harbour-%HB_VF%-win-mingw64" (
   :: MinGW 64-bit base system
   set LIB_TARGET=64
)

:: Assemble package from per-target builds

if exist "%HB_ABSROOT%" rd /s /q "%HB_ABSROOT%"

xcopy /y /s /q "%~dp0..\addons\*.txt"                                             "%HB_ABSROOT%addons\"
xcopy /y /s /q "%~dp0..\extras\*.*"                                               "%HB_ABSROOT%extras\"
xcopy /y /s /q "%~dp0..\tests\*.*"                                                "%HB_ABSROOT%tests\"

if "%LIB_TARGET%" == "32" xcopy /y /s /q "%~dp0..\pkg\win\mingw\harbour-%HB_VF%-win-mingw" "%HB_ABSROOT%"
if "%LIB_TARGET%" == "64" xcopy /y /s /q "%~dp0..\pkg\win\mingw64\harbour-%HB_VF%-win-mingw64" "%HB_ABSROOT%"

xcopy /y /s    "%~dp0..\pkg\linux\watcom\harbour-%HB_VF%-linux-watcom\lib"        "%HB_ABSROOT%lib\linux\watcom\" 2> nul
xcopy /y /s    "%~dp0..\pkg\dos\watcom\hb%HB_VL%wa\lib"                           "%HB_ABSROOT%lib\" 2> nul
xcopy /y /s    "%~dp0..\pkg\os2\watcom\harbour-%HB_VF%-os2-watcom\lib"            "%HB_ABSROOT%lib\" 2> nul
xcopy /y /s    "%~dp0..\pkg\wce\mingwarm\harbour-%HB_VF%-wce-mingwarm\lib"        "%HB_ABSROOT%lib\" 2> nul
xcopy /y /s    "%~dp0..\pkg\win\bcc\harbour-%HB_VF%-win-bcc\lib"                  "%HB_ABSROOT%lib\" 2> nul
xcopy /y /s    "%~dp0..\pkg\win\bcc64\harbour-%HB_VF%-win-bcc64\lib"              "%HB_ABSROOT%lib\" 2> nul
xcopy /y /s    "%~dp0..\pkg\win\mingw\harbour-%HB_VF%-win-mingw\lib"              "%HB_ABSROOT%lib\" 2> nul
xcopy /y /s    "%~dp0..\pkg\win\mingw64\harbour-%HB_VF%-win-mingw64\lib"          "%HB_ABSROOT%lib\" 2> nul
xcopy /y /s    "%~dp0..\pkg\win\msvc\harbour-%HB_VF%-win-msvc\lib"                "%HB_ABSROOT%lib\" 2> nul
xcopy /y /s    "%~dp0..\pkg\win\msvc64\harbour-%HB_VF%-win-msvc64\lib"            "%HB_ABSROOT%lib\" 2> nul
xcopy /y /s    "%~dp0..\pkg\win\watcom\harbour-%HB_VF%-win-watcom\lib"            "%HB_ABSROOT%lib\" 2> nul

xcopy /y       "%~dp0..\pkg\win\mingw64\harbour-%HB_VF%-win-mingw64\bin\*.dll"    "%HB_ABSROOT%bin\" 2> nul
xcopy /y       "%~dp0..\pkg\wce\mingwarm\harbour-%HB_VF%-wce-mingwarm\bin\*.dll"  "%HB_ABSROOT%bin\" 2> nul

:: Create special implibs for Borland (requires BCC in PATH)
:: NOTE: Using intermediate .def files, because direct .dll to .lib conversion
::       is buggy in BCC55 and BCC58 (no other versions tested), leaving off
::       leading underscore from certain ("random") symbols, resulting in
::       unresolved externals, when trying to use it. [vszakats]
if exist "%HB_ABSROOT%lib\win\bcc" (
   for %%I in ( "%HB_ABSROOT%bin\*-%HB_VS%.dll" ) do (
      "%HB_DIR_BCC_IMPLIB%impdef.exe" -a "%HB_ABSROOT%lib\win\bcc\%%~na-bcc.defraw" "%%I"
      echo s/LIBRARY     %%~na.DLL/LIBRARY     "%%~na.dll"/Ig> _hbtemp.sed
      sed -f _hbtemp.sed < "%HB_ABSROOT%lib\win\bcc\%%~na-bcc.defraw" > "%HB_ABSROOT%lib\win\bcc\%%~na-bcc.def"
      "%HB_DIR_BCC_IMPLIB%implib.exe" -c -a "%HB_ABSROOT%lib\win\bcc\%%~na-bcc.lib" "%HB_ABSROOT%lib\win\bcc\%%~na-bcc.def"
      touch "%HB_ABSROOT%lib\win\bcc\%%~na-bcc.lib" -r "%HB_ABSROOT%README.md"
      del "%HB_ABSROOT%lib\win\bcc\%%~na-bcc.defraw"
      del "%HB_ABSROOT%lib\win\bcc\%%~na-bcc.def"
   )
   if exist _hbtemp.sed del _hbtemp.sed
)

:: Workaround for ld --no-insert-timestamp bug that exist as of
:: binutils 2.25, when the PE build timestamp field is often
:: filled with random bytes instead of zeroes. -s option is not
:: fixing this, 'strip' randomly fails either, so we're
:: patching manually. Do this while only Harbour built binaries are
:: copied into the bin directory to not modify 3rd party binaries.
copy /y "%HB_ABSROOT%bin\hbmk2.exe" "%HB_ABSROOT%bin\hbmk2-temp.exe"
"%HB_ABSROOT%bin\hbmk2-temp.exe" "%_SCRIPT%" pe "%_ROOT%" "%HB_ABSROOT%bin\*.exe"
"%HB_ABSROOT%bin\hbmk2-temp.exe" "%_SCRIPT%" pe "%_ROOT%" "%HB_ABSROOT%bin\*.dll"
del /f "%HB_ABSROOT%bin\hbmk2-temp.exe"

:: Workaround for ld --no-insert-timestamp issue in that it
:: won't remove internal timestamps from generated implibs.
:: Slow. Requires binutils 2.23 (maybe 2.24/2.25).
:: Short synonym '-D' is not recognized as of binutils 2.25.
if exist "%HB_ABSROOT%lib\win\mingw\*-*.*"      "%HB_DIR_MINGW%\bin\strip" -p --enable-deterministic-archives -g "%HB_ABSROOT%lib\win\mingw\*-*.*"
if exist "%HB_ABSROOT%lib\win\mingw64\*-*.*"    "%HB_DIR_MINGW%\bin\strip" -p --enable-deterministic-archives -g "%HB_ABSROOT%lib\win\mingw64\*-*.*"
if exist "%HB_ABSROOT%lib\win\mingw\*_dll*.*"   "%HB_DIR_MINGW%\bin\strip" -p --enable-deterministic-archives -g "%HB_ABSROOT%lib\win\mingw\*_dll*.*"
if exist "%HB_ABSROOT%lib\win\mingw64\*_dll*.*" "%HB_DIR_MINGW%\bin\strip" -p --enable-deterministic-archives -g "%HB_ABSROOT%lib\win\mingw64\*_dll*.*"
if exist "%HB_ABSROOT%lib\win\msvc\*.lib"       "%HB_DIR_MINGW%\bin\strip" -p --enable-deterministic-archives -g "%HB_ABSROOT%lib\win\msvc\*.lib"
if exist "%HB_ABSROOT%lib\win\msvc64\*.lib"     "%HB_DIR_MINGW%\bin\strip" -p --enable-deterministic-archives -g "%HB_ABSROOT%lib\win\msvc64\*.lib"

:: Copy upx

if not "%HB_DIR_UPX%" == "" (
   xcopy /y "%HB_DIR_UPX%upx.exe" "%HB_ABSROOT%bin\"
    copy /y "%HB_DIR_UPX%LICENSE" "%HB_ABSROOT%bin\upx_LICENSE.txt"
)

:: Copy 7z

if not "%HB_DIR_7Z%" == "" (
   xcopy /y "%HB_DIR_7Z%7za.exe"     "%HB_ABSROOT%bin\"
    copy /y "%HB_DIR_7Z%license.txt" "%HB_ABSROOT%bin\7za_LICENSE.txt"
)

:: Copy core 3rd party headers

xcopy /y /s /q /i "%~dp0..\src\3rd\*.h" "%HB_ABSROOT%src\3rd\"

:: Copy C compiler

:: TODO: This whole section should only be relevant
::       if the distro is MinGW based. Much of it is
::       useful only if MinGW _is_ actually bundled
::       with the package, which is probably something
::       that should be avoided in the future.

set MINGW_HOST=32
if exist "%HB_DIR_MINGW%\x86_64-w64-mingw32" set MINGW_HOST=64

if "%MINGW_HOST%" == "32" set MINGW_ROOT=comp\mingw\
if "%MINGW_HOST%" == "64" set MINGW_ROOT=comp\mingw64\

if "%_HB_PKG_BUNDLE_C%" == "yes" (
   xcopy /y /s /q /e "%HB_DIR_MINGW%" "%HB_ABSROOT%%MINGW_ROOT%"
) else (
   xcopy /y /q "%~dp0getmingw.bat" "%HB_ABSROOT%bin\"
)

:: Copy MinGW runtime .dlls

:: Pick the ones from a multi-target MinGW distro
:: that match the bitness of our base target.
set _MINGW_DLL_DIR=%HB_DIR_MINGW%\bin
if "%LIB_TARGET%" == "32" if exist "%HB_DIR_MINGW%\x86_64-w64-mingw32\lib32" set _MINGW_DLL_DIR=%HB_DIR_MINGW%\x86_64-w64-mingw32\lib32
if "%LIB_TARGET%" == "64" if exist "%HB_DIR_MINGW%\i686-w64-mingw32\lib64"   set _MINGW_DLL_DIR=%HB_DIR_MINGW%\i686-w64-mingw32\lib64

if exist "%_MINGW_DLL_DIR%\libgcc_s_*.dll" xcopy /y "%HB_DIR_MINGW%\bin\libgcc_s_*.dll" "%HB_ABSROOT%bin\"
if exist "%_MINGW_DLL_DIR%\mingwm*.dll"    xcopy /y "%HB_DIR_MINGW%\bin\mingwm*.dll"    "%HB_ABSROOT%bin\"
:: for posix cc1.exe to run without putting MinGW bin directory into PATH
rem if exist "%_MINGW_DLL_DIR%\libwinpthread-*.dll" xcopy /y "%HB_DIR_MINGW%\bin\libwinpthread-*.dll" "%HB_ABSROOT%bin\"

:: Delete stuff from C compiler directory we don't need:
:: - secondary target from multi-target MinGWs
:: - non-C/C++ language support
:: - gdb (along with Python)
:: - 3rd party libraries

rd /s /q  "%HB_ABSROOT%%MINGW_ROOT%etc\" 2> nul
rd /s /q  "%HB_ABSROOT%%MINGW_ROOT%opt\" 2> nul
rd /s /q  "%HB_ABSROOT%%MINGW_ROOT%share\" 2> nul
del /f /q "%HB_ABSROOT%%MINGW_ROOT%bin\gdb*.exe" 2> nul
del /f /q "%HB_ABSROOT%%MINGW_ROOT%bin\gfortran.exe" 2> nul
del /f /q "%HB_ABSROOT%%MINGW_ROOT%bin\gnat*.exe" 2> nul
del /f /q "%HB_ABSROOT%%MINGW_ROOT%bin\libgfortran-*.dll" 2> nul
del /f /q "%HB_ABSROOT%%MINGW_ROOT%bin\libgnarl-*.dll" 2> nul
del /f /q "%HB_ABSROOT%%MINGW_ROOT%bin\libgnat-*.dll" 2> nul

if "%MINGW_HOST%" == "32" (

   for /f %%I in ('dir /b "%HB_DIR_MINGW%\lib\gcc\i686-w64-mingw32\?.*"') do set MINGW_VER=%%I

   rd /s /q  "%HB_ABSROOT%%MINGW_ROOT%i686-w64-mingw32\lib64\" 2> nul
   rd /s /q  "%HB_ABSROOT%%MINGW_ROOT%lib\gcc\i686-w64-mingw32\%MINGW_VER%\64\" 2> nul
   rd /s /q  "%HB_ABSROOT%%MINGW_ROOT%lib\gcc\i686-w64-mingw32\%MINGW_VER%\adainclude\" 2> nul
   rd /s /q  "%HB_ABSROOT%%MINGW_ROOT%lib\gcc\i686-w64-mingw32\%MINGW_VER%\adalib\" 2> nul
   rd /s /q  "%HB_ABSROOT%%MINGW_ROOT%lib\gcc\i686-w64-mingw32\lib64\" 2> nul
   del /f /q "%HB_ABSROOT%%MINGW_ROOT%bin\i686-w64-mingw32-gfortran.exe" 2> nul
   del /f /q "%HB_ABSROOT%%MINGW_ROOT%i686-w64-mingw32\lib\libgfortran-*.dll" 2> nul
   del /f /q "%HB_ABSROOT%%MINGW_ROOT%i686-w64-mingw32\lib\libgnarl-*.dll" 2> nul
   del /f /q "%HB_ABSROOT%%MINGW_ROOT%i686-w64-mingw32\lib\libgnat-*.dll" 2> nul
   del /f /q "%HB_ABSROOT%%MINGW_ROOT%libexec\gcc\i686-w64-mingw32\%MINGW_VER%\f951.exe" 2> nul
   del /f /q "%HB_ABSROOT%%MINGW_ROOT%libexec\gcc\i686-w64-mingw32\%MINGW_VER%\gnat1.exe" 2> nul

) else if "%MINGW_HOST%" == "64" (

   for /f %%I in ('dir /b "%HB_DIR_MINGW%\lib\gcc\x86_64-w64-mingw32\?.*"') do set MINGW_VER=%%I

   rd /s /q  "%HB_ABSROOT%%MINGW_ROOT%x86_64-w64-mingw32\lib32\" 2> nul
   rd /s /q  "%HB_ABSROOT%%MINGW_ROOT%lib\gcc\x86_64-w64-mingw32\%MINGW_VER%\32\" 2> nul
   rd /s /q  "%HB_ABSROOT%%MINGW_ROOT%lib\gcc\x86_64-w64-mingw32\%MINGW_VER%\adainclude\" 2> nul
   rd /s /q  "%HB_ABSROOT%%MINGW_ROOT%lib\gcc\x86_64-w64-mingw32\%MINGW_VER%\adalib\" 2> nul
   rd /s /q  "%HB_ABSROOT%%MINGW_ROOT%lib\gcc\x86_64-w64-mingw32\lib32\" 2> nul
   del /f /q "%HB_ABSROOT%%MINGW_ROOT%bin\x86_64-w64-mingw32-gfortran.exe" 2> nul
   del /f /q "%HB_ABSROOT%%MINGW_ROOT%x86_64-w64-mingw32\lib\libgfortran-*.dll" 2> nul
   del /f /q "%HB_ABSROOT%%MINGW_ROOT%x86_64-w64-mingw32\lib\libgnarl-*.dll" 2> nul
   del /f /q "%HB_ABSROOT%%MINGW_ROOT%x86_64-w64-mingw32\lib\libgnat-*.dll" 2> nul
   del /f /q "%HB_ABSROOT%%MINGW_ROOT%libexec\gcc\x86_64-w64-mingw32\%MINGW_VER%\f951.exe" 2> nul
   del /f /q "%HB_ABSROOT%%MINGW_ROOT%libexec\gcc\x86_64-w64-mingw32\%MINGW_VER%\gnat1.exe" 2> nul
)

echo ! MinGW version: %MINGW_VER% (%MINGW_HOST%-bit hosted)

:: Burn build information into RELNOTES.txt

set _HB_VER=%HB_VF%
if not "%HB_VF%" == "%HB_VF_DEF%" set _HB_VER=%HB_VF_DEF% %_HB_VER%

for /f %%I in ('git rev-parse --short HEAD') do set VCS_ID=%%I
sed -e "s/_VCS_ID_/%VCS_ID%/g"^
    -e "s/_HB_VERSION_/%_HB_VER%/g" "%~dp0RELNOTES.txt" > "%HB_ABSROOT%RELNOTES.txt"
touch "%HB_ABSROOT%RELNOTES.txt" -r "%HB_ABSROOT%README.md"

:: Register build information

"%HB_ABSROOT%bin\harbour" -build 2>&1 | sed -nr "/^(Version:|Platform:|Extra )/!p" > "%HB_ABSROOT%BUILD.txt"
set | sed -nr "/^(HB_USER_|HB_BUILD_|HB_PLATFORM|HB_COMPILER|HB_CPU|HB_WITH_|HB_STATIC_)/p" >> "%HB_ABSROOT%BUILD.txt"
echo --------------------------->> "%HB_ABSROOT%BUILD.txt"
dir /s /b /ad "%HB_ABSROOT%lib\" | sed -e "s|%HB_ABSROOT:\=.%lib.||g" | sed -nr "/[^a-z0-9]/p" >> "%HB_ABSROOT%BUILD.txt"
touch "%HB_ABSROOT%BUILD.txt" -r "%HB_ABSROOT%README.md"

:: Convert EOLs

"%HB_ABSROOT%bin\hbmk2.exe" "%_SCRIPT%" nl "%HB_ABSROOT%*.md"
"%HB_ABSROOT%bin\hbmk2.exe" "%_SCRIPT%" nl "%HB_ABSROOT%*.txt"
"%HB_ABSROOT%bin\hbmk2.exe" "%_SCRIPT%" nl "%HB_ABSROOT%addons\*.txt"

:: Create installer/archive

pushd "%HB_RT%"

echo "*.md"         > _hbfiles
echo "*.txt"        >> _hbfiles
echo "bin\*.bat"    >> _hbfiles
echo "bin\*.dll"    >> _hbfiles
echo "bin\*.exe"    >> _hbfiles
echo "bin\*.hb"     >> _hbfiles
echo "bin\*.txt"    >> _hbfiles
echo "include\*"    >> _hbfiles
echo "lib\*"        >> _hbfiles
echo "src\*"        >> _hbfiles
echo "doc\*"        >> _hbfiles
echo "comp\*"       >> _hbfiles
echo "contrib\*"    >> _hbfiles
echo "extras\*"     >> _hbfiles
echo "tests\*"      >> _hbfiles
echo "addons\*.txt" >> _hbfiles

set _PKGNAME=harbour-%HB_VF%-win.7z

if exist "%_PKGNAME%" del "%_PKGNAME%"
pushd "%HB_DR%"
"%HB_ABSROOT%bin\hbmk2.exe" "%_SCRIPT%" ts "%_ROOT%"
"%HB_DIR_7Z%7za" a -r -mx "..\%_PKGNAME%" @..\_hbfiles > nul
popd

if exist "%HB_SFX_7Z%" (

   echo ;!@Install@!UTF-8!> _7zconf
   echo Title=^"Harbour %HB_VF%^">> _7zconf
   echo BeginPrompt=^"Do you want to install Harbour %HB_VF%?^">> _7zconf
   echo CancelPrompt=^"Do you want to cancel installation?^">> _7zconf
   echo ExtractPathText=^"Select destination path^">> _7zconf
   echo ExtractPathTitle=^"Harbour %HB_VF%^">> _7zconf
   echo ExtractTitle=^"Extracting^">> _7zconf
   echo ExtractDialogText=^"Please wait...^">> _7zconf
   echo ExtractCancelText=^"Abort^">> _7zconf
   echo Progress=^"yes^">> _7zconf
   echo GUIFlags=^"8+64+256+4096^">> _7zconf
   echo GUIMode=^"1^">> _7zconf
   echo OverwriteMode=^"0^">> _7zconf
   echo InstallPath=^"C:\hb%HB_VS%^">> _7zconf
   echo Shortcut=^"Du,{cmd.exe},{/k cd /d \^"%%%%T\\bin\\\^"},{},{},{Harbour Shell},{%%%%T\\bin\\},{%%%%T\\bin\\hbmk2.exe},{0}^">> _7zconf
   echo RunProgram=^"nowait:notepad.exe \^"%%%%T\\RELNOTES.txt\^"^">> _7zconf
   echo ;RunProgram=^"hbmk2.exe \^"%%%%T\^"\\install.hb^">> _7zconf
   echo ;Delete=^"^">> _7zconf
   echo ;!@InstallEnd@!>> _7zconf

   copy /b "%HB_SFX_7Z%" + ^
      _7zconf + ^
      "%_PKGNAME%" ^
      "%_PKGNAME%.exe"

   del "%_PKGNAME%"
   del _7zconf

   set _PKGNAME=%_PKGNAME%.exe
)

:: Change to ISO date and 24-hour time format
if not "%CI%" == "" (
   reg add "HKCU\Control Panel\International" /v sShortDate /t REG_SZ /d "yyyy-MM-dd" /f
   reg add "HKCU\Control Panel\International" /v sShortTime /t REG_SZ /d "HH:mm" /f
)

touch "%_PKGNAME%" -r "%HB_ABSROOT%README.md"
for %%I in ("%_PKGNAME%") do echo %%~nxI: %%~zI bytes %%~tI
openssl dgst -sha256 "%_PKGNAME%"

del _hbfiles

popd
