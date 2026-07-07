@echo off
rem ---------------------------------------------------------------
rem Build Harbour Windows binary package (stable release)
rem
rem Produces: pkg\win\<compiler>\harbour-<ver>-win-<compiler>\
rem
rem Prerequisites:
rem   - MSYS2 MinGW64 (or MinGW32) in PATH
rem   - Run from Harbour source root (or any subdir; script cds to root)
rem   - For v3.2.0: git checkout v3.2.0
rem
rem Optional environment:
rem   HB_COMPILER=mingw64   (default)
rem   HB_CPU=x86_64         (default)
rem   HB_VF=3.2.0           (informational, used in messages only)
rem
rem See package\BUILD-WIN-RELEASE.txt for full instructions.
rem ---------------------------------------------------------------

setlocal EnableExtensions

cd /d "%~dp0.."
if not exist win-make.exe (
   echo ERROR: win-make.exe not found. Run from Harbour source tree.
   exit /b 1
)

if "%HB_COMPILER%"=="" set HB_COMPILER=mingw64
if "%HB_CPU%"=="" set HB_CPU=x86_64
if "%HB_VF%"=="" set HB_VF=3.2.0

set HB_BUILD_PKG=yes
set HB_BUILD_VERBOSE=yes
set HBMK_WITH_SQLITE3=local

echo.
echo === Harbour Windows release build ===
echo Version target : %HB_VF%
echo Compiler       : %HB_COMPILER%
echo CPU            : %HB_CPU%
echo.

echo --- Core: clean install ---
win-make clean install HB_COMPILER=%HB_COMPILER% HB_CPU=%HB_CPU%
if errorlevel 1 goto _FAIL

echo --- Contribs ---
win-make -C contrib HB_COMPILER=%HB_COMPILER% HB_CPU=%HB_CPU%
if errorlevel 1 goto _FAIL

echo --- Tests ---
bin\win\%HB_COMPILER%\hbtest.exe
if errorlevel 1 goto _FAIL

echo.
echo === Build OK ===
echo Package directory:
dir /b pkg\win\%HB_COMPILER%\harbour-*-win-%HB_COMPILER% 2>nul
echo.
echo Next: archive with package\archive_win_pkg.bat
echo       or build unified installer with package\winuni\mpkg_win_uni.bat
goto _END

:_FAIL
echo.
echo === BUILD FAILED ===
exit /b 1

:_END
endlocal
exit /b 0