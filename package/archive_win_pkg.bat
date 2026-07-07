@echo off
rem ---------------------------------------------------------------
rem Archive a Windows binary package produced by build_win_release.bat
rem
rem Creates: harbour-<ver>-win-<compiler>.7z in the repo root
rem Requires: 7za.exe in PATH (7-Zip) or HB_DIR_7Z set
rem ---------------------------------------------------------------

setlocal EnableExtensions

cd /d "%~dp0.."

if "%HB_COMPILER%"=="" set HB_COMPILER=mingw64
if "%HB_DIR_7Z%"=="" set HB_DIR_7Z=

set PKG_DIR=pkg\win\%HB_COMPILER%
if not exist "%PKG_DIR%" (
   echo ERROR: %PKG_DIR% not found. Run build_win_release.bat first.
   exit /b 1
)

rem Pick the harbour-* package directory (newest match)
set PKG_NAME=
for /f "delims=" %%D in ('dir /b /ad "%PKG_DIR%\harbour-*-win-%HB_COMPILER%" 2^>nul') do set PKG_NAME=%%D

if "%PKG_NAME%"=="" (
   echo ERROR: no harbour-*-win-%HB_COMPILER% package in %PKG_DIR%
   exit /b 1
)

set OUT_FILE=%PKG_NAME%.7z
if exist "%OUT_FILE%" del "%OUT_FILE%"

if defined HB_DIR_7Z (
   set ZIP7=%HB_DIR_7Z%7za.exe
) else (
   set ZIP7=7za.exe
)

where %ZIP7% >nul 2>&1
if errorlevel 1 (
   echo ERROR: 7za.exe not found. Install 7-Zip or set HB_DIR_7Z.
   exit /b 1
)

echo Archiving %PKG_DIR%\%PKG_NAME% to %OUT_FILE% ...
%ZIP7% a -r "%OUT_FILE%" "%PKG_DIR%\%PKG_NAME%"
if errorlevel 1 exit /b 1

echo Created: %CD%\%OUT_FILE%
endlocal
exit /b 0