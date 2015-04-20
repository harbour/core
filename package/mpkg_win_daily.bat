@echo off

:: ---------------------------------------------------------------
:: Copyright 2010-2015 Viktor Szakats (vszakats.net/harbour)
:: See COPYING.txt for licensing terms.
:: ---------------------------------------------------------------

:: REQUIREMENTS FOR BUILD MACHINE:
::   - online 24/7 (reliable)
::   - Windows 7 or higher (server preferred)
::   - 8GB free disk space
::   - downstream internet traffic 20MB per day
::   - upstream internet traffic 100MB per day
::   - 1-5 hours CPU time per day
::   - multi-core CPU recommended
::   - admin rights for MSVC setup (optional)
::   - admin rights for Scheduled Task setup
::   - remote admin (via RDP protocol)

pushd "%~dp0"

echo ! Self: %0

set _HB_DIR_3RD=%~dp03rd\
set _HB_DIR_COMP=%~dp0comp\
set _HB_DIR_TOOL=%~dp0tool\

echo ! Downloading Harbour sources...

if exist harbour-core-master rd /q /s harbour-core-master
git clone -q --depth=8 --branch=master https://github.com/vszakats/harbour-core.git harbore-core-master
if errorlevel 1 goto _EXIT

"%_HB_DIR_TOOL%7z" x master.zip
cd harbour-core-master

echo ! Setting up generic build parameters...

set HB_VF=daily
set HB_VL=%HB_VF%
set HB_RT=%~dp0

set HB_BUILD_PKG=yes

set HB_SFX_7Z=%_HB_DIR_TOOL%7zsd.sfx
set HB_DIR_7Z=%_HB_DIR_TOOL%
set HB_DIR_UPX=%_HB_DIR_TOOL%upx\
set HB_DIR_MINGW=%_HB_DIR_COMP%mingw

set HB_WITH_ADS=%_HB_DIR_3RD%ads\acesdk
set HB_WITH_CAIRO=%_HB_DIR_3RD%cairo\include\cairo
set HB_WITH_CURL=%_HB_DIR_3RD%curl\include
set HB_WITH_FIREBIRD=%_HB_DIR_3RD%firebird\include
set HB_WITH_FREEIMAGE=%_HB_DIR_3RD%freeimage\Dist
set HB_WITH_GD=%_HB_DIR_3RD%gd\include
set HB_WITH_MYSQL=%_HB_DIR_3RD%mariadb\include\mysql
set HB_WITH_OCILIB=%_HB_DIR_3RD%ocilib\include
set HB_WITH_OPENSSL=%_HB_DIR_3RD%openssl\include
set HB_WITH_PGSQL=%_HB_DIR_3RD%pgsql\include

echo ! Building Harbour...

setlocal
echo ! Setting environment for using MinGW GCC
set PATH=%_HB_DIR_COMP%mingw\bin
set HB_CPU=x86
win-make clean install HB_VERSION=%HB_VF% > "%~dp0harbour-%HB_VF%-win-mingw-log.txt" 2>&1
if errorlevel 1 goto _EXIT
endlocal

:: Non-unified release package is here:
::    harbour-%HB_VF%-win-mingw.zip[.exe]

echo ! Creating unified Windows release package...

call package\winuni\mpkg_win_uni.bat > "%~dp0harbour-%HB_VF%-win-log.txt" 2>&1

echo ! Uploading Harbour unified Windows release package...

:: Unified release package is here:
::    %HB_RT%harbour-%HB_VF%-win.7z.exe

:_EXIT

:: Logs are here:
::    %~dp0harbour-%HB_VF%-win-log.txt
::    %~dp0harbour-%HB_VF%-win-mingw-log.txt

echo ! Finished.

popd
