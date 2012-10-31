@rem
@rem $Id$
@rem

@echo off

rem ---------------------------------------------------------------
rem Copyright 2010 Viktor Szakats (harbour syenar.net)
rem See COPYING for licensing terms.
rem ---------------------------------------------------------------

rem ---------------------------------------------------------------
rem REQUIREMENTS FOR BUILD MACHINE:
rem   - Online 24/7 (reliable)
rem   - Windows XP or higher (server preferred)
rem   - 8GB disk space
rem   - downstream internet traffic 100MB per day
rem   - upstream internet traffic 100MB per day
rem   - 1-5hours CPU time per day
rem   - Multicore CPU recommended
rem   - admin rights for MSVC setup
rem   - admin rights for Scheduled Task setup
rem   - remote admin (via RDC or VNC protocol, RDC preferred)
rem
rem NOTES:
rem   - The first run under a new (or reinstalled) user account
rem     must be done interactively to confirm server identity
rem     when doing SCP uploads.
rem ---------------------------------------------------------------

pushd

cd %~dp0

echo ! Self: %0

set _HB_DIR_3RD=%~dp03rd\
set _HB_DIR_COMP=%~dp0comp\
set _HB_DIR_TOOL=%~dp0tool\

echo ! Downloading Harbour sources...

if exist harbour rd /q /s harbour
%_HB_DIR_TOOL%svn\bin\svn export http://harbour-project.svn.sourceforge.net/svnroot/harbour-project/trunk/harbour
if errorlevel 1 goto _EXIT
cd harbour

echo ! Setting up generic build parameters...

set HB_VF=nightly
set HB_VL=%HB_VF%
set HB_RT=%~dp0

set _HB_MAKE_OPTION=HB_VERSION=%HB_VF%
set _HB_SFNET_URL=,harbour-project@frs.sourceforge.net:/home/frs/project/h/ha/harbour-project/binaries-windows/nightly/

set HB_BUILD_PKG=yes

set HB_DIR_NSIS=%_HB_DIR_TOOL%nsis\
set HB_OPT_NSIS=/DPKG_NO_COMP_BCC /DPKG_NO_COMP_MSVC /DPKG_NO_COMP_MSVC64 /DPKG_NO_COMP_MINGW64 /DPKG_NO_COMP_MINGWARM /DPKG_NO_PLAT_LINUX /DPKG_NO_PLAT_OS2 /DPKG_NO_PLAT_DOS /DPKG_NO_COMP_WATCOM
set HB_DIR_ZIP=%_HB_DIR_TOOL%
set HB_DIR_7Z=%_HB_DIR_TOOL%
set HB_DIR_UPX=%_HB_DIR_TOOL%upx\
rem set HB_DIR_BCC_IMPLIB=%_HB_DIR_COMP%bcc\Bin\
set HB_DIR_MINGW=%_HB_DIR_COMP%mingw

set HB_WITH_ADS=%_HB_DIR_3RD%ads\acesdk
set HB_WITH_ALLEGRO=%_HB_DIR_3RD%allegro\include
set HB_WITH_BLAT=%_HB_DIR_3RD%blat\full\source
set HB_WITH_CAIRO=%_HB_DIR_3RD%cairo\include\cairo
set HB_WITH_CURL=%_HB_DIR_3RD%curl\include
set HB_WITH_FIREBIRD=%_HB_DIR_3RD%firebird\include
set HB_WITH_FREEIMAGE=%_HB_DIR_3RD%freeimage\Dist
set HB_WITH_GD=%_HB_DIR_3RD%gd\include
set HB_WITH_MYSQL=%_HB_DIR_3RD%mysql\include
set HB_WITH_OCILIB=%_HB_DIR_3RD%ocilib\include
set HB_WITH_OPENSSL=%_HB_DIR_3RD%openssl\include
set HB_WITH_PGSQL=%_HB_DIR_3RD%pgsql\include

echo ! Building Harbour...

setlocal
echo ! Setting environment for using MinGW GCC
set PATH=%_HB_DIR_COMP%mingw\bin
win-make clean install %_HB_MAKE_OPTION% > "%~dp0harbour-nightly-win-mingw-log.txt" 2>&1
if errorlevel 1 goto _EXIT
endlocal

rem setlocal
rem echo ! Setting environment for using Borland C++
rem set PATH=%_HB_DIR_COMP%bcc\Bin
rem win-make clean install %_HB_MAKE_OPTION% > "%~dp0harbour-nightly-win-bcc-log.txt" 2>&1
rem if errorlevel 1 goto _EXIT
rem endlocal

rem setlocal
rem echo ! Setting environment for using Open Watcom
rem SET WATCOM=%_HB_DIR_COMP%watcom
rem SET PATH=%WATCOM%\BINNT;%WATCOM%\BINW
rem SET EDPATH=%WATCOM%\EDDAT
rem SET INCLUDE=%WATCOM%\H;%WATCOM%\H\NT
rem win-make clean install %_HB_MAKE_OPTION% > "%~dp0harbour-nightly-win-watcom-log.txt" 2>&1
rem endlocal

rem echo ! Uploading Harbour Windows binaries...
rem
rem %_HB_DIR_TOOL%pscp.exe -i %HB_SFNET_FRS_PRIVATE_KEY% harbour-nightly-win-mingw.exe  %HB_SFNET_USER%%_HB_SFNET_URL%
rem %_HB_DIR_TOOL%pscp.exe -i %HB_SFNET_FRS_PRIVATE_KEY% harbour-nightly-win-mingw.zip  %HB_SFNET_USER%%_HB_SFNET_URL%
rem %_HB_DIR_TOOL%pscp.exe -i %HB_SFNET_FRS_PRIVATE_KEY% harbour-nightly-win-bcc.exe    %HB_SFNET_USER%%_HB_SFNET_URL%
rem %_HB_DIR_TOOL%pscp.exe -i %HB_SFNET_FRS_PRIVATE_KEY% harbour-nightly-win-bcc.zip    %HB_SFNET_USER%%_HB_SFNET_URL%
rem %_HB_DIR_TOOL%pscp.exe -i %HB_SFNET_FRS_PRIVATE_KEY% harbour-nightly-win-watcom.exe %HB_SFNET_USER%%_HB_SFNET_URL%
rem %_HB_DIR_TOOL%pscp.exe -i %HB_SFNET_FRS_PRIVATE_KEY% harbour-nightly-win-watcom.zip %HB_SFNET_USER%%_HB_SFNET_URL%

echo ! Creating unified Windows package...

call package\winuni\mpkg_win_uni.bat

echo ! Uploading Harbour unified Windows package...

%_HB_DIR_TOOL%pscp.exe -i %HB_SFNET_FRS_PRIVATE_KEY% %HB_RT%harbour-nightly-win.exe            %HB_SFNET_USER%%_HB_SFNET_URL%
%_HB_DIR_TOOL%pscp.exe -i %HB_SFNET_FRS_PRIVATE_KEY% %HB_RT%harbour-nightly-win.7z             %HB_SFNET_USER%%_HB_SFNET_URL%

:_EXIT

%_HB_DIR_TOOL%pscp.exe -i %HB_SFNET_FRS_PRIVATE_KEY% %HB_RT%harbour-nightly-win-log.txt        %HB_SFNET_USER%%_HB_SFNET_URL%
%_HB_DIR_TOOL%pscp.exe -i %HB_SFNET_FRS_PRIVATE_KEY% %HB_RT%harbour-nightly-win-mingw-log.txt  %HB_SFNET_USER%%_HB_SFNET_URL%
rem %_HB_DIR_TOOL%pscp.exe -i %HB_SFNET_FRS_PRIVATE_KEY% %HB_RT%harbour-nightly-win-bcc-log.txt    %HB_SFNET_USER%%_HB_SFNET_URL%
rem %_HB_DIR_TOOL%pscp.exe -i %HB_SFNET_FRS_PRIVATE_KEY% %HB_RT%harbour-nightly-win-watcom-log.txt %HB_SFNET_USER%%_HB_SFNET_URL%

echo ! Finished.

popd
