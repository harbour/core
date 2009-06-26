@rem
@rem $Id$
@rem

@echo off

rem ---------------------------------------------------------------
rem Copyright 2009 Viktor Szakats (harbour.01 syenar.hu)
rem See COPYING for licensing terms.
rem
rem Harbour Project installer creator script (for Windows/DOS)
rem
rem This script requires:
rem    - Windows NT or upper
rem    - NullSoft Installer installed (NSIS)
rem         https://sourceforge.net/project/showfiles.php?group_id=22049&package_id=15374
rem      (only for Windows builds)
rem    - makensis.exe (part of NSIS) in PATH
rem         or HB_DIR_NSIS envvar set to its dir with an ending backslash.
rem      (only for Windows builds)
rem    - Info-ZIP zip.exe in PATH
rem         https://sourceforge.net/project/showfiles.php?group_id=118012
rem    - C compiler configured (see INSTALL doc)
rem
rem Please read INSTALL for further information.
rem ---------------------------------------------------------------

if not "%OS%" == "Windows_NT" echo This Harbour build script requires Windows NT or upper.
if not "%OS%" == "Windows_NT" goto END

setlocal

rem ; Dir setup
set HB_INSTALL_BASE=%~dp0_pk
set HB_INSTALL_PREFIX=%HB_INSTALL_BASE%\_w
set HB_BIN_INSTALL=
set HB_LIB_INSTALL=
set HB_INC_INSTALL=
set HB_DOC_INSTALL=

rem ; Pre-build cleanup
if exist "%HB_INSTALL_PREFIX%" rmdir /q /s "%HB_INSTALL_PREFIX%"

rem ; Option setup
set HB_BUILD_LOG=yes
set HB_BUILD_DLL=yes
set HB_BUILD_OPTIM=yes
set HB_BUILD_DEBUG=no
set HB_BUILD_IMPLIB=no

rem ; Build Harbour
call "%~dp0make_gnu.bat" clean install

if errorlevel 1 echo Harbour build returned: %ERRORLEVEL%
rem if errorlevel 1 goto MK_ERROR

rem ; Final name setup
if     "%HB_ARCHITECTURE%" == "dos" set HB_VERSION=2b2
if     "%HB_ARCHITECTURE%" == "dos" set HB_PKGNAME=hb%HB_VERSION%%HB_COMPILER:~0,2%
if     "%HB_ARCHITECTURE%" == "dos" set HB_DIRNAME=%HB_PKGNAME%
if not "%HB_ARCHITECTURE%" == "dos" set HB_VERSION=2.0.0beta2
if not "%HB_ARCHITECTURE%" == "dos" set HB_PKGNAME=harbour-%HB_VERSION%-%HB_ARCHITECTURE%-%HB_COMPILER%
if not "%HB_ARCHITECTURE%" == "dos" set HB_DIRNAME=harbour-%HB_ARCHITECTURE%-%HB_COMPILER%

rem ; Rename temp dir to final name
if exist "%HB_INSTALL_BASE%\%HB_DIRNAME%" rmdir /q /s "%HB_INSTALL_BASE%\%HB_DIRNAME%"
move "%HB_INSTALL_PREFIX%" "%HB_INSTALL_BASE%\%HB_DIRNAME%" > nul
set HB_INSTALL_PREFIX=%HB_INSTALL_BASE%\%HB_DIRNAME%

rem ; Post-build cleanup
if exist "%HB_INSTALL_PREFIX%\bin\*.tds" del "%HB_INSTALL_PREFIX%\bin\*.tds"
if exist "%HB_INSTALL_PREFIX%\bin\*.lib" del "%HB_INSTALL_PREFIX%\bin\*.lib"
if exist "%HB_INSTALL_PREFIX%\bin\*.exp" del "%HB_INSTALL_PREFIX%\bin\*.exp"

rem ; Post-build installation
copy /Y ChangeLog* "%HB_INSTALL_PREFIX%\" > nul
copy /Y COPYING    "%HB_INSTALL_PREFIX%\" > nul
copy /Y ERRATA     "%HB_INSTALL_PREFIX%\" > nul
copy /Y INSTALL    "%HB_INSTALL_PREFIX%\" > nul
copy /Y TODO       "%HB_INSTALL_PREFIX%\" > nul

rem ; Build .zip package
echo Harbour .zip install package creation: '%HB_PKGNAME%.zip'
if exist "%HB_PKGNAME%.zip" del "%HB_PKGNAME%.zip"
pushd
cd "%HB_INSTALL_BASE%"
zip -q -9 -X -r -o "%~dp0%HB_PKGNAME%.zip" . -i "%HB_DIRNAME%\*"
popd

rem ; Build installer package
if not "%HB_ARCHITECTURE%" == "dos" echo Harbour .exe install package creation: '%HB_PKGNAME%.exe'
if not "%HB_ARCHITECTURE%" == "dos" "%HB_DIR_NSIS%makensis.exe" /V2 "%~dp0mpkg_win.nsi"

:MK_ERROR

rem ; Cleanup
if "%1" == "--deltemp" rmdir /q /s "%HB_INSTALL_PREFIX%"
if "%1" == "--deltemp" rmdir /q    "%HB_INSTALL_BASE%"

endlocal

:END
