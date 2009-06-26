@rem
@rem $Id$
@rem

@echo off

rem ---------------------------------------------------------------
rem Installer creator for Harbour Project
rem
rem Copyright 2009 Viktor Szakats (harbour.01 syenar.hu)
rem See COPYING for licensing terms.
rem
rem This script requires:
rem    - Windows NT or upper
rem    - NullSoft Installer installed (NSIS)
rem         https://sourceforge.net/project/showfiles.php?group_id=22049&package_id=15374
rem    - makensis.exe (part of NSIS) in PATH
rem         or HB_DIR_NSIS envvar set to its dir with an ending backslash.
rem    - Info-ZIP zip.exe in PATH
rem         https://sourceforge.net/project/showfiles.php?group_id=118012
rem    - HB_COMPILER envvar configured (see INSTALL doc)
rem    - C compiler and GNU Make configured (see INSTALL doc)
rem ---------------------------------------------------------------

if not "%OS%" == "Windows_NT" echo This Harbour build script requires Windows NT or upper.
if not "%OS%" == "Windows_NT" goto END

setlocal

rem ; Basic setup
set HB_VERSION=2.0.0beta2

rem ; Dir setup
set HB_INSTALL_BASE=%~dp0_hb_inst
set HB_INSTALL_PREFIX=%HB_INSTALL_BASE%\_pending
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
call "%~dp0make_gnu.bat"

if errorlevel 1 echo Harbour build returned: %ERRORLEVEL%
rem if errorlevel 1 goto MK_ERROR

rem ; Final name setup
set HB_PKGNAME=harbour-%HB_VERSION%-%HB_ARCHITECTURE%-%HB_COMPILER%
set HB_DIRNAME=harbour-%HB_ARCHITECTURE%-%HB_COMPILER%

rem ; Rename temp dir to final name
if exist "%HB_INSTALL_BASE%\%HB_DIRNAME%" rmdir /q /s "%HB_INSTALL_BASE%\%HB_DIRNAME%"
move "%HB_INSTALL_PREFIX%" "%HB_INSTALL_BASE%\%HB_DIRNAME%" > nul
set HB_INSTALL_PREFIX=%HB_INSTALL_BASE%\%HB_DIRNAME%

rem ; Post-build cleanup
if exist "%HB_BIN_INSTALL%\*.tds" del "%HB_BIN_INSTALL%\*.tds"
if exist "%HB_BIN_INSTALL%\*.lib" del "%HB_BIN_INSTALL%\*.lib"
if exist "%HB_BIN_INSTALL%\*.exp" del "%HB_BIN_INSTALL%\*.exp"

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
echo Harbour .exe install package creation: '%HB_PKGNAME%.exe'
"%HB_DIR_NSIS%makensis.exe" /V2 "%~dp0mpkg_win.nsi"

:MK_ERROR

rem ; Cleanup
if "%1" == "--deltemp" rmdir /q /s "%HB_INSTALL_PREFIX%"
if "%1" == "--deltemp" rmdir /q    "%HB_INSTALL_BASE%"

endlocal

:END
