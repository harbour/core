@rem
@rem $Id$
@rem

@echo off

rem ---------------------------------------------------------------
rem Installer creator for Harbour Project
rem
rem Copyright 2009 Viktor Szakats (harbour.01 syenar.hu)
rem See doc/license.txt for licensing terms.
rem
rem This script requires:
rem    - Windows NT or upper
rem    - NullSoft Installer installed (NSIS)
rem      http://nsis.sourceforge.net
rem    - makensis.exe (part of NSIS) in PATH
rem    - Info-ZIP zip.exe in PATH
rem    - HB_COMPILER envvar configured (see INSTALL doc)
rem    - C compiler and GNU Make configured (see INSTALL doc)
rem ---------------------------------------------------------------

if not "%OS%" == "Windows_NT" goto END

setlocal

rem ; Basic setup
set HB_VERSION=1.1.0dev
set HB_ARCHITECTURE=win
set HB_PKGNAME=harbour-%HB_VERSION%-%HB_ARCHITECTURE%-%HB_COMPILER%
set HB_DIRNAME=harbour-%HB_COMPILER%

rem ; Dir setup
set HB_INSTALL_BASE=%~dp0_hb_install_temp
set HB_INSTALL_PREFIX=%HB_INSTALL_BASE%\%HB_DIRNAME%
set HB_BIN_INSTALL=%HB_INSTALL_PREFIX%\bin
set HB_LIB_INSTALL=%HB_INSTALL_PREFIX%\lib
set HB_INC_INSTALL=%HB_INSTALL_PREFIX%\include
set HB_DOC_INSTALL=%HB_INSTALL_PREFIX%\doc

rem ; Cleanup
if exist %HB_INSTALL_BASE% rmdir /q /s %HB_INSTALL_BASE%

rem ; Build
if "%HB_COMPILER%" == "mingw"  ( sh make_gnu.sh clean install && goto MK_PKG )
if "%HB_COMPILER%" == "cygwin" ( sh make_gnu.sh clean install && goto MK_PKG )
set HB_DIR_IMPLIB=no
set HB_BUILD_DLL=yes
call make_gnu.bat

:MK_PKG

rem ; Installer package
makensis.exe %~dp0mpkg_win.nsi

rem ; .zip package
if exist %HB_PKGNAME%.zip del %HB_PKGNAME%.zip
pushd
cd %HB_INSTALL_BASE%
zip -X -r -o %~dp0%HB_PKGNAME%.zip *
popd

rem ; Cleanup
if "%1" == "--deltemp" rmdir /q /s %HB_INSTALL_BASE%

endlocal

:END
