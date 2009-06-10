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
rem    - Info-ZIP zip.exe in PATH
rem         https://sourceforge.net/project/showfiles.php?group_id=118012
rem    - HB_COMPILER envvar configured (see INSTALL doc)
rem    - C compiler and GNU Make configured (see INSTALL doc)
rem ---------------------------------------------------------------

if not "%OS%" == "Windows_NT" goto END

setlocal

rem ; Basic setup
set HB_VERSION=200
if "%HB_ARCHITECTURE%" == "" set HB_ARCHITECTURE=dos
set HB_PKGNAME=hb%HB_VERSION%
set HB_DIRNAME=%HB_PKGNAME%

rem ; Dir setup
set HB_INSTALL_BASE=%~dp0_hb_inst
set HB_INSTALL_PREFIX=%HB_INSTALL_BASE%\%HB_DIRNAME%
set HB_BIN_INSTALL=%HB_INSTALL_PREFIX%\bin
set HB_LIB_INSTALL=%HB_INSTALL_PREFIX%\lib
set HB_INC_INSTALL=%HB_INSTALL_PREFIX%\include
set HB_DOC_INSTALL=%HB_INSTALL_PREFIX%\doc

rem ; Pre-build cleanup
if exist %HB_INSTALL_PREFIX% rmdir /q /s %HB_INSTALL_PREFIX%

rem ; Option setup
set HB_BUILD_OPTIM=yes
set HB_BUILD_DEBUG=no

rem ; Build Harbour
call make_gnu.bat clean install

rem if errorlevel 1 goto MK_ERROR

rem ; Post-build installation
xcopy /D /Y ChangeLog* "%HB_INSTALL_PREFIX%\"
xcopy /D /Y COPYING    "%HB_INSTALL_PREFIX%\"
xcopy /D /Y ERRATA     "%HB_INSTALL_PREFIX%\"
xcopy /D /Y INSTALL    "%HB_INSTALL_PREFIX%\"
xcopy /D /Y TODO       "%HB_INSTALL_PREFIX%\"

rem ; Build .zip package
if exist %HB_PKGNAME%.zip del %HB_PKGNAME%.zip
pushd
cd %HB_INSTALL_BASE%
zip -9 -X -r -o %~dp0%HB_PKGNAME%.zip . -i %HB_DIRNAME%\*
popd

:MK_ERROR

rem ; Cleanup
if "%1" == "--deltemp" rmdir /q /s %HB_INSTALL_PREFIX%
if "%1" == "--deltemp" rmdir /q    %HB_INSTALL_BASE%

endlocal

:END
