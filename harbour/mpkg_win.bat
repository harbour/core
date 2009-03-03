@rem
@rem $Id$
@rem

@echo off

rem ---------------------------------------------------------------
rem Installer creator for Harbour Project
rem
rem This script requires:
rem    - NullSoft Installer
rem      http://nsis.sourceforge.net
rem    - makensis.exe in PATH.
rem    - Windows NT or upper.
rem
rem Copyright 2009 Viktor Szakats (harbour.01 syenar.hu)
rem See doc/license.txt for licensing terms.
rem ---------------------------------------------------------------

if not "%OS%" == "Windows_NT" goto END

if "%HB_INSTALL_PREFIX%" == "" set HB_INSTALL_PREFIX=%~dp0_install

set HB_BUILD_DLL=yes

call make_gnu.bat

set HB_VERSION=1.1.0dev

makensis.exe %~dp0mpkg_win.nsi

:END
