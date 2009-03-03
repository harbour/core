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
rem    - HB_COMPILER envvar configured (see INSTALL doc)
rem    - C compiler and GNU Make configured (see INSTALL doc)
rem ---------------------------------------------------------------

if not "%OS%" == "Windows_NT" goto END

setlocal

set HB_INSTALL_PREFIX=%~dp0_hb_install_temp
set HB_BUILD_DLL=yes
call make_gnu.bat
set HB_VERSION=1.1.0dev
makensis.exe %~dp0mpkg_win.nsi
if "%1" == "--deltemp" rmdir /q /s %HB_INSTALL_PREFIX%

endlocal

:END
