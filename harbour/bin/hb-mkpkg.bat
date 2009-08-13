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
rem    - Info-ZIP zip.exe in PATH
rem         or HB_DIR_ZIP envvar set to its dir with an ending backslash.
rem         https://sourceforge.net/project/showfiles.php?group_id=118012
rem    - NullSoft Installer installed (NSIS)
rem         https://sourceforge.net/project/showfiles.php?group_id=22049&package_id=15374
rem      (only for Windows builds)
rem    - makensis.exe (part of NSIS) in PATH
rem         or HB_DIR_NSIS envvar set to its dir with an ending backslash.
rem      (only for Windows builds)
rem ---------------------------------------------------------------

if not "%OS%" == "Windows_NT" echo ! hb-mkpkg.bat Harbour build script requires Windows NT or upper.
if not "%OS%" == "Windows_NT" goto END

echo ! Harbour .zip install package creation: '%HB_PKGPATH%.zip'
if exist "%HB_PKGPATH%.zip" del "%HB_PKGPATH%.zip"
pushd
cd "%HB_PKGBASE%"
"%HB_DIR_ZIP%zip.exe" -q -9 -X -r -o "%HB_PKGPATH%.zip" . -i "%HB_PKGNAME%\*" -x *.tds -x *.exp
popd

if not "%HB_ARCHITECTURE%" == "dos" echo ! Harbour .exe install package creation: '%HB_PKGPATH%.exe'
if not "%HB_ARCHITECTURE%" == "dos" "%HB_DIR_NSIS%makensis.exe" /V2 "%~dp0..\mpkg_win.nsi"

:END
