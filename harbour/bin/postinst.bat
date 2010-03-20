@rem
@rem $Id$
@rem

@echo off

rem ---------------------------------------------------------------
rem Copyright 2009 Viktor Szakats (harbour.01 syenar.hu)
rem See COPYING for licensing terms.
rem
rem Script run after Harbour make install to finish install process
rem (for Windows/DOS)
rem
rem Contains: install package creator script, which requires:
rem
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

if "%HB_BIN_INSTALL%" == "" echo ! HB_BIN_INSTALL needs to be set.
if "%HB_BIN_INSTALL%" == "" goto END

if "%HB_SHELL%" == "nt" goto _SH_NT

:_NO_COPYD

   if "%HB_PLATFORM%" == "linux" goto _NO_PKG
   if not "%HB_BUILD_PKG%" == "yes" goto _NO_PKG
   if "%HB_TOP%" == "" goto _NO_PKG

   echo ! Making Harbour .zip install package: '%HB_TOP%\%HB_PKGNAME%.zip'
   if exist "%HB_TOP%\%HB_PKGNAME%.zip" del %HB_TOP%\%HB_PKGNAME%.zip
   cd %HB_INSTALL_PREFIX%
   cd ..
   %HB_DIR_ZIP%zip.exe -q -9 -X -r -o %HB_TOP%\%HB_PKGNAME%.zip . -i %HB_PKGNAME%\* -x *.tds -x *.exp

   goto END

:_SH_NT

   if "%HB_PLATFORM%" == "dos" goto _NO_DLL_BIN
   if "%HB_PLATFORM%" == "linux" goto _NO_DLL_BIN
   if "%HB_BUILD_DLL%" == "no" goto _NO_DLL_BIN

   set HBMK_OPTIONS=
   if "%HB_BUILD_MODE%" == "cpp"  set HBMK_OPTIONS=%HBMK_OPTIONS% -cpp=yes
   if "%HB_BUILD_MODE%" == "c"    set HBMK_OPTIONS=%HBMK_OPTIONS% -cpp=no
   if "%HB_BUILD_DEBUG%" == "yes" set HBMK_OPTIONS=%HBMK_OPTIONS% -debug

   if not "%HB_BUILD_SHARED%" == "yes" (
      echo ! Making shared version of Harbour binaries...
      "%HB_HOST_BIN_DIR%\hbmk2" -quiet -q0 -lang=en -shared "-o%HB_BIN_INSTALL%\hbrun-dll"    "%~dp0..\utils\hbrun\hbrun.hbp"
      "%HB_HOST_BIN_DIR%\hbmk2" -quiet -q0 -lang=en -shared "-o%HB_BIN_INSTALL%\hbmk2-dll"    "%~dp0..\utils\hbmk2\hbmk2.hbp"
      "%HB_HOST_BIN_DIR%\hbmk2" -quiet -q0 -lang=en -shared "-o%HB_BIN_INSTALL%\hbtest-dll"   "%~dp0..\utils\hbtest\hbtest.hbp"
      "%HB_HOST_BIN_DIR%\hbmk2" -quiet -q0 -lang=en -shared "-o%HB_BIN_INSTALL%\hbi18n-dll"   "%~dp0..\utils\hbi18n\hbi18n.hbp"
      "%HB_HOST_BIN_DIR%\hbmk2" -quiet -q0 -lang=en -shared "-o%HB_BIN_INSTALL%\hbformat-dll" "%~dp0..\utils\hbformat\hbformat.hbp"
   )

:_NO_DLL_BIN

   if "%HB_PLATFORM%" == "dos" goto _NO_ICON_BIN
   if "%HB_PLATFORM%" == "linux" goto _NO_ICON_BIN

   rem ; We build this here, because GNU Make wouldn't add the icon.
   echo ! Making hbrun with application icon...
   "%HB_HOST_BIN_DIR%\hbmk2" -quiet -q0 -lang=en "-o%HB_BIN_INSTALL%\hbrun" "%~dp0..\utils\hbrun\hbrun.hbp"

:_NO_ICON_BIN

   if "%HB_BUILD_IMPLIB%" == "yes" call "%~dp0hb-mkimp.bat"

:_NO_IMPLIB

   if "%HB_PLATFORM%" == "linux" goto _NO_PKG
   if not "%HB_BUILD_PKG%" == "yes" goto _NO_PKG
   if "%HB_TOP%" == "" goto _NO_PKG

   rem NOTE: Believe it or not this is the official method to zip a different dir with subdirs
   rem       without including the whole root path in filenames; you have to 'cd' into it.
   rem       Even with zip 3.0. For this reason we need absolute path in HB_TOP. There is also
   rem       no zip 2.x compatible way to force creation of a new .zip, so we have to delete it
   rem       first to avoid mixing in an existing .zip file. [vszakats]

   echo ! Making Harbour .zip install package: '%HB_TOP%\%HB_PKGNAME%.zip'
   if exist "%HB_TOP%\%HB_PKGNAME%.zip" del "%HB_TOP%\%HB_PKGNAME%.zip"
   pushd
   cd "%HB_INSTALL_PREFIX%\.."
   "%HB_DIR_ZIP%zip.exe" -q -9 -X -r -o "%HB_TOP%\%HB_PKGNAME%.zip" . -i "%HB_PKGNAME%\*" -x *.tds -x *.exp
   popd

   echo ! Making Harbour .exe install package: '%HB_TOP%\%HB_PKGNAME%.exe'
   "%HB_DIR_NSIS%makensis.exe" /V2 "%~dp0..\package\mpkg_win.nsi"

:_NO_PKG

:END
