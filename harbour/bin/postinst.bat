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
