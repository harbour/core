@rem
@rem $Id$
@rem

@echo off

rem ---------------------------------------------------------------
rem This is a generic template file, if it doesn't fit your own needs
rem please DON'T MODIFY IT.
rem
rem Instead, make a local copy and modify that one, or make a call to
rem this batch file from your customized one. [vszakats]
rem ---------------------------------------------------------------

rem ---------------------------------------------------------------
rem Template to initialize the environment before starting
rem the GNU make system for Harbour
rem
rem For further information about the GNU make system please
rem check doc/gmake.txt
rem
rem Copyright 1999-2009 Viktor Szakats (viktor.szakats@syenar.hu)
rem See doc/license.txt for licensing terms.
rem ---------------------------------------------------------------

rem Setup defaults.

if "%HB_ARCHITECTURE%" == "" if not "%WINDIR%" == "" set HB_ARCHITECTURE=win
if "%HB_ARCHITECTURE%" == ""                         set HB_ARCHITECTURE=dos
if "%HB_COMPILER%"     == "" if not "%WINDIR%" == "" set HB_COMPILER=msvc
if "%HB_COMPILER%"     == ""                         set HB_COMPILER=djgpp

if "%HB_INSTALL_PREFIX%" == "" if "%OS%" == "Windows_NT" set HB_INSTALL_PREFIX=%~dp0

rem Set to constant value to be consistent with the non-GNU make files.

if "%HB_BIN_INSTALL%" == "" set HB_BIN_INSTALL=%HB_INSTALL_PREFIX%\bin
if "%HB_LIB_INSTALL%" == "" set HB_LIB_INSTALL=%HB_INSTALL_PREFIX%\lib
if "%HB_INC_INSTALL%" == "" set HB_INC_INSTALL=%HB_INSTALL_PREFIX%\include
if "%HB_DOC_INSTALL%" == "" set HB_DOC_INSTALL=%HB_INSTALL_PREFIX%\doc

rem Try to create install dirs.

if not exist %HB_BIN_INSTALL%\*.* md %HB_BIN_INSTALL%
if not exist %HB_LIB_INSTALL%\*.* md %HB_LIB_INSTALL%
if not exist %HB_INC_INSTALL%\*.* md %HB_INC_INSTALL%
if not exist %HB_DOC_INSTALL%\*.* md %HB_DOC_INSTALL%

:START

   if "%HB_ARCHITECTURE%" == "" goto BAD_ARCH
   if "%HB_COMPILER%" == "" goto BAD_COMP

   goto MAKE

:HELP

   echo.
   echo Usage: make_gnu [command]
   echo.
   echo The following commands are supported:
   echo   - all (default)
   echo   - clean
   echo   - install
   echo.
   echo Please read INSTALL for HOWTOs and description
   echo of available options.
   goto END

:BAD_ARCH

   echo Error: HB_ARCHITECTURE is not set.
   goto HELP

:BAD_COMP

   echo Error: HB_COMPILER is not set.
   goto HELP

:MAKE

   rem ---------------------------------------------------------------
   rem Detect name of GNU Make
   rem
   rem Look for mingw32-make.exe and use it if found. Works only
   rem on Windows NT and upper. [vszakats]

   set _HB_MAKE=make.exe
   if not "%OS%" == "Windows_NT" goto _FM_DONE
   set _HB_CHECK=mingw32-make.exe
   if exist "%_HB_CHECK%" ( set _HB_MAKE=%_HB_CHECK%&& goto _FM_DONE )
   set _HB_PATH=%PATH%
   :_FM_LOOP
   for /F "delims=; tokens=1,2*" %%p in ("%_HB_PATH%") do (
      if exist "%%p\%_HB_CHECK%" ( set _HB_MAKE=%_HB_CHECK%&& goto _FM_DONE )
      if exist "%%p%_HB_CHECK%"  ( set _HB_MAKE=%_HB_CHECK%&& goto _FM_DONE )
      set _HB_PATH=%%~q;%%~r
   )
   if not "%_HB_PATH%"==";" goto _FM_LOOP
   :_FM_DONE
   set _HB_CHECK=
   set _HB_PATH=

   rem ---------------------------------------------------------------
   rem Start the GNU make system

   rem ---------------------------------------------------------------
   rem Special build mode when HB_BUILD_DLL=yes on Windows platform.
   rem It will automatically build Harbour in two passes, one for
   rem the .dlls and a final pass for the regular version.

   if not "%HB_ARCHITECTURE%" == "win" goto SKIP_WINDLL
   if not "%HB_BUILD_DLL%" == "yes" goto SKIP_WINDLL

   set _HB_CONTRIBLIBS=%HB_CONTRIBLIBS%
   set _HB_CONTRIB_ADDONS=%HB_CONTRIB_ADDONS%
   set HB_CONTRIBLIBS=no
   set HB_CONTRIB_ADDONS=
   %_HB_MAKE% clean   %HB_USER_MAKEFLAGS% %1 %2 %3 %4 %5 %6 %7 %8 %9
   %_HB_MAKE% install %HB_USER_MAKEFLAGS% %1 %2 %3 %4 %5 %6 %7 %8 %9
   set HB_BUILD_DLL=
   set HB_CONTRIBLIBS=%_HB_CONTRIBLIBS%
   set HB_CONTRIB_ADDONS=%_HB_CONTRIB_ADDONS%
   set _HB_CONTRIBLIBS=
   set _HB_CONTRIB_ADDONS=
   %_HB_MAKE% clean   %HB_USER_MAKEFLAGS% %1 %2 %3 %4 %5 %6 %7 %8 %9
   %_HB_MAKE% install %HB_USER_MAKEFLAGS% %1 %2 %3 %4 %5 %6 %7 %8 %9
   set HB_BUILD_DLL=yes

   goto END

:SKIP_WINDLL

   %_HB_MAKE% %HB_USER_MAKEFLAGS% %1 %2 %3 %4 %5 %6 %7 %8 %9
   goto END

:END

set _HB_MAKE=
