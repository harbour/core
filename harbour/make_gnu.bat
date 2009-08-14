@rem
@rem $Id$
@rem

@echo off

rem ---------------------------------------------------------------
rem Copyright 1999-2009 Viktor Szakats (harbour.01 syenar.hu)
rem See COPYING for licensing terms.
rem
rem Harbour Project build starter script (for Windows/DOS)
rem
rem Please read INSTALL for further information.
rem ---------------------------------------------------------------

rem ---------------------------------------------------------------
rem Decide about GNU Make executable name

rem Some other name variations: gnumake.exe, gmake.exe
set _HB_MAKE=
if "%HB_MAKE%%OS%" == "Windows_NT" if exist "%~dp0config\dj-make.exe" set _HB_MAKE="%~dp0config\dj-make.exe"
if "%HB_MAKE%%OS%" == "Windows_NT" set _HB_MAKE=mingw32-make.exe
if "%OS%" == "Windows_NT" goto _FM_DONE
if     exist config\dj-make.exe set _HB_MAKE=config\dj-make.exe
if not exist config\dj-make.exe set _HB_MAKE=make.exe
:_FM_DONE

rem ---------------------------------------------------------------
rem Start the GNU Make system

%_HB_MAKE% %HB_USER_MAKEFLAGS% %1 %2 %3 %4 %5 %6 %7 %8 %9
if errorlevel 1 echo Harbour GNU Make returned: %ERRORLEVEL%

set _HB_MAKE=
