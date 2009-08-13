@rem
@rem $Id$
@rem

@echo off

rem ---------------------------------------------------------------
rem Copyright 1999-2009 Viktor Szakats (harbour.01 syenar.hu)
rem See COPYING for licensing terms.
rem
rem Harbour Project build script (for Windows/DOS)
rem
rem Please read INSTALL for further information.
rem ---------------------------------------------------------------

   rem ---------------------------------------------------------------
   rem Setup output log

   set _HB_BUILD_LOG=
   set HB_BUILD_LOGFILE=
   if  not "%HB_BUILD_LOG%" == ""    if "%OS%" == "Windows_NT" set _HB_BUILD_LOG=%HB_BUILD_LOG%
:  if      "%HB_BUILD_LOG%" == ""    if "%OS%" == "Windows_NT" set _HB_BUILD_LOG=hb-build-%DATE:~0,4%%DATE:~6,2%%DATE:~10,2%-%TIME:~0,2%%TIME:~3,2%%TIME:~6,2%.txt
   if     "%_HB_BUILD_LOG%" == "yes" if "%OS%" == "Windows_NT" set _HB_BUILD_LOG=hb-build-%DATE:~0,4%%DATE:~6,2%%DATE:~10,2%-%TIME:~0,2%%TIME:~3,2%%TIME:~6,2%.txt
   if     "%_HB_BUILD_LOG%" == "no"  if "%OS%" == "Windows_NT" set _HB_BUILD_LOG=
   if not "%_HB_BUILD_LOG%" == ""    if "%OS%" == "Windows_NT" set HB_BUILD_LOGFILE=%_HB_BUILD_LOG%
   if not "%_HB_BUILD_LOG%" == ""    if "%OS%" == "Windows_NT" echo Harbour build output to: '%_HB_BUILD_LOG%'
   if not "%_HB_BUILD_LOG%" == ""    if "%OS%" == "Windows_NT" if exist "%_HB_BUILD_LOG%" del "%_HB_BUILD_LOG%"
   if not "%_HB_BUILD_LOG%" == ""    if "%OS%" == "Windows_NT" set _HB_BUILD_LOG=^>^> %_HB_BUILD_LOG% 2^>^&1

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

   if "%HB_COMPILER%" == "cygwin" goto SKIP_WINDLL_CYG

   %_HB_MAKE% %HB_USER_MAKEFLAGS% %1 %2 %3 %4 %5 %6 %7 %8 %9 %_HB_BUILD_LOG%
   if errorlevel 1 echo Harbour GNU Make returned: %ERRORLEVEL%
   goto MAKE_DONE

:SKIP_WINDLL_CYG

   sh %~dp0make_gnu.sh %HB_USER_MAKEFLAGS% %1 %2 %3 %4 %5 %6 %7 %8 %9 %_HB_BUILD_LOG%
   if errorlevel 1 echo Harbour GNU Make returned: %ERRORLEVEL%
   goto MAKE_DONE

:MAKE_DONE

   set _HB_MAKE=
   set _HB_BUILD_LOG=
   goto END

:END
