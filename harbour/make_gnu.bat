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
rem Copyright 1999-2001 Viktor Szakats (viktor.szakats@syenar.hu)
rem See doc/license.txt for licensing terms.
rem ---------------------------------------------------------------


if "%HB_ARCHITECTURE%" == "" if not "%WINDIR%" == "" set HB_ARCHITECTURE=win
if "%HB_ARCHITECTURE%" == ""                         set HB_ARCHITECTURE=dos
if "%HB_COMPILER%"     == "" if not "%WINDIR%" == "" set HB_COMPILER=mingw
if "%HB_COMPILER%"     == ""                         set HB_COMPILER=djgpp

rem set HB_USER_PRGFLAGS=
rem set HB_USER_CFLAGS=
rem set HB_USER_LDFLAGS=

rem Set to constant value to be consistent with the non-GNU make files.

if "%HB_BIN_INSTALL%" == "" set HB_BIN_INSTALL=%HB_INSTALL_PREFIX%/bin
if "%HB_LIB_INSTALL%" == "" set HB_LIB_INSTALL=%HB_INSTALL_PREFIX%/lib
if "%HB_INC_INSTALL%" == "" set HB_INC_INSTALL=%HB_INSTALL_PREFIX%/include

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
   echo Notes:
   echo.
   echo   - HB_ARCHITECTURE and HB_COMPILER envvars must be set.
   echo     The following values are currently supported:
   echo.
   echo     HB_ARCHITECTURE:
   echo       - dos
   echo       - win
   echo.
   pause
   echo     HB_COMPILER:
   echo       - When HB_ARCHITECTURE=dos
   echo         - bcc16   (Borland C++ 3.x, 4.x, 5.0x, DOS 16-bit)
   echo         - djgpp   (Delorie GNU C, DOS 32-bit)
   echo         - rxs32   (EMX/RSXNT/DOS GNU C, DOS 32-bit)
   echo         - watcom  (Watcom C++ 9.x, 10.x, 11.x, DOS 32-bit)
   echo       - When HB_ARCHITECTURE=win
   echo         - bcc32   (Borland C++ 4.x, 5.x, Windows 32-bit)
   echo         - gcc     (Cygnus/Cygwin GNU C, Windows 32-bit)
   echo         - mingw   (MinGW GNU C, Windows 32-bit)
   echo         - rxsnt   (EMX/RSXNT/Windows GNU C, Windows 32-bit)
   echo         - icc     (IBM Visual Age C++, Windows 32-bit)
   echo         - msvc    (Microsoft Visual C++, Windows 32-bit)
   echo         - msvc64  (Microsoft Visual C++, Windows 64-bit)
   echo.
   echo   - Use these optional envvars to configure the make process
   echo     when using the 'all' command:
   echo.
   echo     HB_USER_PRGFLAGS - Extra Harbour compiler options
   echo     HB_USER_CFLAGS   - Extra C compiler options
   echo     HB_USER_LDFLAGS  - Extra linker options
   goto END

:BAD_ARCH

   echo Error: HB_ARCHITECTURE is not set.
   goto HELP

:BAD_COMP

   echo Error: HB_COMPILER is not set.
   goto HELP

:MAKE

   rem ---------------------------------------------------------------
   rem Start the GNU make system

   make %HB_USER_MAKEFLAGS% %1 %2 %3 %4 %5 %6 %7 %8 %9
   goto END

:END
