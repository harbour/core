@echo off
rem 
rem $Id$
rem 

rem Flavour: OS/2 command file

rem ---------------------------------------------------------------
rem Template to initialize the environment before starting
rem the GNU make system for Harbour
rem
rem For further information about the GNU make system please
rem check doc/gmake.txt
rem
rem Copyright 1999-2000 Victor Szakats (info@szelvesz.hu)
rem See doc/license.txt for licensing terms.
rem ---------------------------------------------------------------

if "%HB_ARCHITECTURE%" == "" set HB_ARCHITECTURE=os2
if "%HB_COMPILER%" == "" set HB_COMPILER=gcc
if "%HB_GT_LIB%" == "" set HB_GT_LIB=

rem set PRG_USR= 
rem set C_USR= 
rem set L_USR=

rem Set to constant value to be consistent with the non-GNU make files.

set HB_BIN_INSTALL=bin\
set HB_LIB_INSTALL=lib\
set HB_INC_INSTALL=include\

:START

   if "%HB_ARCHITECTURE%" == "" goto BAD_ARCH
   if "%HB_COMPILER%" == "" goto BAD_COMP

   goto MAKE

:HELP

   echo.
   echo Usage: make_gnu.cmd [command]
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
   echo       - dos   (HB_GT_LIB=gtdos by default)
   echo       - w32   (HB_GT_LIB=gtw32 by default)
   echo       - linux (HB_GT_LIB=gtstd by default)
   echo       - os2   (HB_GT_LIB=gtos2 by default)
   echo.   
   pause   
   echo     HB_COMPILER:
   echo       - When HB_ARCHITECTURE=dos
   echo         - bcc16   (Borland C++ 3.x, 16-bit DOS)
   echo         - djgpp   (GCC (DJGPP), 32-bit Windows)
   echo         - watcom
   echo       - When HB_ARCHITECTURE=w32
   echo         - bcc32   (Borland C++ 4.x,5.x, 32-bit Windows)
   echo         - gcc     (GCC (Cygnus), 32-bit Windows)
   echo         - mingw32 (GCC (Cygnus/MingW32), Windows 32 bit)
   echo         - icc
   echo         - msvc    (Microsoft Visual C++, Windows 32 bit)
   echo       - When HB_ARCHITECTURE=linux
   echo         - gcc
   echo       - When HB_ARCHITECTURE=os2
   echo         - gcc     (GCC (EMX), 32-bit OS/2)
   echo         - icc     (ICC (IBM Visual Age C++ 3.0), 32-bit OS/2)
   echo.   
   pause   
   echo     HB_GT_LIB:
   echo       - gtstd (Standard streaming) (for all architectures)
   echo       - gtdos (DOS console)        (for dos architecture)
   echo       - gtwin (Win32 console)      (for w32 architecture)
   echo       - gtos2 (OS/2 console)       (for os2 architecture)
   echo       - gtpca (PC ANSI console)    (for all architectures)
   echo       - gtcrs (Curses console)     (for linux, w32 architectures)
   echo       - gtsln (Slang console)      (for linux, w32 architectures)
   echo.
   echo   - Use these optional envvars to configure the make process
   echo     when using the 'all' command:
   echo.
   echo     PRG_USR - Extra Harbour compiler options
   echo     C_USR   - Extra C compiler options
   echo     L_USR   - Extra linker options
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

   make %1 %2 %3 %4 %5 %6 %7 %8 %9
   goto END

:END

