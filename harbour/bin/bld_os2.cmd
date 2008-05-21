@echo off
rem
rem $Id$
rem

rem ---------------------------------------------------------------
rem Template to build a final Harbour executable, using Harbour
rem with the C code generation feature, then calling the proper C
rem linker/compiler.
rem
rem Copyright 1999-2001 Viktor Szakats (viktor.szakats@syenar.hu)
rem See doc/license.txt for licensing terms.
rem ---------------------------------------------------------------

if "%HB_COMPILER%" == "" set HB_COMPILER=gcc

if "%HB_BIN_INSTALL%" == "" set HB_BIN_INSTALL=..\bin
if "%HB_LIB_INSTALL%" == "" set HB_LIB_INSTALL=..\lib
if "%HB_INC_INSTALL%" == "" set HB_INC_INSTALL=..\include

:START

   if "%HB_COMPILER%" == "" goto NO_COMP

   if not "%1" == "" goto COMPILE

:HELP

   echo.
   echo Usage: bld filename
   echo.
   echo Notes:
   echo.
   echo   - 'filename' is the .prg filename _without_ extension.
   echo   - Don't forget to make a MAIN() function for you application.
   echo   - This batch file assumes you are in some directory off the main
   echo     harbour directory.
   echo   - Environment variables HB_COMPILER, HB_GT_LIB should be set.
   echo     Setting HB_GT_LIB is optional.
   echo     The following values are currently supported:
   echo.
   echo     HB_COMPILER:
   echo       - gcc     (EMX GNU C, OS/2 32-bit)
   echo       - icc     (IBM Visual Age C++ 3.0, OS/2 32-bit)
   echo.   
   echo     HB_GT_LIB:
   echo       - gtstd (Standard streaming) (for all architectures)
   echo       - gtos2 (OS/2 console)       (for os2 architecture)
   echo       - gtpca (PC ANSI console)    (for all architectures)
   echo       - gtcrs (Curses console)     (for linux, w32 architectures)
   echo       - gtsln (Slang console)      (for linux, w32 architectures)
   goto END

:NO_COMP

   echo Error: HB_COMPILER is not set.
   goto HELP

:BAD_COMP

   echo Error: HB_COMPILER value is unsupported.
   goto HELP

:COMPILE

   %HB_BIN_INSTALL%\harbour %1.prg -n -i%HB_INC_INSTALL% %2 %3 %HARBOURFLAGS%

:A_DOS

   set _HB_GT_LIB=%HB_GT_LIB%
   if "%_HB_GT_LIB%" == "" set _HB_GT_LIB=gtos2

   if "%HB_COMPILER%" == "gcc"     gcc %1.c %CFLAGS% -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% -lhbcpage -lhbdebug -lhbvm -lhbrtl -l%_HB_GT_LIB% -lhblang -lhbrdd -lhbrtl -lhbvm -lhbmacro -lhbpp -lrddfpt -lrddntx -lrddcdx -lhbsix -lhbcommon
   if "%HB_COMPILER%" == "icc"     icc /Gs+ /W2 /Se /Sd+ /Ti+ /C- /Tp %CFLAGS% -I%HB_INC_INSTALL% %1.c %HB_LIB_INSTALL%\hbcpage.lib %HB_LIB_INSTALL%\hbdebug.lib %HB_LIB_INSTALL%\hbvm.lib %HB_LIB_INSTALL%\hbrtl.lib %HB_LIB_INSTALL%\%_HB_GT_LIB%.lib %HB_LIB_INSTALL%\hblang.lib %HB_LIB_INSTALL%\hbrdd.lib %HB_LIB_INSTALL%\hbrtl.lib %HB_LIB_INSTALL%\hbvm.lib %HB_LIB_INSTALL%\hbmacro.lib %HB_LIB_INSTALL%\hbpp.lib %HB_LIB_INSTALL%\rddfpt.lib %HB_LIB_INSTALL%\rddntx.lib %HB_LIB_INSTALL%\rddcdx.lib %HB_LIB_INSTALL%\hbsix.lib %HB_LIB_INSTALL%\hbcommon.lib
   goto END

:CLEANUP

   del %1.c

:END
