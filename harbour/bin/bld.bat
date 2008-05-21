@echo off
rem
rem $Id$
rem

rem ---------------------------------------------------------------
rem This is a generic template file, if it doesn't fit your own needs
rem please DON'T MODIFY IT.
rem
rem Instead, make a local copy and modify that one, or make a call to
rem this batch file from your customized one. [vszakats]
rem ---------------------------------------------------------------

rem ---------------------------------------------------------------
rem Template to build a final Harbour executable, using Harbour
rem with the C code generation feature, then calling the proper C
rem linker/compiler.
rem
rem Copyright 1999-2004 Viktor Szakats (viktor.szakats@syenar.hu)
rem See doc/license.txt for licensing terms.
rem ---------------------------------------------------------------

rem if "%HB_ARCHITECTURE%" == "" set HB_ARCHITECTURE=w32
rem if "%HB_COMPILER%" == "" set HB_COMPILER=mingw32
rem if "%HB_GT_LIB%" == "" set HB_GT_LIB=

if "%HB_BIN_INSTALL%" == "" set HB_BIN_INSTALL=..\bin
if "%HB_LIB_INSTALL%" == "" set HB_LIB_INSTALL=..\lib
if "%HB_INC_INSTALL%" == "" set HB_INC_INSTALL=..\include

:START

   if "%HB_ARCHITECTURE%" == "" goto NO_ARCH
   if "%HB_COMPILER%" == "" goto NO_COMP

   if not "%1" == "" goto COMPILE

:HELP

   echo.
   echo Usage: bld filename
   echo.
   echo Notes:
   echo.
   echo   - 'filename' is the .prg filename *without* extension.
   echo   - Don't forget to make a MAIN() function for you application.
   echo   - This batch file assumes you are in some directory off the main
   echo     harbour directory.
   echo   - Environment variables HB_ARCHITECTURE, HB_COMPILER, HB_GT_LIB
   echo     should be set. Setting HB_GT_LIB is optional.
   echo     The following values are currently supported:
   echo.
   echo     HB_ARCHITECTURE:
   echo       - dos   (HB_GT_LIB=gtdos by default)
   echo       - w32   (HB_GT_LIB=gtwin by default)
   echo       - linux (HB_GT_LIB=gtstd by default)
   echo       - os2   (HB_GT_LIB=gtos2 by default)
   echo.
   pause
   echo     HB_COMPILER:
   echo       - When HB_ARCHITECTURE=dos
   echo         - bcc16   (Borland C++ 3.x, 4.x, 5.0x, DOS 16-bit)
   echo         - djgpp   (Delorie GNU C, DOS 32-bit)
   echo         - rxs32   (EMX/RSXNT/DOS GNU C, DOS 32-bit)
   echo         - watcom  (OpenWatcom, DOS 32-bit)
   echo       - When HB_ARCHITECTURE=w32
   echo         - bcc32   (Borland C++ 4.x, 5.x, Windows 32-bit)
   echo         - gcc     (Cygnus/Cygwin GNU C, Windows 32-bit)
   echo         - mingw32 (Cygnus/MinGW GNU C, Windows 32-bit)
   echo         - rxsnt   (EMX/RSXNT/Win32 GNU C, Windows 32-bit)
   echo         - icc     (IBM Visual Age C++, Windows 32-bit)
   echo         - msvc    (Microsoft Visual C++, Windows 32-bit)
   echo         - watcom  (OpenWatcom, Windows 32-bit)
   echo       - When HB_ARCHITECTURE=linux
   echo         - gcc     (GNU C, 32-bit)
   echo       - When HB_ARCHITECTURE=os2
   echo         - gcc     (EMX GNU C, OS/2 32-bit)
   echo         - icc     (IBM Visual Age C++ 3.0, OS/2 32-bit)
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
   goto END

:NO_ARCH

   echo Error: HB_ARCHITECTURE is not set.
   goto HELP

:NO_COMP

   echo Error: HB_COMPILER is not set.
   goto HELP

:BAD_ARCH

   echo Error: HB_ARCHITECTURE value is unsupported.
   goto HELP

:BAD_COMP

   echo Error: HB_COMPILER value is unsupported.
   goto HELP

:COMPILE

   %HB_BIN_INSTALL%\harbour %1.prg -n -q0 -gc0 -i%HB_INC_INSTALL% %2 %3 %HARBOURFLAGS%

:A_DOS

   if not "%HB_GT_LIB%" == "" set _HB_GT_LIB=%HB_GT_LIB%

   if not "%HB_ARCHITECTURE%" == "dos" goto A_W32

   if "%HB_GT_LIB%" == "" set _HB_GT_LIB=gtdos

   if not "%HB_COMPILER%" == "bcc16" goto A_DOS_BCC16_NOT

      echo -O2 -d -mh %CFLAGS% -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% > build.tmp
      echo -e%1.exe >> build.tmp
      echo %1.c >> build.tmp
      echo hbcpage.lib >> build.tmp
      echo hbdebug.lib >> build.tmp
      echo hbvm.lib >> build.tmp
      echo hbrtl.lib >> build.tmp
      echo %_HB_GT_LIB%.lib >> build.tmp
      echo hblang.lib >> build.tmp
      echo hbrdd.lib >> build.tmp
      echo hbmacro.lib >> build.tmp
      echo hbpp.lib >> build.tmp
      echo rddfpt.lib >> build.tmp
      echo rddntx.lib >> build.tmp
      echo rddcdx.lib >> build.tmp
      echo hbsix.lib >> build.tmp
      echo hbcommon.lib >> build.tmp
      bcc @build.tmp
      del build.tmp
      goto END

:A_DOS_BCC16_NOT

   if not "%HB_COMPILER%" == "djgpp" goto A_DOS_DJGPP_NOT

      echo %1.c > build.tmp
      echo -o%1.exe %CFLAGS% -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% >> build.tmp
      echo -lhbcpage >> build.tmp
      echo -lhbdebug >> build.tmp
      echo -lhbvm >> build.tmp
      echo -lhbrtl >> build.tmp
      echo -l%_HB_GT_LIB% >> build.tmp
      echo -lhblang >> build.tmp
      echo -lhbrdd >> build.tmp
      echo -lhbrtl >> build.tmp
      echo -lhbvm >> build.tmp
      echo -lhbmacro >> build.tmp
      echo -lhbpp >> build.tmp
      echo -lrddfpt >> build.tmp
      echo -lrddntx >> build.tmp
      echo -lrddcdx >> build.tmp
      echo -lhbsix >> build.tmp
      echo -lhbcommon >> build.tmp
      echo -lm >> build.tmp
      gcc @build.tmp
      del build.tmp
      goto END

:A_DOS_DJGPP_NOT

   if not "%HB_COMPILER%" == "rsx32" GOTO A_DOS_RSX32_NOT

      gcc %1.c -Zrsx32 %CFLAGS% -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% -lhbcpage -lhbdebug -lhbvm -lhbrtl -l%_HB_GT_LIB% -lhblang -lhbrdd -lhbrtl -lhbvm -lhbmacro -lhbpp -lrddfpt -lrddntx -lrddcdx -lhbsix -lhbcommon -lhbpcre
      goto END

:A_DOS_RSX32_NOT

   if not "%HB_COMPILER%" == "watcom" goto END

      wpp386 -j -w3 -d2 -5s -5r -fp5 -oxehtz -zq -zt0 -bt=DOS %1.c -fo=%1.obj
      echo debug all OP osn=DOS OP stack=65536 OP CASEEXACT OP stub=cwstub.exe NAME %1.exe > build.tmp
      echo FILE %1.obj >> build.tmp
      echo LIB hbcpage.lib >> build.tmp
      echo LIB hbdebug.lib >> build.tmp
      echo LIB hbvm.lib >> build.tmp
      echo LIB hbrtl.lib >> build.tmp
      echo LIB %_HB_GT_LIB%.lib >> build.tmp
      echo LIB hblang.lib >> build.tmp
      echo LIB hbrdd.lib >> build.tmp
      echo LIB hbmacro.lib >> build.tmp
      echo LIB hbpp.lib >> build.tmp
      echo LIB rddntx.lib >> build.tmp
      echo LIB rddcdx.lib >> build.tmp
      echo LIB rddfpt.lib >> build.tmp
      echo LIB hbsix.lib  >> build.tmp
      echo LIB hbcommon.lib >> build.tmp
      echo LIB hbpcre.lib >> build.tmp
      wlink @build.tmp
      del build.tmp
      goto END

:A_W32

if not "%HB_ARCHITECTURE%" == "w32" goto A_OS2

   if "%HB_GT_LIB%" == "" set _HB_GT_LIB=gtwin

   if "%HB_COMPILER%" == "bcc32"   bcc32 -O2 -d %CFLAGS% -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% %1.c %HB_USER_LIBS% hbcpage.lib hbdebug.lib hbvm.lib hbrtl.lib %_HB_GT_LIB%.lib hblang.lib hbrdd.lib hbmacro.lib hbpp.lib rddfpt.lib rddntx.lib rddcdx.lib hbsix.lib hbcommon.lib hbpcre.lib

   if "%HB_COMPILER%" == "gcc"     gcc %1.c -o%1.exe %CFLAGS% -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% -lhbcpage -lhbdebug -lhbvm -lhbrtl -l%_HB_GT_LIB% -lhblang -lhbrdd -lhbrtl -lhbvm -lhbmacro -lhbpp -lrddfpt -lrddntx -lrddcdx -lhbsix -lhbcommon -lhbpcre
   if "%HB_COMPILER%" == "mingw32" gcc %1.c -o%1.exe %CFLAGS% -mno-cygwin -I%HB_INC_INSTALL% %HB_INC_TEMP% -L%HB_LIB_INSTALL% -lhbcpage -lhbdebug -lhbvm -lhbrtl -l%_HB_GT_LIB% -lhblang -lhbrdd -lhbrtl -lhbvm -lhbmacro -lhbpp -lrddfpt -lrddntx -lrddcdx -lhbsix -lhbcommon -lhbpcre
   if "%HB_COMPILER%" == "rsxnt"   gcc %1.c -Zwin32 %CFLAGS% -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% -lhbcpage -lhbdebug -lhbvm -lhbrtl -l%_HB_GT_LIB% -lhblang -lhbrdd -lhbrtl -lhbvm -lhbmacro -lhbpp -lrddfpt -lrddntx -lrddcdx -lhbsix -lhbcommon -lhbpcre

   if "%HB_COMPILER%" == "msvc"    cl -TP -W3 %CFLAGS% -I%HB_INC_INSTALL% %1.c /link /subsystem:CONSOLE %HB_USER_LIBS% %HB_LIB_INSTALL%\hbcpage.lib %HB_LIB_INSTALL%\hbdebug.lib %HB_LIB_INSTALL%\hbvm.lib %HB_LIB_INSTALL%\hbrtl.lib %HB_LIB_INSTALL%\%_HB_GT_LIB%.lib %HB_LIB_INSTALL%\hblang.lib %HB_LIB_INSTALL%\hbrdd.lib %HB_LIB_INSTALL%\hbmacro.lib %HB_LIB_INSTALL%\hbpp.lib %HB_LIB_INSTALL%\rddntx.lib %HB_LIB_INSTALL%\rddcdx.lib %HB_LIB_INSTALL%\rddfpt.lib %HB_LIB_INSTALL%\hbsix.lib %HB_LIB_INSTALL%\hbcommon.lib %HB_LIB_INSTALL%\hbpcre.lib user32.lib winspool.lib
   if "%HB_COMPILER%" == "msvc"    echo Ignore LNK4033 warning

:C_WATCOM

   if not "%HB_COMPILER%" == "watcom"  goto end

   wpp386 -j -w3 -d2 -5s -5r -fp5 -oxehtz -zq -zt0 -mf -bt=NT %1.c -fo=%1.obj
   echo debug all OP osn=NT OP stack=65536 OP CASEEXACT NAME %1.exe > build.tmp
   echo FILE %1.obj >> build.tmp
   echo LIB hbcpage.lib >> build.tmp
   echo LIB hbdebug.lib >> build.tmp
   echo LIB hbvm.lib >> build.tmp
   echo LIB hbrtl.lib >> build.tmp
   echo LIB %_HB_GT_LIB%.lib >> build.tmp
   echo LIB hblang.lib >> build.tmp
   echo LIB hbmacro.lib >> build.tmp
   echo LIB hbpp.lib >> build.tmp
   echo LIB rddntx.lib >> build.tmp
   echo LIB rddcdx.lib >> build.tmp
   echo LIB rddfpt.lib >> build.tmp
   echo LIB hbsix.lib >> build.tmp
   echo LIB hbrdd.lib >> build.tmp
   echo LIB hbcommon.lib >> build.tmp
   echo LIB hbpcre.lib >> build.tmp
   echo LIB kernel32.lib >> build.tmp
   echo LIB user32.lib >> build.tmp
   echo LIB winspool.lib >> build.tmp
   echo LIB oleaut32.lib >> build.tmp
   echo LIB uuid.lib >> build.tmp
   wlink @build.tmp
   del build.tmp
   goto END

:A_OS2

   if not "%HB_ARCHITECTURE%" == "os2" goto A_LINUX

   if "%HB_GT_LIB%" == "" set _HB_GT_LIB=gtos2

   if "%HB_COMPILER%" == "gcc"     gcc %1.c %CFLAGS% -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% -lhbcpage -lhbdebug -lhbvm -lhbrtl -l%_HB_GT_LIB% -lhblang -lhbrdd -lhbrtl -lhbvm -lhbmacro -lhbpp -lrddfpt -lrddntx -lrddcdx -lhbsix -lhbcommon -lhbpcre
   if "%HB_COMPILER%" == "icc"     icc /Gs+ /W2 /Se /Sd+ /Ti+ /C- /Tp %CFLAGS% -I%HB_INC_INSTALL% %1.c %HB_LIB_INSTALL%\hbcpage.lib %HB_LIB_INSTALL%\hbdebug.lib %HB_LIB_INSTALL%\hbvm.lib %HB_LIB_INSTALL%\hbrtl.lib %HB_LIB_INSTALL%\%_HB_GT_LIB%.lib %HB_LIB_INSTALL%\hblang.lib %HB_LIB_INSTALL%\hbrdd.lib %HB_LIB_INSTALL%\hbrtl.lib %HB_LIB_INSTALL%\hbvm.lib %HB_LIB_INSTALL%\hbmacro.lib %HB_LIB_INSTALL%\hbpp.lib %HB_LIB_INSTALL%\rddfpt.lib %HB_LIB_INSTALL%\rddntx.lib %HB_LIB_INSTALL%\rddcdx.lib %HB_LIB_INSTALL%\hbsix.lib %HB_LIB_INSTALL%\hbcommon.lib %HB_LIB_INSTALL%\hbpcre.lib
   goto END

:A_LINUX

   if not "%HB_ARCHITECTURE%" == "linux" goto BAD_ARCH

   if "%HB_GT_LIB%" == "" set _HB_GT_LIB=gtstd

   if "%HB_COMPILER%" == "gcc"     gcc %1.c %CFLAGS% -I%HB_INC_INSTALL% -L../lib -lhbcpage -lhbdebug -lhbvm -lhbrtl -l%_HB_GT_LIB% -lhblang -lhbrdd -lhbrtl -lhbvm -lhbmacro -lhbpp -lrddfpt -lrddntx -lrddcdx -lhbsix -lhbcommon -lhbpcre
   goto END

:CLEANUP

   del %1.c

:END
