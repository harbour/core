@echo off
rem
rem $Id$
rem

rem Flavour: OS/2 command file

rem ---------------------------------------------------------------
rem Template to build a final Harbour executable, using Harbour
rem with the C code generation feature, then calling the proper C
rem linker/compiler.
rem
rem Copyright 1999-2000 Victor Szakats (info@szelvesz.hu)
rem See doc/license.txt for licensing terms.
rem ---------------------------------------------------------------

rem if "%HB_ARCHITECTURE%" == "" set HB_ARCHITECTURE=os2
rem if "%HB_COMPILER%" == "" set HB_COMPILER=gcc
rem if "%HB_GT_LIB%" == "" set HB_GT_LIB=

:START

   if "%HB_ARCHITECTURE%" == "" goto NO_ARCH
   if "%HB_COMPILER%" == "" goto NO_COMP

   if not "%1" == "" goto COMPILE

:HELP

   echo.
   echo Usage: bld.cmd filename
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
   echo       - w32   (HB_GT_LIB=gtw32 by default)
   echo       - linux (HB_GT_LIB=gtstd by default)
   echo       - os2   (HB_GT_LIB=gtos2 by default)
   echo.   
   pause   
   echo     HB_COMPILER:
   echo       - When HB_ARCHITECTURE=dos
   echo         - bcc16   (Borland C++ 3.x, DOS 16-bit)
   echo         - djgpp   (Delorie GNU C, DOS 32-bit)
   echo         - rxs32   (EMX/RSXNT/DOS GNU C, DOS 32-bit)
   echo         - watcom  (Watcom C++ 9.x, 10.x, 11.x, DOS 32-bit)
   echo       - When HB_ARCHITECTURE=w32
   echo         - bcc32   (Borland C++ 4.x, 5.x, Windows 32-bit)
   echo         - gcc     (Cygnus/Cygwin GNU C, Windows 32-bit)
   echo         - mingw32 (Cygnus/Mingw32 GNU C, Windows 32-bit)
   echo         - rxsnt   (EMX/RSXNT/Win32 GNU C, Windows 32-bit)
   echo         - icc     (IBM Visual Age C++, Windows 32-bit)
   echo         - msvc    (Microsoft Visual C++, Windows 32-bit)
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

   harbour %1.prg -n -i..\include

:A_DOS

   if not "%HB_ARCHITECTURE%" == "dos" goto A_W32

   if "%HB_GT_LIB%" == "" set HB_GT_LIB=gtdos

   if "%HB_COMPILER%" == "bcc16"   bcc -O2 -mh -I..\include -L..\lib %1.c tools.lib debug.lib vm.lib rtl.lib %HB_GT_LIB%.lib rdd.lib macro.lib pp.lib dbfntx.lib dbfcdx.lib common.lib
   if "%HB_COMPILER%" == "djgpp"   gcc %1.c -o%1.exe -I..\include -L..\lib -ltools -ldebug -lvm -lrtl -l%HB_GT_LIB% -lrdd -lrtl -lvm -lmacro -lpp -ldbfnt -ldbfcd -lcommo
   if "%HB_COMPILER%" == "rsx32"   gcc %1.c -Zrsx32 -I..\include -L..\lib -ltools -ldebug -lvm -lrtl -l%HB_GT_LIB% -lrdd -lrtl -lvm -lmacro -lpp -ldbfntx -ldbfcdx -lcommon
   goto END

:A_W32

   if not "%HB_ARCHITECTURE%" == "w32" goto A_OS2

   if "%HB_GT_LIB%" == "" set HB_GT_LIB=gtwin

   if "%HB_COMPILER%" == "bcc32"   bcc32 -O2 -I..\include -L..\lib %1.c tools.lib debug.lib vm.lib rtl.lib %HB_GT_LIB%.lib rdd.lib macro.lib pp.lib dbfntx.lib dbfcdx.lib common.lib
   if "%HB_COMPILER%" == "gcc"     gcc %1.c -o%1.exe -I..\include -L..\lib -ltools -ldebug -lvm -lrtl -l%HB_GT_LIB% -lrdd -lrtl -lvm -lmacro -lpp -ldbfntx -ldbfcdx -lcommon
   if "%HB_COMPILER%" == "mingw32" gcc %1.c -o%1.exe -mno-cygwin -I..\include -L..\lib -ltools -ldebug -lvm -lrtl -l%HB_GT_LIB% -lrdd -lrtl -lvm -lmacro -lpp -ldbfntx -ldbfcdx -lcommon
   if "%HB_COMPILER%" == "rsxnt"   gcc %1.c -Zwin32 -I..\include -L..\lib -ltools -ldebug -lvm -lrtl -l%HB_GT_LIB% -lrdd -lrtl -lvm -lmacro -lpp -ldbfntx -ldbfcdx -lcommon
   if "%HB_COMPILER%" == "msvc"    cl -Fd..\bin\harbour -w -Zi -TP -GZ -GA -I..\include %1.c /link /subsystem:CONSOLE ..\lib\tools.lib ..\lib\debug.lib ..\lib\vm.lib ..\lib\rtl.lib ..\lib\%HB_GT_LIB%.lib ..\lib\rdd.lib ..\lib\macro.lib ..\lib\pp.lib ..\lib\dbfntx.lib ..\lib\dbfcdx.lib ..\lib\common.lib user32.lib
   if "%HB_COMPILER%" == "msvc"    echo Ignore LNK4033 warning
   goto END

:A_OS2

   if not "%HB_ARCHITECTURE%" == "os2" goto A_LINUX

   if "%HB_GT_LIB%" == "" set HB_GT_LIB=gtos2

   if "%HB_COMPILER%" == "gcc"     gcc %1.c -I..\include -L..\lib -ltools -ldebug -lvm -lrtl -l%HB_GT_LIB% -lrdd -lrtl -lvm -lmacro -lpp -ldbfntx -ldbfcdx -lcommon
   if "%HB_COMPILER%" == "icc"     icc /Gs+ /W2 /Se /Sd+ /Ti+ -I..\include /C- /Tp %1.c ..\lib\tools.lib ..\lib\debug.lib ..\lib\vm.lib ..\lib\rtl.lib ..\lib\%HB_GT_LIB%.lib ..\lib\rdd.lib ..\lib\rtl.lib ..\lib\vm.lib ..\lib\macro.lib ..\lib\pp.lib ..\lib\dbfntx.lib ..\lib\dbfcdx.lib ..\lib\common.lib
   goto END

:A_LINUX

   if not "%HB_ARCHITECTURE%" == "linux" goto BAD_ARCH

   if "%HB_GT_LIB%" == "" set HB_GT_LIB=gtstd

   if "%HB_COMPILER%" == "gcc"     gcc %1.c -I../include -L../lib -ltools -ldebug -lvm -lrtl -l%HB_GT_LIB% -lrdd -lrtl -lvm -lmacro -lpp -ldbfntx -ldbfcdx -lcommon
   goto END

:CLEANUP

   del %1.c

:END
