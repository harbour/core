@echo off
rem
rem $Id$
rem

rem Flavour: DOS/Windows batch file

rem ---------------------------------------------------------------
rem Template to build a final Harbour executable, using Harbour
rem with the C code generation feature, then calling the proper C
rem linker/compiler.
rem
rem Copyright 1999 Victor Szel <info@szelvesz.hu>
rem See doc/license.txt for licensing terms.
rem ---------------------------------------------------------------

rem set HB_ARCHITECTURE=
rem set HB_COMPILER=

:START

   if "%HB_ARCHITECTURE%" == "" goto BAD_ARCH
   if "%HB_COMPILER%" == "" goto BAD_COMP

   if not "%1" == "" goto COMPILE

:HELP

   echo.
   echo Usage: bld_tpl.bat filename
   echo.
   echo Notes:
   echo.
   echo   - 'filename' is the .prg filename *without* extension.
   echo   - Don't forget to make a MAIN() function for you application.
   echo   - This batch file assumes you are in some directory off the main
   echo     harbour directory.
   echo   - Environment variable HB_ARCHITECTURE and HB_COMPILER should be set.
   echo     The following values are currently supported:
   echo.
   echo   HB_ARCHITECTURE:
   echo    - dos
   echo    - w32
   echo    - linux
   echo    - os2
   echo.
   pause
   echo   HB_COMPILER:
   echo    - When HB_ARCHITECTURE=dos
   echo      - bcc31   (Borland C/C++ 3.1, 16-bit DOS)
   echo      - djgpp   (GCC (DJGPP), 32-bit Windows)
   echo      - watcom
   echo    - When HB_ARCHITECTURE=w32
   echo      - bcc32
   echo      - gcc     (GCC (Cygnus), 32-bit Windows)
   echo      - mingw32 (GCC (Cygnus/MingW32), Windows 32 bit (console mode))
   echo      - icc
   echo      - msvc    (MSVC v 12.00.8168, Windows 32 bit (console mode))
   echo    - When HB_ARCHITECTURE=linux
   echo      - gcc
   echo    - When HB_ARCHITECTURE=os2
   echo      - gcc     (GCC (EMX), 32-bit OS/2)
   echo      - icc     (ICC (IBM Visual Age C++ 3.0), 32-bit OS/2)
   goto END

:BAD_ARCH

   echo Error: HB_ARCHITECTURE is not set.
   goto HELP

:BAD_COMP

   echo Error: HB_COMPILER is not set.
   goto HELP

:COMPILE

   harbour %1.prg /n /i..\include

   if not "%HB_ARCHITECTURE%" == "dos" goto A_W32

   if "%HB_COMPILER%" == "bcc31"   bcc -O2 -mh -I..\include %1.c ..\lib\rtl.lib ..\lib\rdd.lib ..\lib\vm.lib ..\lib\rdd.lib ..\lib\rtl.lib ..\lib\macro.lib ..\lib\dbfntx.lib ..\lib\dbfcdx.lib ..\lib\common.lib
   if "%HB_COMPILER%" == "djgpp"   gcc %1.c -o%1.exe -I..\include -L..\lib -lrtl -lrdd -lvm -lrdd -lrtl -lmacro -ldbfnt -ldbfcd -lcommon
   goto END

:A_W32

   if not "%HB_ARCHITECTURE%" == "w32" goto A_OS2

   if "%HB_COMPILER%" == "gcc"     gcc %1.c -o%1.exe -I..\include -L..\lib -lrtl -lrdd -lvm -lrdd -lrtl -lmacro -ldbfntx -ldbfcdx -lcommon
   if "%HB_COMPILER%" == "mingw32" gcc %1.c -o%1.exe -mno-cygwin -I..\include -L..\lib -lrtl -lrdd -lvm -lrdd -lrtl -lmacro -ldbfntx -ldbfcdx -lcommon
   if "%HB_COMPILER%" == "msvc"    cl -Fd..\bin\harbour -w -Zi -TP -GZ -GA -DHARBOUR_USE_WIN_GTAPI -I..\include %1.c /link /subsystem:CONSOLE ..\lib\harbour.lib ..\lib\terminal.lib ..\lib\macro.lib ..\lib\hbtools.lib ..\lib\dbfntx.lib ..\lib\dbfcdx.lib ..\lib\debug.lib
   if "%HB_COMPILER%" == "msvc"    echo Ignore LNK4033 warning
   goto END

:A_OS2

   if not "%HB_ARCHITECTURE%" == "os2" goto A_LINUX

   if "%HB_COMPILER%" == "gcc"     gcc %1.c -I..\include -L..\lib -lrtl -lrdd -lvm -lrdd -lrtl -lmacro -ldbfntx -ldbfcdx -lcommon
   if "%HB_COMPILER%" == "icc"     icc /Gs+ /W2 /Se /Sd+ /Ti+ -I..\include /C- /Tp %1.c ..\lib\main.obj ..\lib\rtl.lib ..\lib\rdd.lib ..\lib\vm.lib ..\lib\rdd.lib ..\lib\rtl.lib ..\lib\macro.lib ..\lib\dbfntx.lib ..\lib\dbfcdx.lib ..\lib\common.lib
   goto END

:A_LINUX

   if not "%HB_ARCHITECTURE%" == "linux" goto CLEANUP

   if "%HB_COMPILER%" == "gcc"     gcc %1.c -I../include -L../lib -lrtl -lrdd -lvm -lrdd -lrtl -lmacro -ldbfntx -ldbfcdx -lcommon
   goto END

:CLEANUP

   del %1.c

:END
