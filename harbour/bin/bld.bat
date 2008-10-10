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
rem Copyright 1999-2008 Viktor Szakats (viktor.szakats@syenar.hu)
rem See doc/license.txt for licensing terms.
rem ---------------------------------------------------------------

rem if "%HB_ARCHITECTURE%" == "" set HB_ARCHITECTURE=w32
rem if "%HB_COMPILER%" == "" set HB_COMPILER=mingw

if "%HB_INSTALL_PREFIX%" == "" set HB_INSTALL_PREFIX=..
if "%HB_BIN_INSTALL%" == "" set HB_BIN_INSTALL=%HB_INSTALL_PREFIX%\bin
if "%HB_LIB_INSTALL%" == "" set HB_LIB_INSTALL=%HB_INSTALL_PREFIX%\lib
if "%HB_INC_INSTALL%" == "" set HB_INC_INSTALL=%HB_INSTALL_PREFIX%\include

if     "%HB_MT%" == "MT" set _HBVM_LIB=hbvmmt
if not "%HB_MT%" == "MT" set _HBVM_LIB=hbvm

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
   echo   - 'filename' is the .prg filename _without_ extension.
   echo   - Don't forget to make a MAIN() function for you application.
   echo   - This batch file assumes you are in some directory off the main
   echo     harbour directory.
   echo   - Environment variables HB_ARCHITECTURE, HB_COMPILER should be set.
   echo.
   echo     HB_ARCHITECTURE:
   echo       - dos
   echo       - w32
   echo.
   echo     HB_COMPILER:
   echo       - When HB_ARCHITECTURE=dos
   echo         - djgpp   (Delorie GNU C, DOS 32-bit)
   echo         - rxs32   (EMX/RSXNT/DOS GNU C, DOS 32-bit)
   echo         - watcom  (OpenWatcom, DOS 32-bit)
   echo       - When HB_ARCHITECTURE=w32
   echo         - bcc32   (Borland C++ 4.x, 5.x, Windows 32-bit)
   echo         - mingw   (MinGW GNU C, Windows 32-bit)
   echo         - gcc     (Cygnus/Cygwin GNU C, Windows 32-bit)
   echo         - rxsnt   (EMX/RSXNT/Win32 GNU C, Windows 32-bit)
   echo         - icc     (IBM Visual Age C++, Windows 32-bit)
   echo         - msvc    (Microsoft Visual C++, Windows 32/64-bit)
   echo         - watcom  (OpenWatcom, Windows 32-bit)
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

   %HB_BIN_INSTALL%\harbour %1.prg -n -q0 -i%HB_INC_INSTALL% %2 %3 %PRG_USR%

:A_DOS

   if not "%HB_ARCHITECTURE%" == "dos" goto A_W32

   if not "%HB_COMPILER%" == "djgpp" goto A_DOS_DJGPP_NOT

      echo %1.c > build.tmp
      echo -o%1.exe -O3 %C_USR% -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% >> build.tmp
      echo -lhbcpage >> build.tmp
      echo -lhbdebug >> build.tmp
      echo -lhbvm >> build.tmp
      echo -lhbrtl >> build.tmp
      echo -lgtdos >> build.tmp
      echo -lgtcgi >> build.tmp
      echo -lgtstd >> build.tmp
      echo -lgtpca >> build.tmp
      echo -lhblang >> build.tmp
      echo -lhbrdd >> build.tmp
      echo -lhbrtl >> build.tmp
      echo -l%_HBVM_LIB% >> build.tmp
      echo -lhbmacro >> build.tmp
      echo -lhbpp >> build.tmp
      echo -lrddfpt >> build.tmp
      echo -lrddntx >> build.tmp
      echo -lrddcdx >> build.tmp
      echo -lhbhsx >> build.tmp
      echo -lhbsix >> build.tmp
      echo -lhbcommon >> build.tmp
      echo -lm >> build.tmp
      gcc @build.tmp
      del build.tmp
      goto END

:A_DOS_DJGPP_NOT

   if not "%HB_COMPILER%" == "rsx32" GOTO A_DOS_RSX32_NOT

      gcc %1.c -O3 -Zrsx32 %C_USR% -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% -lhbcpage -lhbdebug -lhbvm -lhbrtl -lgtdos -lgtcgi -lgtstd -lgtpca -lhblang -lhbrdd -lhbrtl -lhbvm -lhbmacro -lhbpp -lrddfpt -lrddntx -lrddcdx -lhbhsx -lhbsix -lhbcommon -lhbpcre -lhbzlib
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
      echo LIB gtdos.lib >> build.tmp
      echo LIB gtcgi.lib >> build.tmp
      echo LIB gtstd.lib >> build.tmp
      echo LIB gtpca.lib >> build.tmp
      echo LIB hblang.lib >> build.tmp
      echo LIB hbrdd.lib >> build.tmp
      echo LIB hbmacro.lib >> build.tmp
      echo LIB hbpp.lib >> build.tmp
      echo LIB rddntx.lib >> build.tmp
      echo LIB rddcdx.lib >> build.tmp
      echo LIB rddfpt.lib >> build.tmp
      echo LIB hbhsx.lib >> build.tmp
      echo LIB hbsix.lib >> build.tmp
      echo LIB hbcommon.lib >> build.tmp
      echo LIB hbpcre.lib >> build.tmp
      echo LIB hbzlib.lib >> build.tmp
      wlink @build.tmp
      del build.tmp
      goto END

:A_W32

if not "%HB_ARCHITECTURE%" == "w32" goto A_OS2

   if "%HB_COMPILER%" == "bcc32"   bcc32 -tWM -O2 -OS -Ov -Oi -Oc -d %C_USR% -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% %1.c  %HB_USER_LIBS% hbcpage.lib hbdebug.lib %_HBVM_LIB%.lib hbrtl.lib gtcgi.lib gtgui.lib gtpca.lib gtstd.lib gtwin.lib gtwvt.lib hblang.lib hbrdd.lib hbmacro.lib hbpp.lib rddfpt.lib rddntx.lib rddcdx.lib hbhsx.lib hbsix.lib hbcommon.lib hbpcre.lib hbzlib.lib
   if "%HB_COMPILER%" == "msvc"    cl -TP -W3 %C_USR% -I%HB_INC_INSTALL% %1.c /link /subsystem:CONSOLE /LIBPATH:%HB_LIB_INSTALL% %HB_USER_LIBS% hbcpage.lib hbdebug.lib %_HBVM_LIB%.lib hbrtl.lib  gtcgi.lib gtgui.lib gtpca.lib gtstd.lib gtwin.lib gtwvt.lib hblang.lib hbrdd.lib hbmacro.lib hbpp.lib rddntx.lib rddcdx.lib rddfpt.lib hbhsx.lib hbsix.lib hbcommon.lib hbpcre.lib hbzlib.lib user32.lib wsock32.lib advapi32.lib gdi32.lib
   if "%HB_COMPILER%" == "mingw"   gcc %1.c -O3 -o%1.exe %C_USR% -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% -lhbcpage -lhbdebug -l%_HBVM_LIB% -lhbrtl -lgtcgi -lgtgui -lgtpca -lgtstd -lgtwin -lgtwvt -lhblang -lhbrdd -lhbrtl -l%_HBVM_LIB% -lhbmacro -lhbpp -lrddfpt -lrddntx -lrddcdx -lhbhsx -lhbsix -lhbcommon -lhbpcre -lhbzlib
   if "%HB_COMPILER%" == "gcc"     gcc %1.c -O3 -o%1.exe %C_USR% -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% -lhbcpage -lhbdebug -l%_HBVM_LIB% -lhbrtl -lgtcgi -lgtgui -lgtpca -lgtstd -lgtwin -lgtwvt -lhblang -lhbrdd -lhbrtl -l%_HBVM_LIB% -lhbmacro -lhbpp -lrddfpt -lrddntx -lrddcdx -lhbhsx -lhbsix -lhbcommon -lhbpcre -lhbzlib
   if "%HB_COMPILER%" == "rsxnt"   gcc %1.c -O3 -Zwin32  %C_USR% -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% -lhbcpage -lhbdebug -l%_HBVM_LIB% -lhbrtl -lgtcgi -lgtgui -lgtpca -lgtstd -lgtwin -lgtwvt -lhblang -lhbrdd -lhbrtl -l%_HBVM_LIB% -lhbmacro -lhbpp -lrddfpt -lrddntx -lrddcdx -lhbhsx -lhbsix -lhbcommon -lhbpcre -lhbzlib

:C_WATCOM

   if not "%HB_COMPILER%" == "watcom"  goto end

   wpp386 -j -w3 -d2 -5s -5r -fp5 -oxehtz -zq -zt0 -mf -bt=NT %1.c -fo=%1.obj
   echo debug all OP osn=NT OP stack=65536 OP CASEEXACT NAME %1.exe > build.tmp
   echo FILE %1.obj >> build.tmp
   echo LIB hbcpage.lib >> build.tmp
   echo LIB hbdebug.lib >> build.tmp
   echo LIB %_HBVM_LIB%.lib >> build.tmp
   echo LIB hbrtl.lib >> build.tmp
   echo LIB gtcgi.lib >> build.tmp
   echo LIB gtgui.lib >> build.tmp
   echo LIB gtpca.lib >> build.tmp
   echo LIB gtstd.lib >> build.tmp
   echo LIB gtwin.lib >> build.tmp
   echo LIB gtwvt.lib >> build.tmp
   echo LIB hblang.lib >> build.tmp
   echo LIB hbmacro.lib >> build.tmp
   echo LIB hbpp.lib >> build.tmp
   echo LIB rddntx.lib >> build.tmp
   echo LIB rddcdx.lib >> build.tmp
   echo LIB rddfpt.lib >> build.tmp
   echo LIB hbhsx.lib >> build.tmp
   echo LIB hbsix.lib >> build.tmp
   echo LIB hbrdd.lib >> build.tmp
   echo LIB hbcommon.lib >> build.tmp
   echo LIB hbpcre.lib >> build.tmp
   echo LIB hbzlib.lib >> build.tmp
   echo LIB kernel32.lib >> build.tmp
   echo LIB user32.lib >> build.tmp
   echo LIB wsock32.lib >> build.tmp
   wlink @build.tmp
   del build.tmp
   goto END

:CLEANUP

   del %1.c
   if exist %1.o del %1.o
   if exist %1.obj del %1.obj
   rem Borland stuff
   if exist %1.tds del %1.tds

:END
