rem @echo off
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

rem if "%HB_ARCHITECTURE%" == "" set HB_ARCHITECTURE=win
rem if "%HB_COMPILER%" == "" set HB_COMPILER=mingw

if "%OS%" == "Windows_NT" goto _S_WINNT

   if "%HB_INSTALL_PREFIX%" == "" set HB_INSTALL_PREFIX=..
   if "%HB_BIN_INSTALL%" == "" set HB_BIN_INSTALL=%HB_INSTALL_PREFIX%\bin
   if "%HB_LIB_INSTALL%" == "" set HB_LIB_INSTALL=%HB_INSTALL_PREFIX%\lib
   if "%HB_INC_INSTALL%" == "" set HB_INC_INSTALL=%HB_INSTALL_PREFIX%\include
   goto _S_END

:_S_WINNT

   if "%HB_INSTALL_PREFIX%" == "" set HB_INSTALL_PREFIX=%~dp0..
   if "%HB_BIN_INSTALL%" == "" set HB_BIN_INSTALL=%HB_INSTALL_PREFIX%\bin
   if "%HB_LIB_INSTALL%" == "" set HB_LIB_INSTALL=%HB_INSTALL_PREFIX%\lib
   if "%HB_INC_INSTALL%" == "" set HB_INC_INSTALL=%HB_INSTALL_PREFIX%\include
   goto _S_END

:_S_END

set _HB_USR_C=
set _HB_USR_L=

set _HB_MT=%HB_MT%
set _HB_GUI=%HB_GUI%
set _HB_SHARED=%HB_SHARED%
set _HB_DEBUG=%HB_DEBUG%

:REPEAT

if not "%1" == "-mt" goto NO_MT
   set _HB_MT=yes
   shift
   goto REPEAT
:NO_MT

if not "%1" == "-gui" goto NO_GUI
   set _HB_GUI=yes
   shift
   goto REPEAT
:NO_GUI

if not "%1" == "-shared" goto NO_SHARED
   set _HB_SHARED=yes
   shift
   goto REPEAT
:NO_SHARED

if not "%1" == "-debug" goto NO_DEBUG
   set _HB_DEBUG=yes
   shift
   goto REPEAT
:NO_DEBUG

if not "%_HB_MT%" == "yes" set _HB_MT=
if     "%_HB_MT%" == "yes" set _HB_MT=mt

:START

   if "%HB_ARCHITECTURE%" == "" goto NO_ARCH
   if "%HB_COMPILER%" == "" goto NO_COMP

   if not "%1" == "" goto COMPILE

:HELP

   echo.
   echo Usage: hbmk [-mt] [-gui] [-shared] [-debug] filename
   echo.
   echo Notes:
   echo.
   echo   - 'filename' is the .prg filename (without extension on pre-NT systems).
   echo   - Don't forget to create a MAIN() function in your application.
   echo   - Environment variables HB_ARCHITECTURE, HB_COMPILER must be set.
   echo     The following values are currently supported:
   echo.
   echo     HB_ARCHITECTURE:
   echo       - dos
   echo       - win
   echo.
   echo     HB_COMPILER:
   echo       - When HB_ARCHITECTURE=dos
   echo         - djgpp   (Delorie GNU C, DOS 32-bit)
   echo         - owatcom (OpenWatcom, DOS 32-bit)
   echo         - rxs32   (EMX/RSXNT/DOS GNU C, DOS 32-bit)
   echo       - When HB_ARCHITECTURE=win
   echo         - msvc    (Microsoft Visual C++, Windows 32/64-bit)
   echo         - mingw   (MinGW GNU C, Windows 32-bit)
   echo         - gcc     (Cygnus/Cygwin GNU C, Windows 32-bit)
   echo         - bcc32   (Borland C++ 4.x, 5.x, 6.x, Windows 32-bit)
   echo         - owatcom (OpenWatcom, Windows 32-bit)
   echo         - rxsnt   (EMX/RSXNT/Windows GNU C, Windows 32-bit)
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

   if "%OS%" == "Windows_NT" goto _P_WINNT

      set _HB_P_PRG_MAIN=%1
      set _HB_P_PRG=%1.prg
      set _HB_P_PRG_C=%1.c
      set _HB_P_C=
      set _HB_P_O=%1.o
      set _HB_P_OBJ=%1.obj
      set _HB_P_OPT=%2 %3
      goto _P_END

   :_P_WINNT

      set _HB_P_PRG_MAIN=
      set _HB_P_PRG=
      set _HB_P_PRG_C=
      set _HB_P_C=
      set _HB_P_O=
      set _HB_P_OBJ=
      set _HB_P_OPT=

      :_P_SORT_R
      if not "%~x1" == ".prg" goto _P_SORT_NP
         if "%_HB_P_PRG_MAIN%" == "" set _HB_P_PRG_MAIN=%~dpn1
         set _HB_P_PRG=%_HB_P_PRG% %1
         set _HB_P_PRG_C=%_HB_P_PRG_C% %~dpn1.c
         set _HB_P_O=%_HB_P_O% %~dpn1.o
         set _HB_P_OBJ=%_HB_P_OBJ% %~dpn1.obj
         shift
         goto _P_SORT_R
      :_P_SORT_NP

      if not "%~x1" == ".c" goto _P_SORT_NC
         set _HB_P_C=%_HB_P_C% %1
         set _HB_P_O=%_HB_P_O% %~dpn1.o
         set _HB_P_OBJ=%_HB_P_OBJ% %~dpn1.obj
         shift
         goto _P_SORT_R
      :_P_SORT_NC

      if "%1" == "" goto _P_SORT_E
         set _HB_P_OPT=%_HB_P_OPT%
         shift
         goto _P_SORT_R
      :_P_SORT_E
      goto _P_END

   :_P_END

   %HB_BIN_INSTALL%\harbour %_HB_P_PRG% -n -q0 -i%HB_INC_INSTALL% %HB_USER_PRGFLAGS% %_HB_P_OPT%

:A_DOS

   if not "%HB_ARCHITECTURE%" == "dos" goto A_WIN

   if not "%HB_COMPILER%" == "djgpp" goto A_DOS_DJGPP_NOT

      echo %_HB_P_PRG_C% %_HB_P_C% > _hb_mk.tmp
      echo -o%_HB_P_PRG_MAIN%.exe -O3 %HB_USER_CFLAGS% -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% >> _hb_mk.tmp
      echo -lhbcpage >> _hb_mk.tmp
      echo -lhbdebug >> _hb_mk.tmp
      echo -lhbvm >> _hb_mk.tmp
      echo -lhbrtl >> _hb_mk.tmp
      echo -lgtdos >> _hb_mk.tmp
      echo -lgtcgi >> _hb_mk.tmp
      echo -lgtstd >> _hb_mk.tmp
      echo -lgtpca >> _hb_mk.tmp
      echo -lhblang >> _hb_mk.tmp
      echo -lhbrdd >> _hb_mk.tmp
      echo -lhbrtl >> _hb_mk.tmp
      echo -lhbvm%_HB_MT% >> _hb_mk.tmp
      echo -lhbmacro >> _hb_mk.tmp
      echo -lhbpp >> _hb_mk.tmp
      echo -lrddfpt >> _hb_mk.tmp
      echo -lrddntx >> _hb_mk.tmp
      echo -lrddnsx >> _hb_mk.tmp
      echo -lrddcdx >> _hb_mk.tmp
      echo -lhbhsx >> _hb_mk.tmp
      echo -lhbsix >> _hb_mk.tmp
      echo -lhbcommon >> _hb_mk.tmp
      echo -lm >> _hb_mk.tmp
      gcc @_hb_mk.tmp
      del _hb_mk.tmp
      goto CLEANUP

:A_DOS_DJGPP_NOT

   if not "%HB_COMPILER%" == "rsx32" goto A_DOS_RSX32_NOT

      gcc %_HB_P_PRG_C% -O3 -Zrsx32 %HB_USER_CFLAGS% -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% -lhbcpage -lhbdebug -lhbvm -lhbrtl -lgtdos -lgtcgi -lgtstd -lgtpca -lhblang -lhbrdd -lhbrtl -lhbvm -lhbmacro -lhbpp -lrddfpt -lrddntx -lrddnsx -lrddcdx -lhbhsx -lhbsix -lhbcommon -lhbpcre -lhbzlib
      goto CLEANUP

:A_DOS_RSX32_NOT

   if not "%HB_COMPILER%" == "owatcom" goto END

      wpp386 -j -w3 -5s -5r -fp5 -oxehtz -zq -zt0 -bt=DOS %HB_USER_CFLAGS% %_HB_P_PRG_C% %_HB_P_C%
      echo OP osn=DOS OP stack=65536 OP CASEEXACT OP stub=cwstub.exe %HB_USER_LDFLAGS% NAME %_HB_P_PRG_MAIN%.exe > _hb_mk.tmp
      echo LIB hbcpage.lib >> _hb_mk.tmp
      echo LIB hbdebug.lib >> _hb_mk.tmp
      echo LIB hbvm.lib >> _hb_mk.tmp
      echo LIB hbrtl.lib >> _hb_mk.tmp
      echo LIB gtdos.lib >> _hb_mk.tmp
      echo LIB gtcgi.lib >> _hb_mk.tmp
      echo LIB gtstd.lib >> _hb_mk.tmp
      echo LIB gtpca.lib >> _hb_mk.tmp
      echo LIB hblang.lib >> _hb_mk.tmp
      echo LIB hbrdd.lib >> _hb_mk.tmp
      echo LIB hbmacro.lib >> _hb_mk.tmp
      echo LIB hbpp.lib >> _hb_mk.tmp
      echo LIB rddntx.lib >> _hb_mk.tmp
      echo LIB rddnsx.lib >> _hb_mk.tmp
      echo LIB rddcdx.lib >> _hb_mk.tmp
      echo LIB rddfpt.lib >> _hb_mk.tmp
      echo LIB hbhsx.lib >> _hb_mk.tmp
      echo LIB hbsix.lib >> _hb_mk.tmp
      echo LIB hbcommon.lib >> _hb_mk.tmp
      echo LIB hbpcre.lib >> _hb_mk.tmp
      echo LIB hbzlib.lib >> _hb_mk.tmp
      wlink @_hb_mk.tmp
      del _hb_mk.tmp
      goto CLEANUP

:A_WIN

   rem ; Compatibility. Please use 'win'.
   if "%HB_ARCHITECTURE%" == "w32" set HB_ARCHITECTURE=win

   if not "%HB_ARCHITECTURE%" == "win" goto END

   if not "%HB_COMPILER%" == "bcc32" goto A_WIN_BCC_NOT

      if "%_HB_DEBUG%" == "yes" set _HB_USR_C=-y -v

      if "%_HB_GUI%" == "yes" set _HB_USR_C=%_HB_USR_C% -tW

      if not "%_HB_SHARED%" == "yes" bcc32 -q -tWM -O2 -OS -Ov -Oi -Oc -d %HB_USER_CFLAGS% %_HB_USR_C% -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% %_HB_P_PRG_C% %_HB_P_C% %HB_USER_LIBS% hbcpage.lib hbdebug.lib hbvm%_HB_MT%.lib hbrtl.lib gtcgi.lib gtgui.lib gtpca.lib gtstd.lib gtwin.lib gtwvt.lib hblang.lib hbrdd.lib hbmacro.lib hbpp.lib rddfpt.lib rddntx.lib rddnsx.lib rddcdx.lib hbhsx.lib hbsix.lib hbcommon.lib hbpcre.lib hbzlib.lib
      if     "%_HB_SHARED%" == "yes" bcc32 -q -tWM -O2 -OS -Ov -Oi -Oc -d %HB_USER_CFLAGS% %_HB_USR_C% -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% %_HB_P_PRG_C% %_HB_P_C% %HB_USER_LIBS% harbour%_HB_MT%-11-b32.lib hbmainstd.lib hbmainwin.lib hbcommon.lib

      goto CLEANUP

:A_WIN_BCC_NOT

   if not "%HB_COMPILER%" == "msvc" goto A_WIN_MSVC_NOT

      if "%_HB_DEBUG%" == "yes" set _HB_USR_C=-MTd -Zi

      if not "%_HB_GUI%" == "yes" set _HB_USR_L=/subsystem:console
      if     "%_HB_GUI%" == "yes" set _HB_USR_L=/subsystem:windows

      if not "%_HB_SHARED%" == "yes" cl -nologo -W3 %HB_USER_CFLAGS% %_HB_USR_C% -I%HB_INC_INSTALL% %_HB_P_PRG_C% %_HB_P_C% /link /libpath:%HB_LIB_INSTALL% %HB_USER_LDFLAGS% %_HB_USR_L% %HB_USER_LIBS% hbcpage.lib hbdebug.lib hbvm%_HB_MT%.lib hbrtl.lib gtcgi.lib gtgui.lib gtpca.lib gtstd.lib gtwin.lib gtwvt.lib hblang.lib hbrdd.lib hbmacro.lib hbpp.lib rddntx.lib rddnsx.lib rddcdx.lib rddfpt.lib hbhsx.lib hbsix.lib hbcommon.lib hbpcre.lib hbzlib.lib user32.lib wsock32.lib advapi32.lib gdi32.lib
      if     "%_HB_SHARED%" == "yes" cl -nologo -W3 %HB_USER_CFLAGS% %_HB_USR_C% -I%HB_INC_INSTALL% %_HB_P_PRG_C% %_HB_P_C% /link /libpath:%HB_LIB_INSTALL% %HB_USER_LDFLAGS% %_HB_USR_L% %HB_USER_LIBS% harbour%_HB_MT%-11-vc.lib hbmainstd.lib hbmainwin.lib hbcommon.lib user32.lib wsock32.lib advapi32.lib gdi32.lib
      goto CLEANUP

:A_WIN_MSVC_NOT

   if "%HB_COMPILER%" == "gcc" set HB_COMPILER=mingw
   if not "%HB_COMPILER%" == "mingw" goto A_WIN_MINGW_NOT

      gcc %_HB_P_PRG_C% %_HB_P_C% -O3 -o%_HB_P_PRG_MAIN%.exe %HB_USER_CFLAGS% -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% -lhbcpage -lhbdebug -lhbvm%_HB_MT% -lhbrtl -lgtcgi -lgtgui -lgtpca -lgtstd -lgtwin -lgtwvt -lhblang -lhbrdd -lhbrtl -lhbvm%_HB_MT% -lhbmacro -lhbpp -lrddfpt -lrddntx -lrddnsx -lrddcdx -lhbhsx -lhbsix -lhbcommon -lhbpcre -lhbzlib
      set _HB_P_OBJ=
      if not "%_HB_P_O%" == "" del %_HB_P_O%
      goto CLEANUP

:A_WIN_MINGW_NOT

   if not "%HB_COMPILER%" == "rsxnt" goto A_WIN_RSXNT_NOT

      gcc %_HB_P_PRG_C% %_HB_P_C% -O3 -Zwin32 %HB_USER_CFLAGS% -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% -lhbcpage -lhbdebug -lhbvm%_HB_MT% -lhbrtl -lgtcgi -lgtgui -lgtpca -lgtstd -lgtwin -lgtwvt -lhblang -lhbrdd -lhbrtl -lhbvm%_HB_MT% -lhbmacro -lhbpp -lrddfpt -lrddntx -lrddnsx -lrddcdx -lhbhsx -lhbsix -lhbcommon -lhbpcre -lhbzlib
      set _HB_P_OBJ=
      if not "%_HB_P_O%" == "" del %_HB_P_O%
      goto CLEANUP

:A_WIN_RSXNT_NOT

   if not "%HB_COMPILER%" == "owatcom" goto END

      wpp386 -j -w3 -5s -5r -fp5 -oxehtz -zq -zt0 -mf -bt=NT %HB_USER_CFLAGS% %_HB_P_PRG_C% %_HB_P_C%
      echo OP osn=NT OP stack=65536 OP CASEEXACT %HB_USER_LDFLAGS% NAME %_HB_P_PRG_MAIN%.exe > _hb_mk.tmp
      echo LIB hbcpage.lib >> _hb_mk.tmp
      echo LIB hbdebug.lib >> _hb_mk.tmp
      echo LIB hbvm%_HB_MT%.lib >> _hb_mk.tmp
      echo LIB hbrtl.lib >> _hb_mk.tmp
      echo LIB gtcgi.lib >> _hb_mk.tmp
      echo LIB gtgui.lib >> _hb_mk.tmp
      echo LIB gtpca.lib >> _hb_mk.tmp
      echo LIB gtstd.lib >> _hb_mk.tmp
      echo LIB gtwin.lib >> _hb_mk.tmp
      echo LIB gtwvt.lib >> _hb_mk.tmp
      echo LIB hblang.lib >> _hb_mk.tmp
      echo LIB hbmacro.lib >> _hb_mk.tmp
      echo LIB hbpp.lib >> _hb_mk.tmp
      echo LIB rddntx.lib >> _hb_mk.tmp
      echo LIB rddnsx.lib >> _hb_mk.tmp
      echo LIB rddcdx.lib >> _hb_mk.tmp
      echo LIB rddfpt.lib >> _hb_mk.tmp
      echo LIB hbhsx.lib >> _hb_mk.tmp
      echo LIB hbsix.lib >> _hb_mk.tmp
      echo LIB hbrdd.lib >> _hb_mk.tmp
      echo LIB hbcommon.lib >> _hb_mk.tmp
      echo LIB hbpcre.lib >> _hb_mk.tmp
      echo LIB hbzlib.lib >> _hb_mk.tmp
      echo LIB kernel32.lib >> _hb_mk.tmp
      echo LIB user32.lib >> _hb_mk.tmp
      echo LIB wsock32.lib >> _hb_mk.tmp
      wlink @_hb_mk.tmp
      del _hb_mk.tmp
      goto CLEANUP

:CLEANUP

   if not "%_HB_P_PRG_C%" == "" del %_HB_P_PRG_C%
   if not "%_HB_P_OBJ%" == "" del %_HB_P_OBJ%
   rem Borland stuff
   if not "%_HB_DEBUG%" == "yes" if exist %_HB_P_PRG_MAIN%.tds del %_HB_P_PRG_MAIN%.tds

:END

set _HB_MT=
set _HB_GUI=
set _HB_SHARED=
set _HB_DEBUG=
set _HBVM_LIB=
set _HB_USR_C=
set _HB_USR_L=
