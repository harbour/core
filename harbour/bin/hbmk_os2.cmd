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
rem Template to build a final Harbour executable, using Harbour
rem with the C code generation feature, then calling the proper C
rem linker/compiler.
rem
rem Copyright 1999-2008 Viktor Szakats (viktor.szakats@syenar.hu)
rem See doc/license.txt for licensing terms.
rem ---------------------------------------------------------------

if "%HB_COMPILER%" == "" set HB_COMPILER=gcc

if "%HB_INSTALL_PREFIX%" == "" set HB_INSTALL_PREFIX=..
if "%HB_BIN_INSTALL%" == "" set HB_BIN_INSTALL=%HB_INSTALL_PREFIX%\bin
if "%HB_LIB_INSTALL%" == "" set HB_LIB_INSTALL=%HB_INSTALL_PREFIX%\lib
if "%HB_INC_INSTALL%" == "" set HB_INC_INSTALL=%HB_INSTALL_PREFIX%\include

set _HB_MT=%HB_MT%

if not "%1" == "-mt" goto NO_MT
   set _HB_MT=yes
   shift
:NO_MT

set _HBVM_LIB=hbvm
if "%_HB_MT%" == "yes" set _HBVM_LIB=hbvmmt
if "%_HB_MT%" == "MT"  set _HBVM_LIB=hbvmmt

:START

   if "%HB_COMPILER%" == "" goto NO_COMP

   if not "%1" == "" goto COMPILE

:HELP

   echo.
   echo Usage: hbmk_os2 [-mt] filename
   echo.
   echo Notes:
   echo.
   echo   - 'filename' is the .prg filename _without_ extension.
   echo   - Don't forget to create a MAIN() function in your application.
   echo   - Environment variable HB_COMPILER must be set.
   echo     The following values are currently supported:
   echo.
   echo     HB_COMPILER:
   echo       - gcc     (EMX GNU C, OS/2 32-bit)
   echo       - icc     (IBM Visual Age C++ 3.0, OS/2 32-bit)
   echo       - owatcom (OpenWatcom, OS/2 32-bit)
   goto END

:NO_COMP

   echo Error: HB_COMPILER is not set.
   goto HELP

:BAD_COMP

   echo Error: HB_COMPILER value is unsupported.
   goto HELP

:COMPILE

   %HB_BIN_INSTALL%\harbour %1.prg -n -i%HB_INC_INSTALL% %2 %3 %HB_USER_PRGFLAGS%

:A_DOS

   if not "%HB_COMPILER%" == "gcc" goto A_OS2_GCC_NOT

      gcc %1.c -O3 %HB_USER_CFLAGS% -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% -lhbcpage -lhbdebug -l_HBVM_LIB -lhbrtl -lgtcgi -lgtstd -lgtos2 -lgtpca -lhblang -lhbrdd -lhbrtl -l_HBVM_LIB -lhbmacro -lhbpp -lrddfpt -lrddntx -lrddnsx -lrddcdx -lhbsix -lhbcommon -lhbpcre -lhbzlib
      goto CLEANUP

:A_OS2_GCC_NOT

   if not "%HB_COMPILER%" == "icc" goto A_OS2_ICC_NOT

      icc /Gs+ /W2 /Se /Sd+ /Ti+ /C- /Tp %HB_USER_CFLAGS% -I%HB_INC_INSTALL% %1.c %HB_LIB_INSTALL%\hbcpage.lib %HB_LIB_INSTALL%\hbdebug.lib %HB_LIB_INSTALL%\_HBVM_LIB.lib %HB_LIB_INSTALL%\hbrtl.lib %HB_LIB_INSTALL%\gtcgi.lib %HB_LIB_INSTALL%\gtstd.lib %HB_LIB_INSTALL%\gtos2.lib %HB_LIB_INSTALL%\gtpca.lib %HB_LIB_INSTALL%\hblang.lib %HB_LIB_INSTALL%\hbrdd.lib %HB_LIB_INSTALL%\hbrtl.lib %HB_LIB_INSTALL%\_HBVM_LIB.lib %HB_LIB_INSTALL%\hbmacro.lib %HB_LIB_INSTALL%\hbpp.lib %HB_LIB_INSTALL%\rddfpt.lib %HB_LIB_INSTALL%\rddntx.lib %HB_LIB_INSTALL%\rddnsx.lib %HB_LIB_INSTALL%\rddcdx.lib %HB_LIB_INSTALL%\hbsix.lib %HB_LIB_INSTALL%\hbcommon.lib %HB_LIB_INSTALL%\hbpcre.lib %HB_LIB_INSTALL%\hbzlib.lib
      goto CLEANUP

:A_OS2_ICC_NOT

   if not "%HB_COMPILER%" == "owatcom" goto END

      wpp386 -j -w3 -5s -5r -fp5 -oxehtz -zq -zt0 -mf -bt=OS2 %HB_USER_CFLAGS% %_HB_PRG_NAME%.c -fo=%_HB_PRG_NAME%.obj
      echo OP stack=65536 OP CASEEXACT %HB_USER_LDFLAGS% NAME %_HB_PRG_NAME%.exe > _hb_mk.tmp
      echo FILE %_HB_PRG_NAME%.obj >> _hb_mk.tmp
      echo LIB hbcpage.lib >> _hb_mk.tmp
      echo LIB hbdebug.lib >> _hb_mk.tmp
      echo LIB %_HBVM_LIB%.lib >> _hb_mk.tmp
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

   if exist %1.c del %1.c
   if exist %1.o del %1.o

:END

set _HB_MT=
set _HBVM_LIB=
