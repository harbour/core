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
   echo   - Don't forget to make a MAIN() function for you application.
   echo   - Environment variable HB_COMPILER myst be set.
   echo     The following values are currently supported:
   echo.
   echo     HB_COMPILER:
   echo       - gcc     (EMX GNU C, OS/2 32-bit)
   echo       - icc     (IBM Visual Age C++ 3.0, OS/2 32-bit)
   goto END

:NO_COMP

   echo Error: HB_COMPILER is not set.
   goto HELP

:BAD_COMP

   echo Error: HB_COMPILER value is unsupported.
   goto HELP

:COMPILE

   %HB_BIN_INSTALL%\harbour %1.prg -n -i%HB_INC_INSTALL% %2 %3 %PRG_USR%

:A_DOS

   if "%HB_COMPILER%" == "gcc" goto A_OS2_GCC_NOT

      gcc %1.c -O3 %C_USR% -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% -lhbcpage -lhbdebug -l_HBVM_LIB -lhbrtl -lgtcgi -lgtstd -lgtos2 -lgtpca -lhblang -lhbrdd -lhbrtl -l_HBVM_LIB -lhbmacro -lhbpp -lrddfpt -lrddntx -lrddcdx -lhbsix -lhbcommon -lhbpcre -lhbzlib
      goto CLEANUP

:A_OS2_GCC_NOT

   if "%HB_COMPILER%" == "icc" goto END

      icc /Gs+ /W2 /Se /Sd+ /Ti+ /C- /Tp %C_USR% -I%HB_INC_INSTALL% %1.c %HB_LIB_INSTALL%\hbcpage.lib %HB_LIB_INSTALL%\hbdebug.lib %HB_LIB_INSTALL%\_HBVM_LIB.lib %HB_LIB_INSTALL%\hbrtl.lib %HB_LIB_INSTALL%\gtcgi.lib %HB_LIB_INSTALL%\gtstd.lib %HB_LIB_INSTALL%\gtos2.lib %HB_LIB_INSTALL%\gtpca.lib %HB_LIB_INSTALL%\hblang.lib %HB_LIB_INSTALL%\hbrdd.lib %HB_LIB_INSTALL%\hbrtl.lib %HB_LIB_INSTALL%\_HBVM_LIB.lib %HB_LIB_INSTALL%\hbmacro.lib %HB_LIB_INSTALL%\hbpp.lib %HB_LIB_INSTALL%\rddfpt.lib %HB_LIB_INSTALL%\rddntx.lib %HB_LIB_INSTALL%\rddcdx.lib %HB_LIB_INSTALL%\hbsix.lib %HB_LIB_INSTALL%\hbcommon.lib %HB_LIB_INSTALL%\hbpcre.lib %HB_LIB_INSTALL%\hbzlib.lib
      goto CLEANUP

:CLEANUP

   if exist %1.c del %1.c
   if exist %1.o del %1.o

:END

set _HB_MT=
set _HBVM_LIB=
