#!/bin/bash
#
# $Id$
#

# ---------------------------------------------------------------
# Template to build a final Harbour executable, using Harbour
# with the C code generation feature, then calling the proper C
# linker/compiler.
#
# Copyright 1999-2000 Victor Szakats (info@szelvesz.hu)
# See doc/license.txt for licensing terms.
# ---------------------------------------------------------------

# if [ -z "$HB_ARCHITECTURE" ]; then export HB_ARCHITECTURE=linux; fi
# if [ -z "$HB_COMPILER" ]; then export HB_COMPILER=gcc; fi
# if [ -z "$HB_GT_LIB" ]; then export HB_GT_LIB=; fi

if [ -z "$HB_BIN_INSTALL" ]; then export HB_BIN_INSTALL=../bin/; fi
if [ -z "$HB_LIB_INSTALL" ]; then export HB_LIB_INSTALL=../lib/; fi
if [ -z "$HB_INC_INSTALL" ]; then export HB_INC_INSTALL=../include/; fi

if [ -z "$HB_ARCHITECTURE" ]; then
   echo Error: HB_ARCHITECTURE is not set.
fi
if [ -z "$HB_COMPILER" ]; then
   echo Error: HB_COMPILER is not set.
fi

if [ -z "$1" ] || [ -z "$HB_ARCHITECTURE" ] || [ -z "$HB_COMPILER" ]; then

   echo
   echo Usage: bld.sh filename
   echo
   echo Notes:
   echo
   echo "  - 'filename' is the .prg filename *without* extension."
   echo "  - Don't forget to make a MAIN() function for you application."
   echo "  - This batch file assumes you are in some directory off the main"
   echo "    harbour directory."
   echo "  - Environment variables HB_ARCHITECTURE, HB_COMPILER, HB_GT_LIB"
   echo "    should be set. Setting HB_GT_LIB is optional."
   echo "    The following values are currently supported:"
   echo
   echo "    HB_ARCHITECTURE:"
   echo "      - dos   (HB_GT_LIB=gtdos by default)"
   echo "      - w32   (HB_GT_LIB=gtwin by default)"
   echo "      - linux (HB_GT_LIB=gtstd by default)"
   echo "      - os2   (HB_GT_LIB=gtos2 by default)"
   echo
   read
   echo "    HB_COMPILER:"
   echo "      - When HB_ARCHITECTURE=dos"
   echo "        - bcc16   (Borland C++ 3.x, 4.x, 5.0x, DOS 16-bit)"
   echo "        - djgpp   (Delorie GNU C, DOS 32-bit)"
   echo "        - rxs32   (EMX/RSXNT/DOS GNU C, DOS 32-bit)"
   echo "        - watcom  (Watcom C++ 9.x, 10.x, 11.x, DOS 32-bit)"
   echo "      - When HB_ARCHITECTURE=w32"
   echo "        - bcc32   (Borland C++ 4.x, 5.x, Windows 32-bit)"
   echo "        - gcc     (Cygnus/Cygwin GNU C, Windows 32-bit)"
   echo "        - mingw32 (Cygnus/Mingw32 GNU C, Windows 32-bit)"
   echo "        - rxsnt   (EMX/RSXNT/Win32 GNU C, Windows 32-bit)"
   echo "        - icc     (IBM Visual Age C++, Windows 32-bit)"
   echo "        - msvc    (Microsoft Visual C++, Windows 32-bit)"
   echo "      - When HB_ARCHITECTURE=linux"
   echo "        - gcc     (GNU C, 32-bit)"
   echo "      - When HB_ARCHITECTURE=os2"
   echo "        - gcc     (EMX GNU C, OS/2 32-bit)"
   echo "        - icc     (IBM Visual Age C++ 3.0, OS/2 32-bit)"
   echo
   read
   echo "    HB_GT_LIB:"
   echo "      - gtstd (Standard streaming) (for all architectures)"
   echo "      - gtdos (DOS console)        (for dos architecture)"
   echo "      - gtwin (Win32 console)      (for w32 architecture)"
   echo "      - gtos2 (OS/2 console)       (for os2 architecture)"
   echo "      - gtpca (PC ANSI console)    (for all architectures)"
   echo "      - gtcrs (Curses console)     (for linux, w32 architectures)"
   echo "      - gtsln (Slang console)      (for linux, w32 architectures)"
   exit

else

   $HB_BIN_INSTALL/harbour $1.prg -n -i$HB_INC_INSTALL $2 $3 $HARBOURFLAGS

   if [ "$HB_ARCHITECTURE" == "dos" ]; then

      if [ -z "$HB_GT_LIB" ]; then HB_GT_LIB=gtdos; fi

      if [ "$HB_COMPILER" == "bcc16" ]; then
         bcc -O2 -mh -d $CFLAGS -I$HB_INC_INSTALL -L$HB_LIB_INSTALL $1.c debug.lib vm.lib rtl.lib $HB_GT_LIB.lib lang.lib rdd.lib macro.lib pp.lib dbfntx.lib dbfcdx.lib common.lib
      elif [ "$HB_COMPILER" == "djgpp" ]; then
         gcc $1.c -o$1.exe $CFLAGS -I$HB_INC_INSTALL -L$HB_LIB_INSTALL -ldebug -lvm -lrtl -l$HB_GT_LIB -llang -lrdd -lrtl -lvm -lmacro -lpp -ldbfnt -ldbfcd -lcommo
      elif [ "$HB_COMPILER" == "rsx32" ]; then
         gcc $1.c -Zrsx32 $CFLAGS -I$HB_INC_INSTALL -L$HB_LIB_INSTALL -ldebug -lvm -lrtl -l$HB_GT_LIB -llang -lrdd -lrtl -lvm -lmacro -lpp -ldbfntx -ldbfcdx -lcommon
      else
         echo Error: HB_COMPILER value is unsupported.
      fi

   elif [ "$HB_ARCHITECTURE" == "w32" ]; then

      if [ -z "$HB_GT_LIB" ]; then HB_GT_LIB=gtwin; fi

      if [ "$HB_COMPILER" == "bcc32" ]; then
         bcc32 -O2 -d $CFLAGS -I$HB_INC_INSTALL -L$HB_LIB_INSTALL $1.c debug.lib vm.lib rtl.lib $HB_GT_LIB.lib lang.lib rdd.lib macro.lib pp.lib dbfntx.lib dbfcdx.lib common.lib
      elif [ "$HB_COMPILER" == "gcc" ]; then
         gcc $1.c -o$1.exe $CFLAGS -I$HB_INC_INSTALL -L$HB_LIB_INSTALL -ldebug -lvm -lrtl -l$HB_GT_LIB -llang -lrdd -lrtl -lvm -lmacro -lpp -ldbfntx -ldbfcdx -lcommon
      elif [ "$HB_COMPILER" == "mingw32" ]; then
         gcc $1.c -o$1.exe $CFLAGS -mno-cygwin -I$HB_INC_INSTALL -L$HB_LIB_INSTALL -ldebug -lvm -lrtl -l$HB_GT_LIB -llang -lrdd -lrtl -lvm -lmacro -lpp -ldbfntx -ldbfcdx -lcommon
      elif [ "$HB_COMPILER" == "rsxnt" ]; then
         gcc $1.c -Zwin32 $CFLAGS -I$HB_INC_INSTALL -L$HB_LIB_INSTALL -ldebug -lvm -lrtl -l$HB_GT_LIB -llang -lrdd -lrtl -lvm -lmacro -lpp -ldbfntx -ldbfcdx -lcommon
      elif [ "$HB_COMPILER" == "msvc" ]; then
         cl -TP -W3 $CFLAGS -I$HB_INC_INSTALL $1.c /link /subsystem:CONSOLE $HB_LIB_INSTALL\debug.lib $HB_LIB_INSTALL\vm.lib $HB_LIB_INSTALL\rtl.lib $HB_LIB_INSTALL\$HB_GT_LIB.lib $HB_LIB_INSTALL\lang.lib $HB_LIB_INSTALL\rdd.lib $HB_LIB_INSTALL\macro.lib $HB_LIB_INSTALL\pp.lib $HB_LIB_INSTALL\dbfntx.lib $HB_LIB_INSTALL\dbfcdx.lib
         echo Ignore LNK4033 warning
      else
         echo Error: HB_COMPILER value is unsupported.
      fi

   elif [ "$HB_ARCHITECTURE" == "os2" ]; then

      if [ -z "$HB_GT_LIB" ]; then HB_GT_LIB=gtos2; fi

      if [ "$HB_COMPILER" == "gcc" ]; then
         gcc $1.c $CFLAGS -I$HB_INC_INSTALL -L$HB_LIB_INSTALL -ldebug -lvm -lrtl -l$HB_GT_LIB -llang -lrdd -lrtl -lvm -lmacro -lpp -ldbfntx -ldbfcdx -lcommon
      elif [ "$HB_COMPILER" == "icc" ]; then
         icc /Gs+ /W2 /Se /Sd+ /Ti+ /C- /Tp $CFLAGS -I$HB_INC_INSTALL $1.c $HB_LIB_INSTALL\debug.lib $HB_LIB_INSTALL\vm.lib $HB_LIB_INSTALL\rtl.lib $HB_LIB_INSTALL\$HB_GT_LIB.lib $HB_LIB_INSTALL\lang.lib $HB_LIB_INSTALL\rdd.lib $HB_LIB_INSTALL\rtl.lib $HB_LIB_INSTALL\vm.lib $HB_LIB_INSTALL\macro.lib $HB_LIB_INSTALL\pp.lib $HB_LIB_INSTALL\dbfntx.lib $HB_LIB_INSTALL\dbfcdx.lib
      else
         echo Error: HB_COMPILER value is unsupported.
      fi

   elif [ "$HB_ARCHITECTURE" == "linux" ]; then

      if [ -z "$HB_GT_LIB" ]; then HB_GT_LIB=gtstd; fi

      if [ "$HB_COMPILER" == "gcc" ]; then
         gcc $1.c $CFLAGS -I$HB_INC_INSTALL -L$HB_LIB_INSTALL -ldebug -lvm -lrtl -l$HB_GT_LIB -llang -lrdd -lrtl -lvm -lmacro -lpp -ldbfntx -ldbfcdx -lcommon
      else
         echo Error: HB_COMPILER value is unsupported.
      fi

   else
      echo Error: HB_ARCHITECTURE value is unsupported.
      unlink $1.c
   fi
fi
