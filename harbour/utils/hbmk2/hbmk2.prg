/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour Make
 *
 * Copyright 1999-2009 Viktor Szakats <harbour.01 syenar.hu>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 2003 Przemyslaw Czerpak <druzus@priv.onet.pl>
 *    gcc and *nix configuration elements.
 *    bash script with similar purpose for gcc family.
 *    entry point override method and detection code for gcc.
 *    rtlink/blinker link script parsers.
 *
 * See COPYING for licensing terms.
 *
 */

#pragma linenumber=on

/*
   Program Library HOWTO:
      http://www.linux.org/docs/ldp/howto/Program-Library-HOWTO/index.html

   Man page HOWTO:
      http://www.schweikhardt.net/man_page_howto.html
   Groff manual:
      http://www.gnu.org/software/groff/manual/html_node/index.html
      http://www.gnu.org/software/groff/manual/groff.pdf
   Troff manual:
      http://cm.bell-labs.com/sys/doc/troff.pdf
 */

#include "common.ch"
#include "directry.ch"
#include "fileio.ch"
#include "hbgtinfo.ch"
#include "hbver.ch"

/* NOTE: Keep this code clean from any kind of contribs and Harbour
         level 3rd party library/tool information. This (the hbmk)
         component shall only contain hard-wired knowledge on Harbour
         _core_ (official interfaces preferred), C compilers and OS
         details on the smallest possible level.
         Instead, 3rd party Harbour packages are recommended to
         maintain and provide .hbp files themselves, as part of
         their standard distribution packages. You can find a few
         such .hbp examples in the 'examples' directory.
         For Harbour contribs, the recommended method is to supply
         and maintain .hbp files in their respective directories,
         usually under tests (or utils, samples). As of this
         writing, most of them has one created.
         Thank you. [vszakats] */

/* TODO: Create temporary .c files with mangled names, to
         avoid incidentally overwriting existing .c file with the
         same name. Problems to solve: -hbcc compatibility (the
         feature has to be disabled when this switch is uses).
         Collision with -o harbour option isn't a problem, since
         we're overriding it already fo hbmk, but we will need to
         deal with "/" prefixed variant. Since we need to use -o
         Harbour switch, it will be a problem also when user tries
         to use -p option, .ppo files will be generated in temp dir. */
/* TODO: Sync default C/linker switches with the ones in Harbour GNU make system. */
/* TODO: Add support for library creation for rest of compilers. */
/* TODO: Add support for dynamic library creation for rest of compilers. */
/* TODO: Cleanup on variable names and compiler configuration. */
/* TODO: Optimizations (speed/memory). */
/* TODO: C++/C mode. */
/* TODO: Incremental support:
         - handle libs? (problematic)
         - Reuse Harbour .c output for different compiler targets. */

/* PLANNING:
   hbgtwvg.hbp
   ---
   requires=hbwin xhb
   prgflags=-DHAS_GTWVG
   cflags=-DHAS_GTWVG
   prgincludes=gtwvg.ch
   cincludes=hbgtwvg.h
   libs=gtwvg
   libpaths=C:\libs
   autodetect=yes
   ---
*/

#ifndef HBMK_INTEGRATED_COMPILER
#define HBMK_INTEGRATED_COMPILER
#endif

ANNOUNCE HB_GTSYS
REQUEST HB_GT_CGI_DEFAULT

REQUEST hbmk_ARCH
REQUEST hbmk_COMP
REQUEST hbmk_KEYW

THREAD STATIC t_lQuiet := .F.
THREAD STATIC t_lInfo := .F.
THREAD STATIC t_cARCH
THREAD STATIC t_cCOMP
THREAD STATIC t_aLIBCOREGT
THREAD STATIC t_cGTDEFAULT

THREAD STATIC t_lMT := .F.
THREAD STATIC t_lDEBUG := .F.

THREAD STATIC t_cCCPATH
THREAD STATIC t_cCCPREFIX
THREAD STATIC t_cHBPOSTFIX

#define _PAR_cParam         1
#define _PAR_cFileName      2
#define _PAR_nLine          3

#define _COMPR_OFF          0
#define _COMPR_DEF          1
#define _COMPR_MIN          2
#define _COMPR_MAX          3

PROCEDURE Main( ... )

   LOCAL aLIB_BASE1 := {;
      "hbcpage" ,;
      "hblang" ,;
      "hbcommon" }

   /* NOTE: All base GTs should come here. */
   LOCAL aLIB_BASE2 := {;
      "hbrtl" ,;
      "hbpp" ,;
      "hbmacro" ,;
      "hbextern" }

   LOCAL aLIB_BASE_GT := {;
      "gtcgi" ,;
      "gtpca" ,;
      "gtstd" }

   LOCAL aLIB_BASE_PCRE := {;
      "hbpcre" }

   LOCAL aLIB_BASE_ZLIB := {;
      "hbzlib" }

   LOCAL aLIB_BASE_DEBUG := {;
      "hbdebug" }

   LOCAL aLIB_BASE_CPLR := {;
      "hbcplr" }

   LOCAL aLIB_BASE_ST := {;
      "hbvm" }
   LOCAL aLIB_BASE_MT := {;
      "hbvmmt" }

   LOCAL aLIB_BASE_NULRDD := {;
      "hbnulrdd" }

   LOCAL aLIB_BASE_RDD := {;
      "hbrdd" ,;
      "hbusrrdd" ,;
      "hbuddall" ,;
      "hbhsx" ,;
      "hbsix" ,;
      "rddntx" ,;
      "rddnsx" ,;
      "rddcdx" ,;
      "rddfpt" }

   LOCAL s_cGT
   LOCAL s_cCSTUB

   LOCAL s_cHB_INSTALL_PREFIX
   LOCAL s_cHB_BIN_INSTALL
   LOCAL s_cHB_LIB_INSTALL
   LOCAL s_cHB_DYN_INSTALL
   LOCAL s_cHB_INC_INSTALL

   LOCAL s_aPRG
   LOCAL s_aPRG_TODO
   LOCAL s_aPRG_DONE
   LOCAL s_aC
   LOCAL s_aC_TODO
   LOCAL s_aC_DONE
   LOCAL s_aRESSRC
   LOCAL s_aRESSRC_TODO
   LOCAL s_aRESCMP
   LOCAL s_aLIBSHARED
   LOCAL s_aLIBSHAREDPOST := {}
   LOCAL s_aLIB
   LOCAL s_aLIBVM
   LOCAL s_aLIBUSER
   LOCAL s_aLIBUSERGT
   LOCAL s_aLIBFM
   LOCAL s_aLIBHB
   LOCAL s_aLIBHBGT
   LOCAL s_aLIB3RD
   LOCAL s_aLIBSYS
   LOCAL s_aLIBPATH
   LOCAL s_aLIBDYNHAS
   LOCAL s_aLIBSYSCORE := {}
   LOCAL s_aLIBSYSMISC := {}
   LOCAL s_aOPTPRG
   LOCAL s_aOPTC
   LOCAL s_aOPTL
   LOCAL s_aOPTA
   LOCAL s_aOPTD
   LOCAL s_aOPTRUN
   LOCAL s_cPROGDIR
   LOCAL s_cPROGNAME
   LOCAL s_cFIRST
   LOCAL s_aOBJ
   LOCAL s_aOBJA
   LOCAL s_aOBJUSER
   LOCAL s_aCLEAN
   LOCAL s_lHB_PCRE := .T.
   LOCAL s_lHB_ZLIB := .T.
   LOCAL s_cMAIN := NIL

   LOCAL s_lGUI := .F.
   LOCAL s_lCPP := .F.
   LOCAL s_lSHARED := NIL
   LOCAL s_lSTATICFULL := NIL
   LOCAL s_lDEBUGINC := .F.
   LOCAL s_lNULRDD := .F.
   LOCAL s_lMAP := .F.
   LOCAL s_lSTRIP := .F.
   LOCAL s_nCOMPR := _COMPR_OFF
   LOCAL s_lTRACE := .F.
   LOCAL s_lDONTEXEC := .F.
   LOCAL s_lBLDFLGP := .F.
   LOCAL s_lBLDFLGC := .F.
   LOCAL s_lBLDFLGL := .F.
   LOCAL s_lRUN := .F.
   LOCAL s_lFMSTAT := NIL /* NIL = default, .T. = on, .F. = off */
   LOCAL s_lINC := .F.
   LOCAL s_lREBUILD := .F.
   LOCAL s_lCLEAN := .F.

   LOCAL aCOMPDET
   LOCAL aCOMPDET_LOCAL
   LOCAL aCOMPSUP

   LOCAL cLibPrefix
   LOCAL cLibExt
   LOCAL cObjPrefix
   LOCAL cObjExt
   LOCAL cLibLibExt
   LOCAL cLibLibPrefix := ""
   LOCAL cLibObjPrefix
   LOCAL cDynObjPrefix := NIL
   LOCAL cLibPathPrefix
   LOCAL cLibPathSep
   LOCAL cDynLibNamePrefix
   LOCAL cDynLibExt
   LOCAL cResPrefix
   LOCAL cResExt
   LOCAL cBinExt
   LOCAL cOptPrefix
   LOCAL cBin_Cprs
   LOCAL cOpt_Cprs
   LOCAL cOpt_CprsMin
   LOCAL cOpt_CprsMax

   LOCAL cCommand
#if defined( HBMK_INTEGRATED_COMPILER )
   LOCAL aCommand
#endif
   LOCAL cOpt_CompC
   LOCAL cOpt_Link
   LOCAL cOpt_Res
   LOCAL cOpt_Lib
   LOCAL cOpt_Dyn
   LOCAL cBin_CompPRG
   LOCAL cBin_CompC
   LOCAL cBin_Link
   LOCAL cBin_Res
   LOCAL cBin_Lib
   LOCAL cBin_Dyn
   LOCAL nErrorLevel := 0
   LOCAL tmp, tmp1, tmp2, array
   LOCAL cScriptFile
   LOCAL fhnd
   LOCAL lNOHBP
   LOCAL lSysLoc
   LOCAL cPrefix
   LOCAL cPostfix
   LOCAL nEmbedLevel

   LOCAL lStopAfterInit := .F.
   LOCAL lStopAfterHarbour := .F.
   LOCAL lStopAfterCComp := .F.
   LOCAL lAcceptCFlag := .F.
   LOCAL lAcceptLDFlag := .F.
   LOCAL lCreateLib := .F.
   LOCAL lCreateDyn := .F.
   LOCAL lAcceptLDClipper := .F.

   LOCAL cWorkDir := NIL

   LOCAL aParams
   LOCAL aParam
   LOCAL cParam
   LOCAL cParamL

   LOCAL cTarget
   LOCAL tTarget
   LOCAL lTargetUpToDate

   LOCAL cDir, cName, cExt

   LOCAL lNIX := hb_Version( HB_VERSION_UNIX_COMPAT )

   LOCAL cSelfCOMP    := hb_Version( HB_VERSION_BUILD_COMP )
   LOCAL cSelfFlagPRG := hb_Version( HB_VERSION_FLAG_PRG )
   LOCAL cSelfFlagC   := hb_Version( HB_VERSION_FLAG_C )
   LOCAL cSelfFlagL   := hb_Version( HB_VERSION_FLAG_LINKER )

   LOCAL cDL_Version_Alter := hb_ntos( hb_Version( HB_VERSION_MAJOR ) ) +;
                              hb_ntos( hb_Version( HB_VERSION_MINOR ) )
   LOCAL cDL_Version       := hb_ntos( hb_Version( HB_VERSION_MAJOR ) ) + "." +;
                              hb_ntos( hb_Version( HB_VERSION_MINOR ) ) + "." +;
                              hb_ntos( hb_Version( HB_VERSION_RELEASE ) )

   IF PCount() == 0
      ShowHeader()
      ShowHelp()
      PauseForKey()
      ErrorLevel( 19 )
      RETURN
   ENDIF

   FOR EACH cParam IN hb_AParams()

      cParamL := Lower( cParam )

      /* NOTE: Don't forget to make these ignored in the main
               option processing loop. */
      DO CASE
      CASE cParamL            == "-quiet" ; t_lQuiet := .T. ; t_lInfo := .F.
      CASE Left( cParamL, 6 ) == "-comp=" ; t_cCOMP := SubStr( cParam, 7 )
      CASE Left( cParamL, 6 ) == "-arch=" ; t_cARCH := SubStr( cParam, 7 )
      CASE cParamL            == "-hbcmp" ; t_lInfo := .F. ; lStopAfterHarbour := .F. ; lStopAfterCComp := .T. ; lCreateLib := .F. ; lCreateDyn := .F.
      CASE cParamL            == "-hbcc"  ; t_lInfo := .F. ; lStopAfterHarbour := .F. ; lStopAfterCComp := .F. ; lAcceptCFlag := .T.
      CASE cParamL            == "-hblnk" ; t_lInfo := .F. ; lStopAfterHarbour := .F. ; lStopAfterCComp := .F. ; lAcceptLDFlag := .T.
      CASE cParamL            == "-hblib" ; t_lInfo := .F. ; lStopAfterHarbour := .F. ; lStopAfterCComp := .T. ; lCreateLib := .T. ; lCreateDyn := .F.
      CASE cParamL            == "-hbdyn" ; t_lInfo := .F. ; lStopAfterHarbour := .F. ; lStopAfterCComp := .T. ; lCreateLib := .F. ; lCreateDyn := .T.
      CASE cParamL            == "-info"  ; t_lInfo := .T.
      CASE cParamL == "-help" .OR. ;
           cParamL == "--help"

         ShowHeader()
         ShowHelp( .T. )
         PauseForKey()
         ErrorLevel( 19 )
         RETURN

      CASE cParamL == "--version"

         ShowHeader()
         PauseForKey()
         ErrorLevel( 0 )
         RETURN

      ENDCASE
   NEXT

   /* Emulate -hbcmp, -hbcc, -hblnk switches when certain
      self names are detected.
      For compatibility with hbmk script aliases. */

   tmp := Lower( FN_NameGet( hb_argv( 0 ) ) )
   DO CASE
   CASE Right( tmp, 5 ) == "hbcmp" .OR. ;
        Left(  tmp, 5 ) == "hbcmp" .OR. ;
        tmp == "clipper"
      t_lInfo := .F. ; lStopAfterHarbour := .F. ; lStopAfterCComp := .T. ; lCreateLib := .F. ; lCreateDyn := .F.
      IF t_lInfo
         OutStd( "hbmk: Enabled -hbcmp option." + hb_osNewLine() )
      ENDIF
   CASE Right( tmp, 4 ) == "hbcc" .OR. ;
        Left(  tmp, 4 ) == "hbcc"
      t_lInfo := .F. ; lStopAfterHarbour := .F. ; lStopAfterCComp := .F. ; lAcceptCFlag := .T.
      IF t_lInfo
         OutStd( "hbmk: Enabled -hbcc option." + hb_osNewLine() )
      ENDIF
   CASE Right( tmp, 5 ) == "hblnk" .OR. ;
        Left(  tmp, 5 ) == "hblnk"
      t_lInfo := .F. ; lStopAfterHarbour := .F. ; lStopAfterCComp := .F. ; lAcceptLDFlag := .T.
      IF t_lInfo
         OutStd( "hbmk: Enabled -hblnk option." + hb_osNewLine() )
      ENDIF
   CASE tmp == "rtlink" .OR. ;
        tmp == "exospace" .OR. ;
        tmp == "blinker"
      t_lInfo := .F. ; lStopAfterHarbour := .F. ; lStopAfterCComp := .F. ; lAcceptLDClipper := .T.
      IF t_lInfo
         OutStd( "hbmk: Enabled -hblnk (Clipper compatibility) option." + hb_osNewLine() )
      ENDIF
   CASE Right( tmp, 5 ) == "hblib" .OR. ;
        Left(  tmp, 5 ) == "hblib"
      t_lInfo := .F. ; lStopAfterHarbour := .F. ; lStopAfterCComp := .T. ; lCreateLib := .T. ; lCreateDyn := .F.
      IF t_lInfo
         OutStd( "hbmk: Enabled -hblib option." + hb_osNewLine() )
      ENDIF
   CASE Right( tmp, 5 ) == "hbdyn" .OR. ;
        Left(  tmp, 5 ) == "hbdyn"
      t_lInfo := .F. ; lStopAfterHarbour := .F. ; lStopAfterCComp := .T. ; lCreateLib := .F. ; lCreateDyn := .T.
      IF t_lInfo
         OutStd( "hbmk: Enabled -hblib option." + hb_osNewLine() )
      ENDIF
   ENDCASE

   DO CASE
   CASE Right( tmp, 4 ) == "-x64"
      t_cHBPOSTFIX := Right( tmp, 4 )
   CASE Right( tmp, 5 ) == "-ia64"
      t_cHBPOSTFIX := Right( tmp, 5 )
   OTHERWISE
      t_cHBPOSTFIX := ""
   ENDCASE

   /* Load architecture / compiler settings (compatibility) */

   IF Empty( t_cARCH )
      t_cARCH := Lower( GetEnv( "HB_ARCHITECTURE" ) )
   ENDIF
   IF Empty( t_cCOMP )
      t_cCOMP := Lower( GetEnv( "HB_COMPILER" ) )
   ENDIF

   /* Autodetect architecture */

   IF Empty( t_cARCH )

      /* NOTE: Keep this in sync manually. All compilers should be listed here,
               which are supported on one architecture only. In the future this
               should be automatically extracted from a comp/arch matrix. */
      SWITCH t_cCOMP
      CASE "msvc"
      CASE "msvc64"
      CASE "msvcia64"
      CASE "bcc"
      CASE "xcc"
      CASE "pocc"
         t_cARCH := "win"
         EXIT
      CASE "mingwarm"
      CASE "msvcarm"
      CASE "poccarm"
         t_cARCH := "wce"
         EXIT
      CASE "djgpp"
         t_cARCH := "dos"
         EXIT
      OTHERWISE
         t_cARCH := hb_Version( HB_VERSION_BUILD_ARCH )
      ENDSWITCH
      IF ! Empty( t_cARCH )
         IF t_lInfo
            OutStd( "hbmk: Autodetected architecture: " + t_cARCH + hb_osNewLine() )
         ENDIF
      ENDIF
   ENDIF

   t_cCCPATH   := GetEnv( "HB_CCPATH" )
   t_cCCPREFIX := GetEnv( "HB_CCPREFIX" )

   /* Setup architecture dependent data */

   DO CASE
   CASE t_cARCH $ "bsd|hpux|sunos|linux" .OR. t_cARCH == "darwin" /* Separated to avoid match with 'win' */
      IF t_cARCH == "linux"
         aCOMPSUP := { "gcc", "gpp", "owatcom", "icc" }
      ELSE
         aCOMPSUP := { "gcc" }
      ENDIF
      cBin_CompPRG := "harbour" + t_cHBPOSTFIX
      s_aLIBHBGT := { "gttrm" }
      t_cGTDEFAULT := "gttrm"
      cDynLibNamePrefix := "lib"
      cBinExt := NIL
      cOptPrefix := "-"
      IF t_cARCH == "linux"
         cBin_Cprs := "upx"
         cOpt_Cprs := "{OB}"
         cOpt_CprsMin := "-1"
         cOpt_CprsMax := "-9"
      ENDIF
      SWITCH t_cARCH
      CASE "darwin" ; cDynLibExt := ".dylib" ; EXIT
      CASE "hpux"   ; cDynLibExt := ".sl" ; EXIT
      OTHERWISE     ; cDynLibExt := ".so"
      ENDSWITCH
   CASE t_cARCH == "dos"
      aCOMPDET := { { {|| FindInPath( "gcc"      ) != NIL }, "djgpp"   },;
                    { {|| FindInPath( "wpp386"   ) != NIL }, "owatcom" } } /* TODO: Add full support for wcc386 */
      aCOMPSUP := { "djgpp", "gcc", "owatcom" }
      cBin_CompPRG := "harbour" + t_cHBPOSTFIX + ".exe"
      s_aLIBHBGT := { "gtdos" }
      t_cGTDEFAULT := "gtdos"
      cDynLibNamePrefix := ""
      cDynLibExt := ""
      cBinExt := ".exe"
      cOptPrefix := "-/"
      cBin_Cprs := "upx.exe"
      cOpt_Cprs := "{OB}"
      cOpt_CprsMin := "-1"
      cOpt_CprsMax := "-9"
   CASE t_cARCH == "os2"
      aCOMPDET := { { {|| FindInPath( "gcc"      ) != NIL }, "gcc"     },;
                    { {|| FindInPath( "wpp386"   ) != NIL }, "owatcom" } } /* TODO: Add full support for wcc386 */
      aCOMPSUP := { "gcc", "owatcom" }
      cBin_CompPRG := "harbour" + t_cHBPOSTFIX + ".exe"
      s_aLIBHBGT := { "gtos2" }
      t_cGTDEFAULT := "gtos2"
      cDynLibNamePrefix := ""
      cDynLibExt := ".dll"
      cBinExt := ".exe"
      cOptPrefix := "-/"
   CASE t_cARCH == "win"
      /* Order is significant.
         owatcom also keeps a cl.exe in its binary dir. */
      aCOMPDET := { { {|| FindInPath( t_cCCPREFIX + "gcc" ) != NIL }, "mingw"   },; /* TODO: Add full support for g++ */
                    { {|| FindInPath( "wpp386"   ) != NIL .AND. ;
                          ! Empty( GetEnv( "WATCOM" ) ) }, "owatcom" },; /* TODO: Add full support for wcc386 */
                    { {|| FindInPath( "ml64"     ) != NIL }, "msvc64"  },;
                    { {|| FindInPath( "cl"       ) != NIL .AND. ;
                          FindInPath( "wpp386"   ) == NIL }, "msvc"    },;
                    { {|| FindInPath( "bcc32"    ) != NIL }, "bcc"     },;
                    { {|| FindInPath( "porc64"   ) != NIL }, "pocc64"  },;
                    { {|| FindInPath( "pocc"     ) != NIL }, "pocc"    },;
                    { {|| ( tmp1 := FindInPath( "icl" ) ) != NIL .AND. "itanium" $ Lower( tmp1 ) }, "iccia64" },;
                    { {|| FindInPath( "icl"      ) != NIL }, "icc"     },;
                    { {|| FindInPath( "cygstart" ) != NIL }, "cygwin"  },;
                    { {|| FindInPath( "xcc"      ) != NIL }, "xcc"     } }
      aCOMPSUP := { "mingw", "msvc", "bcc", "owatcom", "icc", "pocc", "xcc", "cygwin",;
                    "mingw64", "msvc64", "msvcia64", "iccia64", "pocc64" }
      cBin_CompPRG := "harbour" + t_cHBPOSTFIX + ".exe"
      s_aLIBHBGT := { "gtwin", "gtwvt", "gtgui" }
      t_cGTDEFAULT := "gtwin"
      cDynLibNamePrefix := ""
      cDynLibExt := ".dll"
      cBinExt := ".exe"
      cOptPrefix := "-/"
      cBin_Cprs := "upx.exe"
      cOpt_Cprs := "{OB}"
      cOpt_CprsMin := "-1"
      cOpt_CprsMax := "-9"
      /* NOTE: Some targets (pocc and owatcom) need kernel32 explicitly. */
      s_aLIBSYSCORE := { "kernel32", "user32", "gdi32", "advapi32", "ws2_32" }
      s_aLIBSYSMISC := { "winspool", "comctl32", "comdlg32", "shell32", "ole32", "oleaut32", "uuid", "mpr", "winmm", "mapi32", "imm32", "msimg32" }
   CASE t_cARCH == "wce"
      aCOMPDET := { { {|| FindInPath( t_cCCPREFIX + "gcc" ) != NIL }, "mingwarm" },;
                    { {|| FindInPath( "cl"       ) != NIL }, "msvcarm" },;
                    { {|| FindInPath( "pocc"     ) != NIL }, "poccarm" } }
      aCOMPSUP := { "mingwarm", "msvcarm", "poccarm" }
      cBin_CompPRG := "harbour" + t_cHBPOSTFIX + ".exe"
      s_aLIBHBGT := { "gtwvt", "gtgui" }
      t_cGTDEFAULT := "gtwvt"
      cDynLibNamePrefix := ""
      cDynLibExt := ".dll"
      cBinExt := ".exe"
      cOptPrefix := "-/"
      cBin_Cprs := "upx.exe"
      cOpt_Cprs := "{OB}"
      cOpt_CprsMin := "-1"
      cOpt_CprsMax := "-9"
      s_aLIBSYSCORE := { "wininet", "ws2", "commdlg", "commctrl" }
      s_aLIBSYSMISC := { "uuid", "ole32" }
   OTHERWISE
      OutErr( "hbmk: Error: Architecture value unknown: " + t_cARCH + hb_osNewLine() )
      PauseForKey()
      ErrorLevel( 1 )
      RETURN
   ENDCASE

   t_aLIBCOREGT := ArrayJoin( aLIB_BASE_GT, s_aLIBHBGT )

   /* Setup GUI state for Harbour default */
   SetupForGT( t_cGTDEFAULT, NIL, @s_lGUI )

   /* Autodetect Harbour environment */

   /* Detect system locations to enable shared library option by default */
   lSysLoc := hb_DirBase() == "/usr/local/bin/" .OR. ;
              hb_DirBase() == "/usr/bin/" .OR. ;
              hb_DirBase() == "/opt/harbour/" .OR. ;
              hb_DirBase() == "/opt/bin/"

   s_cHB_BIN_INSTALL := PathSepToSelf( GetEnv( "HB_BIN_INSTALL" ) )
   s_cHB_LIB_INSTALL := PathSepToSelf( GetEnv( "HB_LIB_INSTALL" ) )
   s_cHB_INC_INSTALL := PathSepToSelf( GetEnv( "HB_INC_INSTALL" ) )

   s_cHB_INSTALL_PREFIX := PathSepToSelf( GetEnv( "HB_INSTALL_PREFIX" ) )
   IF Empty( s_cHB_INSTALL_PREFIX )
      DO CASE
      CASE hb_FileExists( DirAddPathSep( hb_DirBase() ) + cBin_CompPRG )
         s_cHB_INSTALL_PREFIX := DirAddPathSep( hb_DirBase() ) + ".."
      CASE hb_FileExists( DirAddPathSep( hb_DirBase() ) + "bin" + hb_osPathSeparator() + cBin_CompPRG )
         s_cHB_INSTALL_PREFIX := DirAddPathSep( hb_DirBase() )
      CASE hb_FileExists( DirAddPathSep( hb_DirBase() ) + ".." + hb_osPathSeparator() + ".." + hb_osPathSeparator() + "bin" + hb_osPathSeparator() + cBin_CompPRG )
         s_cHB_INSTALL_PREFIX := DirAddPathSep( hb_DirBase() ) + ".." + hb_osPathSeparator() + ".."
      CASE hb_FileExists( DirAddPathSep( hb_DirBase() ) + ".." + hb_osPathSeparator() + ".." + hb_osPathSeparator() + ".." + hb_osPathSeparator() + "bin" + hb_osPathSeparator() + cBin_CompPRG )
         s_cHB_INSTALL_PREFIX := DirAddPathSep( hb_DirBase() ) + ".." + hb_osPathSeparator() + ".." + hb_osPathSeparator() + ".."
      OTHERWISE
         OutErr( "hbmk: Error: HB_INSTALL_PREFIX not set, failed to autodetect." + hb_osNewLine() )
         PauseForKey()
         ErrorLevel( 3 )
         RETURN
      ENDCASE
      /* Detect special *nix dir layout (/bin, /lib/harbour, /include/harbour) */
      IF hb_FileExists( DirAddPathSep( s_cHB_INSTALL_PREFIX ) + "include" +;
                                         hb_osPathSeparator() + "harbour" +;
                                         hb_osPathSeparator() + "hbvm.h" )
         IF Empty( s_cHB_BIN_INSTALL )
            s_cHB_BIN_INSTALL := PathNormalize( s_cHB_INSTALL_PREFIX ) + "bin"
         ENDIF
         IF Empty( s_cHB_LIB_INSTALL )
            s_cHB_LIB_INSTALL := PathNormalize( s_cHB_INSTALL_PREFIX ) + "lib" + hb_osPathSeparator() + "harbour"
         ENDIF
         IF Empty( s_cHB_INC_INSTALL )
            s_cHB_INC_INSTALL := PathNormalize( s_cHB_INSTALL_PREFIX ) + "include" + hb_osPathSeparator() + "harbour"
         ENDIF
      ENDIF
   ENDIF
   IF Empty( s_cHB_INSTALL_PREFIX ) .AND. ;
      ( Empty( s_cHB_BIN_INSTALL ) .OR. Empty( s_cHB_LIB_INSTALL ) .OR. Empty( s_cHB_INC_INSTALL ) )
      OutErr( "hbmk: Error: Harbour locations couldn't be determined." + hb_osNewLine() )
      PauseForKey()
      ErrorLevel( 3 )
      RETURN
   ENDIF

   IF t_cARCH $ "win|wce"
      aCOMPDET_LOCAL := {;
          { {| cPrefix | tmp1 := PathNormalize( s_cHB_INSTALL_PREFIX ) + "mingw"   + hb_osPathSeparator() + "bin", iif( hb_FileExists( tmp1 + hb_osPathSeparator() + cPrefix + "gcc.exe" ), tmp1, NIL ) }, "win", "mingw"   , ""                     } ,;
          { {| cPrefix | tmp1 := PathNormalize( s_cHB_INSTALL_PREFIX ) + "mingw64" + hb_osPathSeparator() + "bin", iif( hb_FileExists( tmp1 + hb_osPathSeparator() + cPrefix + "gcc.exe" ), tmp1, NIL ) }, "win", "mingw64" , "x86_64-pc-mingw32-"   } ,;
          { {| cPrefix | tmp1 := PathNormalize( s_cHB_INSTALL_PREFIX ) + "mingwce" + hb_osPathSeparator() + "bin", iif( hb_FileExists( tmp1 + hb_osPathSeparator() + cPrefix + "gcc.exe" ), tmp1, NIL ) }, "wce", "mingwarm", "arm-wince-mingw32ce-" } }
   ENDIF

   /* Autodetect compiler */

   IF lStopAfterHarbour
      /* If we're just compiling .prg to .c we don't need a C compiler. */
      t_cCOMP := ""
   ELSE
      IF Empty( t_cCOMP ) .OR. t_cCOMP == "bld"
         IF Len( aCOMPSUP ) == 1
            t_cCOMP := aCOMPSUP[ 1 ]
         ELSEIF t_cARCH == "linux" .OR. t_cCOMP == "bld"
            t_cCOMP := cSelfCOMP
            IF AScan( aCOMPSUP, {|tmp| tmp == t_cCOMP } ) == 0
               t_cCOMP := NIL
            ENDIF
         ELSE
            IF Empty( t_cCOMP ) .AND. ! Empty( aCOMPDET )
               /* Look for this compiler first */
               FOR tmp := 1 TO Len( aCOMPDET )
                  IF aCOMPDET[ tmp ][ 2 ] == cSelfCOMP .AND. Eval( aCOMPDET[ tmp ][ 1 ] )
                     t_cCOMP := aCOMPDET[ tmp ][ 2 ]
                     EXIT
                  ENDIF
               NEXT
               IF Empty( t_cCOMP )
                  /* Check the rest of compilers */
                  FOR tmp := 1 TO Len( aCOMPDET )
                     IF !( aCOMPDET[ tmp ][ 2 ] == cSelfCOMP ) .AND. Eval( aCOMPDET[ tmp ][ 1 ] )
                        t_cCOMP := aCOMPDET[ tmp ][ 2 ]
                        EXIT
                     ENDIF
                  NEXT
               ENDIF
            ENDIF
            IF Empty( t_cCOMP ) .AND. t_cARCH $ "win|wce"
               /* Autodetect embedded MinGW installation */
               FOR tmp := 1 TO Len( aCOMPDET_LOCAL )
                  IF t_cARCH == aCOMPDET_LOCAL[ tmp ][ 2 ] .AND. ;
                     ! Empty( tmp1 := Eval( aCOMPDET_LOCAL[ tmp ][ 1 ], aCOMPDET_LOCAL[ tmp ][ 4 ] ) )
                     t_cCOMP := aCOMPDET_LOCAL[ tmp ][ 3 ]
                     t_cCCPREFIX := aCOMPDET_LOCAL[ tmp ][ 4 ]
                     t_cCCPATH := tmp1
                     EXIT
                  ENDIF
               NEXT
            ENDIF
         ENDIF
         IF ! Empty( t_cCOMP )
            IF t_lInfo
               OutStd( "hbmk: Autodetected compiler: " + t_cCOMP + hb_osNewLine() )
            ENDIF
         ELSE
            IF Empty( aCOMPDET )
               OutErr( "hbmk: Please choose a compiler by using -comp= option or envvar HB_COMPILER." + hb_osNewLine() )
               OutErr( "      You have the following choices on your platform:" + hb_osNewLine() )
               OutErr( "      " + ArrayToList( aCOMPSUP, ", " ) + hb_osNewLine() )
            ELSE
               OutErr( "hbmk: Harbour Make couldn't detect any supported C compiler in your PATH." + hb_osNewLine() )
               OutErr( "      Please setup one or set -comp= option or envvar HB_COMPILER" + hb_osNewLine() )
               OutErr( "      to one of these values:" + hb_osNewLine() )
               OutErr( "      " + ArrayToList( aCOMPSUP, ", " ) + hb_osNewLine() )
            ENDIF
            PauseForKey()
            ErrorLevel( 2 )
            RETURN
         ENDIF
      ELSE
         IF AScan( aCOMPSUP, {|tmp| tmp == t_cCOMP } ) == 0
            OutErr( "hbmk: Error: Compiler value unknown: " + t_cCOMP + hb_osNewLine() )
            PauseForKey()
            ErrorLevel( 2 )
            RETURN
         ENDIF
         IF t_cARCH $ "win|wce"
            /* Detect cross platform CCPREFIX and CCPATH if embedded MinGW installation is detected */
            FOR tmp := 1 TO Len( aCOMPDET_LOCAL )
               IF aCOMPDET_LOCAL[ tmp ][ 2 ] == t_cARCH .AND. ;
                  aCOMPDET_LOCAL[ tmp ][ 3 ] == t_cCOMP
                  IF ! Empty( tmp1 := Eval( aCOMPDET_LOCAL[ tmp ][ 1 ], aCOMPDET_LOCAL[ tmp ][ 4 ] ) )
                     t_cCCPATH := tmp1
                  ENDIF
                  t_cCCPREFIX := aCOMPDET_LOCAL[ tmp ][ 4 ]
                  EXIT
               ENDIF
            NEXT
         ENDIF
      ENDIF
   ENDIF

   /* Finish detecting bin/lib/include dirs */

   s_aLIBPATH := {}

   IF Empty( s_cHB_BIN_INSTALL )
      s_cHB_BIN_INSTALL := PathNormalize( s_cHB_INSTALL_PREFIX ) + "bin"
   ENDIF
   IF Empty( s_cHB_LIB_INSTALL )
      /* Autodetect multi-compiler/platform lib structure */
      IF hb_DirExists( tmp := PathNormalize( s_cHB_INSTALL_PREFIX ) + "lib" +;
                                               hb_osPathSeparator() + t_cARCH +;
                                               hb_osPathSeparator() + t_cCOMP )
         s_cHB_DYN_INSTALL := PathNormalize( s_cHB_INSTALL_PREFIX ) + "lib"
         s_cHB_LIB_INSTALL := tmp
      ELSE
         s_cHB_LIB_INSTALL := PathNormalize( s_cHB_INSTALL_PREFIX ) + "lib"
      ENDIF
   ENDIF
   IF Empty( s_cHB_INC_INSTALL )
      s_cHB_INC_INSTALL := PathNormalize( s_cHB_INSTALL_PREFIX ) + "include"
   ENDIF

   DEFAULT s_cHB_DYN_INSTALL TO s_cHB_LIB_INSTALL

   IF t_lInfo
      OutStd( "hbmk: Using Harbour: " + s_cHB_BIN_INSTALL + " " + s_cHB_INC_INSTALL + " " + s_cHB_LIB_INSTALL + " " + s_cHB_DYN_INSTALL + hb_osNewLine() )
   ENDIF

   s_cHB_BIN_INSTALL := PathSepToTarget( s_cHB_BIN_INSTALL )
   s_cHB_LIB_INSTALL := PathSepToTarget( s_cHB_LIB_INSTALL )
   s_cHB_DYN_INSTALL := PathSepToTarget( s_cHB_DYN_INSTALL )
   s_cHB_INC_INSTALL := PathSepToTarget( s_cHB_INC_INSTALL )

   /* Add main Harbour library dir to lib path list */
   AAddNotEmpty( s_aLIBPATH, s_cHB_LIB_INSTALL )
   IF ! Empty( s_cHB_DYN_INSTALL ) .AND. !( s_cHB_DYN_INSTALL == s_cHB_LIB_INSTALL )
      AAddNotEmpty( s_aLIBPATH, s_cHB_DYN_INSTALL )
   ENDIF

   /* Process environment */

   IF    Lower( GetEnv( "HB_MT"     ) ) == "mt" ; t_lMT     := .T. ; ENDIF /* Compatibility */
   IF ValueIsT( GetEnv( "HB_MT"     ) )         ; t_lMT     := .T. ; ENDIF
   IF ValueIsT( GetEnv( "HB_GUI"    ) )         ; s_lGUI    := .T. ; ENDIF
   IF ValueIsT( GetEnv( "HB_SHARED" ) )         ; s_lSHARED := .T. ; s_lSTATICFULL := .F. ; ENDIF
   IF ValueIsT( GetEnv( "HB_DEBUG"  ) )         ; t_lDEBUG  := .T. ; ENDIF
   IF ValueIsT( GetEnv( "HB_NULRDD" ) )         ; s_lNULRDD := .T. ; ENDIF

   IF Lower( Left( GetEnv( "HB_GT" ), 2 ) ) == "gt"
      SetupForGT( GetEnv( "HB_GT" ), @s_cGT, @s_lGUI )
   ENDIF

   /* Process command line */

   s_aPRG := {}
   s_aC := {}
   s_aOPTPRG := {}
   s_aOPTC := {}
   s_aOPTL := {}
   s_aOPTA := {}
   s_aOPTD := {}
   s_aOPTRUN := {}
   s_aRESSRC := {}
   s_aRESCMP := {}
   s_aLIBFM := {}
   s_aLIBUSER := {}
   s_aLIBUSERGT := {}
   s_aLIBDYNHAS := {}
   s_aOBJUSER := {}
   s_aOBJA := {}
   s_cPROGDIR := NIL
   s_cPROGNAME := NIL
   s_cFIRST := NIL

   /* Collect all command line parameters */
   aParams := {}
   FOR EACH cParam IN hb_AParams()
      DO CASE
      CASE ( Len( cParam ) >= 1 .AND. Left( cParam, 1 ) == "@" )
         cParam := SubStr( cParam, 2 )
         IF Empty( FN_ExtGet( cParam ) )
            cParam := FN_ExtSet( cParam, ".hbm" )
         ENDIF
         IF !( Lower( FN_ExtGet( cParam ) ) == ".hbm" ) .AND. lAcceptLDClipper
            rtlnk_process( MemoRead( cParam ), @s_cPROGNAME, @s_aOBJUSER, @s_aLIBUSER )
            IF ! Empty( s_aOBJUSER )
               DEFAULT s_cFIRST TO s_aOBJUSER[ 1 ]
            ENDIF
         ELSE
            nEmbedLevel := 1
            HBM_Load( aParams, cParam, @nEmbedLevel ) /* Load parameters from script file */
         ENDIF
      CASE Lower( FN_ExtGet( cParam ) ) == ".hbm"
         nEmbedLevel := 1
         HBM_Load( aParams, cParam, @nEmbedLevel ) /* Load parameters from script file */
      OTHERWISE
         AAdd( aParams, { cParam, "", 0 } )
      ENDCASE
   NEXT

   /* Process command line (1st pass) */
   lNOHBP := .F.
   FOR EACH aParam IN aParams
      IF Lower( aParam[ _PAR_cParam ] ) == "-nohbp"
         lNOHBP := .T.
      ENDIF
   NEXT

   /* Process automatic control files. */
   HBP_ProcessAll( lNOHBP,;
                   @s_aLIBUSER,;
                   @s_aLIBUSERGT,;
                   @s_aLIBPATH,;
                   @s_aLIBDYNHAS,;
                   @s_aOPTPRG,;
                   @s_aOPTC,;
                   @s_aOPTL,;
                   @s_lGUI,;
                   @t_lMT,;
                   @s_lSHARED,;
                   @s_lSTATICFULL,;
                   @t_lDEBUG,;
                   @s_lNULRDD,;
                   @s_lMAP,;
                   @s_lSTRIP,;
                   @s_nCOMPR,;
                   @s_lRUN,;
                   @s_lINC,;
                   @s_cGT )

   /* Build with shared libs by default, if we're installed to default system locations. */

   IF s_lSHARED == NIL
      IF lSysLoc .AND. ( t_cARCH $ "bsd|hpux|sunos|linux" .OR. t_cARCH == "darwin" )
         s_lSHARED := .T.
         s_lSTATICFULL := .F.
      ELSE
         s_lSHARED := .F.
         s_lSTATICFULL := .F.
      ENDIF
   ENDIF

   /* Process command line (2nd pass) */
   FOR EACH aParam IN aParams

      cParam := aParam[ _PAR_cParam ]
      cParamL := Lower( cParam )

      DO CASE
      CASE cParamL            == "-quiet" .OR. ;
           Left( cParamL, 6 ) == "-comp=" .OR. ;
           Left( cParamL, 6 ) == "-arch=" .OR. ;
           cParamL            == "-hbcmp" .OR. ;
           cParamL            == "-hbcc"  .OR. ;
           cParamL            == "-hblnk" .OR. ;
           cParamL            == "-hblib" .OR. ;
           cParamL            == "-hbdyn" .OR. ;
           cParamL            == "-info"

         /* Simply ignore. They were already processed in the first pass. */

      CASE cParamL == "-gui" .OR. ;
           cParamL == "-mwindows"        ; s_lGUI      := .T. /* Compatibility */
      CASE cParamL == "-std" .OR. ;
           cParamL == "-mconsole"        ; s_lGUI      := .F. /* Compatibility */
      CASE cParamL == "-mt"              ; t_lMT       := .T.
      CASE cParamL == "-st"              ; t_lMT       := .F.
      CASE cParamL == "-shared"          ; s_lSHARED   := .T. ; s_lSTATICFULL := .F.
      CASE cParamL == "-static"          ; s_lSHARED   := .F. ; s_lSTATICFULL := .F.
      CASE cParamL == "-fullstatic"      ; s_lSHARED   := .F. ; s_lSTATICFULL := .T.
      CASE cParamL == "-bldf"            ; s_lBLDFLGP  := s_lBLDFLGC := s_lBLDFLGL := .T.
      CASE cParamL == "-bldf-"           ; s_lBLDFLGP  := s_lBLDFLGC := s_lBLDFLGL := .F.
      CASE Left( cParamL, 6 ) == "-bldf="
         cParam := SubStr( cParam, 7 )
         s_lBLDFLGP := "p" $ cParam
         s_lBLDFLGC := "c" $ cParam
         s_lBLDFLGL := "l" $ cParam
      CASE cParamL == "-debug"           ; t_lDEBUG    := .T.
      CASE cParamL == "-debug-" .OR. ;
           cParamL == "-nodebug"         ; t_lDEBUG    := .F.
      CASE cParamL == "-debuginc"        ; s_lDEBUGINC := .T.
      CASE cParamL == "-debuginc-" .OR. ;
           cParamL == "-nodebuginc"      ; s_lDEBUGINC := .F.
      CASE cParamL == "-nulrdd"          ; s_lNULRDD   := .T.
      CASE cParamL == "-nulrdd-"         ; s_lNULRDD   := .F.
      CASE cParamL == "-map"             ; s_lMAP      := .T.
      CASE cParamL == "-map-" .OR. ;
           cParamL == "-nomap"           ; s_lMAP      := .F.
      CASE cParamL == "-rebuild"         ; s_lINC      := .T. ; s_lREBUILD := .T.
      CASE cParamL == "-clean"           ; s_lINC      := .T. ; s_lCLEAN := .T.
      CASE cParamL == "-inc"             ; s_lINC      := .T.
      CASE cParamL == "-inc-" .OR. ;
           cParamL == "-noinc"           ; s_lINC      := .F.
      CASE cParamL == "-fmstat"          ; s_lFMSTAT   := .T.
      CASE cParamL == "-fmstat-" .OR. ;
           cParamL == "-nofmstat"        ; s_lFMSTAT   := .F.
      CASE cParamL == "-strip"           ; s_lSTRIP    := .T.
      CASE cParamL == "-strip-" .OR. ;
           cParamL == "-nostrip"         ; s_lSTRIP    := .F.
      CASE cParamL == "-compr" .OR. ;
           Left( cParamL, 7 ) == "-compr="

           DO CASE
           CASE SubStr( cParamL, 8 ) == "min" ; s_nCOMPR := _COMPR_MIN
           CASE SubStr( cParamL, 8 ) == "max" ; s_nCOMPR := _COMPR_MAX
           OTHERWISE                          ; s_nCOMPR := _COMPR_DEF
           ENDCASE

      CASE cParamL == "-compr-" .OR. ;
           cParamL == "-nocompr"         ; s_nCOMPR    := _COMPR_OFF
      CASE cParamL == "-run"             ; s_lRUN      := .T.
      CASE cParamL == "-run-" .OR. ;
           cParamL == "-norun"           ; s_lRUN      := .F.
      CASE cParamL == "-trace"           ; s_lTRACE    := .T.
      CASE cParamL == "-trace-" .OR. ;
           cParamL == "-notrace"         ; s_lTRACE    := .F.
      CASE cParamL == "-traceonly"       ; s_lTRACE    := .T. ; s_lDONTEXEC := .T.

      CASE cParamL == "--hbdirbin"       ; lStopAfterInit := .T.

         OutStd( s_cHB_BIN_INSTALL )

      CASE cParamL == "--hbdirdyn"       ; lStopAfterInit := .T.

         OutStd( s_cHB_DYN_INSTALL )

      CASE cParamL == "--hbdirlib"       ; lStopAfterInit := .T.

         OutStd( s_cHB_LIB_INSTALL )

      CASE cParamL == "--hbdirinc"       ; lStopAfterInit := .T.

         OutStd( s_cHB_INC_INSTALL )

      CASE Left( cParamL, 6 ) == "-main="

         IF IsValidHarbourID( cParam := SubStr( cParam, 7 ) )
            s_cMAIN := "@" + cParam
         ELSE
            OutErr( "hbmk: Warning: Invalid -main value ignored: " + cParam + hb_osNewLine() )
         ENDIF

      CASE Left( cParamL, 3 ) == "-gt"

         cParam := ArchCompFilter( SubStr( cParam, 2 ) )
         IF s_cGT == NIL
            IF ! SetupForGT( cParam, @s_cGT, @s_lGUI )
               OutErr( "hbmk: Warning: Invalid -gt value ignored: " + cParam + hb_osNewLine() )
               cParam := NIL
            ENDIF
         ENDIF
         IF ! Empty( cParam ) .AND. !( Lower( cParam ) == "gtnul" )
            IF AScan( t_aLIBCOREGT, {|tmp| Lower( tmp ) == cParamL } ) == 0 .AND. ;
               AScan( s_aLIBUSERGT, {|tmp| Lower( tmp ) == cParamL } ) == 0
               AAddNotEmpty( s_aLIBUSERGT, PathSepToTarget( cParam ) )
            ENDIF
         ENDIF

      CASE ! lNIX .AND. Left( cParamL, 2 ) == "/o" .AND. ! lStopAfterHarbour

         /* Swallow this switch. We don't pass it to Harbour, as it may badly
            interact with hbmk. */

      CASE Left( cParam, 2 ) == "-o" .AND. ! lStopAfterHarbour

         tmp := PathSepToSelf( SubStr( cParam, 3 ) )
         hb_FNameSplit( tmp, @cDir, @cName, @cExt )
         IF ! Empty( cDir ) .AND. Empty( cName ) .AND. Empty( cExt )
            /* Only a dir was passed, let's store that and pick a default name later. */
            s_cPROGDIR := cDir
         ELSEIF ! Empty( tmp )
            s_cPROGDIR := NIL
            s_cPROGNAME := tmp
         ELSE
            s_cPROGDIR := NIL
            s_cPROGNAME := NIL
         ENDIF

      CASE Left( cParam, 2 ) == "-L" .AND. ;
           Len( cParam ) > 2

         cParam := ArchCompFilter( SubStr( cParam, 3 ) )
         IF ! Empty( cParam )
            AAdd( s_aLIBPATH, PathSepToTarget( cParam ) )
         ENDIF

      CASE Left( cParamL, Len( "-prgflag:" ) ) == "-prgflag:"

         cParam := ArchCompFilter( SubStr( cParam, Len( "-prgflag:" ) + 1 ) )
         IF Left( cParam, 1 ) $ cOptPrefix
            IF SubStr( cParamL, 2 ) == "gh"
               lStopAfterHarbour := .T.
            ENDIF
            IF !( SubStr( cParamL, 2, 1 ) == "o" )
               AAdd( s_aOPTPRG , PathSepToTarget( cParam, 2 ) )
            ENDIF
         ENDIF

      CASE Left( cParamL, Len( "-cflag:" ) ) == "-cflag:"

         cParam := ArchCompFilter( SubStr( cParam, Len( "-cflag:" ) + 1 ) )
         IF Left( cParam, 1 ) $ cOptPrefix
            AAdd( s_aOPTC   , PathSepToTarget( cParam, 2 ) )
         ENDIF

      CASE Left( cParamL, Len( "-ldflag:" ) ) == "-ldflag:"

         cParam := ArchCompFilter( SubStr( cParam, Len( "-ldflag:" ) + 1 ) )
         IF Left( cParam, 1 ) $ cOptPrefix
            AAdd( s_aOPTL   , PathSepToTarget( cParam, 2 ) )
         ENDIF

      CASE Left( cParamL, Len( "-dflag:" ) ) == "-dflag:"

         cParam := ArchCompFilter( SubStr( cParam, Len( "-dflag:" ) + 1 ) )
         IF Left( cParam, 1 ) $ cOptPrefix
            AAdd( s_aOPTD   , PathSepToTarget( cParam, 2 ) )
         ENDIF

      CASE Left( cParamL, Len( "-aflag:" ) ) == "-aflag:"

         cParam := ArchCompFilter( SubStr( cParam, Len( "-aflag:" ) + 1 ) )
         IF Left( cParam, 1 ) $ cOptPrefix
            AAdd( s_aOPTA   , PathSepToTarget( cParam, 2 ) )
         ENDIF

      CASE Left( cParamL, Len( "-runflag:" ) ) == "-runflag:"

         cParam := ArchCompFilter( SubStr( cParam, Len( "-runflag:" ) + 1 ) )
         IF Left( cParam, 1 ) $ cOptPrefix
            AAdd( s_aOPTRUN , PathSepToTarget( cParam, 2 ) )
         ENDIF

      CASE Left( cParamL, Len( "-workdir:" ) ) == "-workdir:"

         cWorkDir := PathSepToTarget( ArchCompFilter( SubStr( cParam, Len( "-workdir:" ) + 1 ) ) )

      CASE Left( cParam, 2 ) == "-l" .AND. ;
           Len( cParam ) > 2 .AND. ;
           !( Left( cParam, 3 ) == "-l-" )

         cParam := ArchCompFilter( SubStr( cParam, 3 ) )
         IF ! Empty( cParam )
            AAdd( s_aLIBUSER, PathSepToTarget( cParam ) )
         ENDIF

      CASE Left( cParam, 1 ) $ cOptPrefix

         DO CASE
         CASE lAcceptLDFlag
            AAddNotEmpty( s_aOPTL   , ArchCompFilter( PathSepToTarget( cParam, 2 ) ) )
         CASE lAcceptCFlag
            IF SubStr( cParamL, 2 ) == "c"
               lStopAfterCComp := .T.
            ELSE
               AAddNotEmpty( s_aOPTC   , ArchCompFilter( PathSepToTarget( cParam, 2 ) ) )
            ENDIF
         OTHERWISE
            IF SubStr( cParamL, 2 ) == "gh"
               lStopAfterHarbour := .T.
            ENDIF
            AAddNotEmpty( s_aOPTPRG , PathSepToTarget( cParam, 2 ) )
         ENDCASE

      CASE FN_ExtGet( cParamL ) == ".lib" .OR. ;
           FN_ExtGet( cParamL ) == cDynLibExt

         cParam := ArchCompFilter( cParam )
         IF ! Empty( cParam )
            AAdd( s_aLIBUSER, PathSepToTarget( cParam ) )
         ENDIF

      CASE FN_ExtGet( cParamL ) == ".hbp"

         HBP_ProcessOne( cParam,;
            @s_aLIBUSER,;
            @s_aLIBUSERGT,;
            @s_aLIBPATH,;
            @s_aLIBDYNHAS,;
            @s_aOPTPRG,;
            @s_aOPTC,;
            @s_aOPTL,;
            @s_lGUI,;
            @t_lMT,;
            @s_lSHARED,;
            @s_lSTATICFULL,;
            @t_lDEBUG,;
            @s_lNULRDD,;
            @s_lMAP,;
            @s_lSTRIP,;
            @s_nCOMPR,;
            @s_lRUN,;
            @s_lINC,;
            @s_cGT )

      CASE FN_ExtGet( cParamL ) == ".prg"

         cParam := PathProc( cParam, aParam[ _PAR_cFileName ] )
         AAdd( s_aPRG    , PathSepToTarget( cParam ) )
         DEFAULT s_cFIRST TO PathSepToSelf( cParam )

      CASE FN_ExtGet( cParamL ) == ".rc"

         cParam := PathProc( cParam, aParam[ _PAR_cFileName ] )
         AAdd( s_aRESSRC , PathSepToTarget( cParam ) )

      CASE FN_ExtGet( cParamL ) == ".res"

         IF t_cCOMP $ "mingw|mingw64|mingwarm"
            /* For MinGW family add .res files as source input, as they
               will need to be converted to coff format with windres (just
               like plain .rc files) before feeding them to gcc. */
            cParam := PathProc( cParam, aParam[ _PAR_cFileName ] )
            AAdd( s_aRESSRC , PathSepToTarget( cParam ) )
         ELSE
            cParam := PathProc( cParam, aParam[ _PAR_cFileName ] )
            AAdd( s_aRESCMP , PathSepToTarget( cParam ) )
         ENDIF

      CASE FN_ExtGet( cParamL ) == ".a"

         cParam := PathProc( cParam, aParam[ _PAR_cFileName ] )
         AAdd( s_aOBJA   , PathSepToTarget( cParam ) )

      CASE FN_ExtGet( cParamL ) $ ".o|.obj"

         cParam := PathProc( cParam, aParam[ _PAR_cFileName ] )
         AAdd( s_aOBJUSER, PathSepToTarget( cParam ) )
         DEFAULT s_cFIRST TO PathSepToSelf( cParam )

      CASE FN_ExtGet( cParamL ) $ ".c|.cpp"

         cParam := PathProc( cParam, aParam[ _PAR_cFileName ] )
         AAdd( s_aC      , PathSepToTarget( cParam ) )
         DEFAULT s_cFIRST TO PathSepToSelf( cParam )

      OTHERWISE

         IF ! Empty( cParam )
            cParam := PathProc( cParam, aParam[ _PAR_cFileName ] )
            AAdd( s_aPRG    , PathSepToTarget( cParam ) )
            DEFAULT s_cFIRST TO PathSepToSelf( cParam )
         ENDIF

      ENDCASE
   NEXT

   /* Start doing the make process. */
   IF ! lStopAfterInit .AND. ( Len( s_aPRG ) + Len( s_aC ) + Len( s_aOBJUSER ) + Len( s_aOBJA ) ) == 0
      OutErr( "hbmk: Error: No source files were specified." + hb_osNewLine() )
      PauseForKey()
      ErrorLevel( 4 )
      RETURN
   ENDIF

   IF s_lINC
      IF cWorkDir == NIL
         cWorkDir := t_cARCH + hb_osPathSeparator() + t_cCOMP
      ENDIF
      AAdd( s_aOPTPRG, "-o" + cWorkDir + hb_osPathSeparator() ) /* NOTE: Ending path sep is important. */
      IF ! DirBuild( cWorkDir )
         OutErr( "hbmk: Error: Working directory cannot be created: " + cWorkDir + hb_osNewLine() )
         PauseForKey()
         ErrorLevel( 9 )
         RETURN
      ENDIF
   ELSE
      cWorkDir := ""
   ENDIF

   IF ! lStopAfterInit .AND. ! lStopAfterHarbour

      /* If -o with full name wasn't specified, let's
         make it the first source file specified. */
      DEFAULT s_cPROGNAME TO FN_NameGet( s_cFIRST )

      /* Combine output dir with output name. */
      IF ! Empty( s_cPROGDIR )
         hb_FNameSplit( s_cPROGNAME, @cDir, @cName, @cExt )
         s_cPROGNAME := hb_FNameMerge( iif( Empty( cDir ), s_cPROGDIR, cDir ), cName, cExt )
      ENDIF

      /* Incremental */

      IF s_lINC .AND. ! s_lREBUILD
         s_aPRG_TODO := {}
         FOR EACH tmp IN s_aPRG
            IF s_lDEBUGINC
               OutStd( "hbmk: debuginc: PRG", FN_ExtSet( tmp, ".prg" ),;
                                              FN_DirExtSet( tmp, cWorkDir, ".c" ), hb_osNewLine() )
            ENDIF
            IF ! hb_FGetDateTime( FN_ExtSet( tmp, ".prg" ), @tmp1 ) .OR. ;
               ! hb_FGetDateTime( FN_DirExtSet( tmp, cWorkDir, ".c" ), @tmp2 ) .OR. ;
               tmp1 > tmp2
               AAdd( s_aPRG_TODO, tmp )
            ENDIF
         NEXT
      ELSE
         s_aPRG_TODO := s_aPRG
      ENDIF
   ELSE
      s_aPRG_TODO := s_aPRG
   ENDIF

   /* Harbour compilation */

   IF ! lStopAfterInit .AND. Len( s_aPRG_TODO ) > 0 .AND. ! s_lCLEAN

      IF s_lINC .AND. ! t_lQuiet
         OutStd( "Compiling Harbour sources..." + hb_osNewLine() )
      ENDIF

      PlatformPRGFlags( s_aOPTPRG )

#if defined( HBMK_INTEGRATED_COMPILER )
      aCommand := ArrayAJoin( { { iif( lCreateLib .OR. lCreateDyn, "-n1", "-n2" ) },;
                                s_aPRG_TODO,;
                                { "-i" + s_cHB_INC_INSTALL },;
                                iif( s_lBLDFLGP, { " " + cSelfFlagPRG }, {} ),;
                                ListToArray( iif( ! Empty( GetEnv( "HB_USER_PRGFLAGS" ) ), " " + GetEnv( "HB_USER_PRGFLAGS" ), "" ) ),;
                                s_aOPTPRG } )

      IF s_lTRACE
         IF ! t_lQuiet
            OutStd( "hbmk: Harbour compiler command (internal):" + hb_osNewLine() )
         ENDIF
         OutStd( DirAddPathSep( PathSepToSelf( s_cHB_BIN_INSTALL ) ) + cBin_CompPRG +;
                 " " + ArrayToList( aCommand ) + hb_osNewLine() )
      ENDIF

      IF ! s_lDONTEXEC .AND. ( tmp := hb_compile( "", aCommand ) ) != 0
         OutErr( "hbmk: Error: Running Harbour compiler. " + hb_ntos( tmp ) + hb_osNewLine() )
         OutErr( ArrayToList( aCommand ) + hb_osNewLine() )
         PauseForKey()
         ErrorLevel( 6 )
         RETURN
      ENDIF
#else
      cCommand := DirAddPathSep( PathSepToSelf( s_cHB_BIN_INSTALL ) ) +;
                  cBin_CompPRG +;
                  " " + iif( lCreateLib .OR. lCreateDyn, "-n1", "-n2" ) +;
                  " " + ArrayToList( s_aPRG_TODO ) +;
                  " -i" + s_cHB_INC_INSTALL +;
                  iif( s_lBLDFLGP, " " + cSelfFlagPRG, "" ) +;
                  iif( ! Empty( GetEnv( "HB_USER_PRGFLAGS" ) ), " " + GetEnv( "HB_USER_PRGFLAGS" ), "" ) +;
                  iif( ! Empty( s_aOPTPRG ), " " + ArrayToList( s_aOPTPRG ), "" )

      cCommand := AllTrim( cCommand )

      IF s_lTRACE
         IF ! t_lQuiet
            OutStd( "hbmk: Harbour compiler command:" + hb_osNewLine() )
         ENDIF
         OutStd( cCommand + hb_osNewLine() )
      ENDIF

      IF ! s_lDONTEXEC .AND. ( tmp := hb_run( cCommand ) ) != 0
         OutErr( "hbmk: Error: Running Harbour compiler. " + hb_ntos( tmp ) + ":" + hb_osNewLine() )
         OutErr( cCommand + hb_osNewLine() )
         PauseForKey()
         ErrorLevel( 6 )
         RETURN
      ENDIF
#endif
   ENDIF

   IF ! lStopAfterInit .AND. ! lStopAfterHarbour

      IF s_cGT == t_cGTDEFAULT
         s_cGT := NIL
      ENDIF

      /* Merge user libs from command line and envvar. Command line has priority. */
      s_aLIBUSER := ArrayAJoin( { s_aLIBUSER, s_aLIBUSERGT, ListToArray( PathSepToTarget( GetEnv( "HB_USER_LIBS" ) ) ) } )

      IF lSysLoc
         cPrefix := ""
      ELSE
         cPrefix := PathNormalize( s_cHB_DYN_INSTALL )
      ENDIF
#if 1
      cPostfix := ""
      HB_SYMBOL_UNUSED( cDL_Version )
#else
      cPostfix := "-" + cDL_Version
#endif

      DO CASE
      CASE t_cARCH $ "bsd|linux|hpux|sunos" .OR. t_cARCH == "darwin" /* Separated to avoid match with 'win' */
         IF Empty( cPrefix )
            s_aLIBSHARED := { iif( t_lMT, "harbourmt" + cPostfix,;
                                          "harbour"   + cPostfix ) }
         ELSE
            s_aLIBSHARED := { iif( t_lMT, cPrefix + cDynLibNamePrefix + "harbourmt" + cPostfix + cDynLibExt,;
                                          cPrefix + cDynLibNamePrefix + "harbour"   + cPostfix + cDynLibExt ) }
         ENDIF
      CASE t_cARCH $ "os2|win|wce"
         s_aLIBSHARED := { iif( t_lMT, cDynLibNamePrefix + "harbourmt",;
                                       cDynLibNamePrefix + "harbour" ) }
      OTHERWISE
         s_aLIBSHARED := NIL
      ENDCASE

      /* C compilation/linking */

      s_aLIB3RD := {}
      s_aLIBSYS := {}
      s_aCLEAN := {}

      /* Command macros:

         {LC}     list of C files
         {LR}     list of resource source files (Windows specific)
         {LS}     list of resource binary files (Windows specific)
         {LO}     list of object files
         {LA}     list of object archive (.a) files
         {LL}     list of lib files
         {FC}     flags for C compiler (user + automatic)
         {FL}     flags for linker (user + automatic)
         {OW}     working dir (when in -inc mode)
         {OD}     output dir
         {OO}     output object (when in -hbcmp mode)
         {OE}     output executable
         {OM}     output map name
         {DB}     dir for binaries
         {DI}     dir for includes
         {DL}     dir for libs
         {SCRIPT} save command line to script and pass it to command as @<filename>
      */

      /* Assemble library list */

      s_aLIBVM := iif( t_lMT, aLIB_BASE_MT, aLIB_BASE_ST )
      aLIB_BASE2 := ArrayAJoin( { aLIB_BASE2, t_aLIBCOREGT } )

      IF ! Empty( s_cGT ) .AND. !( Lower( s_cGT ) == "gtnul" )
         IF AScan( aLIB_BASE2, {|tmp| Lower( tmp ) == Lower( s_cGT ) } ) == 0
            AAdd( aLIB_BASE2, s_cGT )
         ENDIF
      ENDIF

      DO CASE
      /* GCC family */
      CASE ( t_cARCH == "bsd"    .AND. t_cCOMP == "gcc" ) .OR. ;
           ( t_cARCH == "darwin" .AND. t_cCOMP == "gcc" ) .OR. ;
           ( t_cARCH == "hpux"   .AND. t_cCOMP == "gcc" ) .OR. ;
           ( t_cARCH == "sunos"  .AND. t_cCOMP == "gcc" ) .OR. ;
           ( t_cARCH == "linux"  .AND. t_cCOMP == "gcc" ) .OR. ;
           ( t_cARCH == "linux"  .AND. t_cCOMP == "gpp" )

         cLibLibPrefix := "lib"
         cLibPrefix := "-l"
         cLibExt := ""
         cObjExt := ".o"
         cBin_Lib := t_cCCPREFIX + "ar"
         cOpt_Lib := "{FA} rcs {OL} {LO}"
         cBin_CompC := t_cCCPREFIX + iif( t_cCOMP == "gpp" .OR. s_lCPP, "g++", "gcc" )
         cOpt_CompC := "-c -O3 {FC} -I{DI}"
         IF s_lINC .AND. ! Empty( cWorkDir )
            cOpt_CompC += " {IC} -o {OO}"
         ELSE
            cOpt_CompC += " {LC}"
         ENDIF
         cBin_Link := cBin_CompC
         cOpt_Link := "{LO} {LA} {FL} {DL}"
         IF ! Empty( t_cCCPATH )
            cBin_Lib   := t_cCCPATH + "/" + cBin_Lib
            cBin_CompC := t_cCCPATH + "/" + cBin_CompC
            cBin_Link  := t_cCCPATH + "/" + cBin_Link
         ENDIF
         cLibPathPrefix := "-L"
         cLibPathSep := " "
         IF ! lStopAfterCComp
            IF t_cARCH == "linux" .OR. ;
               t_cARCH == "bsd"
               AAdd( s_aOPTL, "-Wl,--start-group {LL} -Wl,--end-group" )
            ELSE
               AAdd( s_aOPTL, "{LL}" )
               aLIB_BASE2 := ArrayAJoin( { aLIB_BASE2, { "hbcommon", "hbrtl" }, s_aLIBVM } )
            ENDIF
         ENDIF
         IF s_lMAP
            AAdd( s_aOPTL, "-Wl,-Map {OM}" )
         ENDIF
         IF s_lSTATICFULL
            AAdd( s_aOPTL, "-static" )
         ENDIF
         IF t_cARCH == "darwin"
            AAdd( s_aOPTC, "-no-cpp-precomp" )
            AAdd( s_aOPTC, "-Wno-long-double" )
            IF s_lSHARED
               AAdd( s_aOPTC, "-bind_at_load" )
               AAdd( s_aOPTC, "-multiply_defined suppress" )
            ENDIF
         ENDIF
         IF s_lSTRIP .AND. !( t_cARCH == "sunos" )
            AAdd( s_aOPTC, "-s" )
         ENDIF
         IF lStopAfterCComp
            IF ! lCreateLib .AND. ! lCreateDyn .AND. ( Len( s_aPRG ) + Len( s_aC ) ) == 1
               IF t_cARCH == "darwin"
                  AAdd( s_aOPTL, "-o {OO}" )
               ELSE
                  AAdd( s_aOPTL, "-o{OO}" )
               ENDIF
            ENDIF
         ELSE
            IF t_cARCH == "darwin"
               AAdd( s_aOPTL, "-o {OE}" )
            ELSE
               AAdd( s_aOPTL, "-o{OE}" )
            ENDIF
         ENDIF

         /* Always inherit/reproduce some flags from self */

         IF     "-mlp64" $ cSelfFlagC ; AAdd( s_aOPTC, "-mlp64" )
         ELSEIF "-mlp32" $ cSelfFlagC ; AAdd( s_aOPTC, "-mlp32" )
         ELSEIF "-m64"   $ cSelfFlagC ; AAdd( s_aOPTC, "-m64" )
         ELSEIF "-m32"   $ cSelfFlagC ; AAdd( s_aOPTC, "-m32" )
         ENDIF

         IF     "-fPIC"  $ cSelfFlagC ; AAdd( s_aOPTC, "-fPIC" )
         ELSEIF "-fpic"  $ cSelfFlagC ; AAdd( s_aOPTC, "-fpic" )
         ENDIF

         DO CASE
         CASE "-DHB_PCRE_REGEX" $ cSelfFlagC
            AAdd( s_aLIBSYS, "pcre" )
            s_lHB_PCRE := .F.
         CASE "-DHB_POSIX_REGEX" $ cSelfFlagC
            s_lHB_PCRE := .F.
         ENDCASE
         IF "-DHB_EXT_ZLIB" $ cSelfFlagC
            AAdd( s_aLIBSYS, "z" )
            s_lHB_ZLIB := .F.
         ENDIF
         IF "-DHAVE_GPM_H" $ cSelfFlagC
            AAdd( s_aLIBSYS, "gpm" )
         ENDIF

         /* Add system libraries */
         IF ! s_lSHARED
            AAdd( s_aLIBSYS, "m" )
            IF t_lMT
               AAdd( s_aLIBSYS, "pthread" )
            ENDIF
            DO CASE
            CASE t_cARCH == "linux"
               AAdd( s_aLIBSYS, "dl" )
               AAdd( s_aLIBSYS, "rt" )
            CASE t_cARCH == "sunos"
               AAdd( s_aLIBSYS, "rt" )
               AAdd( s_aLIBSYS, "socket" )
               AAdd( s_aLIBSYS, "nsl" )
               AAdd( s_aLIBSYS, "resolv" )
            CASE t_cARCH == "hpux"
               AAdd( s_aLIBSYS, "rt" )
            ENDCASE
         ENDIF

         IF IsGTRequested( s_cGT, s_aLIBUSERGT, s_aLIBDYNHAS, s_lSHARED, "gtcrs" )
            /* TOFIX: Sometimes 'ncur194' is needed. */
            AAdd( s_aLIBSYS, IIF( t_cARCH == "sunos", "curses", "ncurses" ) )
         ENDIF
         IF IsGTRequested( s_cGT, s_aLIBUSERGT, s_aLIBDYNHAS, s_lSHARED, "gtsln" )
            AAdd( s_aLIBSYS, "slang" )
            /* Add paths, where this isn't a system component */
            DO CASE
            CASE t_cARCH == "darwin"
               AAdd( s_aLIBPATH, "/sw/lib" )
               AAdd( s_aLIBPATH, "/opt/local/lib" )
            CASE t_cARCH == "bsd"
               AAdd( s_aLIBPATH, "/usr/local/lib" )
            ENDCASE
         ENDIF
         IF IsGTRequested( s_cGT, s_aLIBUSERGT, s_aLIBDYNHAS, s_lSHARED, "gtxwc" )
            IF hb_DirExists( "/usr/X11R6/lib64" )
               AAdd( s_aLIBPATH, "/usr/X11R6/lib64" )
            ENDIF
            AAdd( s_aLIBPATH, "/usr/X11R6/lib" )
            AAdd( s_aLIBSYS, "X11" )
         ENDIF

         IF s_lFMSTAT != NIL .AND. s_lFMSTAT
            AAdd( s_aLIBFM, iif( t_lMT, "hbfmmt", "hbfm" ) )
         ENDIF

      CASE ( t_cARCH == "win" .AND. t_cCOMP == "gcc" ) .OR. ;
           ( t_cARCH == "win" .AND. t_cCOMP == "mingw" ) .OR. ;
           ( t_cARCH == "win" .AND. t_cCOMP == "mingw64" ) .OR. ;
           ( t_cARCH == "wce" .AND. t_cCOMP == "mingwarm" ) .OR. ;
           ( t_cARCH == "win" .AND. t_cCOMP == "cygwin" )

         cLibLibPrefix := "lib"
         cLibPrefix := "-l"
         cLibExt := ""
         cObjExt := ".o"
         cBin_CompC := t_cCCPREFIX + "gcc.exe"
         cOpt_CompC := "-c -O3 {FC} -I{DI}"
         IF s_lINC .AND. ! Empty( cWorkDir )
            cOpt_CompC += " {IC} -o {OO}"
         ELSE
            cOpt_CompC += " {LC}"
         ENDIF
         cBin_Link := cBin_CompC
         cOpt_Link := "{LO} {LA} {LS} {FL} {DL}"
         cLibPathPrefix := "-L"
         cLibPathSep := " "
         cLibLibExt := ".a"
         cBin_Lib := t_cCCPREFIX + "ar.exe"
         cOpt_Lib := "{FA} rcs {OL} {LO}"
         cLibObjPrefix := NIL
         IF ! Empty( t_cCCPATH )
            cBin_Lib   := t_cCCPATH + "\" + cBin_Lib
            cBin_CompC := t_cCCPATH + "\" + cBin_CompC
            cBin_Link  := t_cCCPATH + "\" + cBin_Link
         ENDIF
         IF !( t_cARCH == "wce" )
            IF s_lGUI
               AAdd( s_aOPTL, "-mwindows" )
            ELSE
               AAdd( s_aOPTL, "-mconsole" )
            ENDIF
         ENDIF
         IF s_lMAP
            AAdd( s_aOPTL, "-Wl,-Map {OM}" )
         ENDIF
         IF s_lSHARED
            AAdd( s_aLIBPATH, "{DB}" )
         ENDIF
         IF ! lStopAfterCComp
            IF t_cCOMP $ "mingw|mingw64|mingwarm"
               AAdd( s_aOPTL, "-Wl,--start-group {LL} -Wl,--end-group" )
            ELSE
               AAdd( s_aOPTL, "{LL}" )
               aLIB_BASE2 := ArrayAJoin( { aLIB_BASE2, { "hbcommon", "hbrtl" }, s_aLIBVM } )
            ENDIF
         ENDIF
         IF s_lSTRIP
            AAdd( s_aOPTC, "-s" )
         ENDIF
         IF lStopAfterCComp
            IF ! lCreateLib .AND. ! lCreateDyn .AND. ( Len( s_aPRG ) + Len( s_aC ) ) == 1
               AAdd( s_aOPTL, "-o{OO}" )
            ENDIF
         ELSE
            AAdd( s_aOPTL, "-o{OE}" )
         ENDIF
         IF ! s_lSHARED
            s_aLIBSYS := ArrayAJoin( { s_aLIBSYS, s_aLIBSYSCORE, s_aLIBSYSMISC } )
         ENDIF
         DO CASE
         CASE t_cCOMP == "mingw64"
            s_aLIBSHARED := { iif( t_lMT, "harbourmt-" + cDL_Version_Alter + "-x64",;
                                          "harbour-" + cDL_Version_Alter + "-x64" ) }
         CASE t_cCOMP == "mingwarm"
            s_aLIBSHARED := { iif( t_lMT, "harbourmt-" + cDL_Version_Alter + "-arm",;
                                          "harbour-" + cDL_Version_Alter + "-arm" ) }
         OTHERWISE
            s_aLIBSHARED := { iif( t_lMT, "harbourmt-" + cDL_Version_Alter,;
                                          "harbour-" + cDL_Version_Alter ) }
         ENDCASE

         s_aLIBSHAREDPOST := { "hbmainstd", "hbmainwin" }

         IF s_lFMSTAT != NIL .AND. s_lFMSTAT
            AAdd( s_aLIBFM, iif( t_lMT, "hbfmmt", "hbfm" ) )
         ENDIF

         IF t_cCOMP $ "mingw|mingw64|mingwarm" .AND. Len( s_aRESSRC ) > 0
            cBin_Res := t_cCCPREFIX + "windres"
            cResExt := ".reso"
            cOpt_Res := "{IR} -O coff -o {OS}"
            IF ! Empty( t_cCCPATH )
               cBin_Res := t_cCCPATH + "\" + cBin_Res
            ENDIF
         ENDIF

      CASE t_cARCH == "os2" .AND. t_cCOMP == "gcc"

         cLibLibPrefix := "lib"
         cLibPrefix := "-l"
         cLibExt := ""
         cObjExt := ".o"
         cBin_CompC := "gcc.exe"
         cOpt_CompC := "-c -O3 {FC} -I{DI}"
         IF s_lINC .AND. ! Empty( cWorkDir )
            cOpt_CompC += " {IC} -o {OO}"
         ELSE
            cOpt_CompC += " {LC}"
         ENDIF
         cBin_Link := cBin_CompC
         cOpt_Link := "{LO} {LA} {FL} {DL}"
         cLibPathPrefix := "-L"
         cLibPathSep := " "
         IF s_lMAP
            AAdd( s_aOPTL, "-Wl,-Map {OM}" )
         ENDIF
         IF s_lSHARED
            AAdd( s_aLIBPATH, "{DB}" )
         ENDIF
         AAdd( s_aOPTL, "{LL}" )
         aLIB_BASE2 := ArrayAJoin( { aLIB_BASE2, { "hbcommon", "hbrtl" }, s_aLIBVM } )
         IF ! s_lSHARED
            s_aLIBSYS := ArrayJoin( s_aLIBSYS, { "socket" } )
         ENDIF
         IF s_lSTRIP
            AAdd( s_aOPTC, "-s" )
         ENDIF
         /* OS/2 needs a space between -o and file name following it */
         IF lStopAfterCComp
            IF ! lCreateLib .AND. ! lCreateDyn .AND. ( Len( s_aPRG ) + Len( s_aC ) ) == 1
               AAdd( s_aOPTL, "-o {OO}" )
            ENDIF
         ELSE
            AAdd( s_aOPTL, "-o {OE}" )
         ENDIF

         IF s_lFMSTAT != NIL .AND. s_lFMSTAT
            AAdd( s_aLIBFM, iif( t_lMT, "hbfmmt", "hbfm" ) )
         ENDIF

      CASE t_cARCH == "dos" .AND. t_cCOMP == "djgpp"

         cLibLibPrefix := "lib"
         cLibPrefix := "-l"
         cLibExt := ""
         cObjExt := ".o"
         cBin_CompC := "gcc.exe"
         cOpt_CompC := "-c -O3 {FC} -I{DI}"
         IF s_lINC .AND. ! Empty( cWorkDir )
            cOpt_CompC += " {IC} -o {OO}"
         ELSE
            cOpt_CompC += " {LC}{SCRIPT}"
         ENDIF
         cBin_Link := cBin_CompC
         cOpt_Link := "{LO} {LA} {FL} {DL}{SCRIPT}"
         cLibPathPrefix := "-L"
         cLibPathSep := " "
         IF t_cCOMP == "djgpp"
            AAdd( s_aOPTL, "-Wl,--start-group {LL} -Wl,--end-group" )
         ELSE
            AAdd( s_aOPTL, "{LL}" )
            aLIB_BASE2 := ArrayAJoin( { aLIB_BASE2, { "hbcommon", "hbrtl" }, s_aLIBVM } )
         ENDIF
         IF ! s_lSHARED
            s_aLIBSYS := ArrayJoin( s_aLIBSYS, { "m" } )
         ENDIF
         IF s_lSTRIP
            AAdd( s_aOPTC, "-s" )
         ENDIF
         IF lStopAfterCComp
            IF ! lCreateLib .AND. ! lCreateDyn .AND. ( Len( s_aPRG ) + Len( s_aC ) ) == 1
               AAdd( s_aOPTL, "-o{OO}" )
            ENDIF
         ELSE
            AAdd( s_aOPTL, "-o{OE}" )
         ENDIF

         IF s_lFMSTAT != NIL .AND. s_lFMSTAT
            AAdd( s_aLIBFM, "hbfm" )
         ENDIF

      /* Watcom family */
      CASE t_cARCH == "dos" .AND. t_cCOMP == "owatcom"
         cLibPrefix := "LIB "
         cLibExt := ".lib"
         cObjPrefix := "FILE "
         cObjExt := ".obj"
         cLibPathPrefix := "LIBPATH "
         cLibPathSep := " "
         cBin_CompC := "wpp386.exe"
         cOpt_CompC := "-zq -5r -fp5 -onaehtri+ -s -ei -zp4 -zt0 -bt=DOS {FC} -i{DI}"
         IF s_lINC .AND. ! Empty( cWorkDir )
            cOpt_CompC += " {IC} -fo={OO}"
         ELSE
            cOpt_CompC += " {LC}"
         ENDIF
         cBin_Link := "wlink.exe"
         cOpt_Link := "SYS causeway {FL} NAME {OE} {LO} {DL} {LL}{SCRIPT}"
         cBin_Lib := "wlib.exe"
         cOpt_Lib := "{FA} {OL} {LO}{SCRIPT}"
         cLibLibExt := cLibExt
         cLibObjPrefix := "-+ "
         IF t_lDEBUG
            cOpt_Link := "DEBUG ALL" + cOpt_Link
         ENDIF
         IF s_lMAP
            AAdd( s_aOPTL, "OP MAP" )
         ENDIF

      CASE t_cARCH == "win" .AND. t_cCOMP == "owatcom"
         cLibPrefix := "LIB "
         cLibExt := ".lib"
         cObjPrefix := "FILE "
         cObjExt := ".obj"
         cLibPathPrefix := "LIBPATH "
         cLibPathSep := " "
         cBin_CompC := "wpp386.exe"
         cOpt_CompC := "-zq -6s -fp6 -onaehtri+ -s -ei -zp4 -zt0 -bt=NT {FC} -i{DI}"
         IF s_lINC .AND. ! Empty( cWorkDir )
            cOpt_CompC += " {IC} -fo={OO}"
         ELSE
            cOpt_CompC += " {LC}"
         ENDIF
         cBin_Link := "wlink.exe"
         cOpt_Link := "{FL} NAME {OE} {LO} {DL} {LL} {LS}{SCRIPT}"
         cBin_Lib := "wlib.exe"
         cOpt_Lib := "{FA} {OL} {LO}{SCRIPT}"
         cLibLibExt := cLibExt
         cLibObjPrefix := "-+ "
         IF t_lMT
            AAdd( s_aOPTC, "-bm" )
         ENDIF
         IF s_lGUI
            AAdd( s_aOPTC, "-bg" ) /* TOFIX */
            AAdd( s_aOPTL, "RU NAT" ) /* TOFIX */
            AAdd( s_aOPTL, "SYSTEM NT_WIN" ) /* TOFIX */
         ELSE
            AAdd( s_aOPTC, "-bc" ) /* TOFIX */
            AAdd( s_aOPTL, "RU CON" ) /* TOFIX */
            AAdd( s_aOPTL, "SYSTEM NT" ) /* TOFIX */
         ENDIF
         IF t_lDEBUG
            cOpt_Link := "DEBUG ALL" + cOpt_Link
         ENDIF
         IF s_lMAP
            AAdd( s_aOPTL, "OP MAP" )
         ENDIF
         s_aLIBSYS := ArrayAJoin( { s_aLIBSYS, s_aLIBSYSCORE, s_aLIBSYSMISC } )
         s_aLIBSHARED := { iif( t_lMT, "harbourmt-" + cDL_Version_Alter + cLibExt,;
                                       "harbour-" + cDL_Version_Alter + cLibExt ) }

         IF s_lSHARED
            AAdd( s_aOPTL, "FILE " + FN_ExtSet( s_cHB_LIB_INSTALL + hb_osPathSeparator() + iif( s_lGUI, "hbmainwin", "hbmainstd" ), cLibExt ) )
         ENDIF

         IF Len( s_aRESSRC ) > 0
            cBin_Res := "wrc"
            cResExt := ".res"
            cOpt_Res := "-r -zm {IR} -fo={OS}"
            cResPrefix := "OP res="
         ENDIF

         IF s_lFMSTAT != NIL .AND. s_lFMSTAT
            AAdd( s_aLIBFM, iif( t_lMT, "hbfmmt", "hbfm" ) )
         ENDIF

      CASE t_cARCH == "os2" .AND. t_cCOMP == "owatcom"
         cLibPrefix := "LIB "
         cLibExt := ".lib"
         cObjPrefix := "FILE "
         cObjExt := ".obj"
         cLibPathPrefix := "LIBPATH "
         cLibPathSep := " "
         cBin_CompC := "wpp386.exe"
         cOpt_CompC := "-zq -5r -fp5 -onaehtri+ -s -ei -zp4 -zt0 -bt=OS2 {FC} -i{DI}"
         IF s_lINC .AND. ! Empty( cWorkDir )
            cOpt_CompC += " {IC} -fo={OO}"
         ELSE
            cOpt_CompC += " {LC}"
         ENDIF
         cBin_Link := "wlink.exe"
         cOpt_Link := "{FL} NAME {OE} {LO} {DL} {LL}{SCRIPT}"
         cBin_Lib := "wlib.exe"
         cOpt_Lib := "{FA} {OL} {LO}{SCRIPT}"
         cLibLibExt := cLibExt
         cLibObjPrefix := "-+ "
         IF t_lDEBUG
            cOpt_Link := "DEBUG ALL" + cOpt_Link
         ENDIF
         IF t_lMT
            AAdd( s_aOPTC, "-bm" )
         ENDIF
         IF s_lMAP
            AAdd( s_aOPTL, "OP MAP" )
         ENDIF

         IF s_lFMSTAT != NIL .AND. s_lFMSTAT
            AAdd( s_aLIBFM, iif( t_lMT, "hbfmmt", "hbfm" ) )
         ENDIF

      CASE t_cARCH == "linux" .AND. t_cCOMP == "owatcom"
         cLibPrefix := "LIB "
         cLibExt := ".lib"
         cObjPrefix := "FILE "
         cObjExt := ".o"
         cLibPathPrefix := "LIBPATH "
         cLibPathSep := " "
         cBin_CompC := "wpp386"
         cOpt_CompC := "-zq -6r -fp6 -onaehtri+ -s -ei -zp4 -zt0 -bt=LINUX {FC} -i{DI}"
         IF s_lINC .AND. ! Empty( cWorkDir )
            cOpt_CompC += " {IC} -fo={OO}"
         ELSE
            cOpt_CompC += " {LC}"
         ENDIF
         cBin_Link := "wlink"
         cOpt_Link := "SYS LINUX {FL} NAME {OE} {LO} {DL} {LL}{SCRIPT}"
         cBin_Lib := "wlib"
         cOpt_Lib := "{FA} {OL} {LO}{SCRIPT}"
         cLibLibExt := cLibExt
         cLibObjPrefix := "-+ "
         IF t_lMT
            AAdd( s_aOPTC, "-bm" )
         ENDIF
         IF t_lDEBUG
            cOpt_Link := "DEBUG ALL" + cOpt_Link
         ENDIF
         IF s_lMAP
            AAdd( s_aOPTL, "OP MAP" )
         ENDIF

         IF s_lFMSTAT != NIL .AND. s_lFMSTAT
            AAdd( s_aLIBFM, iif( t_lMT, "hbfmmt", "hbfm" ) )
         ENDIF

      /* Misc */
      CASE t_cARCH == "win" .AND. t_cCOMP == "bcc"
         IF t_lDEBUG
            AAdd( s_aOPTC, "-y -v" )
            AAdd( s_aOPTL, "-v" )
         ELSE
            AAdd( s_aCLEAN, PathSepToTarget( FN_ExtSet( s_cPROGNAME, ".tds" ) ) )
         ENDIF
         IF s_lGUI
            AAdd( s_aOPTC, "-tW" )
         ENDIF
         cLibPrefix := NIL
         cLibExt := ".lib"
         cObjExt := ".obj"
         cBin_Lib := "tlib.exe"
         cOpt_Lib := "{FA} {OL} {LO}{SCRIPT}"
         cLibLibExt := cLibExt
         cLibObjPrefix := "-+ "
         cBin_CompC := "bcc32.exe"
         cOpt_CompC := "-c -q -tWM -O2 -OS -Ov -Oi -Oc -d {FC} -I{DI} {LC}"
         cBin_Res := "brcc32.exe"
         cOpt_Res := "{IR} -fo{OS}"
         cResExt := ".res"
         cBin_Link := "ilink32.exe"
         cBin_Dyn := cBin_Link
         cOpt_Link := '-Gn -Tpe -L"{DL}" {FL} ' + iif( s_lGUI, "c0w32.obj", "c0x32.obj" ) + " {LO}, {OE}, " + iif( s_lMAP, "{OM}", "nul" ) + ", cw32mt.lib {LL} import32.lib,, {LS}{SCRIPT}"
         cOpt_Dyn  := '-Gn -Tpd -L"{DL}" {FD} ' +              "c0d32.obj"                + " {LO}, {OD}, " + iif( s_lMAP, "{OM}", "nul" ) + ", cw32mt.lib {LL} import32.lib,, {LS}{SCRIPT}"
         cLibPathPrefix := ""
         cLibPathSep := ";"
         IF s_lGUI
            AAdd( s_aOPTL, "-aa" )
            AAdd( s_aOPTD, "-aa" )
         ELSE
            AAdd( s_aOPTL, "-ap" )
            AAdd( s_aOPTD, "-ap" )
         ENDIF
         IF s_lINC
            IF ! Empty( cWorkDir )
               AAdd( s_aOPTC, "-n{OW}" )
            ENDIF
         ELSE
            IF lStopAfterCComp .AND. ! lCreateLib .AND. ! lCreateDyn
               IF ( Len( s_aPRG ) + Len( s_aC ) ) == 1
                  AAdd( s_aOPTC, "-o{OO}" )
               ELSE
                  AAdd( s_aOPTC, "-n{OD}" )
               ENDIF
            ENDIF
         ENDIF
         IF s_lSHARED
            AAdd( s_aLIBPATH, "{DB}" )
         ENDIF
         s_aLIBSHARED := { iif( t_lMT, "harbourmt-" + cDL_Version_Alter + "-bcc" + cLibExt,;
                                       "harbour-" + cDL_Version_Alter + "-bcc" + cLibExt ) }
         s_aLIBSHAREDPOST := { "hbmainstd", "hbmainwin" }

         IF s_lFMSTAT != NIL .AND. s_lFMSTAT
            AAdd( s_aLIBFM, iif( t_lMT, "hbfmmt", "hbfm" ) )
         ENDIF

      CASE ( t_cARCH == "win" .AND. t_cCOMP $ "msvc|msvc64|msvcia64|icc|iccia64" ) .OR. ;
           ( t_cARCH == "wce" .AND. t_cCOMP == "msvcarm" ) /* NOTE: Cross-platform: wce/ARM on win/x86 */
         IF t_lDEBUG
            IF t_cCOMP == "msvcarm"
               AAdd( s_aOPTC, "-Zi" )
            ELSE
               AAdd( s_aOPTC, "-MTd -Zi" )
            ENDIF
            AAdd( s_aOPTL, "/debug" )
         ENDIF
         IF s_lGUI
            AAdd( s_aOPTL, "/subsystem:windows" )
         ELSE
            AAdd( s_aOPTL, "/subsystem:console" )
         ENDIF
         cLibPrefix := NIL
         cLibExt := ".lib"
         cObjExt := ".obj"
         cLibLibExt := cLibExt
         IF t_cCOMP $ "icc|iccia64"
            cBin_Lib := "xilib.exe"
            cBin_CompC := "icl.exe"
            cBin_Dyn := "xilink.exe"
            cBin_Link := "link.exe"
         ELSE
            cBin_Lib := "lib.exe"
            cBin_CompC := "cl.exe" /* TODO: Pre-8.0 is clarm.exe */
            cBin_Dyn := "link.exe"
            cBin_Link := "link.exe"
         ENDIF
         cOpt_Lib := "{FA} /out:{OL} {LO}"
         cOpt_Dyn := "{FD} /dll /out:{OD} {DL} {LO} {LL} {LS}"
         cOpt_CompC := "-nologo -c {FC} -I{DI} {LC}"
         cOpt_Link := "-nologo /out:{OE} {LO} {DL} {FL} {LL} {LS}"
         cLibPathPrefix := "/libpath:"
         cLibPathSep := " "
         IF s_lMAP
            AAdd( s_aOPTC, "-Fm" )
            AAdd( s_aOPTD, "-Fm" )
         ENDIF
         IF t_cCOMP == "msvcarm"
            /* NOTE: Copied from .cf. Probably needs cleaning. */
            AAdd( s_aOPTC, "-D_WIN32_WCE=0x420" )
            AAdd( s_aOPTC, "-DUNDER_CE=0x420" )
            AAdd( s_aOPTC, "-DWIN32_PLATFORM_PSPC" )
            AAdd( s_aOPTC, "-DWINCE" )
            AAdd( s_aOPTC, "-D_WINCE" )
            AAdd( s_aOPTC, "-D_WINDOWS" )
            AAdd( s_aOPTC, "-DARM" )
            AAdd( s_aOPTC, "-D_ARM_" )
            AAdd( s_aOPTC, "-DARMV4" )
            AAdd( s_aOPTC, "-DPOCKETPC2003_UI_MODEL" )
            AAdd( s_aOPTC, "-D_M_ARM" )
            AAdd( s_aOPTC, "-DUNICODE" )
            AAdd( s_aOPTC, "-D_UNICODE" )
            AAdd( s_aOPTC, "-D_UWIN" )
            AAdd( s_aOPTL, "/subsystem:windowsce,4.20" )
            AAdd( s_aOPTL, "/machine:arm" )
            AAdd( s_aOPTL, "/armpadcode" )
            AAdd( s_aOPTL, "/stack:65536,4096" )
            AAdd( s_aOPTL, "/nodefaultlib:oldnames.lib" )
            AAdd( s_aOPTL, "/nodefaultlib:kernel32.lib" )
            AAdd( s_aOPTL, "/align:4096" )
            AAdd( s_aOPTL, "/opt:ref" )
            AAdd( s_aOPTL, "/opt:icf" )
            AAdd( s_aOPTL, "/manifest:no" )
         ENDIF
         IF s_lINC
            IF ! Empty( cWorkDir )
               AAdd( s_aOPTC, "-Fo{OW}\" ) /* NOTE: Ending path sep is important. */
            ENDIF
         ELSE
            IF lStopAfterCComp .AND. ! lCreateLib .AND. ! lCreateDyn
               IF ( Len( s_aPRG ) + Len( s_aC ) ) == 1
                  AAdd( s_aOPTC, "-Fo{OO}" )
               ELSE
                  AAdd( s_aOPTC, "-Fo{OD}" )
               ENDIF
            ENDIF
         ENDIF
         IF s_lSHARED
            AAdd( s_aOPTL, "/libpath:{DB}" )
         ENDIF
         s_aLIBSYS := ArrayAJoin( { s_aLIBSYS, s_aLIBSYSCORE, s_aLIBSYSMISC } )
         DO CASE
         CASE t_cCOMP $ "msvc|icc"
            s_aLIBSHARED := { iif( t_lMT, "harbourmt-" + cDL_Version_Alter + cLibExt,;
                                          "harbour-" + cDL_Version_Alter + cLibExt ) }
         CASE t_cCOMP == "msvc64"
            s_aLIBSHARED := { iif( t_lMT, "harbourmt-" + cDL_Version_Alter + "-x64" + cLibExt,;
                                          "harbour-" + cDL_Version_Alter + "-x64" + cLibExt ) }
         CASE t_cCOMP $ "msvcia64|iccia64"
            s_aLIBSHARED := { iif( t_lMT, "harbourmt-" + cDL_Version_Alter + "-ia64" + cLibExt,;
                                          "harbour-" + cDL_Version_Alter + "-ia64" + cLibExt ) }
         CASE t_cCOMP == "msvcarm"
            s_aLIBSHARED := { iif( t_lMT, "harbourmt-" + cDL_Version_Alter + "-arm" + cLibExt,;
                                          "harbour-" + cDL_Version_Alter + "-arm" + cLibExt ) }
         ENDCASE

         s_aLIBSHAREDPOST := { "hbmainstd", "hbmainwin" }

         IF !( t_cCOMP $ "icc|iccia64" )
            cBin_Res := "rc.exe"
            cOpt_Res := "/fo {OS} {IR}"
            cResExt := ".res"
         ENDIF

         IF s_lFMSTAT != NIL .AND. s_lFMSTAT
            AAdd( s_aLIBFM, iif( t_lMT, "hbfmmt", "hbfm" ) )
         ENDIF

      CASE ( t_cARCH == "win" .AND. t_cCOMP == "pocc" ) .OR. ;
           ( t_cARCH == "win" .AND. t_cCOMP == "pocc64" ) .OR. ; /* NOTE: Cross-platform: win/amd64 on win/x86 */
           ( t_cARCH == "wce" .AND. t_cCOMP == "poccarm" ) .OR. ; /* NOTE: Cross-platform: wce/ARM on win/x86 */
           ( t_cARCH == "win" .AND. t_cCOMP == "xcc" )

         IF s_lGUI
            AAdd( s_aOPTL, "/subsystem:windows" )
         ELSE
            AAdd( s_aOPTL, "/subsystem:console" )
         ENDIF
         cLibPrefix := NIL
         cLibExt := ".lib"
         cObjExt := ".obj"
         cLibLibExt := cLibExt
         IF t_cCOMP == "xcc"
            cBin_CompC := "xcc.exe"
            cBin_Lib := "xlib.exe"
            cBin_Link := "xlink.exe"
            cBin_Res := "xrc.exe"
         ELSE
            cBin_CompC := "pocc.exe"
            cBin_Lib := "polib.exe"
            cBin_Link := "polink.exe"
            cBin_Res := "porc.exe"
         ENDIF
         cBin_Dyn := cBin_Link
         cOpt_CompC := "/c /Ze /Go {FC} /I{DI} {IC} /Fo{OO}"
         cOpt_Dyn := "{FD} /dll /out:{OD} {DL} {LO} {LL} {LS}"
         DO CASE
         CASE t_cCOMP == "pocc"
            AAdd( s_aOPTC, "/Ot" )
            AAdd( s_aOPTC, "/Tx86-coff" )
         CASE t_cCOMP == "pocc64"
            AAdd( s_aOPTC, "/Tamd64-coff" )
         CASE t_cCOMP == "poccarm"
            AAdd( s_aOPTC, "/Tarm-coff" )
         ENDCASE
         cOpt_Res := "/Fo{OS} {IR}"
         cResExt := ".res"
         cOpt_Lib := "{FA} /out:{OL} {LO}"
         IF t_lMT
            AAdd( s_aOPTC, "/MT" )
         ENDIF
         IF !( lStopAfterCComp .AND. ! lCreateLib .AND. ! lCreateDyn )
            AAdd( s_aOPTL, "/out:{OE}" )
         ENDIF
         cOpt_Link := "{LO} {DL} {FL} {LL} {LS}"
         cLibPathPrefix := "/libpath:"
         cLibPathSep := " "
         IF s_lSHARED
            AAdd( s_aOPTL, "/libpath:{DB}" )
         ENDIF
         IF s_lMAP
            AAdd( s_aOPTL, "/map" )
         ENDIF
         IF t_lDEBUG
            AAdd( s_aOPTL, "/debug" )
         ENDIF
         s_aLIBSYS := ArrayAJoin( { s_aLIBSYS, s_aLIBSYSCORE, s_aLIBSYSMISC } )
         DO CASE
         CASE t_cCOMP == "pocc64"
            s_aLIBSHARED := { iif( t_lMT, "harbourmt-" + cDL_Version_Alter + "-x64" + cLibExt,;
                                          "harbour-" + cDL_Version_Alter + "-x64" + cLibExt ) }
         CASE t_cCOMP == "poccarm"
            s_aLIBSHARED := { iif( t_lMT, "harbourmt-" + cDL_Version_Alter + "-arm" + cLibExt,;
                                          "harbour-" + cDL_Version_Alter + "-arm" + cLibExt ) }
         OTHERWISE
            s_aLIBSHARED := { iif( t_lMT, "harbourmt-" + cDL_Version_Alter + cLibExt,;
                                          "harbour-" + cDL_Version_Alter + cLibExt ) }
         ENDCASE

         s_aLIBSHAREDPOST := { "hbmainstd", "hbmainwin" }

         IF s_lFMSTAT != NIL .AND. s_lFMSTAT
            AAdd( s_aLIBFM, iif( t_lMT, "hbfmmt", "hbfm" ) )
         ENDIF

      /* TODO */
      CASE t_cARCH == "linux" .AND. t_cCOMP == "icc"
      ENDCASE

      IF lCreateDyn .AND. t_cARCH $ "win|wce"
         AAdd( s_aOPTC, "-DHB_DYNLIB" )
      ENDIF

      /* Do entry function detection on platform required and supported */
      IF ! s_lDONTEXEC .AND. ! lStopAfterCComp .AND. s_cMAIN == NIL
         tmp := iif( Lower( FN_ExtGet( s_cFIRST ) ) == ".prg" .OR. Empty( FN_ExtGet( s_cFIRST ) ), FN_ExtSet( s_cFIRST, ".c" ), s_cFIRST )
         IF ! Empty( tmp := getFirstFunc( tmp ) )
            s_cMAIN := tmp
         ENDIF
      ENDIF

      /* HACK: Override entry point requested by user or detected by us,
               and override the GT if requested by user. */
      IF ! lStopAfterCComp .AND. ;
         ! s_lCLEAN .AND. ;
         ( s_cMAIN != NIL .OR. ;
           ! Empty( s_aLIBUSERGT ) .OR. ;
           s_cGT != NIL .OR. ;
           s_lFMSTAT != NIL )

         fhnd := hb_FTempCreateEx( @s_cCSTUB, NIL, "hbmk_", ".c" )
         IF fhnd != F_ERROR

            /* NOTE: This has to be kept synced with Harbour HB_IMPORT values. */
            DO CASE
            CASE ! s_lSHARED .OR. ;
                 !( t_cARCH $ "win|wce" ) .OR. ;
                 t_cCOMP $ "msvc|msvc64|msvcia64|icc|iccia64"

               /* NOTE: MSVC gives the warning:
                        "LNK4217: locally defined symbol ... imported in function ..."
                        if using 'dllimport'. [vszakats] */
               tmp := ""
            CASE t_cCOMP $ "gcc|mingw|mingw64|mingwarm|cygwin" ; tmp := "__attribute__ (( dllimport ))"
            CASE t_cCOMP $ "bcc|owatcom"                       ; tmp := "__declspec( dllimport )"
            OTHERWISE                                          ; tmp := "_declspec( dllimport )"
            ENDCASE

            /* Create list of requested symbols */
            array := {}
            IF s_cMAIN != NIL
               /* NOTE: Request this function to generate link error, rather
                        than starting with the wrong (default) function. */
               AAdd( array, Upper( iif( Left( s_cMAIN, 1 ) == "@", SubStr( s_cMAIN, 2 ), s_cMAIN ) ) )
            ENDIF
            IF s_lFMSTAT != NIL
               AAdd( array, iif( s_lFMSTAT, "HB_FM_STAT", "HB_FM_NOSTAT" ) )
            ENDIF
            IF s_cGT != NIL
               /* Always request default GT first */
               AAdd( array, "HB_GT_" + Upper( SubStr( s_cGT, 3 ) ) )
            ENDIF
            IF ! Empty( s_aLIBUSERGT )
               AEval( s_aLIBUSERGT, {|tmp| AAdd( array, "HB_GT_" + Upper( SubStr( tmp, 3 ) ) ) } )
            ENDIF

            /* Build C stub */
            FWrite( fhnd, '/* This temp source file was generated by Harbour Make tool. */'         + hb_osNewLine() +;
                          '/* You can safely delete it. */'                                         + hb_osNewLine() +;
                          ''                                                                        + hb_osNewLine() +;
                          '#include "hbapi.h"'                                                      + hb_osNewLine() )
            IF ! Empty( array )
               FWrite( fhnd, ''                                                                     + hb_osNewLine() )
               AEval( array, {|tmp| FWrite( fhnd, 'HB_FUNC_EXTERN( ' + tmp + ' );'                  + hb_osNewLine() ) } )
               FWrite( fhnd, ''                                                                     + hb_osNewLine() )
               FWrite( fhnd, 'void _hb_lnk_ForceLink_hbmk( void )'                                  + hb_osNewLine() )
               FWrite( fhnd, '{'                                                                    + hb_osNewLine() )
               AEval( array, {|tmp| FWrite( fhnd, '   HB_FUNC_EXEC( ' + tmp + ' );'                 + hb_osNewLine() ) } )
               FWrite( fhnd, '}'                                                                    + hb_osNewLine() )
               FWrite( fhnd, ''                                                                     + hb_osNewLine() )
            ENDIF

            IF s_cGT != NIL .OR. ;
               s_cMAIN != NIL
               FWrite( fhnd, '#include "hbinit.h"'                                                  + hb_osNewLine() +;
                             ''                                                                     + hb_osNewLine() +;
                             'HB_EXTERN_BEGIN'                                                      + hb_osNewLine() +;
                             'extern ' + tmp + ' void hb_vmSetLinkedMain( const char * szMain );'   + hb_osNewLine() +;
                             'extern ' + tmp + ' void hb_gtSetDefault( const char * szGtName );'    + hb_osNewLine() +;
                             'HB_EXTERN_END'                                                        + hb_osNewLine() +;
                             ''                                                                     + hb_osNewLine() +;
                             'HB_CALL_ON_STARTUP_BEGIN( _hb_hbmk_setdef_ )'                         + hb_osNewLine() )
               IF s_cGT != NIL
                  FWrite( fhnd, '   hb_gtSetDefault( "' + Upper( SubStr( s_cGT, 3 ) ) + '" );'      + hb_osNewLine() )
               ENDIF
               IF s_cMAIN != NIL
                  FWrite( fhnd, '   hb_vmSetLinkedMain( "' + Upper( s_cMAIN ) + '" );'              + hb_osNewLine() )
               ENDIF
               FWrite( fhnd, 'HB_CALL_ON_STARTUP_END( _hb_hbmk_setdef_ )'                           + hb_osNewLine() +;
                             ''                                                                     + hb_osNewLine() +;
                             '#if defined( HB_PRAGMA_STARTUP )'                                     + hb_osNewLine() +;
                             '   #pragma startup_hb_lnk_SetDefault_hbmk_'                           + hb_osNewLine() +;
                             '#elif defined( HB_MSC_STARTUP )'                                      + hb_osNewLine() +;
                             '   #if defined( HB_OS_WIN_64 )'                                       + hb_osNewLine() +;
                             '      #pragma section( HB_MSC_START_SEGMENT, long, read )'            + hb_osNewLine() +;
                             '   #endif'                                                            + hb_osNewLine() +;
                             '   #pragma data_seg( HB_MSC_START_SEGMENT )'                          + hb_osNewLine() +;
                             '   static HB_$INITSYM hb_vm_auto_hbmk_setdef_ = _hb_hbmk_setdef_;'    + hb_osNewLine() +;
                             '   #pragma data_seg()'                                                + hb_osNewLine() +;
                             '#endif'                                                               + hb_osNewLine() )
            ENDIF
            FClose( fhnd )
         ELSE
            OutErr( "hbmk: Warning: Stub helper .c program couldn't be created." + hb_osNewLine() )
            IF ! s_lINC
               AEval( ListDirExt( s_aPRG, cWorkDir, ".c" ), {|tmp| FErase( tmp ) } )
            ENDIF
            PauseForKey()
            ErrorLevel( 5 )
            RETURN
         ENDIF
         AAdd( s_aC, s_cCSTUB )
      ENDIF

      /* Library list assembly */
      IF s_lSHARED .AND. ! Empty( s_aLIBSHARED )
         s_aLIBHB := ArrayAJoin( { s_aLIBSHAREDPOST,;
                                   s_aLIBFM,;
                                   aLIB_BASE_CPLR,;
                                   aLIB_BASE_DEBUG } )
      ELSE
         s_aLIBHB := ArrayAJoin( { s_aLIBFM,;
                                   aLIB_BASE1,;
                                   aLIB_BASE_CPLR,;
                                   aLIB_BASE_DEBUG,;
                                   s_aLIBVM,;
                                   iif( s_lNULRDD, aLIB_BASE_NULRDD, aLIB_BASE_RDD ),;
                                   aLIB_BASE2,;
                                   iif( s_lHB_PCRE, aLIB_BASE_PCRE, {} ),;
                                   iif( s_lHB_ZLIB, aLIB_BASE_ZLIB, {} ) } )
      ENDIF

      /* Merge lib lists. */
      s_aLIB := ArrayAJoin( { s_aLIBUSER, s_aLIBHB, s_aLIB3RD, s_aLIBSYS } )
      /* Dress lib names. */
      s_aLIB := ListCookLib( s_aLIB, cLibPrefix, cLibExt )
      IF s_lSHARED .AND. ! Empty( s_aLIBSHARED )
         s_aLIB := ArrayJoin( ListCookLib( s_aLIBSHARED, cLibPrefix ), s_aLIB )
      ENDIF
      /* Dress obj names. */
      s_aOBJ := ListDirExt( ArrayJoin( s_aPRG, s_aC ), cWorkDir, cObjExt )
      s_aOBJUSER := ListCook( s_aOBJUSER, NIL, cObjExt )

      IF s_lINC .AND. ! s_lREBUILD
         s_aRESSRC_TODO := {}
         FOR EACH tmp IN s_aRESSRC
            IF s_lDEBUGINC
               OutStd( "hbmk: debuginc: RESSRC", tmp, FN_DirExtSet( tmp, cWorkDir, cResExt ), hb_osNewLine() )
            ENDIF
            IF ! hb_FGetDateTime( tmp, @tmp1 ) .OR. ;
               ! hb_FGetDateTime( FN_DirExtSet( tmp, cWorkDir, cResExt ), @tmp2 ) .OR. ;
               tmp1 > tmp2
               AAdd( s_aRESSRC_TODO, tmp )
            ENDIF
         NEXT
      ELSE
         s_aRESSRC_TODO := s_aRESSRC
      ENDIF

      IF Len( s_aRESSRC_TODO ) > 0 .AND. ! Empty( cBin_Res ) .AND. ! s_lCLEAN

         IF s_lINC .AND. ! t_lQuiet
            OutStd( "Compiling resources..." + hb_osNewLine() )
         ENDIF

         /* Compiling resource */

         cOpt_Res := StrTran( cOpt_Res, "{FR}"  , GetEnv( "HB_USER_RESFLAGS" ) )
         cOpt_Res := StrTran( cOpt_Res, "{DI}"  , s_cHB_INC_INSTALL )

         IF "{IR}" $ cOpt_Res

            FOR EACH tmp IN s_aRESSRC_TODO

               cCommand := cOpt_Res
               cCommand := StrTran( cCommand, "{IR}", tmp )
               cCommand := StrTran( cCommand, "{OS}", PathSepToTarget( FN_DirExtSet( tmp, cWorkDir, cResExt ) ) )

               cCommand := cBin_Res + " " + AllTrim( cCommand )

               IF s_lTRACE
                  IF ! t_lQuiet
                     OutStd( "hbmk: Resource compiler command:" + hb_osNewLine() )
                  ENDIF
                  OutStd( cCommand + hb_osNewLine() )
               ENDIF

               IF ! s_lDONTEXEC .AND. ( tmp1 := hb_run( cCommand ) ) != 0
                  OutErr( "hbmk: Error: Running resource compiler. " + hb_ntos( tmp1 ) + ":" + hb_osNewLine() )
                  OutErr( cCommand + hb_osNewLine() )
                  nErrorLevel := 6
                  EXIT
               ENDIF
            NEXT
         ELSE
            cOpt_Res := StrTran( cOpt_Res, "{LR}"  , ArrayToList( s_aRESSRC_TODO ) )

            cOpt_Res := AllTrim( cOpt_Res )

            /* Handle moving the whole command line to a script, if requested. */
            IF "{SCRIPT}" $ cOpt_Res
               fhnd := hb_FTempCreateEx( @cScriptFile, NIL, NIL, ".lnk" )
               IF fhnd != F_ERROR
                  FWrite( fhnd, StrTran( cOpt_Res, "{SCRIPT}", "" ) )
                  FClose( fhnd )
                  cOpt_Res := "@" + cScriptFile
               ELSE
                  OutErr( "hbmk: Warning: Resource compiler script couldn't be created, continuing in command line." + hb_osNewLine() )
               ENDIF
            ENDIF

            cCommand := cBin_Res + " " + cOpt_Res

            IF s_lTRACE
               IF ! t_lQuiet
                  OutStd( "hbmk: Resource compiler command:" + hb_osNewLine() )
               ENDIF
               OutStd( cCommand + hb_osNewLine() )
               IF ! Empty( cScriptFile )
                  OutStd( "hbmk: Resource compiler script:" + hb_osNewLine() )
                  OutStd( hb_MemoRead( cScriptFile ) + hb_osNewLine() )
               ENDIF
            ENDIF

            IF ! s_lDONTEXEC .AND. ( tmp := hb_run( cCommand ) ) != 0
               OutErr( "hbmk: Error: Running resource compiler. " + hb_ntos( tmp ) + ":" + hb_osNewLine() )
               OutErr( cCommand + hb_osNewLine() )
               nErrorLevel := 8
            ENDIF

            IF ! Empty( cScriptFile )
               FErase( cScriptFile )
            ENDIF
         ENDIF
      ENDIF

      IF nErrorLevel == 0
         IF s_lINC .AND. ! s_lREBUILD
            s_aC_TODO := {}
            s_aC_DONE := {}
            FOR EACH tmp IN s_aC
               IF s_lDEBUGINC
                  OutStd( "hbmk: debuginc: C", tmp, FN_DirExtSet( tmp, cWorkDir, cObjExt ), hb_osNewLine() )
               ENDIF
               IF ! hb_FGetDateTime( tmp, @tmp1 ) .OR. ;
                  ! hb_FGetDateTime( FN_DirExtSet( tmp, cWorkDir, cObjExt ), @tmp2 ) .OR. ;
                  tmp1 > tmp2
                  AAdd( s_aC_TODO, tmp )
               ELSE
                  AAdd( s_aC_DONE, tmp )
               ENDIF
            NEXT
         ELSE
            s_aC_TODO := s_aC
            s_aC_DONE := {}
         ENDIF

         IF s_lINC .AND. ! s_lREBUILD
            s_aPRG_TODO := {}
            s_aPRG_DONE := {}
            FOR EACH tmp IN s_aPRG
               IF s_lDEBUGINC
                  OutStd( "hbmk: debuginc: CPRG", FN_DirExtSet( tmp, cWorkDir, ".c" ),;
                                                  FN_DirExtSet( tmp, cWorkDir, cObjExt ), hb_osNewLine() )
               ENDIF
               IF ! hb_FGetDateTime( FN_DirExtSet( tmp, cWorkDir, ".c" ), @tmp1 ) .OR. ;
                  ! hb_FGetDateTime( FN_DirExtSet( tmp, cWorkDir, cObjExt ), @tmp2 ) .OR. ;
                  tmp1 > tmp2
                  AAdd( s_aPRG_TODO, tmp )
               ELSE
                  AAdd( s_aPRG_DONE, tmp )
               ENDIF
            NEXT
         ELSE
            s_aPRG_TODO := s_aPRG
            s_aPRG_DONE := {}
         ENDIF
      ENDIF

      IF nErrorLevel == 0 .AND. ( Len( s_aPRG_TODO ) + Len( s_aC_TODO ) + iif( Empty( cBin_Link ), Len( s_aOBJUSER ) + Len( s_aOBJA ), 0 ) ) > 0 .AND. ! s_lCLEAN

         IF ! Empty( cBin_CompC )

            IF s_lINC .AND. ! t_lQuiet
               OutStd( "Compiling..." + hb_osNewLine() )
            ENDIF

            /* Compiling */

            /* Order is significant */
            cOpt_CompC := StrTran( cOpt_CompC, "{FC}"  , iif( s_lBLDFLGC, cSelfFlagC + " ", "" ) +;
                                                         GetEnv( "HB_USER_CFLAGS" ) + " " + ArrayToList( s_aOPTC ) )
            cOpt_CompC := StrTran( cOpt_CompC, "{FL}"  , iif( s_lBLDFLGL, cSelfFlagL + " ", "" ) +;
                                                         GetEnv( "HB_USER_LDFLAGS" ) + " " + ArrayToList( s_aOPTL ) )
            cOpt_CompC := StrTran( cOpt_CompC, "{LR}"  , ArrayToList( ArrayJoin( ListDirExt( s_aRESSRC, cWorkDir, cResExt ), s_aRESCMP ) ) )
            cOpt_CompC := StrTran( cOpt_CompC, "{LO}"  , ArrayToList( ArrayAJoin( { ListCook( s_aOBJUSER, cObjPrefix ), ListCook( s_aPRG_DONE, cObjPrefix, cObjExt ), ListCook( s_aC_DONE, cObjPrefix, cObjExt ) } ) ) )
            cOpt_CompC := StrTran( cOpt_CompC, "{LS}"  , ArrayToList( ListCook( ArrayJoin( ListDirExt( s_aRESSRC, "", cResExt ), s_aRESCMP ), cResPrefix ) ) )
            cOpt_CompC := StrTran( cOpt_CompC, "{LA}"  , ArrayToList( s_aOBJA ) )
            cOpt_CompC := StrTran( cOpt_CompC, "{LL}"  , ArrayToList( s_aLIB ) )
            cOpt_CompC := StrTran( cOpt_CompC, "{OD}"  , PathSepToTarget( FN_DirGet( s_cPROGNAME ) ) )
            cOpt_CompC := StrTran( cOpt_CompC, "{OE}"  , PathSepToTarget( FN_ExtSet( s_cPROGNAME, cBinExt ) ) )
            cOpt_CompC := StrTran( cOpt_CompC, "{OM}"  , PathSepToTarget( FN_ExtSet( s_cPROGNAME, ".map" ) ) )
            cOpt_CompC := StrTran( cOpt_CompC, "{DL}"  , ArrayToList( ListCook( s_aLIBPATH, cLibPathPrefix ), cLibPathSep ) )
            cOpt_CompC := StrTran( cOpt_CompC, "{DB}"  , s_cHB_BIN_INSTALL )
            cOpt_CompC := StrTran( cOpt_CompC, "{DI}"  , s_cHB_INC_INSTALL )

            IF "{IC}" $ cOpt_CompC

               FOR EACH tmp IN ArrayJoin( ListDirExt( s_aPRG_TODO, cWorkDir, ".c" ), s_aC_TODO )

                  cCommand := cOpt_CompC
                  cCommand := StrTran( cCommand, "{IC}", tmp )
                  cCommand := StrTran( cCommand, "{OO}", PathSepToTarget( FN_DirExtSet( tmp, cWorkDir, cObjExt ) ) )

                  cCommand := cBin_CompC + " " + AllTrim( cCommand )

                  IF s_lTRACE
                     IF ! t_lQuiet
                        OutStd( "hbmk: C compiler command:" + hb_osNewLine() )
                     ENDIF
                     OutStd( cCommand + hb_osNewLine() )
                  ENDIF

                  IF ! s_lDONTEXEC .AND. ( tmp1 := hb_run( cCommand ) ) != 0
                     OutErr( "hbmk: Error: Running C compiler. " + hb_ntos( tmp1 ) + ":" + hb_osNewLine() )
                     OutErr( cCommand + hb_osNewLine() )
                     nErrorLevel := 6
                     EXIT
                  ENDIF
               NEXT
            ELSE
               cOpt_CompC := StrTran( cOpt_CompC, "{LC}"  , ArrayToList( ArrayJoin( ListDirExt( s_aPRG_TODO, cWorkDir, ".c" ), s_aC_TODO ) ) )
               cOpt_CompC := StrTran( cOpt_CompC, "{OO}"  , PathSepToTarget( FN_ExtSet( s_cPROGNAME, cObjExt ) ) )
               cOpt_CompC := StrTran( cOpt_CompC, "{OW}"  , PathSepToTarget( cWorkDir ) )

               cOpt_CompC := AllTrim( cOpt_CompC )

               /* Handle moving the whole command line to a script, if requested. */
               IF "{SCRIPT}" $ cOpt_CompC
                  fhnd := hb_FTempCreateEx( @cScriptFile, NIL, NIL, ".cpl" )
                  IF fhnd != F_ERROR
                     FWrite( fhnd, StrTran( cOpt_CompC, "{SCRIPT}", "" ) )
                     FClose( fhnd )
                     cOpt_CompC := "@" + cScriptFile
                  ELSE
                     OutErr( "hbmk: Warning: C compiler script couldn't be created, continuing in command line." + hb_osNewLine() )
                  ENDIF
               ENDIF

               cCommand := cBin_CompC + " " + cOpt_CompC

               IF s_lTRACE
                  IF ! t_lQuiet
                     OutStd( "hbmk: C compiler command:" + hb_osNewLine() )
                  ENDIF
                  OutStd( cCommand + hb_osNewLine() )
                  IF ! Empty( cScriptFile )
                     OutStd( "hbmk: C compiler script:" + hb_osNewLine() )
                     OutStd( hb_MemoRead( cScriptFile ) + hb_osNewLine() )
                  ENDIF
               ENDIF

               IF ! s_lDONTEXEC .AND. ( tmp := hb_run( cCommand ) ) != 0
                  OutErr( "hbmk: Error: Running C compiler. " + hb_ntos( tmp ) + ":" + hb_osNewLine() )
                  OutErr( cCommand + hb_osNewLine() )
                  nErrorLevel := 6
               ENDIF

               IF ! Empty( cScriptFile )
                  FErase( cScriptFile )
               ENDIF
            ENDIF
         ELSE
            OutErr( "hbmk: Error: This compiler/platform isn't implemented." + hb_osNewLine() )
            nErrorLevel := 8
         ENDIF
      ENDIF

      IF nErrorLevel == 0

         lTargetUpToDate := .F.

         IF s_lINC .AND. ! s_lREBUILD

            DO CASE
            CASE lCreateLib ; cTarget := PathSepToTarget( FN_ExtSet( cLibLibPrefix + s_cPROGNAME, cLibLibExt ) )
            CASE lCreateDyn ; cTarget := PathSepToTarget( FN_ExtSet( s_cPROGNAME, cDynLibExt ) )
            OTHERWISE       ; cTarget := PathSepToTarget( FN_ExtSet( s_cPROGNAME, cBinExt ) )
            ENDCASE

            IF s_lDEBUGINC
               OutStd( "hbmk: debuginc: EXE", cTarget, hb_osNewLine() )
            ENDIF

            IF hb_FGetDateTime( cTarget, @tTarget )

               lTargetUpToDate := .T.
               IF lTargetUpToDate
                  FOR EACH tmp IN ArrayAJoin( { s_aOBJ, s_aOBJUSER, s_aOBJA, s_aRESSRC, s_aRESCMP } )
                     IF s_lDEBUGINC
                        OutStd( "hbmk: debuginc: EXEDEP", tmp, hb_osNewLine() )
                     ENDIF
                     IF ! hb_FGetDateTime( tmp, @tmp1 ) .OR. tmp1 > tTarget
                        lTargetUpToDate := .F.
                        EXIT
                     ENDIF
                  NEXT
               ENDIF
#if 0
               /* We need a way to find and pick libraries according to linker rules. */
               IF lTargetUpToDate
                  FOR EACH tmp IN s_aLIB
                     IF s_lDEBUGINC
                        OutStd( "hbmk: debuginc: EXEDEPLIB", tmp, hb_osNewLine() )
                     ENDIF
                     IF ! hb_FGetDateTime( tmp, @tmp1 ) .OR. tmp1 > tTarget
                        lTargetUpToDate := .F.
                        EXIT
                     ENDIF
                  NEXT
               ENDIF
#endif
            ENDIF
         ENDIF
      ENDIF

      IF nErrorLevel == 0 .AND. ( Len( s_aOBJ ) + Len( s_aOBJUSER ) + Len( s_aOBJA ) ) > 0 .AND. ! s_lCLEAN

         IF lTargetUpToDate
            OutStd( "hbmk: Target up to date: " + cTarget + hb_osNewLine() )
         ELSE
            DO CASE
            CASE ! lStopAfterCComp .AND. ! Empty( cBin_Link )

               IF s_lINC .AND. ! t_lQuiet
                  OutStd( "Linking..." + hb_osNewLine() )
               ENDIF

               /* Linking */

               /* Order is significant */
               cOpt_Link := StrTran( cOpt_Link, "{FL}"  , iif( s_lBLDFLGL, cSelfFlagL + " ", "" ) +;
                                                          GetEnv( "HB_USER_LDFLAGS" ) + " " + ArrayToList( s_aOPTL ) )
               cOpt_Link := StrTran( cOpt_Link, "{LO}"  , ArrayToList( ListCook( ArrayJoin( s_aOBJ, s_aOBJUSER ), cObjPrefix ) ) )
               cOpt_Link := StrTran( cOpt_Link, "{LS}"  , ArrayToList( ListCook( ArrayJoin( ListDirExt( s_aRESSRC, cWorkDir, cResExt ), s_aRESCMP ), cResPrefix ) ) )
               cOpt_Link := StrTran( cOpt_Link, "{LA}"  , ArrayToList( s_aOBJA ) )
               cOpt_Link := StrTran( cOpt_Link, "{LL}"  , ArrayToList( s_aLIB ) )
               cOpt_Link := StrTran( cOpt_Link, "{OE}"  , PathSepToTarget( FN_ExtSet( s_cPROGNAME, cBinExt ) ) )
               cOpt_Link := StrTran( cOpt_Link, "{OM}"  , PathSepToTarget( FN_ExtSet( s_cPROGNAME, ".map" ) ) )
               cOpt_Link := StrTran( cOpt_Link, "{DL}"  , ArrayToList( ListCook( s_aLIBPATH, cLibPathPrefix ), cLibPathSep ) )
               cOpt_Link := StrTran( cOpt_Link, "{DB}"  , s_cHB_BIN_INSTALL )

               cOpt_Link := AllTrim( cOpt_Link )

               /* Handle moving the whole command line to a script, if requested. */
               IF "{SCRIPT}" $ cOpt_Link
                  fhnd := hb_FTempCreateEx( @cScriptFile, NIL, NIL, ".lnk" )
                  IF fhnd != F_ERROR
                     FWrite( fhnd, StrTran( cOpt_Link, "{SCRIPT}", "" ) )
                     FClose( fhnd )
                     cOpt_Link := "@" + cScriptFile
                  ELSE
                     OutErr( "hbmk: Warning: Link script couldn't be created, continuing in command line." + hb_osNewLine() )
                  ENDIF
               ENDIF

               cCommand := cBin_Link + " " + cOpt_Link

               IF s_lTRACE
                  IF ! t_lQuiet
                     OutStd( "hbmk: Linker command:" + hb_osNewLine() )
                  ENDIF
                  OutStd( cCommand + hb_osNewLine() )
                  IF ! Empty( cScriptFile )
                     OutStd( "hbmk: Linker script:" + hb_osNewLine() )
                     OutStd( hb_MemoRead( cScriptFile ) + hb_osNewLine() )
                  ENDIF
               ENDIF

               IF ! s_lDONTEXEC .AND. ( tmp := hb_run( cCommand ) ) != 0
                  OutErr( "hbmk: Error: Running linker. " + hb_ntos( tmp ) + ":" + hb_osNewLine() )
                  OutErr( cCommand + hb_osNewLine() )
                  nErrorLevel := 7
               ENDIF

               IF ! Empty( cScriptFile )
                  FErase( cScriptFile )
               ENDIF

            CASE lStopAfterCComp .AND. lCreateLib .AND. ! Empty( cBin_Lib )

               IF s_lINC .AND. ! t_lQuiet
                  OutStd( "Creating static library..." + hb_osNewLine() )
               ENDIF

               /* Lib creation (static) */

               /* Order is significant */
               cOpt_Lib := StrTran( cOpt_Lib, "{FA}"  , GetEnv( "HB_USER_AFLAGS" ) + " " + ArrayToList( s_aOPTA ) )
               cOpt_Lib := StrTran( cOpt_Lib, "{LO}"  , ArrayToList( ListCook( ArrayJoin( s_aOBJ, s_aOBJUSER ), cLibObjPrefix ) ) )
               cOpt_Lib := StrTran( cOpt_Lib, "{LL}"  , ArrayToList( s_aLIB ) )
               cOpt_Lib := StrTran( cOpt_Lib, "{OL}"  , PathSepToTarget( FN_ExtSet( cLibLibPrefix + s_cPROGNAME, cLibLibExt ) ) )
               cOpt_Lib := StrTran( cOpt_Lib, "{DL}"  , ArrayToList( ListCook( s_aLIBPATH, cLibPathPrefix ), cLibPathSep ) )
               cOpt_Lib := StrTran( cOpt_Lib, "{DB}"  , s_cHB_BIN_INSTALL )

               cOpt_Lib := AllTrim( cOpt_Lib )

               /* Handle moving the whole command line to a script, if requested. */
               IF "{SCRIPT}" $ cOpt_Lib
                  fhnd := hb_FTempCreateEx( @cScriptFile, NIL, NIL, ".lnk" )
                  IF fhnd != F_ERROR
                     FWrite( fhnd, StrTran( cOpt_Lib, "{SCRIPT}", "" ) )
                     FClose( fhnd )
                     cOpt_Lib := "@" + cScriptFile
                  ELSE
                     OutErr( "hbmk: Warning: Lib script couldn't be created, continuing in command line." + hb_osNewLine() )
                  ENDIF
               ENDIF

               cCommand := cBin_Lib + " " + cOpt_Lib

               IF s_lTRACE
                  IF ! t_lQuiet
                     OutStd( "hbmk: Lib command:" + hb_osNewLine() )
                  ENDIF
                  OutStd( cCommand + hb_osNewLine() )
                  IF ! Empty( cScriptFile )
                     OutStd( "hbmk: Lib script:" + hb_osNewLine() )
                     OutStd( hb_MemoRead( cScriptFile ) + hb_osNewLine() )
                  ENDIF
               ENDIF

               IF ! s_lDONTEXEC .AND. ( tmp := hb_run( cCommand ) ) != 0
                  OutErr( "hbmk: Error: Running lib command. " + hb_ntos( tmp ) + ":" + hb_osNewLine() )
                  OutErr( cCommand + hb_osNewLine() )
                  nErrorLevel := 7
               ENDIF

               IF ! Empty( cScriptFile )
                  FErase( cScriptFile )
               ENDIF

            CASE lStopAfterCComp .AND. lCreateDyn .AND. ! Empty( cBin_Dyn )

               IF s_lINC .AND. ! t_lQuiet
                  OutStd( "Creating dynamic library..." + hb_osNewLine() )
               ENDIF

               /* Lib creation (dynamic) */

               /* Order is significant */
               cOpt_Dyn := StrTran( cOpt_Dyn, "{FD}"  , GetEnv( "HB_USER_DFLAGS" ) + " " + ArrayToList( s_aOPTD ) )
               cOpt_Dyn := StrTran( cOpt_Dyn, "{LO}"  , ArrayToList( ListCook( ArrayJoin( s_aOBJ, s_aOBJUSER ), cDynObjPrefix ) ) )
               cOpt_Dyn := StrTran( cOpt_Dyn, "{LS}"  , ArrayToList( ListCook( ArrayJoin( ListDirExt( s_aRESSRC, cWorkDir, cResExt ), s_aRESCMP ), cResPrefix ) ) )
               cOpt_Dyn := StrTran( cOpt_Dyn, "{LL}"  , ArrayToList( s_aLIB ) )
               cOpt_Dyn := StrTran( cOpt_Dyn, "{OD}"  , PathSepToTarget( FN_ExtSet( s_cPROGNAME, cDynLibExt ) ) )
               cOpt_Dyn := StrTran( cOpt_Dyn, "{OM}"  , PathSepToTarget( FN_ExtSet( s_cPROGNAME, ".map" ) ) )
               cOpt_Dyn := StrTran( cOpt_Dyn, "{DL}"  , ArrayToList( ListCook( s_aLIBPATH, cLibPathPrefix ), cLibPathSep ) )
               cOpt_Dyn := StrTran( cOpt_Dyn, "{DB}"  , s_cHB_BIN_INSTALL )

               cOpt_Dyn := AllTrim( cOpt_Dyn )

               /* Handle moving the whole command line to a script, if requested. */
               IF "{SCRIPT}" $ cOpt_Dyn
                  fhnd := hb_FTempCreateEx( @cScriptFile, NIL, NIL, ".lnk" )
                  IF fhnd != F_ERROR
                     FWrite( fhnd, StrTran( cOpt_Dyn, "{SCRIPT}", "" ) )
                     FClose( fhnd )
                     cOpt_Dyn := "@" + cScriptFile
                  ELSE
                     OutErr( "hbmk: Warning: Dynamic lib link script couldn't be created, continuing in command line." + hb_osNewLine() )
                  ENDIF
               ENDIF

               cCommand := cBin_Dyn + " " + cOpt_Dyn

               IF s_lTRACE
                  IF ! t_lQuiet
                     OutStd( "hbmk: Dynamic lib link command:" + hb_osNewLine() )
                  ENDIF
                  OutStd( cCommand + hb_osNewLine() )
                  IF ! Empty( cScriptFile )
                     OutStd( "hbmk: Dynamic lib link script:" + hb_osNewLine() )
                     OutStd( hb_MemoRead( cScriptFile ) + hb_osNewLine() )
                  ENDIF
               ENDIF

               IF ! s_lDONTEXEC .AND. ( tmp := hb_run( cCommand ) ) != 0
                  OutErr( "hbmk: Error: Running dynamic lib link command. " + hb_ntos( tmp ) + ":" + hb_osNewLine() )
                  OutErr( cCommand + hb_osNewLine() )
                  nErrorLevel := 7
               ENDIF

               IF ! Empty( cScriptFile )
                  FErase( cScriptFile )
               ENDIF

            ENDCASE
         ENDIF
      ENDIF

      /* Cleanup */

      IF ! Empty( s_cCSTUB )
         IF hb_ArgCheck( "debugstub" )
            OutStd( "hbmk: Stub kept for debug: " + s_cCSTUB + hb_osNewLine() )
         ELSE
            FErase( s_cCSTUB )
         ENDIF
         FErase( FN_DirExtSet( s_cCSTUB, "", cObjExt ) )
      ENDIF
      IF ! s_lINC .OR. s_lCLEAN
         AEval( ListDirExt( s_aPRG, cWorkDir, ".c" ), {|tmp| FErase( tmp ) } )
      ENDIF
      IF ! lStopAfterCComp .OR. lCreateLib .OR. lCreateDyn
         IF ! s_lINC .OR. s_lCLEAN
            IF ! Empty( cResExt )
               AEval( ListDirExt( s_aRESSRC, cWorkDir, cResExt ), {|tmp| FErase( tmp ) } )
            ENDIF
            AEval( s_aOBJ, {|tmp| FErase( tmp ) } )
         ENDIF
      ENDIF
      AEval( s_aCLEAN, {|tmp| FErase( tmp ) } )
      IF s_lCLEAN
         DirUnbuild( cWorkDir )
      ENDIF

      IF ! lStopAfterCComp .AND. ! s_lCLEAN
         IF nErrorLevel != 0
            PauseForKey()
         ELSE
            IF s_nCOMPR != _COMPR_OFF .AND. ! lCreateLib .AND. ! Empty( cBin_Cprs )

               /* Executable compression */

               DO CASE
               CASE s_nCOMPR == _COMPR_MIN ; cOpt_Cprs += " " + cOpt_CprsMin
               CASE s_nCOMPR == _COMPR_MAX ; cOpt_Cprs += " " + cOpt_CprsMax
               ENDCASE

               IF lCreateDyn
                  cOpt_Cprs := StrTran( cOpt_Cprs, "{OB}"  , PathSepToTarget( FN_ExtSet( s_cPROGNAME, cDynLibExt ) ) )
               ELSE
                  cOpt_Cprs := StrTran( cOpt_Cprs, "{OB}"  , PathSepToTarget( FN_ExtSet( s_cPROGNAME, cBinExt ) ) )
               ENDIF
               cOpt_Cprs := AllTrim( cOpt_Cprs )

               cCommand := cBin_Cprs + " " + cOpt_Cprs

               IF s_lTRACE
                  IF ! t_lQuiet
                     OutStd( "hbmk: Compression command:" + hb_osNewLine() )
                  ENDIF
                  OutStd( cCommand + hb_osNewLine() )
               ENDIF

               IF ! s_lDONTEXEC .AND. ( tmp := hb_run( cCommand ) ) != 0
                  OutErr( "hbmk: Warning: Running compression command. " + hb_ntos( tmp ) + ":" + hb_osNewLine() )
                  OutErr( cCommand + hb_osNewLine() )
               ENDIF
            ENDIF

            IF s_lRUN .AND. ! lCreateLib .AND. ! lCreateDyn
               s_cPROGNAME := FN_ExtSet( s_cPROGNAME, cBinExt )
               #if defined( __PLATFORM__UNIX )
                  IF Empty( FN_DirGet( s_cPROGNAME ) )
                     s_cPROGNAME := "." + hb_osPathSeparator() + s_cPROGNAME
                  ENDIF
               #endif
               cCommand := AllTrim( PathSepToTarget( s_cPROGNAME ) + " " + ArrayToList( s_aOPTRUN ) )
               IF s_lTRACE
                  IF ! t_lQuiet
                     OutStd( "hbmk: Running executable:" + hb_osNewLine() )
                  ENDIF
                  OutStd( cCommand + hb_osNewLine() )
               ENDIF
               IF ! s_lDONTEXEC
                  nErrorLevel := hb_run( cCommand )
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   ErrorLevel( nErrorLevel )

   RETURN

STATIC FUNCTION SetupForGT( cGT, /* @ */ s_cGT, /* @ */ s_lGUI )

   IF IsValidHarbourID( cGT )

      s_cGT := cGT

      /* Setup default GUI mode for core GTs:
         (please don't add contrib/3rd parties here) */
      SWITCH Lower( cGT )
      CASE "gtcgi"
      CASE "gtcrs"
      CASE "gtpca"
      CASE "gtsln"
      CASE "gtstd"
      CASE "gtwin"
         s_lGUI := .F.
         EXIT

      CASE "gtgui"
      CASE "gtwvt"
         s_lGUI := .T.
         EXIT

      ENDSWITCH

      RETURN .T.
   ENDIF

   RETURN .F.

STATIC FUNCTION FindInPath( cFileName )
   LOCAL cDir
   LOCAL cName
   LOCAL cExt

   hb_FNameSplit( cFileName,, @cName, @cExt )
   #if defined( __PLATFORM__WINDOWS ) .OR. ;
       defined( __PLATFORM__DOS ) .OR. ;
       defined( __PLATFORM__OS2 )
      IF Empty( cExt )
         cExt := ".exe"
      ENDIF
   #endif

   /* Check in current dir. */
   IF hb_FileExists( cFileName := hb_FNameMerge( cDir, cName, cExt ) )
      RETURN cFileName
   ENDIF

   /* Check in the dir of this executable. */
   IF ! Empty( hb_DirBase() )
      IF hb_FileExists( cFileName := hb_FNameMerge( hb_DirBase(), cName, cExt ) )
         RETURN cFileName
      ENDIF
   ENDIF

   /* Check in the PATH. */
   #if defined( __PLATFORM__WINDOWS ) .OR. ;
       defined( __PLATFORM__DOS ) .OR. ;
       defined( __PLATFORM__OS2 )
   FOR EACH cDir IN hb_ATokens( GetEnv( "PATH" ), hb_osPathListSeparator(), .T., .T. )
   #else
   FOR EACH cDir IN hb_ATokens( GetEnv( "PATH" ), hb_osPathListSeparator() )
   #endif
      IF ! Empty( cDir )
         IF hb_FileExists( cFileName := hb_FNameMerge( DirAddPathSep( StrStripQuote( cDir ) ), cName, cExt ) )
            RETURN cFileName
         ENDIF
      ENDIF
   NEXT

   RETURN NIL

STATIC FUNCTION ArrayJoin( arraySrc1, arraySrc2 )
   LOCAL arrayNew := AClone( arraySrc1 )
   LOCAL nLen1 := Len( arrayNew )

   ASize( arrayNew, nLen1 + Len( arraySrc2 ) )

   RETURN ACopy( arraySrc2, arrayNew, , , nLen1 + 1 )

STATIC FUNCTION ArrayAJoin( arrayList )
   LOCAL array := AClone( arrayList[ 1 ] )
   LOCAL tmp
   LOCAL nLenArray := Len( arrayList )
   LOCAL nLen
   LOCAL nPos := Len( array ) + 1

   nLen := 0
   FOR tmp := 1 TO nLenArray
      nLen += Len( arrayList[ tmp ] )
   NEXT

   ASize( array, nLen )

   FOR tmp := 2 TO nLenArray
      ACopy( arrayList[ tmp ], array, , , nPos )
      nPos += Len( arrayList[ tmp ] )
   NEXT

   RETURN array

STATIC FUNCTION AAddNotEmpty( array, xItem )

   IF ! Empty( xItem )
      AAdd( array, xItem )
   ENDIF

   RETURN array

STATIC FUNCTION ListDirExt( arraySrc, cDirNew, cExtNew )
   LOCAL array := AClone( arraySrc )
   LOCAL cFileName

   FOR EACH cFileName IN array
      cFileName := FN_DirExtSet( cFileName, cDirNew, cExtNew )
   NEXT

   RETURN array

/* Forms the list of libs as to appear on the command line */
STATIC FUNCTION ListCookLib( arraySrc, cPrefix, cExtNew )
   LOCAL array := AClone( arraySrc )
   LOCAL cDir
   LOCAL cLibName

   IF t_cCOMP $ "gcc|gpp|mingw|mingw64|mingwarm|djgpp|cygwin"
      FOR EACH cLibName IN array
         hb_FNameSplit( cLibName, @cDir )
         IF Empty( cDir )
#if 0
            /* Don't attempt to strip this as it can be valid for libs which have double lib prefixes (f.e. libpng) */
            IF Left( cLibName, 3 ) == "lib"
               cLibName := SubStr( cLibName, 4 )
            ENDIF
#endif
            IF cPrefix != NIL
               cLibName := cPrefix + cLibName
            ENDIF
            IF cExtNew != NIL
               cLibName := FN_ExtSet( cLibName, cExtNew )
            ENDIF
         ENDIF
      NEXT
   ELSE
      FOR EACH cLibName IN array
         IF cPrefix != NIL
            cLibName := cPrefix + cLibName
         ENDIF
         IF cExtNew != NIL
            cLibName := FN_ExtSet( cLibName, cExtNew )
         ENDIF
      NEXT
   ENDIF

   RETURN array

/* Append optional prefix and optional extension to all members */
STATIC FUNCTION ListCook( arraySrc, cPrefix, cExtNew )
   LOCAL array := AClone( arraySrc )
   LOCAL cItem

   FOR EACH cItem IN array
      IF cPrefix != NIL
         cItem := cPrefix + cItem
      ENDIF
      IF cExtNew != NIL
         cItem := FN_ExtSet( cItem, cExtNew )
      ENDIF
   NEXT

   RETURN array

STATIC FUNCTION ArrayToList( array, cSeparator )
   LOCAL cString := ""
   LOCAL tmp

   DEFAULT cSeparator TO " "

   FOR tmp := 1 TO Len( array )
      cString += array[ tmp ]
      IF tmp < Len( array )
         cString += cSeparator
      ENDIF
   NEXT

   RETURN cString

STATIC FUNCTION ListToArray( cList )
   LOCAL array := {}
   LOCAL cItem

   IF ! Empty( cList )
      FOR EACH cItem IN hb_ATokens( cList )
         AAddNotEmpty( array, cItem )
      NEXT
   ENDIF

   RETURN array

/* NOTE: Can hurt if there are symlinks on the way. */
/* NOTE: This function also adds an ending separator. */
STATIC FUNCTION PathNormalize( cPath, lNormalize )
   LOCAL nLastSep
   LOCAL nNextSep

   DEFAULT lNormalize TO .T.

   cPath := DirAddPathSep( cPath )

   IF lNormalize
      nLastSep := iif( Left( cPath, 1 ) == hb_osPathSeparator(), 1, 0 )
      DO WHILE ( nNextSep := hb_At( hb_osPathSeparator(), cPath, nLastSep + 1 ) ) > 0
         SWITCH SubStr( cPath, nLastSep + 1, nNextSep - nLastSep - 1 )
         CASE ".."
            nLastSep := hb_Rat( hb_osPathSeparator(), cPath, 1, nLastSep - 1 )
            IF nLastSep == 0
               /* Underflow. Return where we are. */
               RETURN cPath
            ENDIF
         CASE "."
         CASE ""
            cPath := Left( cPath, nLastSep ) + SubStr( cPath, nNextSep + 1 )
            EXIT
         OTHERWISE
            nLastSep := nNextSep
         ENDSWITCH
      ENDDO
   ENDIF

   RETURN cPath

STATIC FUNCTION PathProc( cPathR, cPathA )
   LOCAL cDirA
   LOCAL cDirR, cDriveR, cNameR, cExtR

   IF Empty( cPathA )
      RETURN cPathR
   ENDIF

   hb_FNameSplit( cPathR, @cDirR, @cNameR, @cExtR, @cDriveR )

   IF ! Empty( cDriveR ) .OR. ( ! Empty( cDirR ) .AND. Left( cDirR, 1 ) $ hb_osPathDelimiters() )
      RETURN cPathR
   ENDIF

   hb_FNameSplit( cPathA, @cDirA )

   IF Empty( cDirA )
      RETURN cPathR
   ENDIF

   RETURN hb_FNameMerge( cDirA + cDirR, cNameR, cExtR )

STATIC FUNCTION PathSepToSelf( cFileName )
#if defined( __PLATFORM__WINDOWS ) .OR. ;
    defined( __PLATFORM__DOS ) .OR. ;
    defined( __PLATFORM__OS2 )
   RETURN StrTran( cFileName, "/", "\" )
#else
   RETURN StrTran( cFileName, "\", "/" )
#endif

STATIC FUNCTION PathSepToTarget( cFileName, nStart )

   DEFAULT nStart TO 1

   IF t_cARCH $ "win|wce|dos|os2" .AND. !( t_cCOMP $ "mingw|mingw64|mingwarm|cygwin" )
      RETURN Left( cFileName, nStart - 1 ) + StrTran( SubStr( cFileName, nStart ), "/", "\" )
   ENDIF

   RETURN Left( cFileName, nStart - 1 ) + StrTran( SubStr( cFileName, nStart ), "\", "/" )

STATIC FUNCTION DirAddPathSep( cDir )

   IF ! Empty( cDir ) .AND. !( Right( cDir, 1 ) == hb_osPathSeparator() )
      cDir += hb_osPathSeparator()
   ENDIF

   RETURN cDir

STATIC FUNCTION DirDelPathSep( cDir )

   IF Empty( hb_osDriveSeparator() )
      DO WHILE Len( cDir ) > 1 .AND. Right( cDir, 1 ) == hb_osPathSeparator()
         cDir := hb_StrShrink( cDir, 1 )
      ENDDO
   ELSE
      DO WHILE Len( cDir ) > 1 .AND. Right( cDir, 1 ) == hb_osPathSeparator() .AND. ;
               !( Right( cDir, 2 ) == hb_osDriveSeparator() + hb_osPathSeparator() )
         cDir := hb_StrShrink( cDir, 1 )
      ENDDO
   ENDIF

   RETURN cDir

#define hb_DirCreate( d ) MakeDir( d )

FUNCTION DirBuild( cDir )
   LOCAL cDirTemp
   LOCAL cDirItem
   LOCAL tmp

   IF ! hb_DirExists( cDir )

      cDir := DirAddPathSep( cDir )

      IF ! Empty( hb_osDriveSeparator() ) .AND. ;
         ( tmp := At( hb_osDriveSeparator(), cDir ) ) > 0
         cDirTemp := Left( cDir, tmp )
         cDir := SubStr( cDir, tmp + 1 )
      ELSE
         cDirTemp := ""
      ENDIF

      FOR EACH cDirItem IN hb_ATokens( cDir, hb_osPathSeparator() )
         IF !( Right( cDirTemp, 1 ) == hb_osPathSeparator() ) .AND. ! Empty( cDirTemp )
            cDirTemp += hb_osPathSeparator()
         ENDIF
         IF ! Empty( cDirItem )  /* Skip root path, if any */
            cDirTemp += cDirItem
            IF hb_FileExists( cDirTemp )
               RETURN .F.
            ELSEIF ! hb_DirExists( cDirTemp )
               IF hb_DirCreate( cDirTemp ) != 0
                  RETURN .F.
               ENDIF
            ENDIF
         ENDIF
      NEXT
   ENDIF

   RETURN .T.

#define hb_DirDelete( d ) DirRemove( d )

FUNCTION DirUnbuild( cDir )
   LOCAL cDirTemp
   LOCAL tmp

   IF hb_DirExists( cDir )

      cDir := DirDelPathSep( cDir )

      cDirTemp := cDir
      DO WHILE ! Empty( cDirTemp )
         IF hb_DirExists( cDirTemp )
            IF hb_DirDelete( cDirTemp ) != 0
               RETURN .F.
            ENDIF
         ENDIF
         IF ( tmp := RAt( hb_osPathSeparator(), cDirTemp ) ) == 0
            EXIT
         ENDIF
         cDirTemp := Left( cDirTemp, tmp - 1 )
         IF ! Empty( hb_osDriveSeparator() ) .AND. ;
            Right( cDirTemp, 1 ) == hb_osDriveSeparator()
            EXIT
         ENDIF
      ENDDO
   ENDIF

   RETURN .T.

STATIC FUNCTION FN_DirGet( cFileName )
   LOCAL cDir

   hb_FNameSplit( cFileName, @cDir )

   RETURN cDir

STATIC FUNCTION FN_NameGet( cFileName )
   LOCAL cName

   hb_FNameSplit( cFileName,, @cName )

   RETURN cName

STATIC FUNCTION FN_ExtGet( cFileName )
   LOCAL cExt

   hb_FNameSplit( cFileName,,, @cExt )

   RETURN cExt

STATIC FUNCTION FN_ExtSet( cFileName, cExt )
   LOCAL cDir, cName

   hb_FNameSplit( cFileName, @cDir, @cName )

   RETURN hb_FNameMerge( cDir, cName, cExt )

STATIC FUNCTION FN_DirExtSet( cFileName, cDirNew, cExtNew )
   LOCAL cDir, cName, cExt

   hb_FNameSplit( cFileName, @cDir, @cName, @cExt )

   IF cDirNew != NIL
      cDir := cDirNew
   ENDIF
   IF cExtNew != NIL
      cExt := cExtNew
   ENDIF

   RETURN hb_FNameMerge( cDir, cName, cExt )

#define HBMK_CFG_NAME  "hbmk.cfg"

STATIC PROCEDURE HBP_ProcessAll( lConfigOnly,;
                                 /* @ */ aLIBUSER,;
                                 /* @ */ aLIBUSERGT,;
                                 /* @ */ aLIBPATH,;
                                 /* @ */ aLIBDYNHAS,;
                                 /* @ */ aOPTPRG,;
                                 /* @ */ aOPTC,;
                                 /* @ */ aOPTL,;
                                 /* @ */ lGUI,;
                                 /* @ */ lMT,;
                                 /* @ */ lSHARED,;
                                 /* @ */ lSTATICFULL,;
                                 /* @ */ lDEBUG,;
                                 /* @ */ lNULRDD,;
                                 /* @ */ lMAP,;
                                 /* @ */ lSTRIP,;
                                 /* @ */ nCOMPR,;
                                 /* @ */ lRUN,;
                                 /* @ */ lINC,;
                                 /* @ */ cGT )
   LOCAL aFile
   LOCAL cDir
   LOCAL cFileName

   LOCAL aCFGDirs

   #if defined( __PLATFORM__UNIX )
      aCFGDirs := { GetEnv( "HOME" ) + "/.harbour/",;
                    "/etc/harbour",;
                    DirAddPathSep( hb_DirBase() ) + "../etc/harbour",;
                    DirAddPathSep( hb_DirBase() ) + "../etc",;
                    hb_DirBase() }
   #else
      aCFGDirs := { hb_DirBase() }
   #endif

   FOR EACH cDir IN aCFGDirs
      IF hb_FileExists( cFileName := ( PathNormalize( DirAddPathSep( cDir ) ) + HBMK_CFG_NAME ) )
         IF ! t_lQuiet
            OutStd( "hbmk: Processing configuration: " + cFileName + hb_osNewLine() )
         ENDIF
         HBP_ProcessOne( cFileName,;
            @aLIBUSER,;
            @aLIBUSERGT,;
            @aLIBPATH,;
            @aLIBDYNHAS,;
            @aOPTPRG,;
            @aOPTC,;
            @aOPTL,;
            @lGUI,;
            @lMT,;
            @lSHARED,;
            @lSTATICFULL,;
            @lDEBUG,;
            @lNULRDD,;
            @lMAP,;
            @lSTRIP,;
            @nCOMPR,;
            @lRUN,;
            @lINC,;
            @cGT )
         EXIT
      ENDIF
   NEXT

   IF ! lConfigOnly
      FOR EACH aFile IN Directory( "*" + ".hbp" )
         cFileName := aFile[ F_NAME ]
         IF !( cFileName == HBMK_CFG_NAME ) .AND. Lower( FN_ExtGet( cFileName ) ) == ".hbp"
            IF ! t_lQuiet
               OutStd( "hbmk: Processing: " + cFileName + hb_osNewLine() )
            ENDIF
            HBP_ProcessOne( cFileName,;
               @aLIBUSER,;
               @aLIBUSERGT,;
               @aLIBPATH,;
               @aLIBDYNHAS,;
               @aOPTPRG,;
               @aOPTC,;
               @aOPTL,;
               @lGUI,;
               @lMT,;
               @lSHARED,;
               @lSTATICFULL,;
               @lDEBUG,;
               @lNULRDD,;
               @lMAP,;
               @lSTRIP,;
               @nCOMPR,;
               @lRUN,;
               @lINC,;
               @cGT )
         ENDIF
      NEXT
   ENDIF

   RETURN

#define _EOL          Chr( 10 )

STATIC PROCEDURE HBP_ProcessOne( cFileName,;
                                 /* @ */ aLIBUSER,;
                                 /* @ */ aLIBUSERGT,;
                                 /* @ */ aLIBPATH,;
                                 /* @ */ aLIBDYNHAS,;
                                 /* @ */ aOPTPRG,;
                                 /* @ */ aOPTC,;
                                 /* @ */ aOPTL,;
                                 /* @ */ lGUI,;
                                 /* @ */ lMT,;
                                 /* @ */ lSHARED,;
                                 /* @ */ lSTATICFULL,;
                                 /* @ */ lDEBUG,;
                                 /* @ */ lNULRDD,;
                                 /* @ */ lMAP,;
                                 /* @ */ lSTRIP,;
                                 /* @ */ nCOMPR,;
                                 /* @ */ lRUN,;
                                 /* @ */ lINC,;
                                 /* @ */ cGT )
   LOCAL cFile := MemoRead( cFileName ) /* NOTE: Intentionally using MemoRead() which handles EOF char. */
   LOCAL cLine
   LOCAL cItem

   IF ! hb_osNewLine() == _EOL
      cFile := StrTran( cFile, hb_osNewLine(), _EOL )
   ENDIF
   IF ! hb_osNewLine() == Chr( 13 ) + Chr( 10 )
      cFile := StrTran( cFile, Chr( 13 ) + Chr( 10 ), _EOL )
   ENDIF

   FOR EACH cLine IN hb_ATokens( cFile, _EOL )

      cLine := AllTrim( ArchCompFilter( AllTrim( cLine ) ) )

      DO CASE
      CASE Lower( Left( cLine, Len( "libs="       ) ) ) == "libs="       ; cLine := SubStr( cLine, Len( "libs="       ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            cItem := PathSepToTarget( StrStripQuote( cItem ) )
            IF AScan( aLIBUSER, {|tmp| tmp == cItem } ) == 0
               AAddNotEmpty( aLIBUSER, cItem )
            ENDIF
         NEXT

      CASE Lower( Left( cLine, Len( "libpaths="   ) ) ) == "libpaths="   ; cLine := SubStr( cLine, Len( "libpaths="   ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            cItem := PathSepToTarget( StrStripQuote( cItem ) )
            IF AScan( aLIBPATH, {|tmp| tmp == cItem } ) == 0
               AAddNotEmpty( aLIBPATH, cItem )
            ENDIF
         NEXT

      /* NOTE: This keyword is used in hbmk.cfg and signals whether
               a given optional module (gtsln, gtcrs, gtxwc) is part of the
               Harbour shared library, so that we can automatically add
               the required libs here. [vszakats] */
      CASE Lower( Left( cLine, Len( "libdynhas="  ) ) ) == "libdynhas="  ; cLine := SubStr( cLine, Len( "libdynhas="  ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            cItem := PathSepToTarget( StrStripQuote( cItem ) )
            IF ! Empty( cItem )
               IF AScan( aLIBDYNHAS, {|tmp| tmp == cItem } ) == 0
                  AAdd( aLIBDYNHAS, cItem )
               ENDIF
               IF Lower( Left( cItem, 2 ) ) == "gt" .AND. ;
                  AScan( t_aLIBCOREGT, {|tmp| Lower( tmp ) == Lower( cItem ) } ) == 0
                  AAdd( t_aLIBCOREGT, cItem )
               ENDIF
            ENDIF
         NEXT

      CASE Lower( Left( cLine, Len( "prgflags="   ) ) ) == "prgflags="   ; cLine := SubStr( cLine, Len( "prgflags="   ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            cItem := PathSepToTarget( StrStripQuote( cItem ) )
            IF AScan( aOPTPRG, {|tmp| tmp == cItem } ) == 0
               AAddNotEmpty( aOPTPRG, cItem )
            ENDIF
         NEXT

      CASE Lower( Left( cLine, Len( "cflags="     ) ) ) == "cflags="     ; cLine := SubStr( cLine, Len( "cflags="     ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            cItem := StrStripQuote( cItem )
            IF AScan( aOPTC, {|tmp| tmp == cItem } ) == 0
               AAddNotEmpty( aOPTC, cItem )
            ENDIF
         NEXT

      CASE Lower( Left( cLine, Len( "ldflags="    ) ) ) == "ldflags="    ; cLine := SubStr( cLine, Len( "ldflags="    ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            cItem := StrStripQuote( cItem )
            IF AScan( aOPTL, {|tmp| tmp == cItem } ) == 0
               AAddNotEmpty( aOPTL, cItem )
            ENDIF
         NEXT

      CASE Lower( Left( cLine, Len( "gui="        ) ) ) == "gui="        ; cLine := SubStr( cLine, Len( "gui="        ) + 1 )
         DO CASE
         CASE ValueIsT( cLine ) ; lGUI := .T.
         CASE ValueIsF( cLine ) ; lGUI := .F.
         ENDCASE

      CASE Lower( Left( cLine, Len( "mt="         ) ) ) == "mt="         ; cLine := SubStr( cLine, Len( "mt="         ) + 1 )
         DO CASE
         CASE Lower( cLine ) == "mt" ; lMT := .T. /* Compatibility */
         CASE ValueIsT( cLine ) ; lMT := .T.
         CASE ValueIsF( cLine ) ; lMT := .F.
         ENDCASE

      CASE Lower( Left( cLine, Len( "shareddef="  ) ) ) == "shareddef="  ; cLine := SubStr( cLine, Len( "shareddef="  ) + 1 )
         IF lSHARED == NIL
            DO CASE
            CASE ValueIsT( cLine ) ; lSHARED := .T. ; lSTATICFULL := .F.
            CASE ValueIsF( cLine ) ; lSHARED := .F. ; lSTATICFULL := .F.
            ENDCASE
         ENDIF
      CASE Lower( Left( cLine, Len( "shared="     ) ) ) == "shared="     ; cLine := SubStr( cLine, Len( "shared="     ) + 1 )
         DO CASE
         CASE ValueIsT( cLine ) ; lSHARED := .T. ; lSTATICFULL := .F.
         CASE ValueIsF( cLine ) ; lSHARED := .F. ; lSTATICFULL := .F.
         ENDCASE

      CASE Lower( Left( cLine, Len( "fullstatic=" ) ) ) == "fullstatic=" ; cLine := SubStr( cLine, Len( "fullstatic=" ) + 1 )
         DO CASE
         CASE ValueIsT( cLine ) ; lSHARED := .F. ; lSTATICFULL := .T.
         CASE ValueIsF( cLine ) ; lSHARED := .F. ; lSTATICFULL := .F.
         ENDCASE

      CASE Lower( Left( cLine, Len( "debug="      ) ) ) == "debug="      ; cLine := SubStr( cLine, Len( "debug="      ) + 1 )
         DO CASE
         CASE ValueIsT( cLine ) ; lDEBUG := .T.
         CASE ValueIsF( cLine ) ; lDEBUG := .F.
         ENDCASE

      CASE Lower( Left( cLine, Len( "nulrdd="     ) ) ) == "nulrdd="     ; cLine := SubStr( cLine, Len( "nulrdd="     ) + 1 )
         DO CASE
         CASE ValueIsT( cLine ) ; lNULRDD := .T.
         CASE ValueIsF( cLine ) ; lNULRDD := .F.
         ENDCASE

      CASE Lower( Left( cLine, Len( "map="        ) ) ) == "map="        ; cLine := SubStr( cLine, Len( "map="        ) + 1 )
         DO CASE
         CASE ValueIsT( cLine ) ; lMAP := .T.
         CASE ValueIsF( cLine ) ; lMAP := .F.
         ENDCASE

      CASE Lower( Left( cLine, Len( "strip="      ) ) ) == "strip="      ; cLine := SubStr( cLine, Len( "strip="      ) + 1 )
         DO CASE
         CASE ValueIsT( cLine ) ; lSTRIP := .T.
         CASE ValueIsF( cLine ) ; lSTRIP := .F.
         ENDCASE

      CASE Lower( Left( cLine, Len( "compr="      ) ) ) == "compr="      ; cLine := SubStr( cLine, Len( "compr="      ) + 1 )
         DO CASE
         CASE ValueIsT( cLine )       ; nCOMPR := _COMPR_DEF
         CASE ValueIsF( cLine )       ; nCOMPR := _COMPR_OFF
         CASE Lower( cLine ) == "def" ; nCOMPR := _COMPR_DEF
         CASE Lower( cLine ) == "min" ; nCOMPR := _COMPR_MIN
         CASE Lower( cLine ) == "max" ; nCOMPR := _COMPR_MAX
         ENDCASE

      CASE Lower( Left( cLine, Len( "run="        ) ) ) == "run="        ; cLine := SubStr( cLine, Len( "run="        ) + 1 )
         DO CASE
         CASE ValueIsT( cLine ) ; lRUN := .T.
         CASE ValueIsF( cLine ) ; lRUN := .F.
         ENDCASE

      CASE Lower( Left( cLine, Len( "inc="        ) ) ) == "inc="        ; cLine := SubStr( cLine, Len( "inc="        ) + 1 )
         DO CASE
         CASE ValueIsT( cLine ) ; lINC := .T.
         CASE ValueIsF( cLine ) ; lINC := .F.
         ENDCASE

      /* NOTE: This keyword is used to signal the default GT used when
               building Harbour. It only needs to be filled if this default
               GT is different from the Harbour default one, IOW when it
               was overridden by user at Harbour build time. [vszakats] */
      CASE Lower( Left( cLine, Len( "gtdef="      ) ) ) == "gtdef="      ; cLine := SubStr( cLine, Len( "gtdef="      ) + 1 )
         IF ! Empty( cLine )
            IF ! SetupForGT( cLine, @t_cGTDEFAULT, @lGUI )
               cLine := NIL
            ENDIF
            IF ! Empty( cLine ) .AND. !( Lower( cLine ) == "gtnul" )
               IF AScan( t_aLIBCOREGT, {|tmp| Lower( tmp ) == Lower( cLine ) } ) == 0 .AND. ;
                  AScan( aLIBUSERGT  , {|tmp| Lower( tmp ) == Lower( cLine ) } ) == 0
                  AAddNotEmpty( aLIBUSERGT, PathSepToTarget( cLine ) )
               ENDIF
            ENDIF
         ENDIF

      CASE Lower( Left( cLine, Len( "gt="         ) ) ) == "gt="         ; cLine := SubStr( cLine, Len( "gt="         ) + 1 )
         IF ! Empty( cLine )
            IF cGT == NIL
               IF ! SetupForGT( cLine, @cGT, @lGUI )
                  cLine := NIL
               ENDIF
            ENDIF
            IF ! Empty( cLine ) .AND. !( Lower( cLine ) == "gtnul" )
               IF AScan( t_aLIBCOREGT, {|tmp| Lower( tmp ) == Lower( cLine ) } ) == 0 .AND. ;
                  AScan( aLIBUSERGT  , {|tmp| Lower( tmp ) == Lower( cLine ) } ) == 0
                  AAddNotEmpty( aLIBUSERGT, PathSepToTarget( cLine ) )
               ENDIF
            ENDIF
         ENDIF

      ENDCASE
   NEXT

   RETURN

/* aGT = GTs requested
   cGT = GT requested for default.
         Can be NIL, when it's the Harbour default.
         Isn't necessarily on the aGT list in case it
         is a _non-default_ core GT. */
STATIC FUNCTION IsGTRequested( cGT, aGT, aLIBDYNHAS, lSHARED, cWhichGT )

   HB_SYMBOL_UNUSED( cGT )
   HB_SYMBOL_UNUSED( aGT )
   HB_SYMBOL_UNUSED( aLIBDYNHAS )

   IF ! lSHARED
      /* Check if it's a core GT. */
      RETURN AScan( t_aLIBCOREGT, {|tmp| Lower( tmp ) == cWhichGT } ) > 0
   ENDIF

   RETURN .F.

STATIC FUNCTION StrStripQuote( cString )
   RETURN iif( Left( cString, 1 ) == Chr( 34 ) .AND. Right( cString, 1 ) == Chr( 34 ),;
             SubStr( cString, 2, Len( cString ) - 2 ),;
             cString )

STATIC FUNCTION ValueIsT( cString )
   cString := Lower( cString )
   RETURN cString == "yes" .OR. ;
          cString == "1" /* Compatibility */

STATIC FUNCTION ValueIsF( cString )
   cString := Lower( cString )
   RETURN cString == "no" .OR. ;
          cString == "0" /* Compatibility */

STATIC PROCEDURE HBM_Load( aParams, cFileName, /* @ */ nEmbedLevel )
   LOCAL cFile
   LOCAL cLine
   LOCAL cParam

   IF hb_FileExists( cFileName )

      cFile := MemoRead( cFileName ) /* NOTE: Intentionally using MemoRead() which handles EOF char. */

      IF ! hb_osNewLine() == _EOL
         cFile := StrTran( cFile, hb_osNewLine(), _EOL )
      ENDIF
      IF ! hb_osNewLine() == Chr( 13 ) + Chr( 10 )
         cFile := StrTran( cFile, Chr( 13 ) + Chr( 10 ), _EOL )
      ENDIF

      FOR EACH cLine IN hb_ATokens( cFile, _EOL )
         IF !( Left( cLine, 1 ) == "#" )
            FOR EACH cParam IN hb_ATokens( cLine,, .T. )
               cParam := StrStripQuote( cParam )
               IF ! Empty( cParam )
                  DO CASE
                  CASE ( Len( cParam ) >= 1 .AND. Left( cParam, 1 ) == "@" )
                     IF nEmbedLevel < 3
                        cParam := SubStr( cParam, 2 )
                        IF Empty( FN_ExtGet( cParam ) )
                           cParam := FN_ExtSet( cParam, ".hbm" )
                        ENDIF
                        nEmbedLevel++
                        HBM_Load( aParams, PathProc( cParam, cFileName ), @nEmbedLevel ) /* Load parameters from script file */
                     ENDIF
                  CASE Lower( FN_ExtGet( cParam ) ) == ".hbm"
                     IF nEmbedLevel < 3
                        nEmbedLevel++
                        HBM_Load( aParams, PathProc( cParam, cFileName ), @nEmbedLevel ) /* Load parameters from script file */
                     ENDIF
                  OTHERWISE
                     AAdd( aParams, { cParam, cFileName, cLine:__enumIndex() } )
                  ENDCASE
               ENDIF
            NEXT
         ENDIF
      NEXT
   ELSE
      OutErr( "hbmk: Warning: File cannot be found: " + cFileName + hb_osNewLine() )
   ENDIF

   RETURN

/* Filter microformat:
   {[!][<arch|comp>]['&'|'|'][...]}
*/

STATIC FUNCTION ArchCompFilter( cItem )
   LOCAL nStart, nEnd, nPos
   LOCAL cFilterSrc
   LOCAL cFilterHarb
   LOCAL bFilter
   LOCAL xResult
   LOCAL cValue

   LOCAL cExpr := "( hbmk_ARCH() == Lower( '%1' ) .OR. " +;
                    "hbmk_COMP() == Lower( '%1' ) .OR. " +;
                    "hbmk_KEYW( Lower( '%' ) ) )"

   IF ( nStart := At( "{", cItem ) ) > 0 .AND. ;
      ( nEnd := hb_At( "}", cItem, nStart ) ) > 0

      /* Separate filter from the rest of the item */
      cFilterSrc := SubStr( cItem, nStart + 1, nEnd - nStart - 1 )
      cItem := Left( cItem, nStart - 1 ) + SubStr( cItem, nEnd + 1 )

      /* Parse filter and convert it to Harbour expression */
      cFilterHarb := ""
      cValue := ""
      FOR nPos := 1 TO Len( cFilterSrc )
         IF IsDigit( SubStr( cFilterSrc, nPos, 1 ) ) .OR. ;
            IsAlpha( SubStr( cFilterSrc, nPos, 1 ) )
            cValue += SubStr( cFilterSrc, nPos, 1 )
         ELSE
            IF ! Empty( cValue )
               cFilterHarb += StrTran( cExpr, "%1", cValue ) + SubStr( cFilterSrc, nPos, 1 )
               cValue := ""
            ELSE
               cFilterHarb += SubStr( cFilterSrc, nPos, 1 )
            ENDIF
         ENDIF
      NEXT
      IF ! Empty( cValue )
         cFilterHarb += StrTran( cExpr, "%1", cValue ) + SubStr( cFilterSrc, nPos, 1 )
      ENDIF

      cFilterHarb := StrTran( cFilterHarb, "&", ".AND." )
      cFilterHarb := StrTran( cFilterHarb, "|", ".OR." )

      /* Evaluate filter */
      bFilter := hb_macroBlock( cFilterHarb )
      IF bFilter != NIL
         IF ISLOGICAL( xResult := Eval( bFilter ) ) .AND. xResult
            RETURN cItem
         ENDIF
      ENDIF
      RETURN ""
   ENDIF

   RETURN cItem

STATIC FUNCTION MacroProc( cString, cDirParent )
   LOCAL nStart
   LOCAL nEnd
   LOCAL cMacro

   DO WHILE ( nStart := At( "${", cString ) ) > 0 .AND. ;
            ( nEnd := hb_At( "}", cString, nStart ) ) > 0

      cMacro := Upper( SubStr( cString, nStart + 2, nEnd - nStart - 1 ) )

      DO CASE
      CASE cMacro == "HB_ROOT"
         cMacro := PathSepToSelf( DirAddPathSep( hb_DirBase() ) )
      CASE cMacro == "HB_PARENT"
         IF Empty( cDirParent )
            cMacro := ""
         ELSE
            cMacro := PathSepToSelf( DirAddPathSep( cDirParent ) )
         ENDIF
      CASE ! Empty( GetEnv( cMacro ) )
         cMacro := GetEnv( cMacro )
      OTHERWISE
         /* NOTE: Macro not found, completely ignore it
                  (for now without warning) [vszakats] */
         cMacro := ""
      ENDCASE

      cString := Left( cString, nStart - 1 ) + cMacro + SubStr( cString, nEnd + 1 )
   ENDDO

   RETURN cString

#define HB_ISALPHA( c )         ( Upper( c ) >= "A" .AND. Upper( c ) <= "Z" )
#define HB_ISFIRSTIDCHAR( c )   ( HB_ISALPHA( c ) .OR. ( c ) == '_' )
#define HB_ISNEXTIDCHAR( c )    ( HB_ISFIRSTIDCHAR(c) .OR. IsDigit( c ) )

STATIC FUNCTION IsValidHarbourID( cName )
   LOCAL tmp
   IF HB_ISFIRSTIDCHAR( Left( cName, 1 ) )
      FOR tmp := 2 TO Len( cName )
         IF ! HB_ISNEXTIDCHAR( SubStr( cName, tmp, 1 ) )
            RETURN .F.
         ENDIF
      NEXT
      RETURN .T.
   ENDIF
   RETURN .F.

/* in GCC LD (except DJGPP) the order of registering init function
 * does not depend directly on the order of linked files. If we want
 * to inform HVM about valid startup function then we should try to
 * locate it ourselves and pass it to HVM using our startup function
 * [druzus]
 */
STATIC FUNCTION getFirstFunc( cFile )
   LOCAL cFuncList, cExecNM, cFuncName, cExt, cLine, n, c

   cFuncName := ""
   IF t_cCOMP $ "gcc|gpp|mingw|mingw64|mingwarm|cygwin"
      hb_FNameSplit( cFile,,, @cExt )
      IF cExt == ".c"
         FOR EACH cLine IN hb_ATokens( StrTran( hb_MemoRead( cFile ), Chr( 13 ), Chr( 10 ) ), Chr( 10 ) )
            cLine := AllTrim( cLine )
            IF cLine = '{ "' .AND. "HB_FS_FIRST" $ cLine
               n := 4
               DO WHILE ( c := SubStr( cLine, n++, 1 ) ) != '"'
                  cFuncName += c
               ENDDO
               EXIT
            ENDIF
         NEXT
      ELSEIF ! Empty( cExecNM := FindInPath( t_cCCPREFIX + "nm" ) )
         cFuncList := commandResult( cExecNM + " " + cFile + " -g -n --defined-only -C" )
         IF ( n := At( " T HB_FUN_", cFuncList ) ) != 0
            n += 10
            DO WHILE ( c := SubStr( cFuncList, n++, 1 ) ) = "_" .OR. ;
                  IsDigit( c ) .OR. IsAlpha( c )
               cFuncName += c
            ENDDO
         ENDIF
      ENDIF
   ENDIF

   RETURN cFuncName

STATIC FUNCTION commandResult( cCommand, nResult )
   LOCAL hFile, cFileName, cResult

   hFile := hb_FTempCreateEx( @cFileName )

   IF hFile != F_ERROR
      FClose( hFile )
      cCommand += ">" + cFileName
      nResult := hb_run( cCommand )
      cResult := hb_MemoRead( cFileName )
      FErase( cFileName )
   ELSE
      OutErr( "hbmk: Error: Cannot create temporary file." + hb_osNewLine() )
   ENDIF

   RETURN cResult

PROCEDURE PlatformPRGFlags( aOPTPRG )

   IF !( t_cARCH == hb_Version( HB_VERSION_BUILD_ARCH ) ) .OR. ;
      t_cARCH == "wce"

      #if   defined( __PLATFORM__WINDOWS )
         AAdd( aOPTPRG, "-undef:__PLATFORM__WINDOWS" )
         #if defined( __PLATFORM__WINCE )
            AAdd( aOPTPRG, "-undef:__PLATFORM__WINCE" )
         #endif
      #elif defined( __PLATFORM__DOS )
         AAdd( aOPTPRG, "-undef:__PLATFORM__DOS" )
      #elif defined( __PLATFORM__OS2 )
         AAdd( aOPTPRG, "-undef:__PLATFORM__OS2" )
      #elif defined( __PLATFORM__LINUX )
         AAdd( aOPTPRG, "-undef:__PLATFORM__LINUX" )
         AAdd( aOPTPRG, "-undef:__PLATFORM__UNIX" )
      #elif defined( __PLATFORM__DARWIN )
         AAdd( aOPTPRG, "-undef:__PLATFORM__DARWIN" )
         AAdd( aOPTPRG, "-undef:__PLATFORM__UNIX" )
      #elif defined( __PLATFORM__BSD )
         AAdd( aOPTPRG, "-undef:__PLATFORM__BSD" )
         AAdd( aOPTPRG, "-undef:__PLATFORM__UNIX" )
      #elif defined( __PLATFORM__SUNOS )
         AAdd( aOPTPRG, "-undef:__PLATFORM__SUNOS" )
         AAdd( aOPTPRG, "-undef:__PLATFORM__UNIX" )
      #elif defined( __PLATFORM__HPUX )
         AAdd( aOPTPRG, "-undef:__PLATFORM__HPUX" )
         AAdd( aOPTPRG, "-undef:__PLATFORM__UNIX" )
      #endif

      DO CASE
      CASE t_cARCH == "wce"
         AAdd( aOPTPRG, "-D__PLATFORM__WINDOWS" )
         AAdd( aOPTPRG, "-D__PLATFORM__WINCE" )
      CASE t_cARCH == "win"
         AAdd( aOPTPRG, "-D__PLATFORM__WINDOWS" )
      CASE t_cARCH == "dos"
         AAdd( aOPTPRG, "-D__PLATFORM__DOS" )
      CASE t_cARCH == "os2"
         AAdd( aOPTPRG, "-D__PLATFORM__OS2" )
      CASE t_cARCH == "linux"
         AAdd( aOPTPRG, "-D__PLATFORM__LINUX" )
         AAdd( aOPTPRG, "-D__PLATFORM__UNIX" )
      CASE t_cARCH == "darwin"
         AAdd( aOPTPRG, "-D__PLATFORM__DARWIN" )
         AAdd( aOPTPRG, "-D__PLATFORM__UNIX" )
      CASE t_cARCH == "bsd"
         AAdd( aOPTPRG, "-D__PLATFORM__BDS" )
         AAdd( aOPTPRG, "-D__PLATFORM__UNIX" )
      CASE t_cARCH == "sunos"
         AAdd( aOPTPRG, "-D__PLATFORM__SUNOS" )
         AAdd( aOPTPRG, "-D__PLATFORM__UNIX" )
      CASE t_cARCH == "hpux"
         AAdd( aOPTPRG, "-D__PLATFORM__HPUX" )
         AAdd( aOPTPRG, "-D__PLATFORM__UNIX" )
      ENDCASE
   ENDIF

   RETURN

#define RTLNK_MODE_NONE       0
#define RTLNK_MODE_OUT        1
#define RTLNK_MODE_FILE       2
#define RTLNK_MODE_FILENEXT   3
#define RTLNK_MODE_LIB        4
#define RTLNK_MODE_LIBNEXT    5
#define RTLNK_MODE_SKIP       6
#define RTLNK_MODE_SKIPNEXT   7

STATIC PROCEDURE rtlnk_libtrans( aLibList )
   STATIC hTrans := { ;
      "CT"        => "hbct"   , ;
      "CTP"       => "hbct"   , ;
      "CLASSY"    => NIL      , ;
      "CSYINSP"   => NIL      , ;
      "SIX3"      => NIL      , ;
      "NOMACH6"   => NIL      , ;
      "BLXRATEX"  => NIL      , ;
      "BLXCLP50"  => NIL      , ;
      "BLXCLP52"  => NIL      , ;
      "BLXCLP53"  => NIL      , ;
      "EXOSPACE"  => NIL      , ;
      "CLIPPER"   => NIL      , ;
      "EXTEND"    => NIL      , ;
      "TERMINAL"  => NIL      , ;
      "PCBIOS"    => NIL      , ;
      "ANSITERM"  => NIL      , ;
      "DBFBLOB"   => NIL      , ;
      "DBFMEMO"   => NIL      , ;
      "DBFNTX"    => NIL      , ;
      "DBFCDX"    => NIL      , ;
      "_DBFCDX"   => NIL      , ;
      "CLD"       => NIL      , ;
      "CLDR"      => NIL      , ;
      "LLIBCE"    => NIL      , ;
      "LLIBCA"    => NIL      }
   LOCAL cLib

   FOR EACH cLib IN aLibList DESCEND
      IF Lower( Right( cLib, 4 ) ) == ".lib"
         cLib := Left( cLib, Len( cLib ) - 4 )
      ENDIF
      IF Upper( cLib ) $ hTrans
         cLib := hTrans[ Upper( cLib ) ]
         IF cLib == NIL
            hb_ADel( aLibList, cLib:__enumIndex(), .T. )
         ENDIF
      ENDIF
   NEXT

   RETURN

STATIC PROCEDURE rtlnk_filetrans( aFileList )
   STATIC hTrans := { ;
      "CTUS"      => NIL      , ;
      "CTUSP"     => NIL      , ;
      "CTINT"     => NIL      , ;
      "CTINTP"    => NIL      , ;
      "__WAIT"    => NIL      , ;
      "__WAIT_4"  => NIL      , ;
      "__WAIT_B"  => NIL      , ;
      "BLXCLP50"  => NIL      , ;
      "BLXCLP52"  => NIL      , ;
      "BLXCLP53"  => NIL      , ;
      "BLDCLP50"  => NIL      , ;
      "BLDCLP52"  => NIL      , ;
      "BLDCLP53"  => NIL      , ;
      "SIXCDX"    => NIL      , ;
      "SIXNSX"    => NIL      , ;
      "SIXNTX"    => NIL      , ;
      "DBT"       => NIL      , ;
      "FPT"       => NIL      , ;
      "SMT"       => NIL      , ;
      "NOMEMO"    => NIL      , ;
      "CLD.LIB"   => NIL      }
   LOCAL cFile

   FOR EACH cFile IN aFileList DESCEND
      IF Lower( Right( cFile, 4 ) ) == ".obj"
         cFile := Left( cFile, Len( cFile ) - 4 )
      ENDIF
      IF Upper( cFile ) $ hTrans
         cFile := hTrans[ Upper( cFile ) ]
         IF cFile == NIL
            hb_ADel( aFileList, cFile:__enumIndex(), .T. )
         ENDIF
      ENDIF
   NEXT

   RETURN

STATIC FUNCTION rtlnk_read( cFileName, aPrevFiles )
   LOCAL cFileBody
   LOCAL cPath, cFile, cExt
   LOCAL hFile

   hb_FNameSplit( cFileName, @cPath, @cFile, @cExt )
   IF Empty( cExt )
      cExt := ".lnk"
   ENDIF

   cFileName := hb_FNameMerge( cPath, cFile, ".lnk" )
   /* it's blinker extension, look for .lnk file in paths
    * specified by LIB envvar
    */
   IF !hb_fileExists( cFileName ) .AND. ;
      !Left( cFileName, 1 ) $ hb_osPathDelimiters() .AND. ;
      !SubStr( cFileName, 2, 1 ) == hb_osDriveSeparator()
      FOR EACH cPath IN hb_aTokens( GetEnv( "LIB" ), hb_osPathListSeparator() )
         cFile := hb_FNameMerge( cPath, cFileName )
         IF hb_fileExists( cFile )
            cFileName := cFile
            EXIT
         ENDIF
      NEXT
   ENDIF

   /* protection against recursive calls */
   IF AScan( aPrevFiles, { |x| x == cFileName } ) == 0
      IF ( hFile := FOpen( cFileName ) ) != -1
         cFileBody := Space( FSeek( hFile, 0, FS_END ) )
         FSeek( hFile, 0, FS_SET )
         IF FRead( hFile, @cFileBody, Len( cFileBody ) ) != Len( cFileBody )
            cFileBody := NIL
         ENDIF
         FClose( hFile )
      ENDIF
      AAdd( aPrevFiles, cFileName )
   ELSE
      cFileBody := ""
   ENDIF

   RETURN cFileBody

STATIC FUNCTION rtlnk_process( cCommands, cFileOut, aFileList, aLibList, ;
                               aPrevFiles )
   LOCAL nCh, nMode
   LOCAL cLine, cWord

   cCommands := StrTran( StrTran( cCommands, Chr( 13 ) ), ",", " , " )
   FOR EACH nCh IN @cCommands
      SWITCH Asc( nCh )
      CASE 9
      CASE 11
      CASE 12
      CASE Asc( ";" )
         nCh := " "
      ENDSWITCH
   NEXT
   nMode := RTLNK_MODE_NONE
   IF ! ISARRAY( aPrevFiles )
      aPrevFiles := {}
   ENDIF
   FOR EACH cLine IN hb_ATokens( cCommands, Chr( 10 ) )
      cLine := AllTrim( cLine )
      IF !Empty( cLine ) .AND. !cLine = "#" .AND. !cLine = "//"
         IF nMode == RTLNK_MODE_NONE
            /* blinker extension */
            IF Upper( cLine ) = "ECHO "
               OutStd( "hbmk2: Blinker ECHO: " + SubStr( cLine, 6 ) + hb_osNewLine() )
               LOOP
            ELSEIF Upper( cLine ) = "BLINKER "
               /* skip blinker commands */
               LOOP
            ELSE /* TODO: add other blinker commands */
            ENDIF
         ENDIF
         FOR EACH cWord IN hb_aTokens( cLine )
            IF nMode == RTLNK_MODE_OUT
               cFileOut := cWord
               nMode := RTLNK_MODE_FILENEXT
            ELSEIF nMode == RTLNK_MODE_FILE
               IF !cWord == ","
                  IF AScan( aFileList, { |x| x == cWord } ) == 0
                     AAdd( aFileList, PathSepToTarget( cWord ) )
                  ENDIF
                  nMode := RTLNK_MODE_FILENEXT
               ENDIF
            ELSEIF nMode == RTLNK_MODE_LIB
               IF !cWord == ","
                  AAdd( aLibList, PathSepToTarget( cWord ) )
                  nMode := RTLNK_MODE_LIBNEXT
               ENDIF
            ELSEIF nMode == RTLNK_MODE_SKIP
               IF !cWord == ","
                  nMode := RTLNK_MODE_SKIPNEXT
               ENDIF
            ELSEIF cWord == ","
               IF nMode == RTLNK_MODE_FILENEXT
                  nMode := RTLNK_MODE_FILE
               ELSEIF nMode == RTLNK_MODE_LIBNEXT
                  nMode := RTLNK_MODE_LIB
               ELSEIF nMode == RTLNK_MODE_SKIPNEXT
                  nMode := RTLNK_MODE_SKIP
               ENDIF
            ELSEIF cWord = "@"
               cWord := SubStr( cWord, 2 )
               cCommands := rtlnk_read( @cWord, aPrevFiles )
               IF cCommands == NIL
                  OutStd( "hbmk2: error: Cannot open file: " + cWord + hb_osNewLine() )
                  RETURN .F.
               ENDIF
               IF !rtlnk_process( cCommands, @cFileOut, @aFileList, @aLibList, ;
                                  aPrevFiles )
                  RETURN .F.
               ENDIF
            ELSE
               cWord := Upper( cWord )
               IF Len( cWord ) >= 2
                  IF "OUTPUT" = cWord
                     nMode := RTLNK_MODE_OUT
                  ELSEIF "FILE" = cWord
                     nMode := RTLNK_MODE_FILE
                  ELSEIF "LIBRARY" = cWord
                     nMode := RTLNK_MODE_LIB
                  ELSEIF "MODULE" = cWord .OR. ;
                         "EXCLUDE" = cWord .OR. ;
                         "REFER" = cWord .OR. ;
                         "INTO" = cWord
                     nMode := RTLNK_MODE_SKIP
                  ENDIF
               ENDIF
            ENDIF
         NEXT
      ENDIF
   NEXT

   RETURN .T.

/* Keep this public, it's used from macro. */
FUNCTION hbmk_ARCH()
   RETURN t_cARCH

/* Keep this public, it's used from macro. */
FUNCTION hbmk_COMP()
   RETURN t_cCOMP

FUNCTION hbmk_KEYW( cKeyword )
   RETURN cKeyword == iif( t_lMT   , "mt"   , "st"      ) .OR. ;
          cKeyword == iif( t_lDEBUG, "debug", "nodebug" )

STATIC PROCEDURE PauseForKey()

   IF ! t_lQUIET .AND. hb_gtInfo( HB_GTI_ISGRAPHIC )
      OutStd( "Press any key to continue..." )
      Inkey( 0 )
   ENDIF

   RETURN

STATIC PROCEDURE ShowHeader()

   OutStd( "Harbour Make " + HBRawVersion() + hb_osNewLine() +;
           "Copyright (c) 1999-2009, Viktor Szakats" + hb_osNewLine() +;
           "http://www.harbour-project.org/" + hb_osNewLine() +;
           hb_osNewLine() )

   RETURN

STATIC PROCEDURE ShowHelp( lLong )

   LOCAL aText_Basic := {;
      "Syntax:  hbmk [options] [<script[s]>] <src[s][.prg|.c|.obj|.o|.rc|.res]>" ,;
      "" ,;
      "Options:" ,;
      "  -o<outname>       output file name" ,;
      "  -l<libname>       link with <libname> library" ,;
      "  -L<libpath>       additional path to search for libraries" ,;
      "  -static|-shared   link with static/shared libs" ,;
      "  -mt|-st           link with multi/single-thread VM" ,;
      "  -gt<name>         link with GT<name> GT driver, can be repeated to link" ,;
      "                    with more GTs. First one will be the default at runtime" }

   LOCAL aText_Help := {;
      "  -help|--help      long help" }

   LOCAL aText_Long := {;
      "" ,;
      "  -gui|-std         create GUI/console executable" ,;
      "  -main=<mainfunc>  override the name of starting function/procedure" ,;
      "  -fullstatic       link with all static libs" ,;
      "  -nulrdd[-]        link with nulrdd" ,;
      "  -[no]debug        add/exclude C compiler debug info" ,;
      "  -[no]map          create (or not) a map file" ,;
      "  -[no]strip        strip (no strip) binaries" ,;
      "  -[no]fmstat       enable/disable runtime memory statistics (gcc builds only)" ,;
      "  -[no]trace        show commands executed" ,;
      "  -traceonly        show commands to be executed, but don't execute them" ,;
      "  -[no]compr[=lev]  compress executable/dynamic lib (needs UPX)" ,;
      "                    level can be: min, max, def" ,;
      "  -[no]run          run/don't run output executable" ,;
      "  -nohbp            do not process .hbp files in current directory" ,;
      "" ,;
      "  -bldf[-]          inherit all/no (default) flags from Harbour build" ,;
      "  -bldf=[p][c][l]   inherit .prg/.c/linker flags (or none) from Harbour build" ,;
      "  -prgflag:<f>      pass flag to Harbour" ,;
      "  -cflag:<f>        pass flag to C compiler" ,;
      "  -ldflag:<f>       pass flag to linker (executable)" ,;
      "  -aflag:<f>        pass flag to linker (static library)" ,;
      "  -dflag:<f>        pass flag to linker (dynamic library)" ,;
      "  -runflag:<f>      pass flag to output executable when -run option is used" ,;
      "  -inc              enable incremental build mode" ,;
      "  -rebuild          rebuild all (in incremental build mode)" ,;
      "  -clean            clean (in incremental build mode)" ,;
      "  -workdir:<dir>    working directory for incremental build mode" ,;
      "                    (default: arch/comp)" ,;
      "  -hbcmp            stop after creating the object files" ,;
      "                    create link/copy hbmk to hbcmp for the same effect" ,;
      "  -hbcc             stop after creating the object files and accept raw C flags" ,;
      "                    create link/copy hbmk to hbcc for the same effect" ,;
      "  -hblnk            accept raw linker flags" ,;
      "  -hblib            create static library" ,;
      "  -hbdyn            create dynamic library (experimental)" ,;
      "  --hbdirbin        output Harbour binary directory" ,;
      "  --hbdirdyn        output Harbour dynamic library directory" ,;
      "  --hbdirlib        output Harbour static library directory" ,;
      "  --hbdirinc        output Harbour header directory" ,;
      "" ,;
      "  -arch=<arch>      assume specific architecure. Same as HB_ARCHITECTURE envvar" ,;
      "  -comp=<comp>      use specific compiler. Same as HB_COMPILER envvar" ,;
      "                    Special value:" ,;
      "                     - bld: use original build settings (default on *nix)" ,;
      "  --version         display version header only" ,;
      "  -info             turn on informational messages" ,;
      "  -quiet            suppress all screen messages" ,;
      "" ,;
      "Notes:" ,;
      "  - <script> can be <@script> (.hbm file), <script.hbm> or <script.hbp>." ,;
      "  - Regular Harbour compiler options are also accepted." ,;
      "  - Multiple -l, -L and <script> parameters are accepted." ,;
      "  - " + HBMK_CFG_NAME + " option file in hbmk directory is always processed if it" ,;
      "    exists. On *nix platforms ~/.harbour, /etc/harbour, <base>/etc/harbour," ,;
      "    <base>/etc are checked (in that order) before the hbmk directory." ,;
      "    The file format is the same as .hbp." ,;
      "  - .hbp option files in current dir are automatically processed." ,;
      "  - .hbp options (they should come in separate lines):" ,;
      "    libs=[<libname[s]>], gt=[gtname], prgflags=[Harbour flags]" ,;
      "    cflags=[C compiler flags], ldflags=[Linker flags], libpaths=[lib paths]" ,;
      "    gui|mt|shared|nulrdd|debug|map|strip|run|inc=[yes|no]" ,;
      "    compr=[yes|no|def|min|max]" ,;
      "    Lines starting with '#' char are ignored" ,;
      "  - Platform filters are accepted in each .hbp line and with -l options." ,;
      "    Filter format: {[!][<arch|comp>]}. Filters can be combined " ,;
      "    using '&', '|' operators and grouped by parantheses." ,;
      "    Ex.: {win}, {gcc}, {linux|darwin}, {win&!pocc}, {(win|linux)&!owatcom}" ,;
      "  - Defaults and feature support vary by architecture/compiler." ,;
      "  - Supported <comp> values for each supported <arch> value:" ,;
      "    linux  : gcc, owatcom, icc" ,;
      "    darwin : gcc" ,;
      "    win    : mingw, msvc, bcc, owatcom, icc, pocc, cygwin," ,;
      "             mingw64, msvc64, msvcia64, iccia64, pocc64, xcc" ,;
      "    wce    : mingwarm, msvcarm, poccarm" ,;
      "    os2    : gcc, owatcom" ,;
      "    dos    : djgpp, owatcom" ,;
      "    bsd    : gcc" ,;
      "    hpux   : gcc" ,;
      "    sunos  : gcc" }

   DEFAULT lLong TO .F.

   AEval( aText_Basic, {|tmp| OutStd( tmp + hb_osNewLine() ) } )
   AEval( iif( lLong, aText_Long, aText_Help ), {|tmp| OutStd( tmp + hb_osNewLine() ) } )

   RETURN

STATIC FUNCTION HBRawVersion()
   RETURN StrTran( Version(), "Harbour ", "" )
