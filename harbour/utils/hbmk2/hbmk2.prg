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
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
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
/* Optimizations */
#pragma -km+
#pragma -ko+

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
/* TODO: Add support for library creation for rest of compilers. */
/* TODO: Add support for dynamic library creation for rest of compilers. */
/* TODO: Cleanup on variable names and compiler configuration. */
/* TODO: Finish C++/C mode selection. */
/* TODO: Add a way to fallback to stop if required headers couldn't be found.
         This needs a way to spec what key headers to look for. */
/* TODO: Reuse Harbour .c output for different compiler targets in incremental mode. */

#ifndef HBMK_INTEGRATED_COMPILER
#define HBMK_INTEGRATED_COMPILER
#endif

#define _PAR_cParam         1
#define _PAR_cFileName      2
#define _PAR_nLine          3

#define _COMPR_OFF          0
#define _COMPR_DEF          1
#define _COMPR_MIN          2
#define _COMPR_MAX          3

#define _HEAD_OFF           0
#define _HEAD_PARTIAL       1
#define _HEAD_FULL          2

ANNOUNCE HB_GTSYS
REQUEST HB_GT_CGI_DEFAULT

REQUEST hbmk_ARCH
REQUEST hbmk_COMP
REQUEST hbmk_KEYW

STATIC s_lQuiet := .F.
STATIC s_lInfo := .F.
STATIC s_cARCH
STATIC s_cCOMP
STATIC s_aLIBCOREGT
STATIC s_cGTDEFAULT

STATIC s_aOPTPRG
STATIC s_aOPTC
STATIC s_lGUI := .F.
STATIC s_lMT := .F.
STATIC s_lDEBUG := .F.
STATIC s_nHEAD := _HEAD_PARTIAL
STATIC s_aINCPATH
STATIC s_aINCTRYPATH
STATIC s_lREBUILD := .F.

STATIC s_lDEBUGINC := .F.
STATIC s_lDEBUGSTUB := .F.

STATIC s_cCCPATH
STATIC s_cCCPREFIX
STATIC s_cHBPOSTFIX := ""

PROCEDURE Main( ... )
   LOCAL aArgs := hb_AParams()
   LOCAL nResult
   LOCAL cName
   LOCAL tmp

   /* Emulate -hbcmp, -hbcc, -hblnk switches when certain
      self names are detected.
      For compatibility with hbmk script aliases. */

   IF ! Empty( aArgs )

      hb_FNameSplit( hb_argv( 0 ),, @cName )

      tmp := Lower( cName )
      DO CASE
      CASE Right( tmp, 5 ) == "hbcmp" .OR. ;
           Left(  tmp, 5 ) == "hbcmp" .OR. ;
           tmp == "clipper"                ; AAdd( aArgs, "-hbcmp" )
      CASE Right( tmp, 4 ) == "hbcc" .OR. ;
           Left(  tmp, 4 ) == "hbcc"       ; AAdd( aArgs, "-hbcc" )
      CASE Right( tmp, 5 ) == "hblnk" .OR. ;
           Left(  tmp, 5 ) == "hblnk"      ; AAdd( aArgs, "-hblnk" )
      CASE tmp == "rtlink" .OR. ;
           tmp == "exospace" .OR. ;
           tmp == "blinker"                ; AAdd( aArgs, "-rtlink" )
      CASE Right( tmp, 5 ) == "hblib" .OR. ;
           Left(  tmp, 5 ) == "hblib"      ; AAdd( aArgs, "-hblib" )
      CASE Right( tmp, 5 ) == "hbdyn" .OR. ;
           Left(  tmp, 5 ) == "hbdyn"      ; AAdd( aArgs, "-hbdyn" )
      ENDCASE
   ENDIF

   nResult := hbmk( aArgs )

   IF nResult != 0 .AND. hb_gtInfo( HB_GTI_ISGRAPHIC )
      OutStd( "Press any key to continue..." )
      Inkey( 0 )
   ENDIF

   ErrorLevel( nResult )

   RETURN

FUNCTION hbmk_run( cCmd )
#if defined( __PLATFORM__DOS )
   RETURN hb_run( cCmd )
#else
   LOCAL h := hb_ProcessOpen( cCmd )
   LOCAL result
   IF h != F_ERROR
      result := hb_processValue( h )
      hb_ProcessClose( h, .T. )
   ELSE
      result := -1
   ENDIF
   RETURN result
#endif

FUNCTION hbmk( aArgs )

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
   LOCAL s_aLIBRAW
   LOCAL s_aLIBVM
   LOCAL s_aLIBUSER
   LOCAL s_aLIBUSERGT
   LOCAL s_aLIBHB
   LOCAL s_aLIBHBGT
   LOCAL s_aLIB3RD
   LOCAL s_aLIBSYS
   LOCAL s_aLIBPATH
   LOCAL s_aLIBDYNHAS
   LOCAL s_aLIBSYSCORE := {}
   LOCAL s_aLIBSYSMISC := {}
   LOCAL s_aOPTRES
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

   LOCAL s_lCPP := NIL
   LOCAL s_lSHARED := NIL
   LOCAL s_lSTATICFULL := NIL
   LOCAL s_lNULRDD := .F.
   LOCAL s_lMAP := .F.
   LOCAL s_lSTRIP := .F.
   LOCAL s_lOPT := .T.
   LOCAL s_nCOMPR := _COMPR_OFF
   LOCAL s_lTRACE := .F.
   LOCAL s_lDONTEXEC := .F.
   LOCAL s_lBLDFLGP := .F.
   LOCAL s_lBLDFLGC := .F.
   LOCAL s_lBLDFLGL := .F.
   LOCAL s_lRUN := .F.
   LOCAL s_lINC := .F.
   LOCAL s_lCLEAN := .F.
   LOCAL s_nJOBS := 1

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
   LOCAL cOptIncMask
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
   LOCAL headstate

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

   IF Empty( aArgs )
      ShowHeader()
      ShowHelp()
      RETURN 19
   ENDIF

   FOR EACH cParam IN aArgs

      cParamL := Lower( cParam )

      /* NOTE: Don't forget to make these ignored in the main
               option processing loop. */
      DO CASE
      CASE cParamL            == "-quiet"   ; s_lQuiet := .T. ; s_lInfo := .F.
      CASE Left( cParamL, 6 ) == "-comp="   ; s_cCOMP := SubStr( cParam, 7 )
      CASE Left( cParamL, 6 ) == "-arch="   ; s_cARCH := SubStr( cParam, 7 )
      CASE cParamL            == "-hbcmp" .OR. ;
           cParamL            == "-clipper" ; s_lInfo := .F. ; lStopAfterHarbour := .F. ; lStopAfterCComp := .T. ; lCreateLib := .F. ; lCreateDyn := .F.
      CASE cParamL            == "-hbcc"    ; s_lInfo := .F. ; lStopAfterHarbour := .F. ; lStopAfterCComp := .F. ; lAcceptCFlag := .T.
      CASE cParamL            == "-hblnk"   ; s_lInfo := .F. ; lStopAfterHarbour := .F. ; lStopAfterCComp := .F. ; lAcceptLDFlag := .T.
      CASE cParamL            == "-rtlink" .OR. ;
           cParamL            == "-exospace" .OR. ;
           cParamL            == "-blinker" ; s_lInfo := .F. ; lStopAfterHarbour := .F. ; lStopAfterCComp := .F. ; lAcceptLDClipper := .T.
      CASE cParamL            == "-info"    ; s_lInfo := .T.
      CASE cParamL == "-help" .OR. ;
           cParamL == "--help"

         ShowHeader()
         ShowHelp( .T. )
         RETURN 19

      CASE cParamL == "--version"

         ShowHeader()
         RETURN 0

      ENDCASE
   NEXT

   /* Load architecture / compiler settings (compatibility) */

   IF Empty( s_cARCH )
      s_cARCH := Lower( GetEnv( "HB_ARCHITECTURE" ) )
   ENDIF
   IF Empty( s_cCOMP )
      s_cCOMP := Lower( GetEnv( "HB_COMPILER" ) )
   ENDIF

   /* Autodetect architecture */

   IF Empty( s_cARCH )

      /* NOTE: Keep this in sync manually. All compilers should be listed here,
               which are supported on one architecture only. In the future this
               should be automatically extracted from a comp/arch matrix. */
      SWITCH s_cCOMP
      CASE "msvc"
      CASE "msvc64"
      CASE "msvcia64"
      CASE "bcc"
      CASE "xcc"
      CASE "pocc"
         s_cARCH := "win"
         EXIT
      CASE "mingwarm"
      CASE "msvcarm"
      CASE "poccarm"
         s_cARCH := "wce"
         EXIT
      CASE "djgpp"
         s_cARCH := "dos"
         EXIT
      OTHERWISE
         s_cARCH := hb_Version( HB_VERSION_BUILD_ARCH )
      ENDSWITCH
      IF ! Empty( s_cARCH )
         IF s_lInfo
            OutStd( "hbmk: Autodetected architecture: " + s_cARCH + hb_osNewLine() )
         ENDIF
      ENDIF
   ENDIF

   s_cCCPATH   := GetEnv( "HB_CCPATH" )
   s_cCCPREFIX := GetEnv( "HB_CCPREFIX" )

   /* Setup architecture dependent data */

   DO CASE
   CASE s_cARCH $ "bsd|hpux|sunos|linux" .OR. s_cARCH == "darwin" /* Separated to avoid match with 'win' */
      IF s_cARCH == "linux"
         aCOMPSUP := { "gcc", "gpp", "owatcom", "icc" }
      ELSE
         aCOMPSUP := { "gcc" }
      ENDIF
      cBin_CompPRG := "harbour" + s_cHBPOSTFIX
      s_aLIBHBGT := { "gttrm" }
      s_cGTDEFAULT := "gttrm"
      cDynLibNamePrefix := "lib"
      cBinExt := NIL
      cOptPrefix := "-"
      IF s_cARCH == "linux"
         cBin_Cprs := "upx"
         cOpt_Cprs := "{OB}"
         cOpt_CprsMin := "-1"
         cOpt_CprsMax := "-9"
      ENDIF
      SWITCH s_cARCH
      CASE "darwin" ; cDynLibExt := ".dylib" ; EXIT
      CASE "hpux"   ; cDynLibExt := ".sl" ; EXIT
      OTHERWISE     ; cDynLibExt := ".so"
      ENDSWITCH
   CASE s_cARCH == "dos"
      aCOMPDET := { { {|| FindInPath( "gcc"      ) != NIL }, "djgpp"   },;
                    { {|| FindInPath( "wpp386"   ) != NIL }, "owatcom" } } /* TODO: Add full support for wcc386 */
      aCOMPSUP := { "djgpp", "gcc", "owatcom" }
      cBin_CompPRG := "harbour" + s_cHBPOSTFIX + ".exe"
      s_aLIBHBGT := { "gtdos" }
      s_cGTDEFAULT := "gtdos"
      cDynLibNamePrefix := ""
      cDynLibExt := ""
      cBinExt := ".exe"
      cOptPrefix := "-/"
      cBin_Cprs := "upx.exe"
      cOpt_Cprs := "{OB}"
      cOpt_CprsMin := "-1"
      cOpt_CprsMax := "-9"
   CASE s_cARCH == "os2"
      aCOMPDET := { { {|| FindInPath( "gcc"      ) != NIL }, "gcc"     },;
                    { {|| FindInPath( "wpp386"   ) != NIL }, "owatcom" } } /* TODO: Add full support for wcc386 */
      aCOMPSUP := { "gcc", "owatcom" }
      cBin_CompPRG := "harbour" + s_cHBPOSTFIX + ".exe"
      s_aLIBHBGT := { "gtos2" }
      s_cGTDEFAULT := "gtos2"
      cDynLibNamePrefix := ""
      cDynLibExt := ".dll"
      cBinExt := ".exe"
      cOptPrefix := "-/"
   CASE s_cARCH == "win"
      /* Order is significant.
         owatcom also keeps a cl.exe in its binary dir. */
      aCOMPDET := { { {|| FindInPath( s_cCCPREFIX + "gcc" ) != NIL }, "mingw"   },; /* TODO: Add full support for g++ */
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
      cBin_CompPRG := "harbour" + s_cHBPOSTFIX + ".exe"
      s_aLIBHBGT := { "gtwin", "gtwvt", "gtgui" }
      s_cGTDEFAULT := "gtwin"
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
   CASE s_cARCH == "wce"
      aCOMPDET := { { {|| FindInPath( s_cCCPREFIX + "gcc" ) != NIL }, "mingwarm" },;
                    { {|| FindInPath( "cl"       ) != NIL }, "msvcarm" },;
                    { {|| FindInPath( "pocc"     ) != NIL }, "poccarm" } }
      aCOMPSUP := { "mingwarm", "msvcarm", "poccarm" }
      cBin_CompPRG := "harbour" + s_cHBPOSTFIX + ".exe"
      s_aLIBHBGT := { "gtwvt", "gtgui" }
      s_cGTDEFAULT := "gtwvt"
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
      OutErr( "hbmk: Error: Architecture value unknown: " + s_cARCH + hb_osNewLine() )
      RETURN 1
   ENDCASE

   s_aLIBCOREGT := ArrayJoin( aLIB_BASE_GT, s_aLIBHBGT )

   /* Setup GUI state for Harbour default */
   SetupForGT( s_cGTDEFAULT, NIL, @s_lGUI )

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
         RETURN 3
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
      RETURN 3
   ENDIF

   IF s_cARCH $ "win|wce"
      aCOMPDET_LOCAL := {;
          { {| cPrefix | tmp1 := PathNormalize( s_cHB_INSTALL_PREFIX ) + "mingw"   + hb_osPathSeparator() + "bin", iif( hb_FileExists( tmp1 + hb_osPathSeparator() + cPrefix + "gcc.exe" ), tmp1, NIL ) }, "win", "mingw"   , ""                     } ,;
          { {| cPrefix | tmp1 := PathNormalize( s_cHB_INSTALL_PREFIX ) + "mingw64" + hb_osPathSeparator() + "bin", iif( hb_FileExists( tmp1 + hb_osPathSeparator() + cPrefix + "gcc.exe" ), tmp1, NIL ) }, "win", "mingw64" , "x86_64-pc-mingw32-"   } ,;
          { {| cPrefix | tmp1 := PathNormalize( s_cHB_INSTALL_PREFIX ) + "mingwce" + hb_osPathSeparator() + "bin", iif( hb_FileExists( tmp1 + hb_osPathSeparator() + cPrefix + "gcc.exe" ), tmp1, NIL ) }, "wce", "mingwarm", "arm-wince-mingw32ce-" } }
   ENDIF

   /* Autodetect compiler */

   IF lStopAfterHarbour
      /* If we're just compiling .prg to .c we don't need a C compiler. */
      s_cCOMP := ""
   ELSE
      IF Empty( s_cCOMP ) .OR. s_cCOMP == "bld"
         IF Len( aCOMPSUP ) == 1
            s_cCOMP := aCOMPSUP[ 1 ]
         ELSEIF s_cARCH == "linux" .OR. s_cCOMP == "bld"
            s_cCOMP := cSelfCOMP
            IF AScan( aCOMPSUP, {|tmp| tmp == s_cCOMP } ) == 0
               s_cCOMP := NIL
            ENDIF
         ELSE
            IF Empty( s_cCOMP ) .AND. ! Empty( aCOMPDET )
               /* Look for this compiler first */
               FOR tmp := 1 TO Len( aCOMPDET )
                  IF aCOMPDET[ tmp ][ 2 ] == cSelfCOMP .AND. Eval( aCOMPDET[ tmp ][ 1 ] )
                     s_cCOMP := aCOMPDET[ tmp ][ 2 ]
                     EXIT
                  ENDIF
               NEXT
               IF Empty( s_cCOMP )
                  /* Check the rest of compilers */
                  FOR tmp := 1 TO Len( aCOMPDET )
                     IF !( aCOMPDET[ tmp ][ 2 ] == cSelfCOMP ) .AND. Eval( aCOMPDET[ tmp ][ 1 ] )
                        s_cCOMP := aCOMPDET[ tmp ][ 2 ]
                        EXIT
                     ENDIF
                  NEXT
               ENDIF
            ENDIF
            IF Empty( s_cCOMP ) .AND. s_cARCH $ "win|wce"
               /* Autodetect embedded MinGW installation */
               FOR tmp := 1 TO Len( aCOMPDET_LOCAL )
                  IF s_cARCH == aCOMPDET_LOCAL[ tmp ][ 2 ] .AND. ;
                     ! Empty( tmp1 := Eval( aCOMPDET_LOCAL[ tmp ][ 1 ], aCOMPDET_LOCAL[ tmp ][ 4 ] ) )
                     s_cCOMP := aCOMPDET_LOCAL[ tmp ][ 3 ]
                     s_cCCPREFIX := aCOMPDET_LOCAL[ tmp ][ 4 ]
                     s_cCCPATH := tmp1
                     EXIT
                  ENDIF
               NEXT
            ENDIF
         ENDIF
         IF ! Empty( s_cCOMP )
            IF s_lInfo
               OutStd( "hbmk: Autodetected compiler: " + s_cCOMP + hb_osNewLine() )
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
            RETURN 2
         ENDIF
      ELSE
         IF AScan( aCOMPSUP, {|tmp| tmp == s_cCOMP } ) == 0
            OutErr( "hbmk: Error: Compiler value unknown: " + s_cCOMP + hb_osNewLine() )
            RETURN 2
         ENDIF
         IF s_cARCH $ "win|wce"
            /* Detect cross platform CCPREFIX and CCPATH if embedded MinGW installation is detected */
            FOR tmp := 1 TO Len( aCOMPDET_LOCAL )
               IF aCOMPDET_LOCAL[ tmp ][ 2 ] == s_cARCH .AND. ;
                  aCOMPDET_LOCAL[ tmp ][ 3 ] == s_cCOMP
                  IF ! Empty( tmp1 := Eval( aCOMPDET_LOCAL[ tmp ][ 1 ], aCOMPDET_LOCAL[ tmp ][ 4 ] ) )
                     s_cCCPATH := tmp1
                  ENDIF
                  s_cCCPREFIX := aCOMPDET_LOCAL[ tmp ][ 4 ]
                  EXIT
               ENDIF
            NEXT
         ENDIF
      ENDIF
   ENDIF

   /* Finish detecting bin/lib/include dirs */

   s_aLIBPATH := {}
   s_aINCPATH := {}

   IF Empty( s_cHB_BIN_INSTALL )
      s_cHB_BIN_INSTALL := PathNormalize( s_cHB_INSTALL_PREFIX ) + "bin"
   ENDIF
   IF Empty( s_cHB_LIB_INSTALL )
      /* Autodetect multi-compiler/platform lib structure */
      IF hb_DirExists( tmp := PathNormalize( s_cHB_INSTALL_PREFIX ) + "lib" +;
                                               hb_osPathSeparator() + s_cARCH +;
                                               hb_osPathSeparator() + s_cCOMP )
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

   IF s_lInfo
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

   /* Add main Harbour header dir to header path list */
   AAddNotEmpty( s_aINCPATH, s_cHB_INC_INSTALL )

   /* Process environment */

   IF    Lower( GetEnv( "HB_MT"     ) ) == "mt" ; s_lMT     := .T. ; ENDIF /* Compatibility */
   IF ValueIsT( GetEnv( "HB_MT"     ) )         ; s_lMT     := .T. ; ENDIF
   IF ValueIsT( GetEnv( "HB_GUI"    ) )         ; s_lGUI    := .T. ; ENDIF
   IF ValueIsT( GetEnv( "HB_SHARED" ) )         ; s_lSHARED := .T. ; s_lSTATICFULL := .F. ; ENDIF
   IF ValueIsT( GetEnv( "HB_DEBUG"  ) )         ; s_lDEBUG  := .T. ; ENDIF
   IF ValueIsT( GetEnv( "HB_NULRDD" ) )         ; s_lNULRDD := .T. ; ENDIF

   IF Lower( Left( GetEnv( "HB_GT" ), 2 ) ) == "gt"
      SetupForGT( GetEnv( "HB_GT" ), @s_cGT, @s_lGUI )
   ENDIF

   /* Process command line */

   s_aPRG := {}
   s_aC := {}
   s_aOPTPRG := {}
   s_aOPTC := {}
   s_aOPTRES := {}
   s_aOPTL := {}
   s_aOPTA := {}
   s_aOPTD := {}
   s_aOPTRUN := {}
   s_aRESSRC := {}
   s_aRESCMP := {}
   s_aINCTRYPATH := {}
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
   FOR EACH cParam IN aArgs
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
                   @s_aINCPATH,;
                   @s_aINCTRYPATH,;
                   @s_aOPTPRG,;
                   @s_aOPTC,;
                   @s_aOPTRES,;
                   @s_aOPTL,;
                   @s_lGUI,;
                   @s_lMT,;
                   @s_lSHARED,;
                   @s_lSTATICFULL,;
                   @s_lDEBUG,;
                   @s_lOPT,;
                   @s_lNULRDD,;
                   @s_lMAP,;
                   @s_lSTRIP,;
                   @s_nCOMPR,;
                   @s_nHEAD,;
                   @s_lRUN,;
                   @s_lINC,;
                   @s_cGT )

   /* Build with shared libs by default, if we're installed to default system locations. */

   IF s_lSHARED == NIL
      IF lSysLoc .AND. ( s_cARCH $ "bsd|hpux|sunos|linux" .OR. s_cARCH == "darwin" )
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
      CASE Left( cParamL, 6 ) == "-comp=" .OR. ;
           Left( cParamL, 6 ) == "-arch=" .OR. ;
           cParamL            == "-hbcmp" .OR. ;
           cParamL            == "-hbcc"  .OR. ;
           cParamL            == "-hblnk" .OR. ;
           cParamL            == "-nohbp" .OR. ;
           cParamL            == "-clipper" .OR. ;
           cParamL            == "-rtlink" .OR. ;
           cParamL            == "-blinker" .OR. ;
           cParamL            == "-exospace"

         /* Simply ignore. They were already processed in the first pass. */

      CASE cParamL == "-quiet"           ; s_lQuiet := .T. ; s_lInfo := .F.
      CASE cParamL == "-info"            ; s_lInfo := .T.
      CASE cParamL == "-hblib"           ; s_lInfo := .F. ; lStopAfterHarbour := .F. ; lStopAfterCComp := .T. ; lCreateLib := .T. ; lCreateDyn := .F.
      CASE cParamL == "-hbdyn"           ; s_lInfo := .F. ; lStopAfterHarbour := .F. ; lStopAfterCComp := .T. ; lCreateLib := .F. ; lCreateDyn := .T.
      CASE cParamL == "-gui" .OR. ;
           cParamL == "-mwindows"        ; s_lGUI       := .T. /* Compatibility */
      CASE cParamL == "-std" .OR. ;
           cParamL == "-mconsole"        ; s_lGUI       := .F. /* Compatibility */
      CASE cParamL == "-mt"              ; s_lMT        := .T.
      CASE cParamL == "-st"              ; s_lMT        := .F.
      CASE cParamL == "-shared"          ; s_lSHARED    := .T. ; s_lSTATICFULL := .F.
      CASE cParamL == "-static"          ; s_lSHARED    := .F. ; s_lSTATICFULL := .F.
      CASE cParamL == "-fullstatic"      ; s_lSHARED    := .F. ; s_lSTATICFULL := .T.
      CASE cParamL == "-bldf"            ; s_lBLDFLGP   := s_lBLDFLGC := s_lBLDFLGL := .T.
      CASE cParamL == "-bldf-"           ; s_lBLDFLGP   := s_lBLDFLGC := s_lBLDFLGL := .F.
      CASE Left( cParamL, 6 ) == "-bldf="
         cParam := SubStr( cParam, 7 )
         s_lBLDFLGP := "p" $ cParam
         s_lBLDFLGC := "c" $ cParam
         s_lBLDFLGL := "l" $ cParam
      CASE cParamL == "-debug"           ; s_lDEBUG     := .T.
      CASE cParamL == "-debug-" .OR. ;
           cParamL == "-nodebug"         ; s_lDEBUG     := .F.
      CASE cParamL == "-opt"             ; s_lOPT       := .T.
      CASE cParamL == "-opt-" .OR. ;
           cParamL == "-noopt"           ; s_lOPT       := .F.
      CASE cParamL == "-debuginc"        ; s_lDEBUGINC  := .T.
      CASE cParamL == "-debugstub"       ; s_lDEBUGSTUB := .T.
      CASE cParamL == "-nulrdd"          ; s_lNULRDD    := .T.
      CASE cParamL == "-nulrdd-"         ; s_lNULRDD    := .F.
      CASE cParamL == "-map"             ; s_lMAP       := .T.
      CASE cParamL == "-map-" .OR. ;
           cParamL == "-nomap"           ; s_lMAP       := .F.
      CASE cParamL == "-rebuild"         ; s_lINC       := .T. ; s_lREBUILD := .T.
      CASE cParamL == "-clean"           ; s_lINC       := .T. ; s_lCLEAN := .T.
      CASE cParamL == "-inc"             ; s_lINC       := .T.
      CASE cParamL == "-inc-" .OR. ;
           cParamL == "-noinc"           ; s_lINC       := .F.
      CASE cParamL == "-strip"           ; s_lSTRIP     := .T.
      CASE cParamL == "-strip-" .OR. ;
           cParamL == "-nostrip"         ; s_lSTRIP     := .F.
      CASE cParamL == "-compr" .OR. ;
           Left( cParamL, 7 ) == "-compr="

           DO CASE
           CASE SubStr( cParamL, 8 ) == "min" ; s_nCOMPR := _COMPR_MIN
           CASE SubStr( cParamL, 8 ) == "max" ; s_nCOMPR := _COMPR_MAX
           OTHERWISE                          ; s_nCOMPR := _COMPR_DEF
           ENDCASE
      CASE cParamL == "-compr-" .OR. ;
           cParamL == "-nocompr"         ; s_nCOMPR     := _COMPR_OFF

      CASE cParamL == "-head" .OR. ;
           Left( cParamL, 6 ) == "-head="

           DO CASE
           CASE SubStr( cParamL, 7 ) == "off"  ; s_nHEAD := _HEAD_OFF
           CASE SubStr( cParamL, 7 ) == "full" ; s_nHEAD := _HEAD_FULL
           OTHERWISE                           ; s_nHEAD := _HEAD_PARTIAL
           ENDCASE
      CASE cParamL == "-head-" .OR. ;
           cParamL == "-nohead"          ; s_nHEAD      := _HEAD_OFF

      CASE cParamL == "-cpp" .OR. ;
           Left( cParamL, 5 ) == "-cpp="

           DO CASE
           CASE SubStr( cParamL, 6 ) == "def" ; s_lCPP := NIL
           CASE SubStr( cParamL, 6 ) == "off" ; s_lCPP := .F.
           OTHERWISE                          ; s_lCPP := .T.
           ENDCASE
      CASE cParamL == "-cpp-" .OR. ;
           cParamL == "-nocpp"           ; s_lCPP       := .F.

      CASE cParamL == "-run"             ; s_lRUN       := .T.
      CASE cParamL == "-run-" .OR. ;
           cParamL == "-norun"           ; s_lRUN       := .F.
      CASE cParamL == "-trace"           ; s_lTRACE     := .T.
      CASE cParamL == "-trace-" .OR. ;
           cParamL == "-notrace"         ; s_lTRACE     := .F.
      CASE cParamL == "-traceonly"       ; s_lTRACE     := .T. ; s_lDONTEXEC := .T.

      CASE cParamL == "--hbdirbin"       ; lStopAfterInit := .T.

         OutStd( s_cHB_BIN_INSTALL )

      CASE cParamL == "--hbdirdyn"       ; lStopAfterInit := .T.

         OutStd( s_cHB_DYN_INSTALL )

      CASE cParamL == "--hbdirlib"       ; lStopAfterInit := .T.

         OutStd( s_cHB_LIB_INSTALL )

      CASE cParamL == "--hbdirinc"       ; lStopAfterInit := .T.

         OutStd( s_cHB_INC_INSTALL )

      CASE Left( cParamL, Len( "-jobs=" ) ) == "-jobs="

         cParam := ArchCompFilter( SubStr( cParam, Len( "-jobs=" ) + 1 ) )
         IF hb_mtvm() .AND. Val( cParam ) > 0
            s_nJOBS := Val( cParam )
         ENDIF

         HB_SYMBOL_UNUSED( s_nJOBS )

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
            IF AScan( s_aLIBCOREGT, {|tmp| Lower( tmp ) == cParamL } ) == 0 .AND. ;
               AScan( s_aLIBUSERGT, {|tmp| Lower( tmp ) == cParamL } ) == 0
               AAddNotEmpty( s_aLIBUSERGT, PathSepToTarget( cParam ) )
            ENDIF
         ENDIF

      CASE ! lNIX .AND. Left( cParamL, 2 ) == "/o" .AND. ! lStopAfterHarbour

         /* Swallow this switch. We don't pass it to Harbour, as it may badly
            interact with hbmk. */

      CASE Left( cParam, 2 ) == "-o" .AND. ! lStopAfterHarbour

         tmp := MacroProc( ArchCompFilter( SubStr( cParam, 3 ) ), FN_DirGet( aParam[ _PAR_cFileName ] ) )
         IF ! Empty( tmp )
            tmp := PathSepToSelf( tmp )
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
         ENDIF

      CASE Left( cParam, 2 ) == "-L" .AND. ;
           Len( cParam ) > 2

         cParam := MacroProc( ArchCompFilter( SubStr( cParam, 3 ) ), FN_DirGet( aParam[ _PAR_cFileName ] ) )
         IF ! Empty( cParam )
            AAdd( s_aLIBPATH, PathSepToTarget( cParam ) )
         ENDIF

      CASE Left( cParamL, 2 ) == "-i" .AND. ;
           Len( cParamL ) > 2

         cParam := MacroProc( tmp := ArchCompFilter( SubStr( cParam, 3 ) ), FN_DirGet( aParam[ _PAR_cFileName ] ) )
         IF ! Empty( cParam )
            AAdd( s_aINCPATH, PathSepToTarget( cParam ) )
         ENDIF

      CASE Left( cParamL, Len( "-incpath=" ) ) == "-incpath=" .AND. ;
           Len( cParamL ) > Len( "-incpath=" )

         cParam := MacroProc( tmp := ArchCompFilter( SubStr( cParam, Len( "-incpath=" ) + 1 ) ), FN_DirGet( aParam[ _PAR_cFileName ] ) )
         IF ! Empty( cParam )
            AAdd( s_aINCPATH, PathSepToTarget( cParam ) )
         ENDIF

      CASE Left( cParamL, Len( "-inctrypath=" ) ) == "-inctrypath=" .AND. ;
           Len( cParamL ) > Len( "-inctrypath=" )

         cParam := MacroProc( tmp := ArchCompFilter( SubStr( cParam, Len( "-inctrypath=" ) + 1 ) ), FN_DirGet( aParam[ _PAR_cFileName ] ) )
         IF ! Empty( cParam )
            AAdd( s_aINCTRYPATH, PathSepToTarget( cParam ) )
         ENDIF

      CASE Left( cParamL, Len( "-stop" ) ) == "-stop"

         cParam := ArchCompFilter( cParam )
         IF ! Empty( cParam )
            lStopAfterInit := .T.
         ENDIF

      CASE Left( cParamL, Len( "-prgflag=" ) ) == "-prgflag="

         cParam := MacroProc( ArchCompFilter( SubStr( cParam, Len( "-prgflag=" ) + 1 ) ), FN_DirGet( aParam[ _PAR_cFileName ] ) )
         IF Left( cParam, 1 ) $ cOptPrefix
            IF SubStr( cParamL, 2 ) == "gh"
               lStopAfterHarbour := .T.
            ENDIF
            IF !( SubStr( cParamL, 2, 1 ) == "o" )
               AAdd( s_aOPTPRG , PathSepToTarget( cParam, 2 ) )
            ENDIF
         ENDIF

      CASE Left( cParamL, Len( "-cflag=" ) ) == "-cflag="

         cParam := MacroProc( ArchCompFilter( SubStr( cParam, Len( "-cflag=" ) + 1 ) ), FN_DirGet( aParam[ _PAR_cFileName ] ) )
         IF Left( cParam, 1 ) $ cOptPrefix
            AAdd( s_aOPTC   , PathSepToTarget( cParam, 2 ) )
         ENDIF

      CASE Left( cParamL, Len( "-resflag=" ) ) == "-resflag="

         cParam := MacroProc( ArchCompFilter( SubStr( cParam, Len( "-resflag=" ) + 1 ) ), FN_DirGet( aParam[ _PAR_cFileName ] ) )
         IF Left( cParam, 1 ) $ cOptPrefix
            AAdd( s_aOPTRES , PathSepToTarget( cParam, 2 ) )
         ENDIF

      CASE Left( cParamL, Len( "-ldflag=" ) ) == "-ldflag="

         cParam := MacroProc( ArchCompFilter( SubStr( cParam, Len( "-ldflag=" ) + 1 ) ), FN_DirGet( aParam[ _PAR_cFileName ] ) )
         IF Left( cParam, 1 ) $ cOptPrefix
            AAdd( s_aOPTL   , PathSepToTarget( cParam, 2 ) )
         ENDIF

      CASE Left( cParamL, Len( "-dflag=" ) ) == "-dflag="

         cParam := MacroProc( ArchCompFilter( SubStr( cParam, Len( "-dflag=" ) + 1 ) ), FN_DirGet( aParam[ _PAR_cFileName ] ) )
         IF Left( cParam, 1 ) $ cOptPrefix
            AAdd( s_aOPTD   , PathSepToTarget( cParam, 2 ) )
         ENDIF

      CASE Left( cParamL, Len( "-aflag=" ) ) == "-aflag="

         cParam := MacroProc( ArchCompFilter( SubStr( cParam, Len( "-aflag=" ) + 1 ) ), FN_DirGet( aParam[ _PAR_cFileName ] ) )
         IF Left( cParam, 1 ) $ cOptPrefix
            AAdd( s_aOPTA   , PathSepToTarget( cParam, 2 ) )
         ENDIF

      CASE Left( cParamL, Len( "-runflag=" ) ) == "-runflag="

         cParam := MacroProc( ArchCompFilter( SubStr( cParam, Len( "-runflag=" ) + 1 ) ), FN_DirGet( aParam[ _PAR_cFileName ] ) )
         IF Left( cParam, 1 ) $ cOptPrefix
            AAdd( s_aOPTRUN , PathSepToTarget( cParam, 2 ) )
         ENDIF

      CASE Left( cParamL, Len( "-workdir=" ) ) == "-workdir="

         cWorkDir := PathSepToTarget( MacroProc( ArchCompFilter( SubStr( cParam, Len( "-workdir=" ) + 1 ) ), FN_DirGet( aParam[ _PAR_cFileName ] ) ) )

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

         cParam := PathProc( cParam, aParam[ _PAR_cFileName ] )

         IF ! hb_FileExists( cParam )
            FOR EACH tmp IN s_aLIBPATH
               IF hb_FileExists( DirAddPathSep( tmp ) + FN_NameExtGet( cParam ) )
                  cParam := DirAddPathSep( tmp ) + FN_NameExtGet( cParam )
                  EXIT
               ENDIF
            NEXT
         ENDIF

         IF s_lInfo
            OutStd( "hbmk: Processing: " + cParam + hb_osNewLine() )
         ENDIF

         HBP_ProcessOne( cParam,;
            @s_aLIBUSER,;
            @s_aLIBUSERGT,;
            @s_aLIBPATH,;
            @s_aLIBDYNHAS,;
            @s_aINCPATH,;
            @s_aINCTRYPATH,;
            @s_aOPTPRG,;
            @s_aOPTC,;
            @s_aOPTRES,;
            @s_aOPTL,;
            @s_lGUI,;
            @s_lMT,;
            @s_lSHARED,;
            @s_lSTATICFULL,;
            @s_lDEBUG,;
            @s_lOPT,;
            @s_lNULRDD,;
            @s_lMAP,;
            @s_lSTRIP,;
            @s_nCOMPR,;
            @s_nHEAD,;
            @s_lRUN,;
            @s_lINC,;
            @s_cGT )

      CASE FN_ExtGet( cParamL ) == ".prg"

         FOR EACH cParam IN FN_Expand( PathProc( cParam, aParam[ _PAR_cFileName ] ) )
            AAdd( s_aPRG    , PathSepToTarget( cParam ) )
            DEFAULT s_cFIRST TO PathSepToSelf( cParam )
         NEXT

      CASE FN_ExtGet( cParamL ) == ".rc"

         FOR EACH cParam IN FN_Expand( PathProc( cParam, aParam[ _PAR_cFileName ] ) )
            AAdd( s_aRESSRC , PathSepToTarget( cParam ) )
         NEXT

      CASE FN_ExtGet( cParamL ) == ".res"

         IF s_cCOMP $ "mingw|mingw64|mingwarm"
            /* For MinGW family add .res files as source input, as they
               will need to be converted to coff format with windres (just
               like plain .rc files) before feeding them to gcc. */
            FOR EACH cParam IN FN_Expand( PathProc( cParam, aParam[ _PAR_cFileName ] ) )
               AAdd( s_aRESSRC , PathSepToTarget( cParam ) )
            NEXT
         ELSE
            FOR EACH cParam IN FN_Expand( PathProc( cParam, aParam[ _PAR_cFileName ] ) )
               AAdd( s_aRESCMP , PathSepToTarget( cParam ) )
            NEXT
         ENDIF

      CASE FN_ExtGet( cParamL ) == ".a"

         cParam := PathProc( cParam, aParam[ _PAR_cFileName ] )
         AAdd( s_aOBJA   , PathSepToTarget( cParam ) )

      CASE FN_ExtGet( cParamL ) == ".o" .OR. ;
           FN_ExtGet( cParamL ) == ".obj"

         FOR EACH cParam IN FN_Expand( PathProc( cParam, aParam[ _PAR_cFileName ] ) )
            AAdd( s_aOBJUSER, PathSepToTarget( cParam ) )
            DEFAULT s_cFIRST TO PathSepToSelf( cParam )
         NEXT

      CASE FN_ExtGet( cParamL ) == ".c" .OR. ;
           FN_ExtGet( cParamL ) == ".cpp" /* .cc, .cxx, .cx */

         FOR EACH cParam IN FN_Expand( PathProc( cParam, aParam[ _PAR_cFileName ] ) )
            AAdd( s_aC      , PathSepToTarget( cParam ) )
            DEFAULT s_cFIRST TO PathSepToSelf( cParam )
         NEXT

      OTHERWISE

         IF ! Empty( cParam )
            cParam := PathProc( cParam, aParam[ _PAR_cFileName ] )
            AAdd( s_aPRG    , PathSepToTarget( cParam ) )
            DEFAULT s_cFIRST TO PathSepToSelf( cParam )
         ENDIF

      ENDCASE
   NEXT

   IF lCreateDyn .AND. s_lSHARED
      s_lSHARED := .F.
   ENDIF

   /* Start doing the make process. */
   IF ! lStopAfterInit .AND. ( Len( s_aPRG ) + Len( s_aC ) + Len( s_aOBJUSER ) + Len( s_aOBJA ) ) == 0
      OutErr( "hbmk: Error: No source files were specified." + hb_osNewLine() )
      RETURN 4
   ENDIF

   IF ! lStopAfterInit
      IF s_lINC
         IF cWorkDir == NIL
            cWorkDir := s_cARCH + hb_osPathSeparator() + s_cCOMP
         ENDIF
         AAdd( s_aOPTPRG, "-o" + cWorkDir + hb_osPathSeparator() ) /* NOTE: Ending path sep is important. */
         IF ! DirBuild( cWorkDir )
            OutErr( "hbmk: Error: Working directory cannot be created: " + cWorkDir + hb_osNewLine() )
            RETURN 9
         ENDIF
      ELSE
         cWorkDir := ""
      ENDIF
   ENDIF

   /* Decide about output name */

   IF ! lStopAfterInit .AND. ! lStopAfterHarbour

      /* If -o with full name wasn't specified, let's
         make it the first source file specified. */
      DEFAULT s_cPROGNAME TO FN_NameGet( s_cFIRST )

      /* Combine output dir with output name. */
      IF ! Empty( s_cPROGDIR )
         hb_FNameSplit( s_cPROGNAME, @cDir, @cName, @cExt )
         s_cPROGNAME := hb_FNameMerge( iif( Empty( cDir ), s_cPROGDIR, cDir ), cName, cExt )
      ENDIF
   ENDIF

   IF ! lStopAfterInit .AND. ! lStopAfterHarbour

      IF s_cGT == s_cGTDEFAULT
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
      CASE s_cARCH $ "bsd|linux|hpux|sunos" .OR. s_cARCH == "darwin" /* Separated to avoid match with 'win' */
         IF Empty( cPrefix )
            s_aLIBSHARED := { iif( s_lMT, "harbourmt" + cPostfix,;
                                          "harbour"   + cPostfix ) }
         ELSE
            s_aLIBSHARED := { iif( s_lMT, cPrefix + cDynLibNamePrefix + "harbourmt" + cPostfix + cDynLibExt,;
                                          cPrefix + cDynLibNamePrefix + "harbour"   + cPostfix + cDynLibExt ) }
         ENDIF
      CASE s_cARCH $ "os2|win|wce"
         s_aLIBSHARED := { iif( s_lMT, cDynLibNamePrefix + "harbourmt",;
                                       cDynLibNamePrefix + "harbour" ) }
      OTHERWISE
         s_aLIBSHARED := NIL
      ENDCASE

      /* C compilation/linking */

      s_aLIB3RD := {}
      s_aLIBSYS := {}
      s_aCLEAN := {}

      cOptIncMask := "-I{DI}"

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

      s_aLIBVM := iif( s_lMT, aLIB_BASE_MT, aLIB_BASE_ST )
      aLIB_BASE2 := ArrayAJoin( { aLIB_BASE2, s_aLIBCOREGT } )

      IF ! Empty( s_cGT ) .AND. !( Lower( s_cGT ) == "gtnul" )
         IF AScan( aLIB_BASE2, {|tmp| Lower( tmp ) == Lower( s_cGT ) } ) == 0
            AAdd( aLIB_BASE2, s_cGT )
         ENDIF
      ENDIF

      IF s_cCOMP $ "owatcom|gpp" .AND. s_lCPP == NIL
         s_lCPP := .T.
      ENDIF

      DO CASE
      /* GCC family */
      CASE ( s_cARCH == "bsd"    .AND. s_cCOMP == "gcc" ) .OR. ;
           ( s_cARCH == "darwin" .AND. s_cCOMP == "gcc" ) .OR. ;
           ( s_cARCH == "hpux"   .AND. s_cCOMP == "gcc" ) .OR. ;
           ( s_cARCH == "sunos"  .AND. s_cCOMP == "gcc" ) .OR. ;
           ( s_cARCH == "linux"  .AND. s_cCOMP == "gcc" ) .OR. ;
           ( s_cARCH == "linux"  .AND. s_cCOMP == "gpp" )

         IF s_lDEBUG
            AAdd( s_aOPTC, "-g" )
         ENDIF
         cLibLibPrefix := "lib"
         cLibPrefix := "-l"
         cLibExt := ""
         cObjExt := ".o"
         cBin_Lib := s_cCCPREFIX + "ar"
         cOpt_Lib := "{FA} rcs {OL} {LO}"
         cBin_CompC := s_cCCPREFIX + iif( s_lCPP != NIL .AND. s_lCPP, "g++", "gcc" )
         cOpt_CompC := "-c"
         IF s_lOPT
            cOpt_CompC += " -O3"
         ENDIF
         cOpt_CompC += " {FC}"
         IF s_lINC .AND. ! Empty( cWorkDir )
            cOpt_CompC += " {IC} -o {OO}"
         ELSE
            cOpt_CompC += " {LC}"
         ENDIF
         cBin_Link := cBin_CompC
         cOpt_Link := "{LO} {LA} {FL} {DL}"
         IF ! Empty( s_cCCPATH )
            cBin_Lib   := s_cCCPATH + "/" + cBin_Lib
            cBin_CompC := s_cCCPATH + "/" + cBin_CompC
            cBin_Link  := s_cCCPATH + "/" + cBin_Link
         ENDIF
         cLibPathPrefix := "-L"
         cLibPathSep := " "
         IF ! lStopAfterCComp
            IF s_cARCH == "linux" .OR. ;
               s_cARCH == "bsd"
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
         IF s_cARCH == "darwin"
            AAdd( s_aOPTC, "-no-cpp-precomp" )
            AAdd( s_aOPTC, "-Wno-long-double" )
            IF s_lSHARED
               AAdd( s_aOPTL, "-bind_as_load" )
               AAdd( s_aOPTL, "-multiply_defined suppress" )
            ENDIF
         ENDIF
         IF s_lSTRIP .AND. !( s_cARCH == "sunos" )
            AAdd( s_aOPTL, "-s" )
         ENDIF
         IF lStopAfterCComp
            IF ! lCreateLib .AND. ! lCreateDyn .AND. ( Len( s_aPRG ) + Len( s_aC ) ) == 1
               IF s_cARCH == "darwin"
                  AAdd( s_aOPTL, "-o {OO}" )
               ELSE
                  AAdd( s_aOPTL, "-o{OO}" )
               ENDIF
            ENDIF
         ELSE
            IF s_cARCH == "darwin"
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
            IF s_lMT
               AAdd( s_aLIBSYS, "pthread" )
            ENDIF
            DO CASE
            CASE s_cARCH == "linux"
               AAdd( s_aLIBSYS, "dl" )
               AAdd( s_aLIBSYS, "rt" )
            CASE s_cARCH == "sunos"
               AAdd( s_aLIBSYS, "rt" )
               AAdd( s_aLIBSYS, "socket" )
               AAdd( s_aLIBSYS, "nsl" )
               AAdd( s_aLIBSYS, "resolv" )
            CASE s_cARCH == "hpux"
               AAdd( s_aLIBSYS, "rt" )
            ENDCASE
         ENDIF

         IF IsGTRequested( s_cGT, s_aLIBUSERGT, s_aLIBDYNHAS, s_lSHARED, "gtcrs" )
            /* TOFIX: Sometimes 'ncur194' is needed. */
            AAdd( s_aLIBSYS, IIF( s_cARCH == "sunos", "curses", "ncurses" ) )
         ENDIF
         IF IsGTRequested( s_cGT, s_aLIBUSERGT, s_aLIBDYNHAS, s_lSHARED, "gtsln" )
            AAdd( s_aLIBSYS, "slang" )
            /* Add paths, where this isn't a system component */
            DO CASE
            CASE s_cARCH == "darwin"
               AAdd( s_aLIBPATH, "/sw/lib" )
               AAdd( s_aLIBPATH, "/opt/local/lib" )
            CASE s_cARCH == "bsd"
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

      CASE ( s_cARCH == "win" .AND. s_cCOMP == "gcc" ) .OR. ;
           ( s_cARCH == "win" .AND. s_cCOMP == "mingw" ) .OR. ;
           ( s_cARCH == "win" .AND. s_cCOMP == "mingw64" ) .OR. ;
           ( s_cARCH == "wce" .AND. s_cCOMP == "mingwarm" ) .OR. ;
           ( s_cARCH == "win" .AND. s_cCOMP == "cygwin" )

         IF s_lDEBUG
            AAdd( s_aOPTC, "-g" )
         ENDIF
         cLibLibPrefix := "lib"
         cLibPrefix := "-l"
         cLibExt := ""
         cObjExt := ".o"
         cBin_CompC := s_cCCPREFIX + iif( s_lCPP != NIL .AND. s_lCPP, "g++.exe", "gcc.exe" )
         cOpt_CompC := "-c"
         IF s_lOPT
            cOpt_CompC += " -O3"
            IF !( s_cCOMP == "cygwin" )
               cOpt_CompC += " -march=i586 -mtune=pentiumpro"
            ENDIF
            IF ! s_lDEBUG
               cOpt_CompC += " -fomit-frame-pointer"
            ENDIF
         ENDIF
         cOpt_CompC += " {FC}"
         cOptIncMask := '-I"{DI}"'
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
         cBin_Lib := s_cCCPREFIX + "ar.exe"
         cOpt_Lib := "{FA} rcs {OL} {LO}"
         cLibObjPrefix := NIL
         IF ! Empty( s_cCCPATH )
            cBin_Lib   := s_cCCPATH + "\" + cBin_Lib
            cBin_CompC := s_cCCPATH + "\" + cBin_CompC
            cBin_Link  := s_cCCPATH + "\" + cBin_Link
         ENDIF
         IF !( s_cARCH == "wce" )
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
            AAdd( s_aLIBPATH, s_cHB_BIN_INSTALL )
         ENDIF
         IF ! lStopAfterCComp
            IF s_cCOMP $ "mingw|mingw64|mingwarm"
               AAdd( s_aOPTL, "-Wl,--start-group {LL} -Wl,--end-group" )
            ELSE
               AAdd( s_aOPTL, "{LL}" )
               aLIB_BASE2 := ArrayAJoin( { aLIB_BASE2, { "hbcommon", "hbrtl" }, s_aLIBVM } )
            ENDIF
         ENDIF
         IF s_lSTRIP
            AAdd( s_aOPTL, "-s" )
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
         CASE s_cCOMP == "mingw64"
            s_aLIBSHARED := { iif( s_lMT, "harbourmt-" + cDL_Version_Alter + "-x64",;
                                          "harbour-" + cDL_Version_Alter + "-x64" ) }
         CASE s_cCOMP == "mingwarm"
            s_aLIBSHARED := { iif( s_lMT, "harbourmt-" + cDL_Version_Alter + "-arm",;
                                          "harbour-" + cDL_Version_Alter + "-arm" ) }
         OTHERWISE
            s_aLIBSHARED := { iif( s_lMT, "harbourmt-" + cDL_Version_Alter,;
                                          "harbour-" + cDL_Version_Alter ) }
         ENDCASE

         s_aLIBSHAREDPOST := { "hbmainstd", "hbmainwin" }

         IF s_cCOMP $ "mingw|mingw64|mingwarm" .AND. Len( s_aRESSRC ) > 0
            cBin_Res := s_cCCPREFIX + "windres"
            cResExt := ".reso"
            cOpt_Res := "{FR} {IR} -O coff -o {OS}"
            IF ! Empty( s_cCCPATH )
               cBin_Res := s_cCCPATH + "\" + cBin_Res
            ENDIF
         ENDIF

      CASE s_cARCH == "os2" .AND. s_cCOMP == "gcc"

         IF s_lDEBUG
            AAdd( s_aOPTC, "-g" )
         ENDIF
         cLibLibPrefix := "lib"
         cLibPrefix := "-l"
         cLibExt := ""
         cObjExt := ".o"
         cBin_CompC := iif( s_lCPP != NIL .AND. s_lCPP, "g++.exe", "gcc.exe" )
         cOpt_CompC := "-c"
         IF s_lOPT
            cOpt_CompC += " -O3"
         ENDIF
         cOpt_CompC += " {FC}"
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
            AAdd( s_aLIBPATH, s_cHB_BIN_INSTALL )
         ENDIF
         AAdd( s_aOPTL, "{LL}" )
         aLIB_BASE2 := ArrayAJoin( { aLIB_BASE2, { "hbcommon", "hbrtl" }, s_aLIBVM } )
         IF ! s_lSHARED
            s_aLIBSYS := ArrayJoin( s_aLIBSYS, { "socket" } )
         ENDIF
         IF s_lSTRIP
            AAdd( s_aOPTL, "-s" )
         ENDIF
         /* OS/2 needs a space between -o and file name following it */
         IF lStopAfterCComp
            IF ! lCreateLib .AND. ! lCreateDyn .AND. ( Len( s_aPRG ) + Len( s_aC ) ) == 1
               AAdd( s_aOPTL, "-o {OO}" )
            ENDIF
         ELSE
            AAdd( s_aOPTL, "-o {OE}" )
         ENDIF

      CASE s_cARCH == "dos" .AND. s_cCOMP == "djgpp"

         IF s_lDEBUG
            AAdd( s_aOPTC, "-g" )
         ENDIF
         cLibLibPrefix := "lib"
         cLibPrefix := "-l"
         cLibExt := ""
         cObjExt := ".o"
         cBin_CompC := iif( s_lCPP != NIL .AND. s_lCPP, "gxx.exe", "gcc.exe" )
         cOpt_CompC := "-c"
         IF s_lOPT
            cOpt_CompC += " -O3"
         ENDIF
         cOpt_CompC += " {FC}"
         IF s_lINC .AND. ! Empty( cWorkDir )
            cOpt_CompC += " {IC} -o {OO}"
         ELSE
            cOpt_CompC += " {LC}{SCRIPT}"
         ENDIF
         cBin_Link := cBin_CompC
         cOpt_Link := "{LO} {LA} {FL} {DL}{SCRIPT}"
         cLibPathPrefix := "-L"
         cLibPathSep := " "
         IF s_cCOMP == "djgpp"
            AAdd( s_aOPTL, "-Wl,--start-group {LL} -Wl,--end-group" )
         ELSE
            AAdd( s_aOPTL, "{LL}" )
            aLIB_BASE2 := ArrayAJoin( { aLIB_BASE2, { "hbcommon", "hbrtl" }, s_aLIBVM } )
         ENDIF
         IF ! s_lSHARED
            s_aLIBSYS := ArrayJoin( s_aLIBSYS, { "m" } )
         ENDIF
         IF s_lSTRIP
            AAdd( s_aOPTL, "-s" )
         ENDIF
         IF lStopAfterCComp
            IF ! lCreateLib .AND. ! lCreateDyn .AND. ( Len( s_aPRG ) + Len( s_aC ) ) == 1
               AAdd( s_aOPTL, "-o{OO}" )
            ENDIF
         ELSE
            AAdd( s_aOPTL, "-o{OE}" )
         ENDIF

      /* Watcom family */
      CASE s_cARCH == "dos" .AND. s_cCOMP == "owatcom"
         cLibPrefix := "LIB "
         cLibExt := ".lib"
         cObjPrefix := "FILE "
         cObjExt := ".obj"
         cLibPathPrefix := "LIBPATH "
         cLibPathSep := " "
         IF s_lCPP != NIL .AND. s_lCPP
            cBin_CompC := "wpp386.exe"
         ELSE
            cBin_CompC := "wcc386.exe"
         ENDIF
         cOpt_CompC := ""
         IF s_lOPT
            cOpt_CompC += " -5r -fp5"
            cOpt_CompC += " -onaehtr -s -ei -zp4 -zt0"
            IF s_lCPP != NIL .AND. s_lCPP
               cOpt_CompC += " -oi+"
            ELSE
               cOpt_CompC += " -oi"
            ENDIF
         ENDIF
         cOpt_CompC += " -zq -bt=DOS {FC}"
         cOptIncMask := "-i{DI}"
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
         IF s_lDEBUG
            AAdd( s_aOPTC, "-d2" )
            cOpt_Link := "DEBUG ALL" + cOpt_Link
         ENDIF
         IF s_lMAP
            AAdd( s_aOPTL, "OP MAP" )
         ENDIF

      CASE s_cARCH == "win" .AND. s_cCOMP == "owatcom"
         cLibPrefix := "LIB "
         cLibExt := ".lib"
         cObjPrefix := "FILE "
         cObjExt := ".obj"
         cLibPathPrefix := "LIBPATH "
         cLibPathSep := " "
         IF s_lCPP != NIL .AND. s_lCPP
            cBin_CompC := "wpp386.exe"
         ELSE
            cBin_CompC := "wcc386.exe"
         ENDIF
         cOpt_CompC := ""
         IF s_lOPT
            cOpt_CompC += " -6s -fp6"
            cOpt_CompC += " -onaehtr -s -ei -zp4 -zt0"
            IF s_lCPP != NIL .AND. s_lCPP
               cOpt_CompC += " -oi+"
            ELSE
               cOpt_CompC += " -oi"
            ENDIF
         ELSE
            cOpt_CompC += " -3s"
         ENDIF
         cOpt_CompC += " -zq -bt=NT {FC}"
         cOptIncMask := "-i{DI}"
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
         IF s_lMT
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
         IF s_lDEBUG
            AAdd( s_aOPTC, "-d2" )
            cOpt_Link := "DEBUG ALL" + cOpt_Link
         ENDIF
         IF s_lMAP
            AAdd( s_aOPTL, "OP MAP" )
         ENDIF
         s_aLIBSYS := ArrayAJoin( { s_aLIBSYS, s_aLIBSYSCORE, s_aLIBSYSMISC } )
         s_aLIBSHARED := { iif( s_lMT, "harbourmt-" + cDL_Version_Alter + cLibExt,;
                                       "harbour-" + cDL_Version_Alter + cLibExt ) }

         IF s_lSHARED
            AAdd( s_aOPTL, "FILE " + FN_ExtSet( s_cHB_LIB_INSTALL + hb_osPathSeparator() + iif( s_lGUI, "hbmainwin", "hbmainstd" ), cLibExt ) )
         ENDIF

         IF Len( s_aRESSRC ) > 0
            cBin_Res := "wrc"
            cResExt := ".res"
            cOpt_Res := "-r {FR} -zm {IR} -fo={OS}"
            cResPrefix := "OP res="
         ENDIF

      CASE s_cARCH == "os2" .AND. s_cCOMP == "owatcom"
         cLibPrefix := "LIB "
         cLibExt := ".lib"
         cObjPrefix := "FILE "
         cObjExt := ".obj"
         cLibPathPrefix := "LIBPATH "
         cLibPathSep := " "
         IF s_lCPP != NIL .AND. s_lCPP
            cBin_CompC := "wpp386.exe"
         ELSE
            cBin_CompC := "wcc386.exe"
         ENDIF
         cOpt_CompC := ""
         IF s_lOPT
            cOpt_CompC += " -5s -fp5"
            cOpt_CompC += " -onaehtr -s -ei -zp4 -zt0"
            IF s_lCPP != NIL .AND. s_lCPP
               cOpt_CompC += " -oi+"
            ELSE
               cOpt_CompC += " -oi"
            ENDIF
         ENDIF
         cOpt_CompC += " -zq -bt=OS2 {FC}"
         cOptIncMask := "-i{DI}"
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
         IF s_lDEBUG
            AAdd( s_aOPTC, "-d2" )
            cOpt_Link := "DEBUG ALL" + cOpt_Link
         ENDIF
         IF s_lMT
            AAdd( s_aOPTC, "-bm" )
         ENDIF
         IF s_lMAP
            AAdd( s_aOPTL, "OP MAP" )
         ENDIF

      CASE s_cARCH == "linux" .AND. s_cCOMP == "owatcom"
         cLibPrefix := "LIB "
         cLibExt := ".lib"
         cObjPrefix := "FILE "
         cObjExt := ".o"
         cLibPathPrefix := "LIBPATH "
         cLibPathSep := " "
         IF s_lCPP != NIL .AND. s_lCPP
            cBin_CompC := "wpp386"
         ELSE
            cBin_CompC := "wcc386"
         ENDIF
         cOpt_CompC := ""
         IF s_lOPT
            cOpt_CompC += " -6s -fp6"
            cOpt_CompC += " -onaehtr -s -ei -zp4 -zt0"
            IF s_lCPP != NIL .AND. s_lCPP
               cOpt_CompC += " -oi+"
            ELSE
               cOpt_CompC += " -oi"
            ENDIF
         ENDIF
         cOpt_CompC += " -zq -bt=linux {FC}"
         cOptIncMask := "-i{DI}"
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
         IF s_lMT
            AAdd( s_aOPTC, "-bm" )
         ENDIF
         IF s_lDEBUG
            AAdd( s_aOPTC, "-d2" )
            cOpt_Link := "DEBUG ALL" + cOpt_Link
         ENDIF
         IF s_lMAP
            AAdd( s_aOPTL, "OP MAP" )
         ENDIF

      /* Misc */
      CASE s_cARCH == "win" .AND. s_cCOMP == "bcc"
         IF s_lDEBUG
            AAdd( s_aOPTC, "-y -v" )
            AAdd( s_aOPTL, "-v" )
         ELSE
            AAdd( s_aCLEAN, PathSepToTarget( FN_ExtSet( s_cPROGNAME, ".tds" ) ) )
         ENDIF
         IF s_lGUI
            AAdd( s_aOPTC, "-tW" )
         ENDIF
         IF s_lCPP != NIL .AND. s_lCPP
            AAdd( s_aOPTC, "-P" )
         ENDIF
         cLibPrefix := NIL
         cLibExt := ".lib"
         cObjExt := ".obj"
         cBin_Lib := "tlib.exe"
         cOpt_Lib := "{FA} {OL} {LO}{SCRIPT}"
         cLibLibExt := cLibExt
         cLibObjPrefix := "-+ "
         cBin_CompC := "bcc32.exe"
         cOpt_CompC := "-c -q -tWM"
         IF s_lOPT
            cOpt_CompC += " -d -6 -O2 -OS -Ov -Oi -Oc"
         ENDIF
         cOpt_CompC += " {FC} {LC}"
         cBin_Res := "brcc32.exe"
         cOpt_Res := "{FR} {IR} -fo{OS}"
         cResExt := ".res"
         cBin_Link := "ilink32.exe"
         cBin_Dyn := cBin_Link
         cOpt_Link := '-Gn -Tpe -L"{DL}" {FL} ' + iif( s_lGUI, "c0w32.obj", "c0x32.obj" ) + " {LO}, {OE}, " + iif( s_lMAP, "{OM}", "nul" ) + ", {LL} import32.lib cw32mt.lib,, {LS}{SCRIPT}"
         cOpt_Dyn  := '-Gn -Tpd -L"{DL}" {FD} ' +              "c0d32.obj"                + " {LO}, {OD}, " + iif( s_lMAP, "{OM}", "nul" ) + ", {LL} import32.lib cw32mt.lib,, {LS}{SCRIPT}"
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
            AAdd( s_aLIBPATH, s_cHB_BIN_INSTALL )
         ENDIF
         s_aLIBSHARED := { iif( s_lMT, "harbourmt-" + cDL_Version_Alter + "-bcc" + cLibExt,;
                                       "harbour-" + cDL_Version_Alter + "-bcc" + cLibExt ) }
         s_aLIBSHAREDPOST := { "hbmainstd", "hbmainwin" }
         s_aLIBSYS := ArrayAJoin( { s_aLIBSYS, s_aLIBSYSCORE, s_aLIBSYSMISC } )

      CASE ( s_cARCH == "win" .AND. s_cCOMP $ "msvc|msvc64|msvcia64|icc|iccia64" ) .OR. ;
           ( s_cARCH == "wce" .AND. s_cCOMP == "msvcarm" ) /* NOTE: Cross-platform: wce/ARM on win/x86 */
         IF s_lDEBUG
            IF s_cCOMP == "msvcarm"
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
         IF s_lCPP != NIL
            IF s_lCPP
               AAdd( s_aOPTC, "-TP" )
            ELSE
               AAdd( s_aOPTC, "-TC" )
            ENDIF
         ENDIF
         cLibPrefix := NIL
         cLibExt := ".lib"
         cObjExt := ".obj"
         cLibLibExt := cLibExt
         IF s_cCOMP $ "icc|iccia64"
            cBin_Lib := "xilib.exe"
            cBin_CompC := "icl.exe"
            cBin_Link := "xilink.exe"
            cBin_Dyn := cBin_Link
         ELSE
            cBin_Lib := "lib.exe"
            IF s_cCOMP == "msvcarm" .AND. ! Empty( GetEnv( "HB_VISUALC_VER_PRE80" ) )
               cBin_CompC := "clarm.exe"
            ELSE
               cBin_CompC := "cl.exe"
            ENDIF
            cBin_Link := "link.exe"
            cBin_Dyn := cBin_Link
         ENDIF
         cOpt_Lib := "{FA} /out:{OL} {LO}"
         cOpt_Dyn := "{FD} /dll /out:{OD} {DL} {LO} {LL} {LS}"
         cOpt_CompC := "-nologo -c -Gs"
         IF s_lOPT
            IF s_cCOMP == "msvcarm"
               IF Empty( GetEnv( "HB_VISUALC_VER_PRE80" ) )
                  cOpt_CompC += " -Od -Os -Gy -GS- -EHsc- -Gm -Zi -GR-"
               ELSE
                  cOpt_CompC += " -Oxsb1 -EHsc -YX -GF"
               ENDIF
            ELSE
               IF Empty( GetEnv( "HB_VISUALC_VER_PRE80" ) )
                  cOpt_CompC += " -Ot2b1 -EHs-c-"
               ELSE
                  cOpt_CompC += " -Ogt2yb1p -GX- -G6 -YX"
               ENDIF
            ENDIF
         ENDIF
         cOpt_CompC += " {FC} {LC}"
         cOptIncMask := '-I"{DI}"'
         cOpt_Link := "-nologo /out:{OE} {LO} {DL} {FL} {LL} {LS}"
         cLibPathPrefix := "/libpath:"
         cLibPathSep := " "
         IF s_lMAP
            AAdd( s_aOPTC, "-Fm" )
            AAdd( s_aOPTD, "-Fm" )
         ENDIF
         IF s_cCOMP == "msvcarm"
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
            AAdd( s_aLIBPATH, s_cHB_BIN_INSTALL )
         ENDIF
         s_aLIBSYS := ArrayAJoin( { s_aLIBSYS, s_aLIBSYSCORE, s_aLIBSYSMISC } )
         DO CASE
         CASE s_cCOMP $ "msvc|icc"
            s_aLIBSHARED := { iif( s_lMT, "harbourmt-" + cDL_Version_Alter + cLibExt,;
                                          "harbour-" + cDL_Version_Alter + cLibExt ) }
         CASE s_cCOMP == "msvc64"
            s_aLIBSHARED := { iif( s_lMT, "harbourmt-" + cDL_Version_Alter + "-x64" + cLibExt,;
                                          "harbour-" + cDL_Version_Alter + "-x64" + cLibExt ) }
         CASE s_cCOMP $ "msvcia64|iccia64"
            s_aLIBSHARED := { iif( s_lMT, "harbourmt-" + cDL_Version_Alter + "-ia64" + cLibExt,;
                                          "harbour-" + cDL_Version_Alter + "-ia64" + cLibExt ) }
         CASE s_cCOMP == "msvcarm"
            s_aLIBSHARED := { iif( s_lMT, "harbourmt-" + cDL_Version_Alter + "-arm" + cLibExt,;
                                          "harbour-" + cDL_Version_Alter + "-arm" + cLibExt ) }
         ENDCASE

         s_aLIBSHAREDPOST := { "hbmainstd", "hbmainwin" }

         IF !( s_cCOMP $ "icc|iccia64" )
            cBin_Res := "rc.exe"
            cOpt_Res := "{FR} /fo {OS} {IR}"
            cResExt := ".res"
         ENDIF

      CASE ( s_cARCH == "win" .AND. s_cCOMP == "pocc" ) .OR. ;
           ( s_cARCH == "win" .AND. s_cCOMP == "pocc64" ) .OR. ; /* NOTE: Cross-platform: win/amd64 on win/x86 */
           ( s_cARCH == "wce" .AND. s_cCOMP == "poccarm" ) .OR. ; /* NOTE: Cross-platform: wce/ARM on win/x86 */
           ( s_cARCH == "win" .AND. s_cCOMP == "xcc" )

         IF s_lGUI
            AAdd( s_aOPTL, "/subsystem:windows" )
         ELSE
            AAdd( s_aOPTL, "/subsystem:console" )
         ENDIF
         IF s_lDEBUG
            AAdd( s_aOPTC, "-Zi" )
         ENDIF
         cLibPrefix := NIL
         cLibExt := ".lib"
         cObjExt := ".obj"
         cLibLibExt := cLibExt
         IF s_cCOMP == "xcc"
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
         cOpt_CompC := "/c /Ze /Go {FC} {IC} /Fo{OO}"
         cOptIncMask := "/I{DI}"
         cOpt_Dyn := "{FD} /dll /out:{OD} {DL} {LO} {LL} {LS}"
         DO CASE
         CASE s_cCOMP == "pocc"
            IF s_lOPT
               AAdd( s_aOPTC, "/Ot" )
            ENDIF
            AAdd( s_aOPTC, "/Tx86-coff" )
         CASE s_cCOMP == "pocc64"
            AAdd( s_aOPTC, "/Tamd64-coff" )
         CASE s_cCOMP == "poccarm"
            AAdd( s_aOPTC, "/Tarm-coff" )
            AAdd( s_aOPTC, "-D_M_ARM" )
            AAdd( s_aOPTC, "-D_WINCE" )
            AAdd( s_aOPTC, "-DUNICODE" )
            AAdd( s_aOPTC, "-DHB_NO_WIN_CONSOLE" )
         ENDCASE
         cOpt_Res := "{FR} /Fo{OS} {IR}"
         cResExt := ".res"
         cOpt_Lib := "{FA} /out:{OL} {LO}"
         IF s_lMT
            AAdd( s_aOPTC, "/MT" )
         ENDIF
         IF !( lStopAfterCComp .AND. ! lCreateLib .AND. ! lCreateDyn )
            AAdd( s_aOPTL, "/out:{OE}" )
         ENDIF
         cOpt_Link := "{LO} {DL} {FL} {LL} {LS}"
         cLibPathPrefix := "/libpath:"
         cLibPathSep := " "
         IF s_lSHARED
            AAdd( s_aLIBPATH, s_cHB_BIN_INSTALL )
         ENDIF
         IF s_lMAP
            AAdd( s_aOPTL, "/map" )
         ENDIF
         IF s_lDEBUG
            AAdd( s_aOPTL, "/debug" )
         ENDIF
         s_aLIBSYS := ArrayAJoin( { s_aLIBSYS, s_aLIBSYSCORE, s_aLIBSYSMISC } )
         DO CASE
         CASE s_cCOMP == "pocc64"
            s_aLIBSHARED := { iif( s_lMT, "harbourmt-" + cDL_Version_Alter + "-x64" + cLibExt,;
                                          "harbour-" + cDL_Version_Alter + "-x64" + cLibExt ) }
         CASE s_cCOMP == "poccarm"
            s_aLIBSHARED := { iif( s_lMT, "harbourmt-" + cDL_Version_Alter + "-arm" + cLibExt,;
                                          "harbour-" + cDL_Version_Alter + "-arm" + cLibExt ) }
         OTHERWISE
            s_aLIBSHARED := { iif( s_lMT, "harbourmt-" + cDL_Version_Alter + cLibExt,;
                                          "harbour-" + cDL_Version_Alter + cLibExt ) }
         ENDCASE

         s_aLIBSHAREDPOST := { "hbmainstd", "hbmainwin" }

      /* TODO */
      CASE s_cARCH == "linux" .AND. s_cCOMP == "icc"
      ENDCASE

      IF lCreateDyn .AND. s_cARCH $ "win|wce"
         AAdd( s_aOPTC, "-DHB_DYNLIB" )
         AAdd( aLIB_BASE1, "hbmaindllp" )
      ENDIF
   ENDIF

   /* Header paths */

   FOR EACH tmp IN s_aINCPATH
      AAdd( s_aOPTPRG, "-i" + tmp )
      AAdd( s_aOPTC, StrTran( cOptIncMask, "{DI}", tmp ) )
      AAdd( s_aOPTRES, StrTran( cOptIncMask, "{DI}", tmp ) )
   NEXT

   /* Do header detection and create incremental file list for .c files */

   IF ! lStopAfterInit .AND. ! lStopAfterHarbour

      headstate := NIL

      IF s_lINC .AND. ! s_lREBUILD
         s_aC_TODO := {}
         s_aC_DONE := {}
         FOR EACH tmp IN s_aC
            IF s_lDEBUGINC
               OutStd( "hbmk: debuginc: C", tmp, FN_DirExtSet( tmp, cWorkDir, cObjExt ), hb_osNewLine() )
            ENDIF
            IF ! hb_FGetDateTime( FN_DirExtSet( tmp, cWorkDir, cObjExt ), @tmp2 ) .OR. ;
               ! hb_FGetDateTime( tmp, @tmp1 ) .OR. ;
               tmp1 > tmp2 .OR. ;
               ( s_nHEAD != _HEAD_OFF .AND. FindNewerHeaders( tmp, NIL, tmp2, ! Empty( s_aINCTRYPATH ), .T., @headstate ) )
               AAdd( s_aC_TODO, tmp )
            ELSE
               AAdd( s_aC_DONE, tmp )
            ENDIF
         NEXT
      ELSE
         s_aC_TODO := s_aC
         s_aC_DONE := {}
      ENDIF

      /* Header dir detection if needed and if FindNewerHeaders() wasn't called yet. */
      IF ! Empty( s_aINCTRYPATH ) .AND. ! Empty( s_aC_TODO ) .AND. headstate == NIL
         FOR EACH tmp IN s_aC
            FindNewerHeaders( tmp, NIL, NIL, .T., .T., @headstate )
         NEXT
      ENDIF
   ENDIF

   /* Create incremental file list for .prg files */

   IF ! lStopAfterInit .AND. ! lStopAfterHarbour

      /* Incremental */

      IF s_lINC .AND. ! s_lREBUILD
         s_aPRG_TODO := {}
         FOR EACH tmp IN s_aPRG
            IF s_lDEBUGINC
               OutStd( "hbmk: debuginc: PRG", FN_ExtSet( tmp, ".prg" ),;
                                              FN_DirExtSet( tmp, cWorkDir, ".c" ), hb_osNewLine() )
            ENDIF
            IF ! hb_FGetDateTime( FN_DirExtSet( tmp, cWorkDir, ".c" ), @tmp2 ) .OR. ;
               ! hb_FGetDateTime( FN_ExtSet( tmp, ".prg" ), @tmp1 ) .OR. ;
               tmp1 > tmp2 .OR. ;
               ( s_nHEAD != _HEAD_OFF .AND. FindNewerHeaders( FN_ExtSet( tmp, ".prg" ), NIL, tmp2, .F., .F., @headstate ) )
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

      IF s_lINC .AND. ! s_lQuiet
         OutStd( "Compiling Harbour sources..." + hb_osNewLine() )
      ENDIF

      PlatformPRGFlags( s_aOPTPRG )

#if defined( HBMK_INTEGRATED_COMPILER )
      aCommand := ArrayAJoin( { { iif( lCreateLib .OR. lCreateDyn, "-n1", "-n2" ) },;
                                s_aPRG_TODO,;
                                iif( s_lBLDFLGP, { " " + cSelfFlagPRG }, {} ),;
                                ListToArray( iif( ! Empty( GetEnv( "HB_USER_PRGFLAGS" ) ), " " + GetEnv( "HB_USER_PRGFLAGS" ), "" ) ),;
                                s_aOPTPRG } )

      IF s_lTRACE
         IF ! s_lQuiet
            OutStd( "hbmk: Harbour compiler command (internal):" + hb_osNewLine() )
         ENDIF
         OutStd( DirAddPathSep( PathSepToSelf( s_cHB_BIN_INSTALL ) ) + cBin_CompPRG +;
                 " " + ArrayToList( aCommand ) + hb_osNewLine() )
      ENDIF

      IF ! s_lDONTEXEC .AND. ( tmp := hb_compile( "", aCommand ) ) != 0
         OutErr( "hbmk: Error: Running Harbour compiler. " + hb_ntos( tmp ) + hb_osNewLine() )
         OutErr( ArrayToList( aCommand ) + hb_osNewLine() )
         RETURN 6
      ENDIF
#else
      cCommand := DirAddPathSep( PathSepToSelf( s_cHB_BIN_INSTALL ) ) +;
                  cBin_CompPRG +;
                  " " + iif( lCreateLib .OR. lCreateDyn, "-n1", "-n2" ) +;
                  " " + ArrayToList( s_aPRG_TODO ) +;
                  iif( s_lBLDFLGP, " " + cSelfFlagPRG, "" ) +;
                  iif( ! Empty( GetEnv( "HB_USER_PRGFLAGS" ) ), " " + GetEnv( "HB_USER_PRGFLAGS" ), "" ) +;
                  iif( ! Empty( s_aOPTPRG ), " " + ArrayToList( s_aOPTPRG ), "" )

      cCommand := AllTrim( cCommand )

      IF s_lTRACE
         IF ! s_lQuiet
            OutStd( "hbmk: Harbour compiler command:" + hb_osNewLine() )
         ENDIF
         OutStd( cCommand + hb_osNewLine() )
      ENDIF

      IF ! s_lDONTEXEC .AND. ( tmp := hbmk_run( cCommand ) ) != 0
         OutErr( "hbmk: Error: Running Harbour compiler. " + hb_ntos( tmp ) + ":" + hb_osNewLine() )
         IF ! s_lQuiet
            OutErr( cCommand + hb_osNewLine() )
         ENDIF
         RETURN 6
      ENDIF
#endif
   ENDIF

   IF ! lStopAfterInit .AND. ! lStopAfterHarbour

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
           s_cGT != NIL )

         fhnd := hb_FTempCreateEx( @s_cCSTUB, NIL, "hbmk_", ".c" )
         IF fhnd != F_ERROR

            /* NOTE: This has to be kept synced with Harbour HB_IMPORT values. */
            DO CASE
            CASE ! s_lSHARED .OR. ;
                 !( s_cARCH $ "win|wce" ) .OR. ;
                 s_cCOMP $ "msvc|msvc64|msvcia64|icc|iccia64"

               /* NOTE: MSVC gives the warning:
                        "LNK4217: locally defined symbol ... imported in function ..."
                        if using 'dllimport'. [vszakats] */
               tmp := ""
            CASE s_cCOMP $ "gcc|mingw|mingw64|mingwarm|cygwin" ; tmp := "__attribute__ (( dllimport ))"
            CASE s_cCOMP $ "bcc|owatcom"                       ; tmp := "__declspec( dllimport )"
            OTHERWISE                                          ; tmp := "_declspec( dllimport )"
            ENDCASE

            /* Create list of requested symbols */
            array := {}
            IF s_cMAIN != NIL
               /* NOTE: Request this function to generate link error, rather
                        than starting with the wrong (default) function. */
               AAdd( array, Upper( iif( Left( s_cMAIN, 1 ) == "@", SubStr( s_cMAIN, 2 ), s_cMAIN ) ) )
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
            RETURN 5
         ENDIF
         AAdd( s_aC, s_cCSTUB )
      ENDIF

      /* Library list assembly */
      IF s_lSHARED .AND. ! Empty( s_aLIBSHARED )
         s_aLIBHB := ArrayAJoin( { s_aLIBSHAREDPOST,;
                                   aLIB_BASE_CPLR,;
                                   aLIB_BASE_DEBUG } )
      ELSE
         s_aLIBHB := ArrayAJoin( { aLIB_BASE1,;
                                   aLIB_BASE_CPLR,;
                                   aLIB_BASE_DEBUG,;
                                   s_aLIBVM,;
                                   iif( s_lNULRDD, aLIB_BASE_NULRDD, aLIB_BASE_RDD ),;
                                   aLIB_BASE2,;
                                   iif( s_lHB_PCRE, aLIB_BASE_PCRE, {} ),;
                                   iif( s_lHB_ZLIB, aLIB_BASE_ZLIB, {} ) } )
      ENDIF

      /* Merge lib lists. */
      s_aLIBRAW := ArrayAJoin( { s_aLIBUSER, s_aLIBHB, s_aLIB3RD, s_aLIBSYS } )
      /* Dress lib names. */
      s_aLIB := ListCookLib( s_aLIBRAW, cLibPrefix, cLibExt )
      IF s_lSHARED .AND. ! Empty( s_aLIBSHARED )
         s_aLIBRAW := ArrayJoin( s_aLIBSHARED, s_aLIBRAW )
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
            IF ! hb_FGetDateTime( FN_DirExtSet( tmp, cWorkDir, cResExt ), @tmp2 ) .OR. ;
               ! hb_FGetDateTime( tmp, @tmp1 ) .OR. ;
               tmp1 > tmp2 .OR. ;
               ( s_nHEAD != _HEAD_OFF .AND. FindNewerHeaders( tmp, NIL, tmp2, .F., .T., @headstate ) )
               AAdd( s_aRESSRC_TODO, tmp )
            ENDIF
         NEXT
      ELSE
         s_aRESSRC_TODO := s_aRESSRC
      ENDIF

      IF Len( s_aRESSRC_TODO ) > 0 .AND. ! Empty( cBin_Res ) .AND. ! s_lCLEAN

         IF s_lINC .AND. ! s_lQuiet
            OutStd( "Compiling resources..." + hb_osNewLine() )
         ENDIF

         /* Compiling resource */

         cOpt_Res := StrTran( cOpt_Res, "{FR}"  , GetEnv( "HB_USER_RESFLAGS" ) + " " + ArrayToList( s_aOPTRES ) )
         cOpt_Res := StrTran( cOpt_Res, "{DI}"  , s_cHB_INC_INSTALL )

         IF "{IR}" $ cOpt_Res

            FOR EACH tmp IN s_aRESSRC_TODO

               cCommand := cOpt_Res
               cCommand := StrTran( cCommand, "{IR}", tmp )
               cCommand := StrTran( cCommand, "{OS}", PathSepToTarget( FN_DirExtSet( tmp, cWorkDir, cResExt ) ) )

               cCommand := cBin_Res + " " + AllTrim( cCommand )

               IF s_lTRACE
                  IF ! s_lQuiet
                     OutStd( "hbmk: Resource compiler command:" + hb_osNewLine() )
                  ENDIF
                  OutStd( cCommand + hb_osNewLine() )
               ENDIF

               IF ! s_lDONTEXEC .AND. ( tmp1 := hbmk_run( cCommand ) ) != 0
                  OutErr( "hbmk: Error: Running resource compiler. " + hb_ntos( tmp1 ) + ":" + hb_osNewLine() )
                  IF ! s_lQuiet
                     OutErr( cCommand + hb_osNewLine() )
                  ENDIF
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
               IF ! s_lQuiet
                  OutStd( "hbmk: Resource compiler command:" + hb_osNewLine() )
               ENDIF
               OutStd( cCommand + hb_osNewLine() )
               IF ! Empty( cScriptFile )
                  OutStd( "hbmk: Resource compiler script:" + hb_osNewLine() )
                  OutStd( hb_MemoRead( cScriptFile ) + hb_osNewLine() )
               ENDIF
            ENDIF

            IF ! s_lDONTEXEC .AND. ( tmp := hbmk_run( cCommand ) ) != 0
               OutErr( "hbmk: Error: Running resource compiler. " + hb_ntos( tmp ) + ":" + hb_osNewLine() )
               IF ! s_lQuiet
                  OutErr( cCommand + hb_osNewLine() )
               ENDIF
               nErrorLevel := 8
            ENDIF

            IF ! Empty( cScriptFile )
               FErase( cScriptFile )
            ENDIF
         ENDIF
      ENDIF

      IF nErrorLevel == 0

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

            IF s_lINC .AND. ! s_lQuiet
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
                     IF ! s_lQuiet
                        OutStd( "hbmk: C compiler command:" + hb_osNewLine() )
                     ENDIF
                     OutStd( cCommand + hb_osNewLine() )
                  ENDIF

                  IF ! s_lDONTEXEC .AND. ( tmp1 := hbmk_run( cCommand ) ) != 0
                     OutErr( "hbmk: Error: Running C compiler. " + hb_ntos( tmp1 ) + ":" + hb_osNewLine() )
                     IF ! s_lQuiet
                        OutErr( cCommand + hb_osNewLine() )
                     ENDIF
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
                  IF ! s_lQuiet
                     OutStd( "hbmk: C compiler command:" + hb_osNewLine() )
                  ENDIF
                  OutStd( cCommand + hb_osNewLine() )
                  IF ! Empty( cScriptFile )
                     OutStd( "hbmk: C compiler script:" + hb_osNewLine() )
                     OutStd( hb_MemoRead( cScriptFile ) + hb_osNewLine() )
                  ENDIF
               ENDIF

               IF ! s_lDONTEXEC .AND. ( tmp := hbmk_run( cCommand ) ) != 0
                  OutErr( "hbmk: Error: Running C compiler. " + hb_ntos( tmp ) + ":" + hb_osNewLine() )
                  IF ! s_lQuiet
                     OutErr( cCommand + hb_osNewLine() )
                  ENDIF
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
               /* We need a way to find and pick libraries according to linker rules. */
               IF lTargetUpToDate
                  FOR EACH tmp IN s_aLIBRAW
                     IF ! Empty( tmp2 := FindLib( tmp, s_aLIBPATH, cLibExt ) )
                        IF s_lDEBUGINC
                           OutStd( "hbmk: debuginc: EXEDEPLIB", tmp2, hb_osNewLine() )
                        ENDIF
                        IF ! hb_FGetDateTime( tmp2, @tmp1 ) .OR. tmp1 > tTarget
                           lTargetUpToDate := .F.
                           EXIT
                        ENDIF
                     ENDIF
                  NEXT
               ENDIF
            ENDIF
         ENDIF
      ENDIF

      IF nErrorLevel == 0 .AND. ( Len( s_aOBJ ) + Len( s_aOBJUSER ) + Len( s_aOBJA ) ) > 0 .AND. ! s_lCLEAN

         IF lTargetUpToDate
            OutStd( "hbmk: Target up to date: " + cTarget + hb_osNewLine() )
         ELSE
            DO CASE
            CASE ! lStopAfterCComp .AND. ! Empty( cBin_Link )

               IF s_lINC .AND. ! s_lQuiet
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
                  IF ! s_lQuiet
                     OutStd( "hbmk: Linker command:" + hb_osNewLine() )
                  ENDIF
                  OutStd( cCommand + hb_osNewLine() )
                  IF ! Empty( cScriptFile )
                     OutStd( "hbmk: Linker script:" + hb_osNewLine() )
                     OutStd( hb_MemoRead( cScriptFile ) + hb_osNewLine() )
                  ENDIF
               ENDIF

               IF ! s_lDONTEXEC .AND. ( tmp := hbmk_run( cCommand ) ) != 0
                  OutErr( "hbmk: Error: Running linker. " + hb_ntos( tmp ) + ":" + hb_osNewLine() )
                  IF ! s_lQuiet
                     OutErr( cCommand + hb_osNewLine() )
                  ENDIF
                  nErrorLevel := 7
               ENDIF

               IF ! Empty( cScriptFile )
                  FErase( cScriptFile )
               ENDIF

            CASE lStopAfterCComp .AND. lCreateLib .AND. ! Empty( cBin_Lib )

               IF s_lINC .AND. ! s_lQuiet
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
                  IF ! s_lQuiet
                     OutStd( "hbmk: Lib command:" + hb_osNewLine() )
                  ENDIF
                  OutStd( cCommand + hb_osNewLine() )
                  IF ! Empty( cScriptFile )
                     OutStd( "hbmk: Lib script:" + hb_osNewLine() )
                     OutStd( hb_MemoRead( cScriptFile ) + hb_osNewLine() )
                  ENDIF
               ENDIF

               IF ! s_lDONTEXEC .AND. ( tmp := hbmk_run( cCommand ) ) != 0
                  OutErr( "hbmk: Error: Running lib command. " + hb_ntos( tmp ) + ":" + hb_osNewLine() )
                  IF ! s_lQuiet
                     OutErr( cCommand + hb_osNewLine() )
                  ENDIF
                  nErrorLevel := 7
               ENDIF

               IF ! Empty( cScriptFile )
                  FErase( cScriptFile )
               ENDIF

            CASE lStopAfterCComp .AND. lCreateDyn .AND. ! Empty( cBin_Dyn )

               IF s_lINC .AND. ! s_lQuiet
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
                  IF ! s_lQuiet
                     OutStd( "hbmk: Dynamic lib link command:" + hb_osNewLine() )
                  ENDIF
                  OutStd( cCommand + hb_osNewLine() )
                  IF ! Empty( cScriptFile )
                     OutStd( "hbmk: Dynamic lib link script:" + hb_osNewLine() )
                     OutStd( hb_MemoRead( cScriptFile ) + hb_osNewLine() )
                  ENDIF
               ENDIF

               IF ! s_lDONTEXEC .AND. ( tmp := hbmk_run( cCommand ) ) != 0
                  OutErr( "hbmk: Error: Running dynamic lib link command. " + hb_ntos( tmp ) + ":" + hb_osNewLine() )
                  IF ! s_lQuiet
                     OutErr( cCommand + hb_osNewLine() )
                  ENDIF
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
         IF s_lDEBUGSTUB
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

      IF nErrorLevel == 0 .AND. ! lStopAfterCComp .AND. ! s_lCLEAN
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
               IF ! s_lQuiet
                  OutStd( "hbmk: Compression command:" + hb_osNewLine() )
               ENDIF
               OutStd( cCommand + hb_osNewLine() )
            ENDIF

            IF ! s_lDONTEXEC .AND. ( tmp := hbmk_run( cCommand ) ) != 0
               OutErr( "hbmk: Warning: Running compression command. " + hb_ntos( tmp ) + ":" + hb_osNewLine() )
               IF ! s_lQuiet
                  OutErr( cCommand + hb_osNewLine() )
               ENDIF
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
               IF ! s_lQuiet
                  OutStd( "hbmk: Running executable:" + hb_osNewLine() )
               ENDIF
               OutStd( cCommand + hb_osNewLine() )
            ENDIF
            IF ! s_lDONTEXEC
               nErrorLevel := hbmk_run( cCommand )
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   RETURN nErrorLevel

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

/* This function will scan and detect header dependencies newer than
   root file. It won't attempt to parse all possible #include syntaxes
   and source code formats, won't try to interpret comments, line
   continuation, different keyword and filename cases, etc, etc. In
   order to work, it will need #include "filename" format in source.
   If this isn't enough for your needs, feel free to update the code.
   [vszakats] */

#define _HEADSTATE_hFiles       1
#define _HEADSTATE_lAnyNewer    2
#define _HEADSTATE_MAX_         2

STATIC FUNCTION FindNewerHeaders( cFileName, cParentDir, tTimeParent, lIncTry, lCMode, /* @ */ headstate, nEmbedLevel )
   LOCAL cFile
   LOCAL fhnd
   LOCAL nPos
   LOCAL tTimeSelf
   LOCAL tmp
   LOCAL cNameExtL

   STATIC s_aExcl := { "windows.h", "ole2.h", "os2.h" }

   DEFAULT nEmbedLevel TO 1
   DEFAULT cParentDir TO FN_DirGet( cFileName )

   IF nEmbedLevel == 1
      headstate := Array( _HEADSTATE_MAX_ )
      headstate[ _HEADSTATE_hFiles ] := hb_Hash()
      headstate[ _HEADSTATE_lAnyNewer ] := .F.
   ENDIF

   IF ! lIncTry .AND. s_nHEAD == _HEAD_OFF
      RETURN .F.
   ENDIF

   IF nEmbedLevel > 10
      RETURN .F.
   ENDIF

   /* Filter out non-source format inputs for MinGW / windres */
   IF s_cCOMP $ "gcc|gpp|mingw|mingw64|mingwarm|cygwin" .AND. s_cARCH $ "win|wce" .AND. ;
      Lower( FN_ExtGet( cFileName ) ) == ".res"
      RETURN .F.
   ENDIF

   /* Don't spend time on some known headers */
   cNameExtL := Lower( FN_NameExtGet( cFileName ) )
   IF AScan( s_aExcl, { |tmp| Lower( tmp ) == cNameExtL } ) > 0
      RETURN .F.
   ENDIF

   cFileName := FindHeader( cFileName, cParentDir, iif( lIncTry, s_aINCTRYPATH, NIL ) )
   IF Empty( cFileName )
      RETURN .F.
   ENDIF

   IF hb_HPos( headstate[ _HEADSTATE_hFiles ], cFileName ) > 0
      RETURN .F.
   ENDIF
   hb_HSet( headstate[ _HEADSTATE_hFiles ], cFileName, .T. )

   IF s_lDEBUGINC
      OutStd( "hbmk: debuginc: HEADER", cFileName, hb_osNewLine() )
   ENDIF

   IF ! hb_FGetDateTime( cFileName, @tTimeSelf )
      RETURN .F.
   ENDIF

   IF tTimeParent != NIL .AND. tTimeSelf > tTimeParent
      headstate[ _HEADSTATE_lAnyNewer ] := .T.
      /* Let it continue if we want to scan for header locations */
      IF ! lIncTry
         RETURN .T.
      ENDIF
   ENDIF

   /* NOTE: Beef up this section if you need a more intelligent source
            parser. Notice that this code is meant to process both
            .prg, .c and .res sources. Please try to keep it simple,
            as speed and maintainability is also important. [vszakats] */

   IF lIncTry .OR. s_nHEAD == _HEAD_FULL
      cFile := MemoRead( cFileName )
   ELSE
      IF ( fhnd := FOpen( cFileName, FO_READ + FO_SHARED ) ) == F_ERROR
         RETURN .F.
      ENDIF
      cFile := Space( 16384 )
      FRead( fhnd, @cFile, Len( cFile ) )
      FClose( fhnd )
   ENDIF

   nPos := 1
   DO WHILE ( tmp := hb_At( '#include "', cFile, nPos ) ) > 0
      nPos := tmp + Len( '#include "' )
      IF ( tmp := hb_At( '"', cFile, nPos ) ) > 0
         IF FindNewerHeaders( SubStr( cFile, nPos, tmp - nPos ),;
               iif( lCMode, FN_DirGet( cFileName ), cParentDir ), tTimeParent, lIncTry, lCMode, @headstate, nEmbedLevel + 1 )
            headstate[ _HEADSTATE_lAnyNewer ] := .T.
            /* Let it continue if we want to scan for header locations */
            IF ! lIncTry
               RETURN .T.
            ENDIF
         ENDIF
      ENDIF
   ENDDO

   RETURN headstate[ _HEADSTATE_lAnyNewer ]

STATIC FUNCTION FindHeader( cFileName, cParentDir, aINCTRYPATH )
   LOCAL cDir

   /* Check in current dir */
   IF hb_FileExists( cFileName )
      RETURN cFileName
   ENDIF

   /* Check in parent dir */

   IF hb_FileExists( DirAddPathSep( PathSepToSelf( cParentDir ) ) + cFileName )
      RETURN DirAddPathSep( PathSepToSelf( cParentDir ) ) + cFileName
   ENDIF

   /* Check in include path list specified via -incpath options */
   FOR EACH cDir IN s_aINCPATH
      IF hb_FileExists( DirAddPathSep( PathSepToSelf( cDir ) ) + cFileName )
         RETURN DirAddPathSep( PathSepToSelf( cDir ) ) + cFileName
      ENDIF
   NEXT

   /* Check in potential include path list */
   IF ! Empty( aINCTRYPATH )
      FOR EACH cDir IN aINCTRYPATH
         IF hb_FileExists( DirAddPathSep( PathSepToSelf( cDir ) ) + cFileName )
            /* Add these dir to include paths */
            IF AScan( s_aINCPATH, { |tmp| tmp == cDir } ) == 0
               AAdd( s_aINCPATH, cDir )
               IF s_lDEBUGINC
                  OutStd( "hbmk: debuginc: Autodetected header dir for " + cFileName + ": " + cDir, hb_osNewLine() )
               ENDIF
            ENDIF
            RETURN DirAddPathSep( PathSepToSelf( cDir ) ) + cFileName
         ENDIF
      NEXT
   ENDIF

   RETURN NIL

/* Replicating logic used by compilers. */

STATIC FUNCTION FindLib( cLib, aLIBPATH, cLibExt )
   LOCAL cDir
   LOCAL tmp

   /* Check libnames containing dirs */
   IF s_cCOMP $ "msvc|msvc64|msvcarm|bcc|pocc|pocc64|poccarm|owatcom"
      IF ! Empty( FN_DirGet( cLib ) )
         IF hb_FileExists( cLib := FN_ExtSet( cLib, cLibExt ) )
            RETURN cLib
         ENDIF
         IF s_cCOMP $ "pocc|pocc64|poccarm"
            IF hb_FileExists( cLib := FN_ExtSet( cLib, ".a" ) )
               RETURN cLib
            ENDIF
         ENDIF
         RETURN NIL
      ENDIF
   ENDIF

   /* Check in current dir */
   IF s_cCOMP $ "msvc|msvc64|msvcarm|bcc|pocc|pocc64|poccarm|owatcom"
      IF ! Empty( tmp := LibExists( "", cLib, cLibExt ) )
         RETURN tmp
      ENDIF
   ENDIF

   /* Check in libpaths */
   FOR EACH cDir IN aLIBPATH
      IF ! Empty( cDir )
         IF ! Empty( tmp := LibExists( cDir, cLib, cLibExt ) )
            RETURN tmp
         ENDIF
      ENDIF
   NEXT

#if 0
   /* Check in certain other compiler specific locations. */
   IF s_cCOMP $ "msvc|msvc64|msvcarm"
      FOR EACH cDir IN hb_ATokens( GetEnv( "LIB" ), hb_osPathListSeparator(), .T., .T. )
         IF ! Empty( cDir )
            IF ! Empty( tmp := LibExists( cDir, cLib, cLibExt ) )
               RETURN tmp
            ENDIF
         ENDIF
      NEXT
   ENDIF
#endif

   RETURN NIL

STATIC FUNCTION LibExists( cDir, cLib, cLibExt )
   LOCAL tmp

   cDir := DirAddPathSep( PathSepToSelf( cDir ) )

   DO CASE
   CASE s_cCOMP $ "gcc|gpp|mingw|mingw64|mingwarm|cygwin" .AND. s_cARCH $ "win|wce"
      /* NOTE: ld/gcc option -dll-search-prefix isn't taken into account here,
               So, '<prefix>xxx.dll' format libs won't be found by hbmk2. */
      DO CASE
      CASE                           hb_FileExists( tmp := cDir + "lib" + FN_ExtSet( cLib, ".dll.a" ) ) ; RETURN tmp
      CASE                           hb_FileExists( tmp := cDir +         FN_ExtSet( cLib, ".dll.a" ) ) ; RETURN tmp
      CASE                           hb_FileExists( tmp := cDir + "lib" + FN_ExtSet( cLib, ".a" )     ) ; RETURN tmp
      CASE s_cCOMP == "cygwin" .AND. hb_FileExists( tmp := cDir + "cyg" + FN_ExtSet( cLib, ".dll" )   ) ; RETURN tmp
      CASE                           hb_FileExists( tmp := cDir + "lib" + FN_ExtSet( cLib, ".dll" )   ) ; RETURN tmp
      CASE                           hb_FileExists( tmp := cDir +         FN_ExtSet( cLib, ".dll" )   ) ; RETURN tmp
      ENDCASE
   CASE s_cCOMP $ "gcc|gpp" .AND. s_cARCH $ "linux|sunos"
      DO CASE
      CASE                           hb_FileExists( tmp := cDir + "lib" + FN_ExtSet( cLib, ".so" )    ) ; RETURN tmp
      CASE                           hb_FileExists( tmp := cDir + "lib" + FN_ExtSet( cLib, ".a" )     ) ; RETURN tmp
      ENDCASE
   CASE s_cCOMP $ "pocc|pocc64|poccarm"
      DO CASE
      CASE                           hb_FileExists( tmp := cDir +         FN_ExtSet( cLib, cLibExt )  ) ; RETURN tmp
      CASE                           hb_FileExists( tmp := cDir +         FN_ExtSet( cLib, ".a" )     ) ; RETURN tmp
      ENDCASE
   OTHERWISE
      DO CASE
      CASE                           hb_FileExists( tmp := cDir +         FN_ExtSet( cLib, cLibExt )  ) ; RETURN tmp
      ENDCASE
   ENDCASE

   RETURN NIL

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

STATIC FUNCTION ArraySplit( arrayIn, nChunksReq )
   LOCAL arrayOut
   LOCAL nChunkSize
   LOCAL nChunkPos
   LOCAL item

   IF nChunksReq > 0

      arrayOut := {}
      nChunkSize := Max( Round( Len( arrayIn ) / nChunksReq, 0 ), 1 )
      nChunkPos := 0

      FOR EACH item IN arrayIn
         IF nChunkPos == 0
            AAdd( arrayOut, {} )
         ENDIF
         AAdd( ATail( arrayOut ), item )
         IF ++nChunkPos == nChunkSize .AND. Len( arrayOut ) < nChunksReq
            nChunkPos := 0
         ENDIF
      NEXT
   ELSE
      arrayOut := arrayIn
   ENDIF

   RETURN arrayOut

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

   IF s_cCOMP $ "gcc|gpp|mingw|mingw64|mingwarm|djgpp|cygwin"
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

   IF s_cARCH $ "win|wce|dos|os2" .AND. !( s_cCOMP $ "mingw|mingw64|mingwarm|cygwin" )
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

STATIC FUNCTION FN_NameExtGet( cFileName )
   LOCAL cName, cExt

   hb_FNameSplit( cFileName,, @cName, @cExt )

   RETURN hb_FNameMerge( NIL, cName, cExt )

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

STATIC FUNCTION FN_Expand( cFileName )
   LOCAL aFileList
   LOCAL aFile
   LOCAL aDir
   LOCAL cExt

   IF ! FN_HasWildcard( cFileName )
      RETURN { cFileName }
   ENDIF

   aFileList := {}

   cExt := FN_ExtGet( cFileName )
   aDir := Directory( cFileName )
   FOR EACH aFile IN aDir
      IF FN_ExtGet( aFile[ F_NAME ] ) == cExt /* Workaround to not find 'hello.prga' when looking for '*.prg' */
         AAdd( aFilelist, hb_FNameMerge( FN_DirGet( cFileName ), aFile[ F_NAME ] ) )
      ENDIF
   NEXT

   RETURN aFileList

STATIC FUNCTION FN_HasWildcard( cFileName )
   RETURN "?" $ cFileName .OR. ;
          "*" $ cFileName

#define HBMK_CFG_NAME  "hbmk.cfg"

STATIC PROCEDURE HBP_ProcessAll( lConfigOnly,;
                                 /* @ */ aLIBUSER,;
                                 /* @ */ aLIBUSERGT,;
                                 /* @ */ aLIBPATH,;
                                 /* @ */ aLIBDYNHAS,;
                                 /* @ */ aINCPATH,;
                                 /* @ */ aINCTRYPATH,;
                                 /* @ */ aOPTPRG,;
                                 /* @ */ aOPTC,;
                                 /* @ */ aOPTRES,;
                                 /* @ */ aOPTL,;
                                 /* @ */ lGUI,;
                                 /* @ */ lMT,;
                                 /* @ */ lSHARED,;
                                 /* @ */ lSTATICFULL,;
                                 /* @ */ lDEBUG,;
                                 /* @ */ lOPT,;
                                 /* @ */ lNULRDD,;
                                 /* @ */ lMAP,;
                                 /* @ */ lSTRIP,;
                                 /* @ */ nCOMPR,;
                                 /* @ */ nHEAD,;
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
         IF ! s_lQuiet
            OutStd( "hbmk: Processing configuration: " + cFileName + hb_osNewLine() )
         ENDIF
         HBP_ProcessOne( cFileName,;
            @aLIBUSER,;
            @aLIBUSERGT,;
            @aLIBPATH,;
            @aLIBDYNHAS,;
            @aINCPATH,;
            @aINCTRYPATH,;
            @aOPTPRG,;
            @aOPTC,;
            @aOPTRES,;
            @aOPTL,;
            @lGUI,;
            @lMT,;
            @lSHARED,;
            @lSTATICFULL,;
            @lDEBUG,;
            @lOPT,;
            @lNULRDD,;
            @lMAP,;
            @lSTRIP,;
            @nCOMPR,;
            @nHEAD,;
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
            IF ! s_lQuiet
               OutStd( "hbmk: Processing: " + cFileName + hb_osNewLine() )
            ENDIF
            HBP_ProcessOne( cFileName,;
               @aLIBUSER,;
               @aLIBUSERGT,;
               @aLIBPATH,;
               @aLIBDYNHAS,;
               @aINCPATH,;
               @aINCTRYPATH,;
               @aOPTPRG,;
               @aOPTC,;
               @aOPTRES,;
               @aOPTL,;
               @lGUI,;
               @lMT,;
               @lSHARED,;
               @lSTATICFULL,;
               @lDEBUG,;
               @lOPT,;
               @lNULRDD,;
               @lMAP,;
               @lSTRIP,;
               @nCOMPR,;
               @nHEAD,;
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
                                 /* @ */ aINCPATH,;
                                 /* @ */ aINCTRYPATH,;
                                 /* @ */ aOPTPRG,;
                                 /* @ */ aOPTC,;
                                 /* @ */ aOPTRES,;
                                 /* @ */ aOPTL,;
                                 /* @ */ lGUI,;
                                 /* @ */ lMT,;
                                 /* @ */ lSHARED,;
                                 /* @ */ lSTATICFULL,;
                                 /* @ */ lDEBUG,;
                                 /* @ */ lOPT,;
                                 /* @ */ lNULRDD,;
                                 /* @ */ lMAP,;
                                 /* @ */ lSTRIP,;
                                 /* @ */ nCOMPR,;
                                 /* @ */ nHEAD,;
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
      CASE Lower( Left( cLine, Len( "libs="         ) ) ) == "libs="         ; cLine := SubStr( cLine, Len( "libs="         ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            cItem := PathSepToTarget( StrStripQuote( cItem ) )
            IF AScan( aLIBUSER, {|tmp| tmp == cItem } ) == 0
               AAddNotEmpty( aLIBUSER, cItem )
            ENDIF
         NEXT

      CASE Lower( Left( cLine, Len( "libpaths="     ) ) ) == "libpaths="     ; cLine := SubStr( cLine, Len( "libpaths="     ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            cItem := PathSepToTarget( MacroProc( StrStripQuote( cItem ), FN_DirGet( cFileName ) ) )
            IF AScan( aLIBPATH, {|tmp| tmp == cItem } ) == 0
               AAddNotEmpty( aLIBPATH, cItem )
            ENDIF
         NEXT

      CASE Lower( Left( cLine, Len( "incpaths="     ) ) ) == "incpaths="     ; cLine := SubStr( cLine, Len( "incpaths="     ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            cItem := PathSepToTarget( MacroProc( StrStripQuote( cItem ), FN_DirGet( cFileName ) ) )
            IF AScan( aINCPATH, {|tmp| tmp == cItem } ) == 0
               AAddNotEmpty( aINCPATH, cItem )
            ENDIF
         NEXT

      CASE Lower( Left( cLine, Len( "inctrypaths="  ) ) ) == "inctrypaths="  ; cLine := SubStr( cLine, Len( "inctrypaths="  ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            cItem := PathSepToTarget( MacroProc( StrStripQuote( cItem ), FN_DirGet( cFileName ) ) )
            IF AScan( aINCTRYPATH, {|tmp| tmp == cItem } ) == 0
               AAddNotEmpty( aINCTRYPATH, cItem )
            ENDIF
         NEXT

      /* NOTE: This keyword is used in hbmk.cfg and signals whether
               a given optional module (gtsln, gtcrs, gtxwc) is part of the
               Harbour shared library, so that we can automatically add
               the required libs here. [vszakats] */
      CASE Lower( Left( cLine, Len( "libdynhas="    ) ) ) == "libdynhas="    ; cLine := SubStr( cLine, Len( "libdynhas="    ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            cItem := PathSepToTarget( StrStripQuote( cItem ) )
            IF ! Empty( cItem )
               IF AScan( aLIBDYNHAS, {|tmp| tmp == cItem } ) == 0
                  AAdd( aLIBDYNHAS, cItem )
               ENDIF
               IF Lower( Left( cItem, 2 ) ) == "gt" .AND. ;
                  AScan( s_aLIBCOREGT, {|tmp| Lower( tmp ) == Lower( cItem ) } ) == 0
                  AAdd( s_aLIBCOREGT, cItem )
               ENDIF
            ENDIF
         NEXT

      CASE Lower( Left( cLine, Len( "echo="         ) ) ) == "echo="         ; cLine := SubStr( cLine, Len( "echo="         ) + 1 )
         cLine := MacroProc( cLine, FN_DirGet( cFileName ) )
         IF ! Empty( cLine )
            OutStd( cLine + hb_osNewLine() )
         ENDIF

      CASE Lower( Left( cLine, Len( "prgflags="     ) ) ) == "prgflags="     ; cLine := SubStr( cLine, Len( "prgflags="     ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            cItem := PathSepToTarget( MacroProc( StrStripQuote( cItem ), FN_DirGet( cFileName ) ) )
            IF AScan( aOPTPRG, {|tmp| tmp == cItem } ) == 0
               AAddNotEmpty( aOPTPRG, cItem )
            ENDIF
         NEXT

      CASE Lower( Left( cLine, Len( "cflags="       ) ) ) == "cflags="       ; cLine := SubStr( cLine, Len( "cflags="       ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            cItem := MacroProc( StrStripQuote( cItem ), FN_DirGet( cFileName ) )
            IF AScan( aOPTC, {|tmp| tmp == cItem } ) == 0
               AAddNotEmpty( aOPTC, cItem )
            ENDIF
         NEXT

      CASE Lower( Left( cLine, Len( "resflags="     ) ) ) == "resflags="     ; cLine := SubStr( cLine, Len( "resflags="     ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            cItem := MacroProc( StrStripQuote( cItem ), FN_DirGet( cFileName ) )
            IF AScan( aOPTRES, {|tmp| tmp == cItem } ) == 0
               AAddNotEmpty( aOPTRES, cItem )
            ENDIF
         NEXT

      CASE Lower( Left( cLine, Len( "ldflags="      ) ) ) == "ldflags="      ; cLine := SubStr( cLine, Len( "ldflags="      ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            cItem := MacroProc( StrStripQuote( cItem ), FN_DirGet( cFileName ) )
            IF AScan( aOPTL, {|tmp| tmp == cItem } ) == 0
               AAddNotEmpty( aOPTL, cItem )
            ENDIF
         NEXT

      CASE Lower( Left( cLine, Len( "gui="          ) ) ) == "gui="          ; cLine := SubStr( cLine, Len( "gui="          ) + 1 )
         DO CASE
         CASE ValueIsT( cLine ) ; lGUI := .T.
         CASE ValueIsF( cLine ) ; lGUI := .F.
         ENDCASE

      CASE Lower( Left( cLine, Len( "mt="           ) ) ) == "mt="           ; cLine := SubStr( cLine, Len( "mt="           ) + 1 )
         DO CASE
         CASE Lower( cLine ) == "mt" ; lMT := .T. /* Compatibility */
         CASE ValueIsT( cLine ) ; lMT := .T.
         CASE ValueIsF( cLine ) ; lMT := .F.
         ENDCASE

      CASE Lower( Left( cLine, Len( "shareddef="    ) ) ) == "shareddef="    ; cLine := SubStr( cLine, Len( "shareddef="    ) + 1 )
         IF lSHARED == NIL
            DO CASE
            CASE ValueIsT( cLine ) ; lSHARED := .T. ; lSTATICFULL := .F.
            CASE ValueIsF( cLine ) ; lSHARED := .F. ; lSTATICFULL := .F.
            ENDCASE
         ENDIF
      CASE Lower( Left( cLine, Len( "shared="       ) ) ) == "shared="       ; cLine := SubStr( cLine, Len( "shared="       ) + 1 )
         DO CASE
         CASE ValueIsT( cLine ) ; lSHARED := .T. ; lSTATICFULL := .F.
         CASE ValueIsF( cLine ) ; lSHARED := .F. ; lSTATICFULL := .F.
         ENDCASE

      CASE Lower( Left( cLine, Len( "fullstatic="   ) ) ) == "fullstatic="   ; cLine := SubStr( cLine, Len( "fullstatic="   ) + 1 )
         DO CASE
         CASE ValueIsT( cLine ) ; lSHARED := .F. ; lSTATICFULL := .T.
         CASE ValueIsF( cLine ) ; lSHARED := .F. ; lSTATICFULL := .F.
         ENDCASE

      CASE Lower( Left( cLine, Len( "debug="        ) ) ) == "debug="        ; cLine := SubStr( cLine, Len( "debug="        ) + 1 )
         DO CASE
         CASE ValueIsT( cLine ) ; lDEBUG := .T.
         CASE ValueIsF( cLine ) ; lDEBUG := .F.
         ENDCASE

      CASE Lower( Left( cLine, Len( "opt="          ) ) ) == "opt="          ; cLine := SubStr( cLine, Len( "opt="          ) + 1 )
         DO CASE
         CASE ValueIsT( cLine ) ; lOPT := .T.
         CASE ValueIsF( cLine ) ; lOPT := .F.
         ENDCASE

      CASE Lower( Left( cLine, Len( "nulrdd="       ) ) ) == "nulrdd="       ; cLine := SubStr( cLine, Len( "nulrdd="       ) + 1 )
         DO CASE
         CASE ValueIsT( cLine ) ; lNULRDD := .T.
         CASE ValueIsF( cLine ) ; lNULRDD := .F.
         ENDCASE

      CASE Lower( Left( cLine, Len( "map="          ) ) ) == "map="          ; cLine := SubStr( cLine, Len( "map="          ) + 1 )
         DO CASE
         CASE ValueIsT( cLine ) ; lMAP := .T.
         CASE ValueIsF( cLine ) ; lMAP := .F.
         ENDCASE

      CASE Lower( Left( cLine, Len( "strip="        ) ) ) == "strip="        ; cLine := SubStr( cLine, Len( "strip="        ) + 1 )
         DO CASE
         CASE ValueIsT( cLine ) ; lSTRIP := .T.
         CASE ValueIsF( cLine ) ; lSTRIP := .F.
         ENDCASE

      CASE Lower( Left( cLine, Len( "compr="        ) ) ) == "compr="        ; cLine := SubStr( cLine, Len( "compr="        ) + 1 )
         DO CASE
         CASE ValueIsT( cLine )       ; nCOMPR := _COMPR_DEF
         CASE ValueIsF( cLine )       ; nCOMPR := _COMPR_OFF
         CASE Lower( cLine ) == "def" ; nCOMPR := _COMPR_DEF
         CASE Lower( cLine ) == "min" ; nCOMPR := _COMPR_MIN
         CASE Lower( cLine ) == "max" ; nCOMPR := _COMPR_MAX
         ENDCASE

      CASE Lower( Left( cLine, Len( "head="         ) ) ) == "head="         ; cLine := SubStr( cLine, Len( "head="         ) + 1 )
         DO CASE
         CASE Lower( cLine ) == "off"     ; nHEAD := _HEAD_OFF
         CASE Lower( cLine ) == "full"    ; nHEAD := _HEAD_FULL
         CASE Lower( cLine ) == "partial" ; nHEAD := _HEAD_PARTIAL
         ENDCASE

      CASE Lower( Left( cLine, Len( "run="          ) ) ) == "run="          ; cLine := SubStr( cLine, Len( "run="          ) + 1 )
         DO CASE
         CASE ValueIsT( cLine ) ; lRUN := .T.
         CASE ValueIsF( cLine ) ; lRUN := .F.
         ENDCASE

      CASE Lower( Left( cLine, Len( "inc="          ) ) ) == "inc="          ; cLine := SubStr( cLine, Len( "inc="          ) + 1 )
         DO CASE
         CASE ValueIsT( cLine ) ; lINC := .T.
         CASE ValueIsF( cLine ) ; lINC := .F.
         ENDCASE

      /* NOTE: This keyword is used to signal the default GT used when
               building Harbour. It only needs to be filled if this default
               GT is different from the Harbour default one, IOW when it
               was overridden by user at Harbour build time. [vszakats] */
      CASE Lower( Left( cLine, Len( "gtdef="        ) ) ) == "gtdef="        ; cLine := SubStr( cLine, Len( "gtdef="        ) + 1 )
         IF ! Empty( cLine )
            IF ! SetupForGT( cLine, @s_cGTDEFAULT, @lGUI )
               cLine := NIL
            ENDIF
            IF ! Empty( cLine ) .AND. !( Lower( cLine ) == "gtnul" )
               IF AScan( s_aLIBCOREGT, {|tmp| Lower( tmp ) == Lower( cLine ) } ) == 0 .AND. ;
                  AScan( aLIBUSERGT  , {|tmp| Lower( tmp ) == Lower( cLine ) } ) == 0
                  AAddNotEmpty( aLIBUSERGT, PathSepToTarget( cLine ) )
               ENDIF
            ENDIF
         ENDIF

      CASE Lower( Left( cLine, Len( "gt="           ) ) ) == "gt="           ; cLine := SubStr( cLine, Len( "gt="           ) + 1 )
         IF ! Empty( cLine )
            IF cGT == NIL
               IF ! SetupForGT( cLine, @cGT, @lGUI )
                  cLine := NIL
               ENDIF
            ENDIF
            IF ! Empty( cLine ) .AND. !( Lower( cLine ) == "gtnul" )
               IF AScan( s_aLIBCOREGT, {|tmp| Lower( tmp ) == Lower( cLine ) } ) == 0 .AND. ;
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
      RETURN AScan( s_aLIBCOREGT, {|tmp| Lower( tmp ) == cWhichGT } ) > 0
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
                    "hbmk_KEYW( Lower( '%1' ) ) )"

   IF ( nStart := At( "{", cItem ) ) > 0 .AND. ;
      !( SubStr( cItem, nStart - 1, 1 ) == "$" ) .AND. ;
      ( nEnd := hb_At( "}", cItem, nStart ) ) > 0

      /* Separate filter from the rest of the item */
      cFilterSrc := SubStr( cItem, nStart + 1, nEnd - nStart - 1 )
      cItem := Left( cItem, nStart - 1 ) + SubStr( cItem, nEnd + 1 )

      IF ! Empty( cFilterSrc )

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
   ENDIF

   RETURN cItem

STATIC FUNCTION MacroProc( cString, cDirParent )
   LOCAL nStart
   LOCAL nEnd
   LOCAL cMacro

   DO WHILE ( nStart := At( "${", cString ) ) > 0 .AND. ;
            ( nEnd := hb_At( "}", cString, nStart ) ) > 0

      cMacro := Upper( SubStr( cString, nStart + 2, nEnd - nStart - 2 ) )

      DO CASE
      CASE cMacro == "HB_ROOT"
         cMacro := PathSepToSelf( DirAddPathSep( hb_DirBase() ) )
      CASE cMacro == "HB_SELF"
         IF Empty( cDirParent )
            cMacro := ""
         ELSE
            cMacro := PathSepToSelf( cDirParent )
         ENDIF
      CASE cMacro == "HB_ARCH" .OR. cMacro == "HB_ARCHITECTURE"
         cMacro := s_cARCH
      CASE cMacro == "HB_COMP" .OR. cMacro == "HB_COMPILER"
         cMacro := s_cCOMP
      CASE cMacro == "HB_CPU"
         cMacro := hbmk_CPU()
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
   IF s_cCOMP $ "gcc|gpp|mingw|mingw64|mingwarm|cygwin"
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
      ELSEIF ! Empty( cExecNM := FindInPath( s_cCCPREFIX + "nm" ) )
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

   IF !( s_cARCH == hb_Version( HB_VERSION_BUILD_ARCH ) ) .OR. ;
      s_cARCH == "wce"

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
      CASE s_cARCH == "wce"
         AAdd( aOPTPRG, "-D__PLATFORM__WINDOWS" )
         AAdd( aOPTPRG, "-D__PLATFORM__WINCE" )
      CASE s_cARCH == "win"
         AAdd( aOPTPRG, "-D__PLATFORM__WINDOWS" )
      CASE s_cARCH == "dos"
         AAdd( aOPTPRG, "-D__PLATFORM__DOS" )
      CASE s_cARCH == "os2"
         AAdd( aOPTPRG, "-D__PLATFORM__OS2" )
      CASE s_cARCH == "linux"
         AAdd( aOPTPRG, "-D__PLATFORM__LINUX" )
         AAdd( aOPTPRG, "-D__PLATFORM__UNIX" )
      CASE s_cARCH == "darwin"
         AAdd( aOPTPRG, "-D__PLATFORM__DARWIN" )
         AAdd( aOPTPRG, "-D__PLATFORM__UNIX" )
      CASE s_cARCH == "bsd"
         AAdd( aOPTPRG, "-D__PLATFORM__BDS" )
         AAdd( aOPTPRG, "-D__PLATFORM__UNIX" )
      CASE s_cARCH == "sunos"
         AAdd( aOPTPRG, "-D__PLATFORM__SUNOS" )
         AAdd( aOPTPRG, "-D__PLATFORM__UNIX" )
      CASE s_cARCH == "hpux"
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
   RETURN s_cARCH

/* Keep this public, it's used from macro. */
FUNCTION hbmk_COMP()
   RETURN s_cCOMP

FUNCTION hbmk_CPU()

   DO CASE
   CASE s_cCOMP $ "gcc|gpp|cygwin|owatcom|bcc|icc|xcc" .OR. ;
        s_cCOMP == "pocc" .OR. ;
        s_cCOMP == "mingw" .OR. ;
        s_cCOMP == "msvc"
      RETURN "x86"
   CASE s_cCOMP == "mingw64" .OR. ;
        s_cCOMP == "msvc64" .OR. ;
        s_cCOMP == "pocc64"
      RETURN "x86_64"
   CASE s_cCOMP == "iccia64" .OR. ;
        s_cCOMP == "msvcia64"
      RETURN "ia64"
   CASE s_cCOMP == "mingwarm" .OR. ;
        s_cCOMP == "msvcarm" .OR. ;
        s_cCOMP == "poccarm"
      RETURN "arm"
   ENDCASE

   RETURN ""

FUNCTION hbmk_KEYW( cKeyword )

   IF cKeyword == iif( s_lMT   , "mt"   , "st"      ) .OR. ;
      cKeyword == iif( s_lGUI  , "gui"  , "std"     ) .OR. ;
      cKeyword == iif( s_lDEBUG, "debug", "nodebug" )
      RETURN .T.
   ENDIF

   IF ( cKeyword == "unix"     .AND. ( s_cARCH $ "bsd|hpux|sunos|linux" .OR. s_cARCH == "darwin" )   ) .OR. ;
      ( cKeyword == "allwin"   .AND. s_cARCH $ "win|wce"                                             ) .OR. ;
      ( cKeyword == "allgcc"   .AND. s_cCOMP $ "gcc|gpp|mingw|mingw64|mingwarm|cygwin|djgpp"         ) .OR. ;
      ( cKeyword == "allmingw" .AND. s_cCOMP $ "mingw|mingw64|mingwarm"                              ) .OR. ;
      ( cKeyword == "allmsvc"  .AND. s_cCOMP $ "msvc|msvc64|msvcarm"                                 ) .OR. ;
      ( cKeyword == "allpocc"  .AND. s_cCOMP $ "pocc|pocc64|poccarm"                                 ) .OR. ;
      ( cKeyword == "allicc"   .AND. s_cCOMP $ "icc|iccia64"                                         )
      RETURN .T.
   ENDIF

   IF cKeyword == hbmk_CPU()
      RETURN .T.
   ENDIF

   RETURN .F.

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
      "  -o<outname>        output file name" ,;
      "  -l<libname>        link with <libname> library" ,;
      "  -L<libpath>        additional path to search for libraries" ,;
      "  -i<p>|-incpath=<p> additional path to search for headers" ,;
      "  -static|-shared    link with static/shared libs" ,;
      "  -mt|-st            link with multi/single-thread VM" ,;
      "  -gt<name>          link with GT<name> GT driver, can be repeated to link" ,;
      "                     with more GTs. First one will be the default at runtime" }

   LOCAL aText_Help := {;
      "  -help|--help       long help" }

   LOCAL aText_Long := {;
      "" ,;
      "  -gui|-std          create GUI/console executable" ,;
      "  -main=<mainfunc>   override the name of starting function/procedure" ,;
      "  -fullstatic        link with all static libs" ,;
      "  -nulrdd[-]         link with nulrdd" ,;
      "  -[no]debug         add/exclude C compiler debug info" ,;
      "  -[no]opt           toggle C compiler optimizations (default: on)" ,;
      "  -[no]map           create (or not) a map file" ,;
      "  -[no]strip         strip (no strip) binaries" ,;
      "  -[no]trace         show commands executed" ,;
      "  -traceonly         show commands to be executed, but don't execute them" ,;
      "  -[no]compr[=lev]   compress executable/dynamic lib (needs UPX)" ,;
      "                     <lev> can be: min, max, def" ,;
      "  -[no]run           run/don't run output executable" ,;
      "  -nohbp             do not process .hbp files in current directory" ,;
      "  -stop              stop without doing anything" ,;
      "" ,;
      "  -bldf[-]           inherit all/no (default) flags from Harbour build" ,;
      "  -bldf=[p][c][l]    inherit .prg/.c/linker flags (or none) from Harbour build" ,;
      "  -inctrypath=<p>    additional path to autodetect .c header locations" ,;
      "  -prgflag=<f>       pass flag to Harbour" ,;
      "  -cflag=<f>         pass flag to C compiler" ,;
      "  -resflag=<f>       pass flag to resource compiler (Windows only)" ,;
      "  -ldflag=<f>        pass flag to linker (executable)" ,;
      "  -aflag=<f>         pass flag to linker (static library)" ,;
      "  -dflag=<f>         pass flag to linker (dynamic library)" ,;
      "  -runflag=<f>       pass flag to output executable when -run option is used" ,;
      "  -jobs=<n>          start n compilation threads (MT builds only)" ,;
      "  -inc               enable incremental build mode" ,;
      "  -[no]head[=<m>]    control source header parsing (in incremental build mode)" ,;
      "                     <m> can be: full, partial (default), off" ,;
      "  -rebuild           rebuild all (in incremental build mode)" ,;
      "  -clean             clean (in incremental build mode)" ,;
      "  -workdir=<dir>     working directory for incremental build mode" ,;
      "                     (default: arch/comp)" ,;
      "  -hbcmp|-clipper    stop after creating the object files" ,;
      "                     create link/copy hbmk to hbcmp/clipper for the same effect" ,;
      "  -hbcc              stop after creating the object files and accept raw C flags" ,;
      "                     create link/copy hbmk to hbcc for the same effect" ,;
      "  -hblnk             accept raw linker flags" ,;
      "  -hblib             create static library" ,;
      "  -hbdyn             create dynamic library" ,;
      "  -rtlink            " ,;
      "  -blinker           " ,;
      "  -exospace          emulate Clipper compatible linker behavior" ,;
      "                     create link/copy hbmk to rtlink/blinker/exospace for the" ,;
      "                     same effect" ,;
      "  --hbdirbin         output Harbour binary directory" ,;
      "  --hbdirdyn         output Harbour dynamic library directory" ,;
      "  --hbdirlib         output Harbour static library directory" ,;
      "  --hbdirinc         output Harbour header directory" ,;
      "" ,;
      "  -arch=<arch>       assume specific architecure. Same as HB_ARCHITECTURE envvar" ,;
      "  -comp=<comp>       use specific compiler. Same as HB_COMPILER envvar" ,;
      "                     Special value:" ,;
      "                      - bld: use original build settings (default on *nix)" ,;
      "  --version          display version header only" ,;
      "  -info              turn on informational messages" ,;
      "  -quiet             suppress all screen messages" ,;
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
      "    cflags=[C compiler flags], resflags=[resource compiler flags]" ,;
      "    ldflags=[Linker flags], libpaths=[paths]," ,;
      "    incpaths=[paths], inctrypaths=[paths]" ,;
      "    gui|mt|shared|nulrdd|debug|opt|map|strip|run|inc=[yes|no]" ,;
      "    compr=[yes|no|def|min|max], head=[off|partial|full], echo=<text>" ,;
      "    Lines starting with '#' char are ignored" ,;
      "  - Platform filters are accepted in each .hbp line and with several options." ,;
      "    Filter format: {[!][<arch>|<comp>|<keyword>]}. Filters can be combined " ,;
      "    using '&', '|' operators and grouped by parantheses." ,;
      "    Ex.: {win}, {gcc}, {linux|darwin}, {win&!pocc}, {(win|linux)&!owatcom}," ,;
      "         {unix&mt&gui}, -cflag={win}-DMYDEF, -stop{dos}, -stop{!allwin}," ,;
      "         {allpocc}, {allgcc}, {allmingw}, {allmsvc}, {x86|x86_64|ia64|arm}" ,;
      "  - Certain .hbp lines (prgflags=, cflags=, ldflags=, libpaths=, inctrypaths=," ,;
      "    echo=) and corresponding command line parameters will accept macros:" ,;
      "    ${hb_root}, ${hb_self}, ${hb_arch}, ${hb_comp}, ${hb_cpu}, ${<envvar>}" ,;
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
