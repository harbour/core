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
 *
 * See doc/license.txt for licensing terms.
 *
 */

/*
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

/* TOFIX: We should try using the same .dll name format for all compilers/platform.
          This also means they should be bniary compatible with each other
          (within a given architecture of course). */

ANNOUNCE HB_GTSYS
REQUEST HB_GT_CGI_DEFAULT

REQUEST hbmk_ARCH
REQUEST hbmk_COMP

THREAD STATIC t_lQuiet := .F.
THREAD STATIC t_lInfo := .F.
THREAD STATIC t_cARCH
THREAD STATIC t_cCOMP
THREAD STATIC t_aLIBCOREGT
THREAD STATIC t_cGTDEFAULT

THREAD STATIC t_cCCPATH
THREAD STATIC t_cCCPREFIX

#define _PAR_cParam         1
#define _PAR_cFileName      2
#define _PAR_nLine          3

FUNCTION Main( ... )

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
   LOCAL s_cHB_INC_INSTALL

   LOCAL s_aPRG
   LOCAL s_aC
   LOCAL s_aRESSRC
   LOCAL s_aRESCMP
   LOCAL s_aLIBSHARED
   LOCAL s_aLIB
   LOCAL s_aLIBVM
   LOCAL s_aLIBUSER
   LOCAL s_aLIBUSERGT
   LOCAL s_aLIBHB
   LOCAL s_aLIBHBGT
   LOCAL s_aLIB3RD
   LOCAL s_aLIBSYS
   LOCAL s_aLIBPATH
   LOCAL s_aLIBDYNHAS
   LOCAL s_aOPTPRG
   LOCAL s_aOPTC
   LOCAL s_aOPTL
   LOCAL s_aOPTA
   LOCAL s_aOPTD
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
   LOCAL s_lMT := .F.
   LOCAL s_lSHARED := NIL
   LOCAL s_lSTATICFULL := NIL
   LOCAL s_lDEBUG := .F.
   LOCAL s_lNULRDD := .F.
   LOCAL s_lMAP := .F.
   LOCAL s_lSTRIP := .F.
   LOCAL s_lTRACE := .F.
   LOCAL s_lBLDFLGP := .F.
   LOCAL s_lBLDFLGC := .F.
   LOCAL s_lBLDFLGL := .F.
   LOCAL s_lRUN := .F.
   LOCAL s_lFMSTAT := NIL /* NIL = default, .T. = on, .F. = off */

   LOCAL aCOMPDET
   LOCAL aCOMPSUP

   LOCAL cLibPrefix
   LOCAL cLibExt
   LOCAL cObjPrefix
   LOCAL cObjExt
   LOCAL cLibLibExt
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
   LOCAL nErrorLevel
   LOCAL tmp, array
   LOCAL cScriptFile
   LOCAL fhnd
   LOCAL lNOHBP
   LOCAL lSysLoc
   LOCAL cPrefix
   LOCAL cPostfix
   LOCAL nEmbedLevel

   LOCAL lStopAfterHarbour := .F.
   LOCAL lStopAfterCComp := .F.
   LOCAL lAcceptCFlag := .F.
   LOCAL lAcceptLDFlag := .F.
   LOCAL lCreateLib := .F.
   LOCAL lCreateDyn := .F.

   LOCAL aParams
   LOCAL aParam
   LOCAL cParam
   LOCAL cParamL

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
      RETURN 9
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
         RETURN 9

      CASE cParamL == "--version"

         ShowHeader()
         PauseForKey()
         RETURN 0

      ENDCASE
   NEXT

   /* Emulate -hbcmp, -hbcc, -hblnk switches when certain
      self names are detected.
      For compatibility with hbmk script aliases. */

   tmp := Lower( FN_NameGet( hb_argv( 0 ) ) )
   DO CASE
   CASE Right( tmp, 5 ) == "hbcmp" .OR. ;
        Left(  tmp, 5 ) == "hbcmp"
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
         aCOMPSUP := { "gcc", "gpp", "owatcom", "icc", "mingw", "mingwce" }
      ELSE
         aCOMPSUP := { "gcc" }
      ENDIF
      cBin_CompPRG := "harbour"
      s_aLIBHBGT := { "gttrm" }
      t_cGTDEFAULT := "gttrm"
      cDynLibNamePrefix := "lib"
      cBinExt := NIL
      cOptPrefix := "-"
      SWITCH t_cARCH
      CASE "darwin" ; cDynLibExt := ".dylib" ; EXIT
      CASE "hpux"   ; cDynLibExt := ".sl" ; EXIT
      OTHERWISE     ; cDynLibExt := ".so"
      ENDSWITCH
   CASE t_cARCH == "dos"
      aCOMPDET := { { {|| FindInPath( "gcc"      ) != NIL }, "djgpp"   },;
                    { {|| FindInPath( "wpp386"   ) != NIL }, "owatcom" } } /* TODO: Add full support for wcc386 */
      aCOMPSUP := { "djgpp", "gcc", "owatcom" }
      cBin_CompPRG := "harbour.exe"
      s_aLIBHBGT := { "gtdos" }
      t_cGTDEFAULT := "gtdos"
      cDynLibNamePrefix := ""
      cDynLibExt := ""
      cBinExt := ".exe"
      cOptPrefix := "-/"
   CASE t_cARCH == "os2"
      aCOMPDET := { { {|| FindInPath( "gcc"      ) != NIL }, "gcc"     },;
                    { {|| FindInPath( "wpp386"   ) != NIL }, "owatcom" } } /* TODO: Add full support for wcc386 */
      aCOMPSUP := { "gcc", "owatcom" }
      cBin_CompPRG := "harbour.exe"
      s_aLIBHBGT := { "gtos2" }
      t_cGTDEFAULT := "gtos2"
      cDynLibNamePrefix := ""
      cDynLibExt := ".dll"
      cBinExt := ".exe"
      cOptPrefix := "-/"
   CASE t_cARCH == "win"
      /* Order is significant.
         owatcom also keeps a cl.exe in it's binary dir. */
      aCOMPDET := { { {|| FindInPath( t_cCCPREFIX + "gcc" ) != NIL }, "mingw"   },; /* TODO: Add full support for g++ */
                    { {|| FindInPath( "wpp386"   ) != NIL .AND. ;
                          ! Empty( GetEnv( "WATCOM" ) ) }, "owatcom" },; /* TODO: Add full support for wcc386 */
                    { {|| FindInPath( "ml64"     ) != NIL }, "msvc64"  },;
                    { {|| FindInPath( "cl"       ) != NIL .AND. ;
                          FindInPath( "wpp386"   ) == NIL }, "msvc"    },;
                    { {|| FindInPath( "bcc32"    ) != NIL }, "bcc"     },;
                    { {|| FindInPath( "porc64"   ) != NIL }, "pocc64"  },;
                    { {|| FindInPath( "pocc"     ) != NIL }, "pocc"    },;
                    { {|| FindInPath( "icl"      ) != NIL }, "icc"     },;
                    { {|| FindInPath( "cygstart" ) != NIL }, "cygwin"  },;
                    { {|| FindInPath( "xcc"      ) != NIL }, "xcc"     } }
      aCOMPSUP := { "mingw", "msvc", "bcc", "owatcom", "icc", "pocc", "xcc", "cygwin",;
                    "msvc64", "msvcia64", "pocc64",;
                    "mingwce", "msvcce", "poccce" }
      cBin_CompPRG := "harbour.exe"
      s_aLIBHBGT := { "gtwin", "gtwvt", "gtgui" }
      t_cGTDEFAULT := "gtwin"
      cDynLibNamePrefix := ""
      cDynLibExt := ".dll"
      cBinExt := ".exe"
      cOptPrefix := "-/"
   OTHERWISE
      OutErr( "hbmk: Error: Architecture value unknown: " + t_cARCH + hb_osNewLine() )
      PauseForKey()
      RETURN 1
   ENDCASE

   t_aLIBCOREGT := ArrayJoin( aLIB_BASE_GT, s_aLIBHBGT )

   /* Setup GUI state for Harbour default */
   SetupForGT( t_cGTDEFAULT, NIL, @s_lGUI )

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
         ELSEIF ! Empty( aCOMPDET )
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
            RETURN 2
         ENDIF
      ELSE
         IF AScan( aCOMPSUP, {|tmp| tmp == t_cCOMP } ) == 0
            OutErr( "hbmk: Error: Compiler value unknown: " + t_cCOMP + hb_osNewLine() )
            PauseForKey()
            RETURN 2
         ENDIF
      ENDIF
   ENDIF

   /* Detect system locations to enable shared library option by default */
   lSysLoc := hb_DirBase() == "/usr/local/bin/" .OR. ;
              hb_DirBase() == "/usr/bin/" .OR. ;
              hb_DirBase() == "/opt/harbour/" .OR. ;
              hb_DirBase() == "/opt/bin/"

   /* Autodetect Harbour environment */

   s_aLIBPATH := {}

   s_cHB_BIN_INSTALL := PathSepToTarget( GetEnv( "HB_BIN_INSTALL" ) )
   s_cHB_LIB_INSTALL := PathSepToTarget( GetEnv( "HB_LIB_INSTALL" ) )
   s_cHB_INC_INSTALL := PathSepToTarget( GetEnv( "HB_INC_INSTALL" ) )

   s_cHB_INSTALL_PREFIX := PathSepToTarget( GetEnv( "HB_INSTALL_PREFIX" ) )
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
      PauseForKey()
      RETURN 3
   ENDIF
   IF Empty( s_cHB_BIN_INSTALL )
      s_cHB_BIN_INSTALL := PathNormalize( s_cHB_INSTALL_PREFIX ) + "bin"
   ENDIF
   IF Empty( s_cHB_LIB_INSTALL )
      s_cHB_LIB_INSTALL := PathNormalize( s_cHB_INSTALL_PREFIX ) + "lib"
   ENDIF
   IF Empty( s_cHB_INC_INSTALL )
      s_cHB_INC_INSTALL := PathNormalize( s_cHB_INSTALL_PREFIX ) + "include"
   ENDIF

   IF t_lInfo
      OutStd( "hbmk: Using Harbour: " + s_cHB_BIN_INSTALL + " " + s_cHB_INC_INSTALL + " " + s_cHB_LIB_INSTALL + hb_osNewLine() )
   ENDIF

   /* Add main Harbour library dir to lib path list */
   AAddNotEmpty( s_aLIBPATH, s_cHB_LIB_INSTALL )

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
   s_aOPTL := {}
   s_aOPTA := {}
   s_aOPTD := {}
   s_aRESSRC := {}
   s_aRESCMP := {}
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
         nEmbedLevel := 1
         HBM_Load( aParams, cParam, @nEmbedLevel ) /* Load parameters from script file */
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
                   @s_lMT,;
                   @s_lSHARED,;
                   @s_lSTATICFULL,;
                   @s_lDEBUG,;
                   @s_lNULRDD,;
                   @s_lMAP,;
                   @s_lSTRIP,;
                   @s_lRUN,;
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
      CASE cParamL == "-mt"              ; s_lMT       := .T.
      CASE cParamL == "-st"              ; s_lMT       := .F.
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
      CASE cParamL == "-debug"           ; s_lDEBUG    := .T.
      CASE cParamL == "-debug-" .OR. ;
           cParamL == "-nodebug"         ; s_lDEBUG    := .F.
      CASE cParamL == "-nulrdd"          ; s_lNULRDD   := .T.
      CASE cParamL == "-nulrdd-"         ; s_lNULRDD   := .F.
      CASE cParamL == "-map"             ; s_lMAP      := .T.
      CASE cParamL == "-map-" .OR. ;
           cParamL == "-nomap"           ; s_lMAP      := .F.
      CASE cParamL == "-fmstat"          ; s_lFMSTAT   := .T.
      CASE cParamL == "-fmstat-" .OR. ;
           cParamL == "-nofmstat"        ; s_lFMSTAT   := .F.
      CASE cParamL == "-strip"           ; s_lSTRIP    := .T.
      CASE cParamL == "-strip-" .OR. ;
           cParamL == "-nostrip"         ; s_lSTRIP    := .F.
      CASE cParamL == "-run"             ; s_lRUN      := .T.
      CASE cParamL == "-run-" .OR. ;
           cParamL == "-norun"           ; s_lRUN      := .F.
      CASE cParamL == "-trace"           ; s_lTRACE    := .T.
      CASE cParamL == "-trace-" .OR. ;
           cParamL == "-notrace"         ; s_lTRACE    := .F.

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

      CASE Left( cParam, 2 ) == "-l" .AND. ;
           Len( cParam ) > 2 .AND. ;
           !( Left( cParam, 3 ) == "-l-" )

         cParam := ArchCompFilter( SubStr( cParam, 3 ) )
         IF ! Empty( cParam )
            AAdd( s_aLIBUSER, PathSepToTarget( cParam ) )
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
            @s_lMT,;
            @s_lSHARED,;
            @s_lSTATICFULL,;
            @s_lDEBUG,;
            @s_lNULRDD,;
            @s_lMAP,;
            @s_lSTRIP,;
            @s_lRUN,;
            @s_cGT )

      CASE FN_ExtGet( cParamL ) == ".prg"

         cParam := PathProc( cParam, aParam[ _PAR_cFileName ] )
         AAdd( s_aPRG    , PathSepToTarget( cParam ) )
         DEFAULT s_cFIRST TO PathSepToSelf( cParam )

      CASE FN_ExtGet( cParamL ) == ".rc"

         cParam := PathProc( cParam, aParam[ _PAR_cFileName ] )
         AAdd( s_aRESSRC , PathSepToTarget( cParam ) )

      CASE FN_ExtGet( cParamL ) == ".res"

         cParam := PathProc( cParam, aParam[ _PAR_cFileName ] )
         AAdd( s_aRESCMP , PathSepToTarget( cParam ) )

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
   IF ( Len( s_aPRG ) + Len( s_aC ) + Len( s_aOBJUSER ) + Len( s_aOBJA ) ) == 0
      OutErr( "hbmk: Error: No source files were specified." + hb_osNewLine() )
      PauseForKey()
      RETURN 4
   ENDIF

   /* Harbour compilation */

   IF Len( s_aPRG ) > 0

      PlatformPRGFlags( s_aOPTPRG )

#if defined( HBMK_INTEGRATED_COMPILER )
      aCommand := ArrayAJoin( { { iif( lCreateLib .OR. lCreateDyn, "-n1", "-n2" ) },;
                                s_aPRG,;
                                { "-i" + s_cHB_INC_INSTALL },;
                                ListToArray( cSelfFlagPRG ),;
                                ListToArray( iif( ! Empty( GetEnv( "HB_USER_PRGFLAGS" ) ), " " + GetEnv( "HB_USER_PRGFLAGS" ), "" ) ),;
                                s_aOPTPRG } )

      IF s_lTRACE
         OutStd( "hbmk: Harbour compiler command:" + hb_osNewLine() + ArrayToList( aCommand ) + hb_osNewLine() )
      ENDIF

      IF ( tmp := hb_compile( "", aCommand ) ) != 0
         OutErr( "hbmk: Error: Running Harbour compiler. " + hb_ntos( tmp ) + hb_osNewLine() + ArrayToList( aCommand ) + hb_osNewLine() )
         PauseForKey()
         RETURN 6
      ENDIF
#else
      cCommand := DirAddPathSep( s_cHB_BIN_INSTALL ) +;
                  cBin_CompPRG +;
                  " " + iif( lCreateLib .OR. lCreateDyn, "-n1", "-n2" ) +;
                  " " + ArrayToList( s_aPRG ) +;
                  " -i" + s_cHB_INC_INSTALL +;
                  iif( s_lBLDFLGP, " " + cSelfFlagPRG, "" ) +;
                  iif( ! Empty( GetEnv( "HB_USER_PRGFLAGS" ) ), " " + GetEnv( "HB_USER_PRGFLAGS" ), "" ) +;
                  iif( ! Empty( s_aOPTPRG ), " " + ArrayToList( s_aOPTPRG ), "" )

      cCommand := AllTrim( cCommand )

      IF s_lTRACE
         OutStd( "hbmk: Harbour compiler command:" + hb_osNewLine() + cCommand + hb_osNewLine() )
      ENDIF

      IF ( tmp := hb_run( cCommand ) ) != 0
         OutErr( "hbmk: Error: Running Harbour compiler. " + hb_ntos( tmp ) + ":" + hb_osNewLine() + cCommand + hb_osNewLine() )
         PauseForKey()
         RETURN 6
      ENDIF
#endif
   ENDIF

   IF ! lStopAfterHarbour

      /* If -o with full name wasn't specified, let's
         make it the first source file specified. */
      DEFAULT s_cPROGNAME TO FN_NameGet( s_cFIRST )

      IF t_cCOMP == "mingwce" .OR. ;
         t_cCOMP == "poccce"
         t_cGTDEFAULT := "gtwvt"
      ENDIF

      IF s_cGT == t_cGTDEFAULT
         s_cGT := NIL
      ENDIF

      /* Merge user libs from command line and envvar. Command line has priority. */
      s_aLIBUSER := ArrayAJoin( { s_aLIBUSER, s_aLIBUSERGT, ListToArray( PathSepToTarget( GetEnv( "HB_USER_LIBS" ) ) ) } )

      /* Combine output dir with output name. */
      IF ! Empty( s_cPROGDIR )
         hb_FNameSplit( s_cPROGNAME, @cDir, @cName, @cExt )
         s_cPROGNAME := hb_FNameMerge( iif( Empty( cDir ), s_cPROGDIR, cDir ), cName, cExt )
      ENDIF

      IF lSysLoc
         cPrefix := ""
      ELSE
         cPrefix := PathNormalize( s_cHB_LIB_INSTALL )
      ENDIF
#if 1
      cPostfix := ""
      HB_SYMBOL_UNUSED( cDL_Version )
#else
      cPostfix := "-" + cDL_Version
#endif

      DO CASE
      CASE t_cARCH $ "bsd|linux|hpux|sunos" .OR. t_cARCH == "darwin" /* Separated to avoid match with 'win' */
         s_aLIBSHARED := { iif( s_lMT, cPrefix + cDynLibNamePrefix + "harbourmt" + cPostfix + cDynLibExt,;
                                       cPrefix + cDynLibNamePrefix + "harbour"   + cPostfix + cDynLibExt ) }
      CASE t_cARCH $ "os2|win"
         s_aLIBSHARED := { iif( s_lMT, cDynLibNamePrefix + "harbourmt",;
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
      aLIB_BASE2 := ArrayAJoin( { aLIB_BASE2, t_aLIBCOREGT } )

      IF ! Empty( s_cGT )
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

         cLibPrefix := "-l"
         cLibExt := ""
         cObjExt := ".o"
         cBin_Lib := t_cCCPREFIX + "ar"
         cOpt_Lib := "{FA} cr {OL} {LO}"
         cBin_CompC := t_cCCPREFIX + iif( t_cCOMP == "gpp", "g++", "gcc" )
         IF ! Empty( t_cCCPATH )
            cBin_Lib   := t_cCCPATH + "/" + cBin_Lib
            cBin_CompC := t_cCCPATH + "/" + cBin_CompC
         ENDIF
         cOpt_CompC := "{LC} {LO} {LA} -O3 {FC} -I{DI} {DL}"
         cLibPathPrefix := "-L"
         cLibPathSep := " "
         IF ! lStopAfterCComp
            IF t_cARCH == "linux" .OR. ;
               t_cARCH == "bsd"
               cOpt_CompC += " -Wl,--start-group {LL} -Wl,--end-group"
            ELSE
               cOpt_CompC += " {LL}"
               aLIB_BASE2 := ArrayAJoin( { aLIB_BASE2, { "hbcommon", "hbrtl" }, s_aLIBVM } )
            ENDIF
         ENDIF
         IF s_lMAP
            cOpt_CompC += " -Wl,-Map {OM}"
         ENDIF
         IF s_lSTATICFULL
            cOpt_CompC += " -static"
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
            AAdd( s_aOPTC, "-c" )
            IF ( Len( s_aPRG ) + Len( s_aC ) ) == 1
               IF t_cARCH == "darwin"
                  AAdd( s_aOPTC, "-o {OO}" )
               ELSE
                  AAdd( s_aOPTC, "-o{OO}" )
               ENDIF
            ENDIF
         ELSE
            IF t_cARCH == "darwin"
               AAdd( s_aOPTC, "-o {OE}" )
            ELSE
               AAdd( s_aOPTC, "-o{OE}" )
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
            AAdd( iif( s_lSHARED, s_aLIBSHARED, s_aLIBUSER ), iif( s_lMT, "hbfmmt", "hbfm" ) )
         ENDIF

      CASE ( t_cARCH == "win" .AND. t_cCOMP == "gcc" ) .OR. ;
           ( t_cARCH == "win" .AND. t_cCOMP == "mingw" ) .OR. ;
           ( t_cARCH == "win" .AND. t_cCOMP == "mingwce" ) .OR. ;
           ( t_cARCH == "win" .AND. t_cCOMP == "cygwin" )

         cLibPrefix := "-l"
         cLibExt := ""
         cObjExt := ".o"
         cBin_CompC := t_cCCPREFIX + "gcc.exe"
         cOpt_CompC := "{LC} {LO} {LA} {LR} -O3 {FC} -I{DI} {DL}"
         cLibPathPrefix := "-L"
         cLibPathSep := " "
         cLibLibExt := ".a"
         cBin_Lib := t_cCCPREFIX + "ar.exe"
         cOpt_Lib := "{FA} cr {OL} {LO}"
         cLibObjPrefix := NIL
         IF ! Empty( t_cCCPATH )
            cBin_Lib   := t_cCCPATH + "\" + cBin_Lib
            cBin_CompC := t_cCCPATH + "\" + cBin_CompC
         ENDIF
         IF !( t_cCOMP == "mingwce" )
            IF s_lGUI
               cOpt_CompC += " -mwindows"
            ELSE
               cOpt_CompC += " -mconsole"
            ENDIF
         ENDIF
         IF s_lMAP
            cOpt_CompC += " -Wl,-Map {OM}"
         ENDIF
         IF s_lSHARED
            AAdd( s_aLIBPATH, "{DB}" )
         ENDIF
         IF ! lStopAfterCComp
            IF t_cCOMP == "mingw"
               cOpt_CompC += " -Wl,--start-group {LL} -Wl,--end-group"
            ELSE
               cOpt_CompC += " {LL}"
               aLIB_BASE2 := ArrayAJoin( { aLIB_BASE2, { "hbcommon", "hbrtl" }, s_aLIBVM } )
            ENDIF
         ENDIF
         IF s_lSTRIP
            AAdd( s_aOPTC, "-s" )
         ENDIF
         IF lStopAfterCComp
            AAdd( s_aOPTC, "-c" )
            IF ( Len( s_aPRG ) + Len( s_aC ) ) == 1
               AAdd( s_aOPTC, "-o{OO}" )
            ENDIF
         ELSE
            AAdd( s_aOPTC, "-o{OE}" )
         ENDIF
         IF ! s_lSHARED
            IF t_cCOMP == "mingwce"
               s_aLIBSYS := ArrayJoin( s_aLIBSYS, { "wininet", "ws2", "commdlg", "commctrl", "uuid", "ole32" } )
            ELSE
               s_aLIBSYS := ArrayJoin( s_aLIBSYS, { "user32", "winspool", "gdi32", "comctl32", "comdlg32", "ole32", "oleaut32", "uuid", "wsock32", "ws2_32" } )
            ENDIF
         ENDIF
         s_aLIBSHARED := { iif( s_lMT, "harbourmt", "harbour" ) }

         IF s_lFMSTAT != NIL .AND. s_lFMSTAT
            AAdd( iif( s_lSHARED, s_aLIBSHARED, s_aLIBUSER ), iif( s_lMT, "hbfmmt", "hbfm" ) )
         ENDIF

         IF t_cCOMP == "mingw" .AND. Len( s_aRESSRC ) > 0
            IF Len( s_aRESSRC ) == 1
               cBin_Res := "windres"
               cOpt_Res := "{LR} -o {LS}"
               cResExt := ".o"
            ELSE
               OutErr( "hbmk: Warning: Resource files ignored. Multiple ones not supported with mingw." + hb_osNewLine() )
            ENDIF
         ENDIF

      CASE t_cARCH == "os2" .AND. t_cCOMP == "gcc"

         cLibPrefix := "-l"
         cLibExt := ""
         cObjExt := ".o"
         cBin_CompC := "gcc.exe"
         /* OS/2 needs a space between -o and file name following it */
         cOpt_CompC := "{LC} {LO} -O3 {FC} -I{DI} {DL}"
         cLibPathPrefix := "-L"
         cLibPathSep := " "
         IF s_lMAP
            cOpt_CompC += " -Wl,-Map {OM}"
         ENDIF
         IF s_lSHARED
            AAdd( s_aLIBPATH, "{DB}" )
         ENDIF
         cOpt_CompC += " {LL}"
         aLIB_BASE2 := ArrayAJoin( { aLIB_BASE2, { "hbcommon", "hbrtl" }, s_aLIBVM } )
         IF ! s_lSHARED
            s_aLIBSYS := ArrayJoin( s_aLIBSYS, { "socket" } )
         ENDIF
         IF s_lSTRIP
            AAdd( s_aOPTC, "-s" )
         ENDIF
         IF lStopAfterCComp
            AAdd( s_aOPTC, "-c" )
            IF ( Len( s_aPRG ) + Len( s_aC ) ) == 1
               AAdd( s_aOPTC, "-o {OO}" )
            ENDIF
         ELSE
            AAdd( s_aOPTC, "-o {OE}" )
         ENDIF

      CASE t_cARCH == "dos" .AND. t_cCOMP == "djgpp"

         cLibPrefix := "-l"
         cLibExt := ""
         cObjExt := ".o"
         cBin_CompC := "gcc.exe"
         cOpt_CompC := "{LC} {LO} {LA} -O3 {FC} -I{DI} {DL}{SCRIPT}"
         cLibPathPrefix := "-L"
         cLibPathSep := " "
         IF t_cCOMP == "djgpp"
            cOpt_CompC += " -Wl,--start-group {LL} -Wl,--end-group"
         ELSE
            cOpt_CompC += " {LL}"
            aLIB_BASE2 := ArrayAJoin( { aLIB_BASE2, { "hbcommon", "hbrtl" }, s_aLIBVM } )
         ENDIF
         IF ! s_lSHARED
            s_aLIBSYS := ArrayJoin( s_aLIBSYS, { "m" } )
         ENDIF
         IF s_lSTRIP
            AAdd( s_aOPTC, "-s" )
         ENDIF
         IF lStopAfterCComp
            AAdd( s_aOPTC, "-c" )
            IF ( Len( s_aPRG ) + Len( s_aC ) ) == 1
               AAdd( s_aOPTC, "-o{OO}" )
            ENDIF
         ELSE
            AAdd( s_aOPTC, "-o{OE}" )
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
         cOpt_CompC := "-j -w3 -5s -5r -fp5 -oxehtz -zq -zt0 -bt=DOS {FC} -i{DI} {LC}"
         cBin_Link := "wlink.exe"
         cOpt_Link := "OP osn=DOS OP stack=65536 OP CASEEXACT OP stub=cwstub.exe {FL} NAME {OE} {LO} {DL} {LL}{SCRIPT}"
         cBin_Lib := "wlib.exe"
         cOpt_Lib := "{FA} {OL} {LO}{SCRIPT}"
         cLibLibExt := cLibExt
         cLibObjPrefix := "-+ "
         IF s_lDEBUG
            cOpt_Link := "DEBUG " + cOpt_Link
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
         cOpt_CompC := "-w3 -zq -bt=NT -5s -fp5 -onaehtzr -oi+ -ei -zp8 -s -zt0 {FC} -i{DI} {LC}"
         cBin_Link := "wlink.exe"
         cOpt_Link := "OP osn=NT OP stack=65536 OP CASEEXACT {FL} NAME {OE} {LO} {DL} {LL} {LS}{SCRIPT}"
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
            cOpt_Link := "DEBUG " + cOpt_Link
         ENDIF
         IF s_lMAP
            AAdd( s_aOPTL, "OP MAP" )
         ENDIF
         s_aLIBSYS := ArrayJoin( s_aLIBSYS, { "kernel32", "user32", "wsock32" } )
         s_aLIBSHARED := { iif( s_lMT, "harbourmt-" + cDL_Version_Alter + "-ow",;
                                       "harbour-" + cDL_Version_Alter + "-ow" ),;
                           "hbmainstd",;
                           "hbmainwin" }

         IF Len( s_aRESSRC ) > 0
            IF Len( s_aRESSRC ) == 1
               cBin_Res := "wrc"
               cOpt_Res := "-r -zm {LR} -fo={LS}"
               cResPrefix := "OP res="
               cResExt := ".res"
            ELSE
               OutErr( "hbmk: Warning: Resource files ignored. Multiple ones not supported with owatcom." + hb_osNewLine() )
            ENDIF
         ENDIF

      CASE t_cARCH == "os2" .AND. t_cCOMP == "owatcom"
         cLibPrefix := "LIB "
         cLibExt := ".lib"
         cObjPrefix := "FILE "
         cObjExt := ".obj"
         cLibPathPrefix := "LIBPATH "
         cLibPathSep := " "
         cBin_CompC := "wpp386.exe"
         cOpt_CompC := "-j -w3 -5s -5r -fp5 -oxehtz -zq -zt0 -mf -bt=OS2 {FC} -i{DI} {LC}"
         cBin_Link := "wlink.exe"
         cOpt_Link := "OP stack=65536 OP CASEEXACT {FL} NAME {OE} {LO} {DL} {LL}{SCRIPT}"
         cBin_Lib := "wlib.exe"
         cOpt_Lib := "{FA} {OL} {LO}{SCRIPT}"
         cLibLibExt := cLibExt
         cLibObjPrefix := "-+ "
         IF s_lDEBUG
            cOpt_Link := "DEBUG " + cOpt_Link
         ENDIF
         IF s_lMT
            AAdd( s_aOPTC, "-bm" )
         ENDIF
         IF s_lMAP
            AAdd( s_aOPTL, "OP MAP" )
         ENDIF

      CASE t_cARCH == "linux" .AND. t_cCOMP == "owatcom"
         cLibPrefix := "LIB "
         cLibExt := ".lib"
         cObjPrefix := "FILE "
         cObjExt := ".obj"
         cLibPathPrefix := "LIBPATH "
         cLibPathSep := " "
         cBin_CompC := "wpp386"
         cOpt_CompC := "-j -w3 -5s -5r -fp5 -oxehtz -zq -zt0 -mf -bt=LINUX {FC} -i{DI} {LC}"
         cBin_Link := "wlink"
         cOpt_Link := "ALL SYS LINUX OP CASEEXACT {FL} NAME {OE} {LO} {DL} {LL}{SCRIPT}"
         cBin_Lib := "wlib"
         cOpt_Lib := "{FA} {OL} {LO}{SCRIPT}"
         cLibLibExt := cLibExt
         cLibObjPrefix := "-+ "
         IF s_lMT
            AAdd( s_aOPTC, "-bm" )
         ENDIF
         IF s_lDEBUG
            cOpt_Link := "DEBUG " + cOpt_Link
         ENDIF
         IF s_lMAP
            AAdd( s_aOPTL, "OP MAP" )
         ENDIF

      /* Misc */
      CASE t_cARCH == "win" .AND. t_cCOMP == "bcc"
         IF s_lDEBUG
            AAdd( s_aOPTC, "-y -v" )
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
         IF ( Len( s_aRESSRC ) + Len( s_aRESCMP ) ) > 0
            cOpt_CompC := "-c -q -tWM -O2 -OS -Ov -Oi -Oc -d {FC} -I{DI} {LC}"
            cBin_Res := "brcc32.exe"
            cOpt_Res := "{LR}"
            cResExt := ".res"
            cBin_Link := "ilink32.exe"
            cOpt_Link := "-Gn -C -ap -Tpe -L{DL} {FL} c0d32.obj {LO}, {OE}, " + iif( s_lMAP, "{OM}", "nul" ) + ", cw32mt.lib {LL} import32.lib,, {LS}{SCRIPT}"
         ELSE
            cOpt_CompC := "-q -tWM -O2 -OS -Ov -Oi -Oc -d {FC} -I{DI} -L{DL} {LC} {LO} {LL}"
            IF lStopAfterCComp
               AAdd( s_aOPTC, "-c" )
            ENDIF
            IF s_lMAP
               AAdd( s_aOPTC, "-M" )
            ENDIF
         ENDIF
         cLibPathPrefix := ""
         cLibPathSep := ";"
         IF lStopAfterCComp
            IF ( Len( s_aPRG ) + Len( s_aC ) ) == 1
               AAdd( s_aOPTC, "-o{OO}" )
            ELSE
               AAdd( s_aOPTC, "-n{OD}" )
            ENDIF
         ELSE
            AAdd( s_aOPTC, "-e{OE}" )
         ENDIF
         IF s_lSHARED
            AAdd( s_aLIBPATH, "{DB}" )
         ENDIF
         s_aLIBSHARED := { iif( s_lMT, "harbourmt-" + cDL_Version_Alter + "-b32",;
                                       "harbour-" + cDL_Version_Alter + "-b32" ),;
                           "hbmainstd",;
                           "hbmainwin" }

      CASE t_cARCH == "win" .AND. t_cCOMP $ "msvc|msvc64|msvcia64|icc"
         IF s_lDEBUG
            AAdd( s_aOPTC, "-MTd -Zi" )
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
         IF t_cCOMP == "icc"
            cBin_Lib := "xilib.exe"
            cBin_CompC := "icl.exe"
            cBin_Dyn := "xilink.exe"
         ELSE
            cBin_Lib := "lib.exe"
            cBin_CompC := "cl.exe"
            cBin_Dyn := "link.exe"
         ENDIF
         cOpt_Lib := "{FA} /out:{OL} {LO}"
         cOpt_Dyn := "{FD} /dll /out:{OD} {DL} {LO} {LL} {LS}"
         cOpt_CompC := "-nologo -W3 {FC} -I{DI} {LC} {LO} /link {DL} {FL} {LL} {LS}"
         cLibPathPrefix := "/libpath:"
         cLibPathSep := " "
         IF s_lMAP
            AAdd( s_aOPTC, "-Fm" )
            AAdd( s_aOPTD, "-Fm" )
         ENDIF
         IF lStopAfterCComp
            AAdd( s_aOPTC, "-c" )
            IF ( Len( s_aPRG ) + Len( s_aC ) ) == 1
               AAdd( s_aOPTC, "-Fo{OO}" )
            ELSE
               AAdd( s_aOPTC, "-Fo{OD}" )
            ENDIF
         ELSE
            AAdd( s_aOPTC, "-Fe{OE}" )
         ENDIF
         IF s_lSHARED
            AAdd( s_aOPTL, "/libpath:{DB}" )
         ENDIF
         s_aLIBSYS := ArrayJoin( s_aLIBSYS, { "user32", "wsock32", "advapi32", "gdi32" } )
         DO CASE
         CASE t_cCOMP $ "msvc|icc"
            s_aLIBSHARED := { iif( s_lMT, "harbourmt-" + cDL_Version_Alter + "-vc",;
                                          "harbour-" + cDL_Version_Alter + "-vc" ),;
                              "hbmainstd",;
                              "hbmainwin" }
         CASE t_cCOMP == "msvc64"
            s_aLIBSHARED := { iif( s_lMT, "harbourmt-" + cDL_Version_Alter + "-vc-x64",;
                                          "harbour-" + cDL_Version_Alter + "-vc-x64" ),;
                              "hbmainstd",;
                              "hbmainwin" }
         CASE t_cCOMP == "msvcia64"
            s_aLIBSHARED := { iif( s_lMT, "harbourmt-" + cDL_Version_Alter + "-vc-ia64",;
                                          "harbour-" + cDL_Version_Alter + "-vc-ia64" ),;
                              "hbmainstd",;
                              "hbmainwin" }
         ENDCASE

         IF !( t_cCOMP == "icc" )
            cBin_Res := "rc.exe"
            cOpt_Res := "/r {LR}"
            cResExt := ".res"
         ENDIF

      CASE ( t_cARCH == "win" .AND. t_cCOMP == "pocc" ) .OR. ;
           ( t_cARCH == "win" .AND. t_cCOMP == "xcc" )

         /* TODO: pocc doesn't support multiple .c input files.
                  This means we need to do the heavy lifting. */

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
         cOpt_CompC := "/c /Ze /Go {FC} /I{DI} {LC}"
         cOpt_Dyn := "{FD} /dll /out:{OD} {DL} {LO} {LL} {LS}"
         IF t_cCOMP == "pocc"
            AAdd( s_aOPTC, "/Ot" )
            AAdd( s_aOPTC, "/Tx86-coff" )
         ENDIF
         cOpt_Res := "{LR}"
         cResExt := ".res"
         cOpt_Lib := "{FA} /out:{OL} {LO}"
         IF s_lMT
            AAdd( s_aOPTC, "/MT" )
         ENDIF
         IF lStopAfterCComp
            IF ( Len( s_aPRG ) + Len( s_aC ) ) == 1
               AAdd( s_aOPTC, "/Fo{OO}" )
            ENDIF
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
         IF s_lDEBUG
            AAdd( s_aOPTL, "/debug" )
         ENDIF
         s_aLIBSYS := ArrayJoin( s_aLIBSYS, { "kernel32", "user32", "wsock32", "advapi32", "gdi32" } )
         s_aLIBSHARED := { iif( s_lMT, "harbourmt-" + cDL_Version_Alter + "-pocc",;
                                       "harbour-" + cDL_Version_Alter + "-pocc" ),;
                           "hbmainstd",;
                           "hbmainwin" }

      /* TODO */
      CASE t_cARCH == "linux" .AND. t_cCOMP == "icc"
      CASE t_cARCH == "win" .AND. t_cCOMP == "msvcce"  /* NOTE: Cross-platform: wince/ARM on win/x86 */
      CASE t_cARCH == "win" .AND. t_cCOMP == "pocc64"  /* NOTE: Cross-platform: win/amd64 on win/x86 */
      CASE t_cARCH == "win" .AND. t_cCOMP == "poccce"  /* NOTE: Cross-platform: wince/ARM on win/x86 */
      CASE t_cARCH == "linux" .AND. t_cCOMP == "mingwce" /* NOTE: Cross-platform: wince/ARM on win/x86 */
         IF ! s_lSHARED
            s_aLIBSYS := ArrayJoin( s_aLIBSYS, { "wininet", "ws2", "commdlg", "commctrl", "uuid", "ole32" } )
         ENDIF
      ENDCASE

      IF lCreateDyn .AND. t_cARCH == "win"
         AAdd( s_aOPTC, "-DHB_DYNLIB" )
      ENDIF

      /* Do entry function detection on platform required and supported */
      IF ! lStopAfterCComp .AND. s_cMAIN == NIL
         tmp := iif( Lower( FN_ExtGet( s_cFIRST ) ) == ".prg" .OR. Empty( FN_ExtGet( s_cFIRST ) ), FN_ExtSet( s_cFIRST, ".c" ), s_cFIRST )
         IF ! Empty( tmp := getFirstFunc( tmp ) )
            s_cMAIN := tmp
         ENDIF
      ENDIF

      /* HACK: Override entry point requested by user or detected by us,
               and override the GT if requested by user. */
      IF ! lStopAfterCComp .AND. ;
         ( s_cMAIN != NIL .OR. ;
           ! Empty( s_aLIBUSERGT ) .OR. ;
           s_cGT != NIL .OR. ;
           s_lFMSTAT != NIL )

         fhnd := hb_FTempCreateEx( @s_cCSTUB, NIL, "hbmk_", ".c" )
         IF fhnd != F_ERROR

            /* NOTE: This has to be kept synced with Harbour HB_IMPORT values. */
            DO CASE
            CASE ! s_lSHARED .OR. ;
                 !( t_cARCH == "win" ) .OR. ;
                 t_cCOMP $ "msvc|msvc64|msvcia64|icc"

               /* NOTE: MSVC gives the warning:
                        "LNK4217: locally defined symbol ... imported in function ..."
                        if using 'dllimport'. [vszakats] */
               tmp := ""
            CASE t_cCOMP $ "gcc|mingw|cygwin" ; tmp := "__attribute__ (( dllimport ))"
            CASE t_cCOMP $ "bcc|owatcom"      ; tmp := "__declspec( dllimport )"
            OTHERWISE                         ; tmp := "_declspec( dllimport )"
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
            AEval( ListDirExt( s_aPRG, "", ".c" ), {|tmp| FErase( tmp ) } )
            PauseForKey()
            RETURN 5
         ENDIF
         AAdd( s_aC, s_cCSTUB )
      ENDIF

      /* Library list assembly */
      IF s_lSHARED .AND. ! Empty( s_aLIBSHARED )
         s_aLIBHB := ArrayAJoin( { s_aLIBSHARED,;
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
      s_aLIB := ArrayAJoin( { s_aLIBHB, s_aLIBUSER, s_aLIB3RD, s_aLIBSYS } )
      /* Dress lib names. */
      s_aLIB := ListCookLib( s_aLIB, cLibPrefix, cLibExt )
      /* Dress obj names. */
      s_aOBJ := ListDirExt( ArrayJoin( s_aPRG, s_aC ), "", cObjExt )
      s_aOBJUSER := ListCook( s_aOBJUSER, NIL, cObjExt )

      nErrorLevel := 0

      IF Len( s_aRESSRC ) > 0 .AND. ! Empty( cBin_Res )

         /* Compiling resource */

         cOpt_Res := StrTran( cOpt_Res, "{LR}"  , ArrayToList( s_aRESSRC ) )
         cOpt_Res := StrTran( cOpt_Res, "{LS}"  , ArrayToList( ListDirExt( s_aRESSRC, "", cResExt ) ) )
         cOpt_Res := StrTran( cOpt_Res, "{FR}"  , GetEnv( "HB_USER_RESFLAGS" ) )
         cOpt_Res := StrTran( cOpt_Res, "{DI}"  , s_cHB_INC_INSTALL )

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
            OutStd( "hbmk: Resource compiler command:" + hb_osNewLine() + cCommand + hb_osNewLine() )
            IF ! Empty( cScriptFile )
               OutStd( "hbmk: Resource compiler script:" + hb_osNewLine() + hb_MemoRead( cScriptFile ) + hb_osNewLine() )
            ENDIF
         ENDIF

         IF ( tmp := hb_run( cCommand ) ) != 0
            OutErr( "hbmk: Error: Running resource compiler. " + hb_ntos( tmp ) + ":" + hb_osNewLine() + cCommand + hb_osNewLine() )
            nErrorLevel := 8
         ENDIF

         IF ! Empty( cScriptFile )
            FErase( cScriptFile )
         ENDIF
      ENDIF

      IF nErrorLevel == 0 .AND. ( Len( s_aPRG ) + Len( s_aC ) + iif( Empty( cBin_Link ), Len( s_aOBJUSER ) + Len( s_aOBJA ), 0 ) ) > 0

         IF ! Empty( cBin_CompC )

            /* Compiling */

            /* Order is significant */
            cOpt_CompC := StrTran( cOpt_CompC, "{LC}"  , ArrayToList( ArrayJoin( ListDirExt( s_aPRG, "", ".c" ), s_aC ) ) )
            cOpt_CompC := StrTran( cOpt_CompC, "{LR}"  , ArrayToList( ArrayJoin( ListDirExt( s_aRESSRC, "", cResExt ), s_aRESCMP ) ) )
            cOpt_CompC := StrTran( cOpt_CompC, "{LO}"  , ArrayToList( ListCook( s_aOBJUSER, cObjPrefix ) ) )
            cOpt_CompC := StrTran( cOpt_CompC, "{LS}"  , ArrayToList( ListCook( ArrayJoin( ListDirExt( s_aRESSRC, "", cResExt ), s_aRESCMP ), cResPrefix ) ) )
            cOpt_CompC := StrTran( cOpt_CompC, "{LA}"  , ArrayToList( s_aOBJA ) )
            cOpt_CompC := StrTran( cOpt_CompC, "{LL}"  , ArrayToList( s_aLIB ) )
            cOpt_CompC := StrTran( cOpt_CompC, "{FC}"  , iif( s_lBLDFLGC, cSelfFlagC + " ", "" ) +;
                                                         GetEnv( "HB_USER_CFLAGS" ) + " " + ArrayToList( s_aOPTC ) )
            cOpt_CompC := StrTran( cOpt_CompC, "{FL}"  , iif( s_lBLDFLGL, cSelfFlagL + " ", "" ) +;
                                                         GetEnv( "HB_USER_LDFLAGS" ) + " " + ArrayToList( s_aOPTL ) )
            cOpt_CompC := StrTran( cOpt_CompC, "{OD}"  , PathSepToTarget( FN_DirGet( s_cPROGNAME ) ) )
            cOpt_CompC := StrTran( cOpt_CompC, "{OO}"  , PathSepToTarget( FN_ExtSet( s_cPROGNAME, cObjExt ) ) )
            cOpt_CompC := StrTran( cOpt_CompC, "{OE}"  , PathSepToTarget( FN_ExtSet( s_cPROGNAME, cBinExt ) ) )
            cOpt_CompC := StrTran( cOpt_CompC, "{OM}"  , PathSepToTarget( FN_ExtSet( s_cPROGNAME, ".map" ) ) )
            cOpt_CompC := StrTran( cOpt_CompC, "{DL}"  , ArrayToList( ListCook( s_aLIBPATH, cLibPathPrefix ), cLibPathSep ) )
            cOpt_CompC := StrTran( cOpt_CompC, "{DB}"  , s_cHB_BIN_INSTALL )
            cOpt_CompC := StrTran( cOpt_CompC, "{DI}"  , s_cHB_INC_INSTALL )

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
               OutStd( "hbmk: C compiler command:" + hb_osNewLine() + cCommand + hb_osNewLine() )
               IF ! Empty( cScriptFile )
                  OutStd( "hbmk: C compiler script:" + hb_osNewLine() + hb_MemoRead( cScriptFile ) + hb_osNewLine() )
               ENDIF
            ENDIF

            IF ( tmp := hb_run( cCommand ) ) != 0
               OutErr( "hbmk: Error: Running C compiler. " + hb_ntos( tmp ) + ":" + hb_osNewLine() + cCommand + hb_osNewLine() )
               nErrorLevel := 6
            ENDIF

            IF ! Empty( cScriptFile )
               FErase( cScriptFile )
            ENDIF
         ELSE
            OutErr( "hbmk: Error: This compiler/platform isn't implemented." + hb_osNewLine() )
            nErrorLevel := 8
         ENDIF
      ENDIF

      IF nErrorLevel == 0 .AND. ( Len( s_aOBJ ) + Len( s_aOBJUSER ) + Len( s_aOBJA ) ) > 0

         DO CASE
         CASE ! lStopAfterCComp .AND. ! Empty( cBin_Link )

            /* Linking */

            /* Order is significant */
            cOpt_Link := StrTran( cOpt_Link, "{LO}"  , ArrayToList( ListCook( ArrayJoin( s_aOBJ, s_aOBJUSER ), cObjPrefix ) ) )
            cOpt_Link := StrTran( cOpt_Link, "{LS}"  , ArrayToList( ListCook( ArrayJoin( ListDirExt( s_aRESSRC, "", cResExt ), s_aRESCMP ), cResPrefix ) ) )
            cOpt_Link := StrTran( cOpt_Link, "{LA}"  , ArrayToList( s_aOBJA ) )
            cOpt_Link := StrTran( cOpt_Link, "{LL}"  , ArrayToList( s_aLIB ) )
            cOpt_Link := StrTran( cOpt_Link, "{FL}"  , iif( s_lBLDFLGL, cSelfFlagL + " ", "" ) +;
                                                       GetEnv( "HB_USER_LDFLAGS" ) + " " + ArrayToList( s_aOPTL ) )
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
               OutStd( "hbmk: Linker command:" + hb_osNewLine() + cCommand + hb_osNewLine() )
               IF ! Empty( cScriptFile )
                  OutStd( "hbmk: Linker script:" + hb_osNewLine() + hb_MemoRead( cScriptFile ) + hb_osNewLine() )
               ENDIF
            ENDIF

            IF ( tmp := hb_run( cCommand ) ) != 0
               OutErr( "hbmk: Error: Running linker. " + hb_ntos( tmp ) + ":" + hb_osNewLine() + cCommand + hb_osNewLine() )
               nErrorLevel := 7
            ENDIF

            IF ! Empty( cScriptFile )
               FErase( cScriptFile )
            ENDIF

         CASE lStopAfterCComp .AND. lCreateLib .AND. ! Empty( cBin_Lib )

            /* Lib creation (static) */

            /* Order is significant */
            cOpt_Lib := StrTran( cOpt_Lib, "{LO}"  , ArrayToList( ListCook( ArrayJoin( s_aOBJ, s_aOBJUSER ), cLibObjPrefix ) ) )
            cOpt_Lib := StrTran( cOpt_Lib, "{LL}"  , ArrayToList( s_aLIB ) )
            cOpt_Lib := StrTran( cOpt_Lib, "{FA}"  , GetEnv( "HB_USER_AFLAGS" ) + " " + ArrayToList( s_aOPTA ) )
            cOpt_Lib := StrTran( cOpt_Lib, "{OL}"  , PathSepToTarget( FN_ExtSet( s_cPROGNAME, cLibLibExt ) ) )
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
               OutStd( "hbmk: Lib command:" + hb_osNewLine() + cCommand + hb_osNewLine() )
               IF ! Empty( cScriptFile )
                  OutStd( "hbmk: Lib script:" + hb_osNewLine() + hb_MemoRead( cScriptFile ) + hb_osNewLine() )
               ENDIF
            ENDIF

            IF ( tmp := hb_run( cCommand ) ) != 0
               OutErr( "hbmk: Error: Running lib command. " + hb_ntos( tmp ) + ":" + hb_osNewLine() + cCommand + hb_osNewLine() )
               nErrorLevel := 7
            ENDIF

            IF ! Empty( cScriptFile )
               FErase( cScriptFile )
            ENDIF

         CASE lStopAfterCComp .AND. lCreateDyn .AND. ! Empty( cBin_Dyn )

            /* Lib creation (dynamic) */

            /* Order is significant */
            cOpt_Dyn := StrTran( cOpt_Dyn, "{LO}"  , ArrayToList( ListCook( ArrayJoin( s_aOBJ, s_aOBJUSER ), cDynObjPrefix ) ) )
            cOpt_Dyn := StrTran( cOpt_Dyn, "{LS}"  , ArrayToList( ListCook( ArrayJoin( ListDirExt( s_aRESSRC, "", cResExt ), s_aRESCMP ), cResPrefix ) ) )
            cOpt_Dyn := StrTran( cOpt_Dyn, "{LL}"  , ArrayToList( s_aLIB ) )
            cOpt_Dyn := StrTran( cOpt_Dyn, "{FD}"  , GetEnv( "HB_USER_DFLAGS" ) + " " + ArrayToList( s_aOPTD ) )
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
               OutStd( "hbmk: Dynamic lib link command:" + hb_osNewLine() + cCommand + hb_osNewLine() )
               IF ! Empty( cScriptFile )
                  OutStd( "hbmk: Dynamic lib link script:" + hb_osNewLine() + hb_MemoRead( cScriptFile ) + hb_osNewLine() )
               ENDIF
            ENDIF

            IF ( tmp := hb_run( cCommand ) ) != 0
               OutErr( "hbmk: Error: Running dynamic lib link command. " + hb_ntos( tmp ) + ":" + hb_osNewLine() + cCommand + hb_osNewLine() )
               nErrorLevel := 7
            ENDIF

            IF ! Empty( cScriptFile )
               FErase( cScriptFile )
            ENDIF

         ENDCASE
      ENDIF

      /* Cleanup */

      IF ! Empty( s_cCSTUB )
         FErase( s_cCSTUB )
      ENDIF
      AEval( ListDirExt( s_aPRG, "", ".c" ), {|tmp| FErase( tmp ) } )
      IF ! lStopAfterCComp .OR. lCreateLib .OR. lCreateDyn
         IF ! Empty( cResExt )
            AEval( ListDirExt( s_aRESSRC, "", cResExt ), {|tmp| FErase( tmp ) } )
         ENDIF
         AEval( s_aOBJ, {|tmp| FErase( tmp ) } )
      ENDIF
      AEval( s_aCLEAN, {|tmp| FErase( tmp ) } )

      IF ! lStopAfterCComp
         IF nErrorLevel != 0
            PauseForKey()
         ELSEIF s_lRUN
            s_cPROGNAME := FN_ExtSet( s_cPROGNAME, cBinExt )
            #if defined( __PLATFORM__UNIX )
               IF Empty( FN_DirGet( s_cPROGNAME ) )
                  s_cPROGNAME := "." + hb_osPathSeparator() + s_cPROGNAME
               ENDIF
            #endif
            IF s_lTRACE
               OutStd( "hbmk: Running executable:" + hb_osNewLine() + PathSepToTarget( s_cPROGNAME ) + hb_osNewLine() )
            ENDIF
            nErrorLevel := hb_run( PathSepToTarget( s_cPROGNAME ) )
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
   LOCAL cDir, cName, cExt

   FOR EACH cFileName IN array
      hb_FNameSplit( cFileName, @cDir, @cName, @cExt )
      IF cDirNew != NIL
         cDir := cDirNew
      ENDIF
      IF cExtNew != NIL
         cExt := cExtNew
      ENDIF
      cFileName := hb_FNameMerge( cDir, cName, cExt )
   NEXT

   RETURN array

/* Forms the list of libs as to appear on the command line */
STATIC FUNCTION ListCookLib( arraySrc, cPrefix, cExtNew )
   LOCAL array := AClone( arraySrc )
   LOCAL cDir
   LOCAL cLibName

   IF t_cCOMP $ "gcc|gpp|mingw|djgpp|cygwin"
      FOR EACH cLibName IN array
         hb_FNameSplit( cLibName, @cDir )
         IF Empty( cDir )
            IF Left( cLibName, 3 ) == "lib"
               cLibName := SubStr( cLibName, 4 )
            ENDIF
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

   IF t_cARCH $ "win|dos|os2" .AND. !( t_cCOMP $ "mingw|mingwce|cygwin" )
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
                                 /* @ */ lRUN,;
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
            @lRUN,;
            @cGT )
         EXIT
      ENDIF
   NEXT

   IF ! lConfigOnly
      FOR EACH aFile IN Directory( "*" + ".hbp" )
         cFileName := aFile[ F_NAME ]
         IF !( cFileName == HBMK_CFG_NAME )
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
               @lRUN,;
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
                                 /* @ */ lRUN,;
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

      CASE Lower( Left( cLine, Len( "run="        ) ) ) == "run="        ; cLine := SubStr( cLine, Len( "run="        ) + 1 )
         DO CASE
         CASE ValueIsT( cLine ) ; lRUN := .T.
         CASE ValueIsF( cLine ) ; lRUN := .F.
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

   LOCAL cExpr := "( hbmk_ARCH() == Lower( '%1' ) .OR. hbmk_COMP() == Lower( '%1' ) )"

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
   IF t_cCOMP $ "gcc|gpp|mingw|cygwin"
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
      t_cCOMP $ "mingwce|poccce|msvcce"

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
      CASE t_cCOMP $ "mingwce|poccce|msvcce"
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

/* Keep this public, it's used from macro. */
FUNCTION hbmk_ARCH()
   RETURN t_cARCH

/* Keep this public, it's used from macro. */
FUNCTION hbmk_COMP()
   RETURN t_cCOMP

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
      "  -mt|-st           link with multi-thread/single-thread VM" ,;
      "  -gt<name>         link with GT<name> GT driver, can be repeated to link" ,;
      "                    with more GTs. First one will be the default at runtime" }

   LOCAL aText_Help := {;
      "  -help             long help" }

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
      "  -[no]run          run/don't run the created executable" ,;
      "  -nohbp            do not process .hbp files in current directory" ,;
      "" ,;
      "  -bldf[-]          inherit all/no (default) flags from Harbour build" ,;
      "  -bldf=[p][c][l]   inherit .prg/.c/linker flags (or none) from Harbour build" ,;
      "  -prgflag:<f>      pass flag to Harbour" ,;
      "  -cflag:<f>        pass flag to C compiler" ,;
      "  -ldflag:<f>       pass flag to linker" ,;
      "  -hbcmp            stop after creating the object files" ,;
      "                    create link/copy hbmk to hbcmp for the same effect" ,;
      "  -hbcc             stop after creating the object files and accept raw C flags" ,;
      "                    create link/copy hbmk to hbcc for the same effect" ,;
      "  -hblnk            accept raw linker flags" ,;
      "  -hblib            create static library (experimental)" ,;
      "  -hbdyn            create dynamic library (experimental)" ,;
      "" ,;
      "  -arch=<arch>      assume specific architecure. Same as HB_ARCHITECTURE envvar" ,;
      "  -comp=<comp>      use specific compiler. Same as HB_COMPILER envvar" ,;
      "                    Special value:" ,;
      "                     - bld: use original build settings (default on *nix)" ,;
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
      "    gui|mt|shared|nulrdd|debug|map|strip|run=[yes|no]" ,;
      "    Lines starting with '#' char are ignored" ,;
      "  - Platform filters are accepted in each .hbp line and with -l options." ,;
      "    Filter format: {[!][<arch|comp>]}. Filters can be combined " ,;
      "    using '&', '|' operators and grouped by parantheses." ,;
      "    Ex.: {win}, {gcc}, {linux|darwin}, {win&!pocc}, {(win|linux)&!owatcom}" ,;
      "  - Defaults and feature support vary by architecture/compiler." ,;
      "  - Supported <comp> values for each supported <arch> value:" ,;
      "    linux  : gcc, gpp, owatcom, icc, mingw, mingwce" ,;
      "    darwin : gcc" ,;
      "    win    : mingw, msvc, bcc, owatcom, icc, pocc, cygwin," ,;
      "             mingwce, msvc64, msvcia64, msvcce, pocc64, poccce, xcc" ,;
      "    os2    : gcc, owatcom" ,;
      "    dos    : djgpp, owatcom" ,;
      "    bsd, hpux, sunos: gcc" }

   DEFAULT lLong TO .F.

   AEval( aText_Basic, {|tmp| OutStd( tmp + hb_osNewLine() ) } )
   AEval( iif( lLong, aText_Long, aText_Help ), {|tmp| OutStd( tmp + hb_osNewLine() ) } )

   RETURN

STATIC FUNCTION HBRawVersion()
   RETURN StrTran( Version(), "Harbour ", "" )
