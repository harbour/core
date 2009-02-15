/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour Make
 *
 * Copyright 2009 Viktor Szakats <harbour.01 syenar.hu>
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

#include "common.ch"
#include "directry.ch"
#include "fileio.ch"
#include "hbgtinfo.ch"

/* TODO: Add support for more hbmk script features. */

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
      "hbpcre" ,;
      "hbzlib" ,;
      "hbextern" ,;
      "gtcgi" ,;
      "gtstd" ,;
      "gtpca" }

   LOCAL aLIB_BASE_DEBUG := {;
      "hbdebug" }

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

   LOCAL s_cCOMP
   LOCAL s_cARCH
   LOCAL s_cGT
   LOCAL s_cGTPRG

   LOCAL s_cHB_INSTALL_PREFIX
   LOCAL s_cHB_BIN_INSTALL
   LOCAL s_cHB_LIB_INSTALL
   LOCAL s_cHB_INC_INSTALL

   LOCAL s_aPRG
   LOCAL s_aC
   LOCAL s_aLIBSHARED
   LOCAL s_aLIB
   LOCAL s_aLIBVM
   LOCAL s_aLIBUSER
   LOCAL s_aLIBHB
   LOCAL s_aLIBHBGT
   LOCAL s_aLIB3RD
   LOCAL s_aLIBSYS
   LOCAL s_aOPTP
   LOCAL s_aOPTC
   LOCAL s_aOPTL
   LOCAL s_cPROGNAME
   LOCAL s_aOBJ
   LOCAL s_aOBJUSER
   LOCAL s_aCLEAN

   LOCAL s_lGUI := .F.
   LOCAL s_lMT := .F.
   LOCAL s_lSHARED := .F.
   LOCAL s_lDEBUG := .F.
   LOCAL s_lNULRDD := .F.
   LOCAL s_lSTRIP := .F.
   LOCAL s_lTRACE := .F.

   LOCAL aCOMPDET
   LOCAL aCOMPSUP

   LOCAL cLibPrefix
   LOCAL cLibExt
   LOCAL cObjExt

   LOCAL cCommand
   LOCAL cOpt_CompC
   LOCAL cOpt_Link
   LOCAL cBin_CompPRG
   LOCAL cBin_CompC
   LOCAL cBin_Link
   LOCAL nErrorLevel
   LOCAL tmp
   LOCAL cScriptFile
   LOCAL fhnd
   LOCAL lNOHBP

   LOCAL aParams
   LOCAL cParam

   IF PCount() == 0
      ShowHeader()
      ShowHelp()
      PauseForKey()
      RETURN 9
   ENDIF

   ShowHeader()

   /* Autodetect architecture */

   s_cARCH := GetEnv( "HB_ARCHITECTURE" )
   IF Empty( GetEnv( "HB_ARCHITECTURE" ) )
#if defined( __PLATFORM__BSD )
      s_cARCH := "bsd"
#elif defined( __PLATFORM__DARWIN )
      s_cARCH := "darwin"
#elif defined( __PLATFORM__DOS )
      s_cARCH := "dos"
#elif defined( __PLATFORM__HPUX )
      s_cARCH := "hpux"
#elif defined( __PLATFORM__LINUX )
      s_cARCH := "linux"
#elif defined( __PLATFORM__OS2 )
      s_cARCH := "os2"
#elif defined( __PLATFORM__SUNOS )
      s_cARCH := "sunos"
#elif defined( __PLATFORM__WINDOWS )
      s_cARCH := "win"
#endif
      IF ! Empty( s_cARCH )
         OutStd( "hbmk: Autodetected HB_ARCHITECTURE: " + s_cARCH + hb_osNewLine() )
      ENDIF
   ENDIF

   /* Setup architecture dependent data */

   DO CASE
   CASE s_cARCH == "bsd"
      aCOMPSUP := { "gcc" }
      cBin_CompPRG := "harbour"
      s_aLIBHBGT := { "gttrm", "gtxwc" }
   CASE s_cARCH == "darwin"
      aCOMPSUP := { "gcc" }
      cBin_CompPRG := "harbour"
      s_aLIBHBGT := { "gttrm", "gtxwc" }
   CASE s_cARCH == "hpux"
      aCOMPSUP := { "gcc" }
      cBin_CompPRG := "harbour"
      s_aLIBHBGT := { "gttrm", "gtxwc" }
   CASE s_cARCH == "linux"
      aCOMPSUP := { "gcc", "gpp", "owatcom" }
      cBin_CompPRG := "harbour"
      s_aLIBHBGT := { "gttrm", "gtxwc" }
   CASE s_cARCH == "sunos"
      aCOMPSUP := { "gcc" }
      cBin_CompPRG := "harbour"
      s_aLIBHBGT := { "gttrm", "gtxwc" }
   CASE s_cARCH == "dos"
      aCOMPDET := { { "gcc.exe", "djgpp" },;
                    { "wpp386.exe", "owatcom" } }
      aCOMPSUP := { "djgpp", "gcc", "owatcom", "rsx32" }
      cBin_CompPRG := "harbour.exe"
      s_aLIBHBGT := { "gtdos" }
   CASE s_cARCH == "os2"
      aCOMPDET := { { "gcc.exe", "gcc" },;
                    { "icc.exe", "icc" },;
                    { "wpp386.exe", "owatcom" } }
      aCOMPSUP := { "gcc", "icc" }
      cBin_CompPRG := "harbour.exe"
      s_aLIBHBGT := { "gtos2" }
   CASE s_cARCH == "win"
      aCOMPDET := { { "gcc.exe"   , "mingw"   },;
                    { "cl.exe"    , "msvc"    },;
                    { "bcc32.exe" , "bcc32"   },;
                    { "wpp386.exe", "owatcom" } }
      aCOMPSUP := { "bcc32", "dm", "gcc", "icc", "mingw", "mingwce", "msvc", "msvcce", "owatcom", "pocc", "pocc64", "poccce", "rsxnt", "xcc" }
      cBin_CompPRG := "harbour.exe"
      s_aLIBHBGT := { "gtwin", "gtwvt", "gtgui" }
   OTHERWISE
      OutErr( "hbmk: Error: Architecture not properly set." + hb_osNewLine() )
      PauseForKey()
      RETURN 1
   ENDCASE

   /* Autodetect compiler */

   s_cCOMP := GetEnv( "HB_COMPILER" )
   IF Empty( GetEnv( "HB_COMPILER" ) )
      IF Len( aCOMPSUP ) == 1
         s_cCOMP := aCOMPSUP[ 1 ]
      ELSEIF ! Empty( aCOMPDET )
         FOR tmp := 1 TO Len( aCOMPDET )
            IF FindInPath( aCOMPDET[ tmp ][ 1 ] )
               s_cCOMP := aCOMPDET[ tmp ][ 2 ]
               EXIT
            ENDIF
         NEXT
      ENDIF
      IF ! Empty( s_cCOMP )
         OutStd( "hbmk: Autodetected HB_COMPILER: " + s_cCOMP + hb_osNewLine() )
      ENDIF
   ENDIF

   IF AScan( aCOMPSUP, {|tmp| tmp == s_cCOMP } ) == 0
      OutErr( "hbmk: Error: Compiler not properly set." + hb_osNewLine() )
      PauseForKey()
      RETURN 2
   ENDIF

   /* Autodetect Harbour environment */

   IF Empty( GetEnv( "HB_INSTALL_PREFIX" ) )

      DO CASE
      CASE hb_ProgName() == "/opt/harbour/hbmk"

         s_cHB_INSTALL_PREFIX := "/opt/harbour"

         /* Build shared libs by default, if we're installed to default system locations. */
         IF s_cARCH $ "bsd|hpux|sunos|linux|darwin"
            s_lSHARED := .T.
         ENDIF

      CASE hb_ProgName() == "/usr/local/bin/hbmk"

         IF Empty( GetEnv( "HB_BIN_INSTALL" ) )
            s_cHB_BIN_INSTALL := "/usr/local/bin"
         ENDIF
         IF Empty( GetEnv( "HB_LIB_INSTALL" ) )
            s_cHB_LIB_INSTALL := "/usr/local/include/harbour"
         ENDIF
         IF Empty( GetEnv( "HB_INC_INSTALL" ) )
            s_cHB_INC_INSTALL := "/usr/local/lib/harbour"
         ENDIF

         /* Build shared libs by default, if we're installed to default system locations. */
         IF s_cARCH $ "bsd|hpux|sunos|linux|darwin"
            s_lSHARED := .T.
         ENDIF

      CASE hb_FileExists( DirAddPathSep( hb_DirBase() ) + cBin_CompPRG )
         s_cHB_INSTALL_PREFIX := DirAddPathSep( hb_DirBase() ) + ".."
      CASE hb_FileExists( DirAddPathSep( hb_DirBase() ) + "bin" + hb_osPathSeparator() + cBin_CompPRG )
         s_cHB_INSTALL_PREFIX := DirAddPathSep( hb_DirBase() )
      CASE hb_FileExists( DirAddPathSep( hb_DirBase() ) + ".." + hb_osPathSeparator() + ".." + hb_osPathSeparator() + "bin" + hb_osPathSeparator() + cBin_CompPRG )
         s_cHB_INSTALL_PREFIX := DirAddPathSep( hb_DirBase() ) + ".." + hb_osPathSeparator() + ".."
      OTHERWISE
         OutErr( "hbmk: Error: HB_INSTALL_PREFIX not set, failed to autodetect." + hb_osNewLine() )
         PauseForKey()
         RETURN 3
      ENDCASE
   ELSE
      s_cHB_INSTALL_PREFIX := GetEnv( "HB_INSTALL_PREFIX" )
   ENDIF
   IF Empty( GetEnv( "HB_BIN_INSTALL" ) )
      s_cHB_BIN_INSTALL := DirAddPathSep( s_cHB_INSTALL_PREFIX ) + "bin"
   ENDIF
   IF Empty( GetEnv( "HB_LIB_INSTALL" ) )
      s_cHB_LIB_INSTALL := DirAddPathSep( s_cHB_INSTALL_PREFIX ) + "lib"
   ENDIF
   IF Empty( GetEnv( "HB_INC_INSTALL" ) )
      s_cHB_INC_INSTALL := DirAddPathSep( s_cHB_INSTALL_PREFIX ) + "include"
   ENDIF

   /* Process command line */

   IF Lower( GetEnv( "HB_GUI"    ) ) == "yes" ; s_lGUI    := .T. ; ENDIF
   IF Lower( GetEnv( "HB_MT"     ) ) == "yes" ; s_lMT     := .T. ; ENDIF
   IF Lower( GetEnv( "HB_SHARED" ) ) == "yes" ; s_lSHARED := .T. ; ENDIF
   IF Lower( GetEnv( "HB_DEBUG"  ) ) == "yes" ; s_lDEBUG  := .T. ; ENDIF
   IF Lower( GetEnv( "HB_NULRDD" ) ) == "yes" ; s_lNULRDD := .T. ; ENDIF

   IF Lower( Left( GetEnv( "HB_GT" ), 2 ) ) == "gt"
      s_cGT := GetEnv( "HB_GT" )
   ENDIF

   s_aPRG := {}
   s_aC := {}
   s_aLIBUSER := {}
   s_aOBJUSER := {}
   s_aOPTP := {}
   s_cPROGNAME := NIL

   /* Collect all command line parameters */
   aParams := {}
   FOR EACH cParam IN hb_AParams()
      IF Len( cParam ) >= 1 .AND. Left( cParam, 1 ) == "@"
         /* Load parameters from script file */
         HBM_Load( aParams, SubStr( cParam, 2 ) )
      ELSE
         AAdd( aParams, cParam )
      ENDIF
   NEXT

   /* Process command line (1st pass) */
   lNOHBP := .F.
   FOR EACH cParam IN aParams
      IF Lower( cParam ) == "-nohbp"
         lNOHBP := .T.
      ENDIF
   NEXT

   IF ! lNOHBP
      /* Process automatic control files. */
      HBP_ProcessAll( @s_aLIBUSER,;
                      @s_aOPTP,;
                      @s_lGUI,;
                      @s_lMT,;
                      @s_lSHARED,;
                      @s_lDEBUG,;
                      @s_lNULRDD,;
                      @s_lSTRIP,;
                      @s_cGT )
   ENDIF

   /* Process command line (2nd pass) */
   FOR EACH cParam IN aParams

      DO CASE
      CASE Lower( cParam ) == "-gui"             ; s_lGUI    := .T.
      CASE Lower( cParam ) == "-std"             ; s_lGUI    := .F.
      CASE Lower( cParam ) == "-mt"              ; s_lMT     := .T.
      CASE Lower( cParam ) == "-st"              ; s_lMT     := .F.
      CASE Lower( cParam ) == "-shared"          ; s_lSHARED := .T.
      CASE Lower( cParam ) == "-static"          ; s_lSHARED := .F.
      CASE Lower( cParam ) == "-debug"           ; s_lDEBUG  := .T.
      CASE Lower( cParam ) == "-nodebug"         ; s_lDEBUG  := .F.
      CASE Lower( cParam ) == "-nulrdd"          ; s_lNULRDD := .T.
      CASE Lower( cParam ) == "-strip"           ; s_lSTRIP  := .T.
      CASE Lower( cParam ) == "-nostrip"         ; s_lSTRIP  := .F.
      CASE Lower( cParam ) == "-trace"           ; s_lTRACE  := .T.
      CASE Lower( Left( cParam, 3 ) ) == "-gt"   ; DEFAULT s_cGT TO SubStr( cParam, 2 )
      CASE Left( cParam, 2 ) == "-o"             ; s_cPROGNAME := SubStr( cParam, 3 )
      CASE Left( cParam, 2 ) == "-l" .AND. ;
           Len( cParam ) > 2                     ; AAdd( s_aLIBUSER, SubStr( cParam, 3 ) )
      CASE Left( cParam, 1 ) == "-"              ; AAdd( s_aOPTP   , cParam )
      CASE Lower( ExtGet( cParam ) ) == ".prg"   ; AAdd( s_aPRG    , cParam ) ; DEFAULT s_cPROGNAME TO cParam
      CASE Lower( ExtGet( cParam ) ) $ ".o|.obj" ; AAdd( s_aOBJUSER, cParam )
      CASE Lower( ExtGet( cParam ) ) $ ".c|.cpp" ; AAdd( s_aC      , cParam ) ; DEFAULT s_cPROGNAME TO cParam
      CASE Lower( ExtGet( cParam ) ) $ ".a|.lib" ; AAdd( s_aLIBUSER, cParam )
      OTHERWISE                                  ; AAdd( s_aPRG    , cParam ) ; DEFAULT s_cPROGNAME TO cParam
      ENDCASE
   NEXT

   /* Start doing the make process. */
   IF ( Len( s_aPRG ) + Len( s_aC ) ) == 0
      OutErr( "hbmk: Error: No source files were specified." + hb_osNewLine() )
      PauseForKey()
      RETURN 4
   ENDIF

   IF ! Empty( s_cGT )
      fhnd := hb_FTempCreateEx( @s_cGTPRG, ".", "hbmkgt$", ".prg" )
      IF fhnd != F_ERROR
         FWrite( fhnd, "ANNOUNCE HB_GTSYS" + hb_osNewLine() +;
                       "REQUEST HB_GT_" + Upper( SubStr( s_cGT, 3 ) ) + "_DEFAULT" + hb_osNewLine() )
         FClose( fhnd )
      ELSE
         OutErr( "hbmk: Warning: C compiler script couldn't be created, continuing in command line." + hb_osNewLine() )
      ENDIF
      AAdd( s_aPRG, s_cGTPRG )
   ENDIF

   /* Merge user libs from command line and envvar. Command line has priority. */
   s_aLIBUSER := ArrayAJoin( { s_aLIBUSER, ListToArray( GetEnv( "HB_USER_LIBS" ) ) } )

   /* Strip lib/obj names from extension. */
   ListDelExt( s_aLIBUSER )
   ListDelExt( s_aOBJUSER )

   /* TOFIX: s_aLIBSHARED to be fixed for some *IX platforms. */
   DO CASE
   CASE s_cARCH $ "bsd|linux|sunos"
      s_aLIBSHARED := { iif( s_lMT, "harbourmt.so", "harbour.so" ) }
   CASE s_cARCH == "hpux"
      s_aLIBSHARED := { iif( s_lMT, "harbourmt.sl", "harbour.sl" ) }
   CASE s_cARCH == "darwin"
      s_aLIBSHARED := { iif( s_lMT, "harbourmt.dylib", "harbour.dylib" ) }
   CASE s_cARCH $ "os2|win"
      s_aLIBSHARED := { iif( s_lMT, "harbourmt", "harbour" ) }
   OTHERWISE
      s_aLIBSHARED := NIL
   ENDCASE

   /* Harbour compilation */

   s_cPROGNAME := DirNameGet( s_cPROGNAME )

   IF Len( s_aPRG ) > 0

      cCommand := DirAddPathSep( s_cHB_BIN_INSTALL ) +;
                  cBin_CompPRG +;
                  " " + ArrayToList( s_aPRG ) +;
                  " -n -q0" +;
                  " -i" + s_cHB_INC_INSTALL +;
                  iif( ! Empty( GetEnv( "HB_USER_PRGFLAGS" ) ), " " + GetEnv( "HB_USER_PRGFLAGS" ), "" ) +;
                  iif( ! Empty( s_aOPTP ), " " + ArrayToList( s_aOPTP ), "" )

      IF s_lTRACE
         OutErr( "hbmk: Harbour compiler command: '" + cCommand + "'" + hb_osNewLine() )
      ENDIF

      IF ( tmp := hb_run( cCommand ) ) != 0
         OutErr( "hbmk: Error: Running Harbour compiler. " + hb_ntos( tmp ) + ": '" + cCommand + "'" + hb_osNewLine() )
         IF ! Empty( s_cGTPRG )
            FErase( s_cGTPRG )
         ENDIF
         PauseForKey()
         RETURN 5
      ENDIF
   ENDIF

   /* Assemble library list */

   /* C compilation/linking */

   s_aLIB3RD := {}
   s_aLIBSYS := {}
   s_aOPTC := {}
   s_aOPTL := {}
   s_aCLEAN := {}

   /* Command macros:

      {C}      list of C files,
      {O}      list of object files,
      {L}      list of lib files,
      {OPTC}   C compiler flags (user + automatic),
      {OPTL}   linker flags (user + automatic),
      {E}      binary name,
      {B}      binary path,
      {I}      include path,
      {A}      lib path,
      {SCRIPT} save command line to script and pass it to command as @<filename>
   */

   s_aLIBVM := iif( s_lMT, aLIB_BASE_MT, aLIB_BASE_ST )
   aLIB_BASE2 := ArrayAJoin( { aLIB_BASE2, s_aLIBHBGT } )

   IF ! Empty( s_cGT )
      IF AScan( aLIB_BASE2, {|tmp| Upper( tmp ) == Upper( s_cGT ) } ) == 0
         AAdd( aLIB_BASE2, s_cGT )
      ENDIF
   ENDIF

   DO CASE
   /* GCC family */
   CASE ( s_cARCH == "bsd"    .AND. s_cCOMP == "gcc" ) .OR. ;
        ( s_cARCH == "darwin" .AND. s_cCOMP == "gcc" ) .OR. ;
        ( s_cARCH == "hpux"   .AND. s_cCOMP == "gcc" ) .OR. ;
        ( s_cARCH == "linux"  .AND. s_cCOMP $ "gcc|gpp" ) .OR. ;
        ( s_cARCH == "sunos"  .AND. s_cCOMP == "gcc" )

      cLibPrefix := "-l"
      cLibExt := NIL
      cObjExt := ".o"
      cBin_CompC := iif( s_cCOMP == "gpp", "g++", "gcc" )
      cOpt_CompC := "{C} -O3 -o{E} {OPTC} -I{I} -L{A} {L}"
      aLIB_BASE2 := ArrayAJoin( { aLIB_BASE2, { "hbcommon", "hbrtl" }, s_aLIBVM } )
      IF s_cARCH == "darwin"
         AAdd( s_aOPTC, "-no-cpp-precomp -Wno-long-double" )
      ENDIF
      IF s_lSTRIP .AND. !( s_cARCH == "sunos" )
         AAdd( s_aOPTC, "-s" )
      ENDIF

   CASE ( s_cARCH == "win" .AND. s_cCOMP == "gcc" ) .OR. ;
        ( s_cARCH == "win" .AND. s_cCOMP == "mingw" ) .OR. ;
        ( s_cARCH == "os2" .AND. s_cCOMP == "gcc" )

      cLibPrefix := "-l"
      cLibExt := NIL
      cObjExt := ".o"
      cBin_CompC := "gcc"
      cOpt_CompC := "{C} -O3 -mno-cygwin -o{E}.exe {OPTC} -I{I} -L{A}"
      IF s_lSHARED
           cOpt_CompC += " -L{B}"
      ENDIF
      cOpt_CompC += " {L}"
      aLIB_BASE2 := ArrayAJoin( { aLIB_BASE2, { "hbcommon", "hbrtl" }, s_aLIBVM } )
      IF s_lSTRIP
         AAdd( s_aOPTC, "-s" )
      ENDIF

   CASE s_cARCH == "dos" .AND. s_cCOMP == "djgpp"

      cLibPrefix := "-l"
      cLibExt := NIL
      cObjExt := ".o"
      cBin_CompC := "gcc"
      cOpt_CompC := "{C} -O3 -o{E}.exe {OPTC} -I{I} -L{A} {L}{SCRIPT}"
      s_aLIBSYS := ArrayJoin( s_aLIBSYS, { "m" } )
      aLIB_BASE2 := ArrayAJoin( { aLIB_BASE2, { "hbcommon", "hbrtl" }, s_aLIBVM } )
      IF s_lSTRIP
         AAdd( s_aOPTC, "-s" )
      ENDIF

   CASE s_cARCH == "dos" .AND. s_cCOMP == "rsx32"

      cLibPrefix := "-l"
      cLibExt := NIL
      cObjExt := ".o"
      cBin_CompC := "gcc"
      cOpt_CompC := "{C} -O3 -Zrsx32 -o{E}.exe {OPTC} -I{I} -L{A} {L}"
      aLIB_BASE2 := ArrayAJoin( { aLIB_BASE2, { "hbcommon", "hbrtl" }, s_aLIBVM } )
      IF s_lSTRIP
         AAdd( s_aOPTC, "-s" )
      ENDIF

   /* Watcom family */
   CASE s_cARCH == "dos" .AND. s_cCOMP == "owatcom"
      cLibPrefix := "LIB "
      cLibExt := ".lib"
      cObjExt := ".obj"
      cBin_CompC := "wpp386"
      cOpt_CompC := "-j -w3 -5s -5r -fp5 -oxehtz -zq -zt0 -bt=DOS {OPTC} {C}"
      cBin_Link := "wlink"
      cOpt_Link := "OP osn=DOS OP stack=65536 OP CASEEXACT OP stub=cwstub.exe {OPTL} NAME {E}.exe {L}"

   CASE s_cARCH == "win" .AND. s_cCOMP == "owatcom"
      cLibPrefix := "LIB "
      cLibExt := ".lib"
      cObjExt := ".obj"
      cBin_CompC := "wpp386"
      cOpt_CompC := "-j -w3 -5s -5r -fp5 -oxehtz -zq -zt0 -mf -bt=NT {OPTC} {C}"
      cBin_Link := "wlink"
      cOpt_Link := "OP osn=NT OP stack=65536 OP CASEEXACT {OPTL} NAME {E}.exe {L}"
      s_aLIBSYS := ArrayJoin( s_aLIBSYS, { "kernel32", "user32", "wsock32" } )

   CASE s_cARCH == "os2" .AND. s_cCOMP == "owatcom"
      cLibPrefix := "LIB "
      cLibExt := ".lib"
      cObjExt := ".obj"
      cBin_CompC := "wpp386"
      cOpt_CompC := "-j -w3 -5s -5r -fp5 -oxehtz -zq -zt0 -mf -bt=OS2 {OPTC} {C}"
      cBin_Link := "wlink"
      cOpt_Link := "OP stack=65536 OP CASEEXACT {OPTL} NAME {E}.exe {L}"

   CASE s_cARCH == "linux" .AND. s_cCOMP == "owatcom"
      cLibPrefix := "LIB "
      cLibExt := ".lib"
      cObjExt := ".obj"
      cBin_CompC := "wpp386"
      cOpt_CompC := "-j -w3 -5s -5r -fp5 -oxehtz -zq -zt0 -mf -bt=LINUX {OPTC} {C}"
      cBin_Link := "wlink"
      cOpt_Link := "ALL SYS LINUX OP CASEEXACT {OPTL} NAME {E} {L}"
      IF s_lDEBUG
         cOpt_Link := "DEBUG " + cOpt_Link
      ENDIF

   /* Misc */
   CASE s_cARCH == "win" .AND. s_cCOMP == "bcc32"
      IF s_lDEBUG
         AAdd( s_aOPTC, "-y -v" )
      ELSE
         AAdd( s_aCLEAN, ExtSet( s_cPROGNAME, ".tds" ) )
      ENDIF
      IF s_lGUI
         AAdd( s_aOPTC, "-tW" )
      ENDIF
      cLibPrefix := NIL
      cLibExt := ".lib"
      cObjExt := ".obj"
      cBin_CompC := "bcc32"
      cOpt_CompC := "-q -tWM -O2 -OS -Ov -Oi -Oc -d {OPTC} -e{E}.exe -I{I} -L{A} {C} {L}"
      IF s_lSHARED
           cOpt_CompC += " -L{B}"
      ENDIF
      /* TOFIX: The two build systems should generate the same .dll name, otherwise
                we can only be compatible with one of them. non-GNU is the common choice here. */
      s_aLIBSHARED := { iif( s_lMT, "harbourmt-11-b32", "harbour-11-b32" ), "hbmainstd", "hbmainwin", "hbcommon" }

   CASE s_cARCH == "win" .AND. s_cCOMP == "msvc"
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
      cBin_CompC := "cl"

      /* odbc32 ole32 oleaut32 comdlg32 comctl32 shell32 winspool user32 wsock32 advapi32 gdi32 */

      cOpt_CompC := "-nologo -W3 {OPTC} -I{I} {C} -Fe{E}.exe /link /libpath:{A} {OPTL} {L}"
      IF s_lSHARED
           AAdd( s_aOPTL, "/libpath:{B}" )
      ENDIF
      s_aLIBSYS := ArrayJoin( s_aLIBSYS, { "user32", "wsock32", "advapi32", "gdi32" } )
      /* TOFIX: The two build systems should generate the same .dll name, otherwise
                we can only be compatible with one of them. non-GNU is the common choice here. */
      s_aLIBSHARED := { iif( s_lMT, "harbourmt-11-vc", "harbour-11-vc" ), "hbmainstd", "hbmainwin", "hbcommon" }

   CASE s_cARCH == "os2" .AND. s_cCOMP == "icc"
      cLibPrefix := "{A}\"
      cLibExt := ".lib"
      cObjExt := ".obj"
      cBin_CompC := "icc"
      cOpt_CompC := "/Gs+ /W2 /Se /Sd+ /Ti+ /C- /Tp {OPTC} -I{I} {C}"

   /* TODO */
   CASE s_cARCH == "win" .AND. s_cCOMP == "dm"
   CASE s_cARCH == "win" .AND. s_cCOMP == "icc"
   CASE s_cARCH == "win" .AND. s_cCOMP == "mingwce"
   CASE s_cARCH == "win" .AND. s_cCOMP == "msvcce"
   CASE s_cARCH == "win" .AND. s_cCOMP == "pocc"
   CASE s_cARCH == "win" .AND. s_cCOMP == "pocc64"
   CASE s_cARCH == "win" .AND. s_cCOMP == "poccce"
   CASE s_cARCH == "win" .AND. s_cCOMP == "rsxnt"
   CASE s_cARCH == "win" .AND. s_cCOMP == "xcc"
   ENDCASE

   IF s_lSHARED .AND. ! Empty( s_aLIBSHARED )
      s_aLIBHB := s_aLIBSHARED
   ELSE
      s_aLIBHB := ArrayAJoin( { aLIB_BASE1,;
                                aLIB_BASE_DEBUG,;
                                s_aLIBVM,;
                                iif( s_lNULRDD, aLIB_BASE_NULRDD, aLIB_BASE_RDD ),;
                                aLIB_BASE2 } )
   ENDIF

   /* Merge lib lists. */
   s_aLIB := ArrayAJoin( { s_aLIBHB, s_aLIBUSER, s_aLIB3RD, s_aLIBSYS } )
   /* Dress lib names. */
   s_aLIB := ListCook( s_aLIB, cLibPrefix, cLibExt )
   /* Dress obj names. */
   s_aOBJ := ListCook( ArrayJoin( s_aPRG, s_aC ), NIL, cObjExt )
   s_aOBJUSER := ListCook( s_aOBJUSER, NIL, cObjExt )

   nErrorLevel := 0

   IF ! Empty( cOpt_CompC )

      /* Compiling */

      cOpt_CompC := StrTran( cOpt_CompC, "{C}"   , ArrayToList( ArrayJoin( ListCook( s_aPRG, "", ".c" ), s_aC ) ) )
      cOpt_CompC := StrTran( cOpt_CompC, "{O}"   , ArrayToList( s_aOBJ ) )
      cOpt_CompC := StrTran( cOpt_CompC, "{OUSR}", ArrayToList( s_aOBJUSER ) )
      cOpt_CompC := StrTran( cOpt_CompC, "{L}"   , ArrayToList( s_aLIB ) )
      cOpt_CompC := StrTran( cOpt_CompC, "{OPTC}", GetEnv( "HB_USER_CFLAGS" ) + " " + ArrayToList( s_aOPTC ) )
      cOpt_CompC := StrTran( cOpt_CompC, "{OPTL}", GetEnv( "HB_USER_LDFLAGS" ) + " " + ArrayToList( s_aOPTL ) )
      cOpt_CompC := StrTran( cOpt_CompC, "{E}"   , s_cPROGNAME )
      cOpt_CompC := StrTran( cOpt_CompC, "{B}"   , s_cHB_BIN_INSTALL )
      cOpt_CompC := StrTran( cOpt_CompC, "{I}"   , s_cHB_INC_INSTALL )
      cOpt_CompC := StrTran( cOpt_CompC, "{A}"   , s_cHB_LIB_INSTALL )

      cOpt_CompC := AllTrim( cOpt_CompC )

      /* Handle moving the whole command line to a script, if requested. */
      IF "{SCRIPT}" $ cOpt_CompC
         fhnd := hb_FTempCreateEx( @cScriptFile )
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
         OutErr( "hbmk: C compiler command: '" + cCommand + "'" + hb_osNewLine() )
         IF ! Empty( cScriptFile )
            OutErr( "hbmk: C compiler script: '" + hb_MemoRead( cScriptFile ) + "'" + hb_osNewLine() )
         ENDIF
      ENDIF

      IF ( tmp := hb_run( cCommand ) ) != 0
         OutErr( "hbmk: Error: Running C compiler. " + hb_ntos( tmp ) + ": '" + cCommand + "'" + hb_osNewLine() )
         nErrorLevel := 6
      ENDIF

      IF ! Empty( cScriptFile )
         FErase( cScriptFile )
      ENDIF

      IF ! Empty( cOpt_Link ) .AND. nErrorLevel == 0

         /* Linking */

         cOpt_Link := StrTran( cOpt_Link, "{O}"   , ArrayToList( ArrayJoin( s_aOBJ, s_aOBJUSER ) ) )
         cOpt_Link := StrTran( cOpt_Link, "{L}"   , ArrayToList( s_aLIB ) )
         cOpt_Link := StrTran( cOpt_Link, "{OPTL}", GetEnv( "HB_USER_LDFLAGS" ) + " " + ArrayToList( s_aOPTL ) )
         cOpt_Link := StrTran( cOpt_Link, "{E}"   , s_cPROGNAME )
         cOpt_Link := StrTran( cOpt_Link, "{B}"   , s_cHB_BIN_INSTALL )
         cOpt_Link := StrTran( cOpt_Link, "{A}"   , s_cHB_LIB_INSTALL )

         cOpt_Link := AllTrim( cOpt_Link )

         /* Handle moving the whole command line to a script, if requested. */
         IF "{SCRIPT}" $ cOpt_Link
            fhnd := hb_FTempCreateEx( @cScriptFile )
            IF fhnd != F_ERROR
               FWrite( fhnd, StrTran( cOpt_CompC, "{SCRIPT}", "" ) )
               FClose( fhnd )
               cOpt_Link := "@" + cScriptFile
            ELSE
               OutErr( "hbmk: Warning: Link script couldn't be created, continuing in command line." + hb_osNewLine() )
            ENDIF
         ENDIF

         cCommand := cBin_Link + " " + cOpt_Link

         IF s_lTRACE
            OutErr( "hbmk: Linker command: '" + cCommand + "'" + hb_osNewLine() )
            IF ! Empty( cScriptFile )
               OutErr( "hbmk: Linker script: '" + hb_MemoRead( cScriptFile ) + "'" + hb_osNewLine() )
            ENDIF
         ENDIF

         IF ( tmp := hb_run( cCommand ) ) != 0
            OutErr( "hbmk: Error: Running linker. " + hb_ntos( tmp ) + ": '" + cCommand + "'" + hb_osNewLine() )
            nErrorLevel := 7
         ENDIF

         IF ! Empty( cScriptFile )
            FErase( cScriptFile )
         ENDIF
      ENDIF
   ELSE
      OutErr( "hbmk: Error: This compiler/platform isn't implemented." + hb_osNewLine() )
      nErrorLevel := 8
   ENDIF

   /* Cleanup */

   IF ! Empty( s_cGTPRG )
      FErase( s_cGTPRG )
   ENDIF
   AEval( ListCook( s_aPRG, NIL, ".c" ), {|tmp| FErase( tmp ) } )
   AEval( s_aOBJ, {|tmp| FErase( tmp ) } )
   AEval( s_aCLEAN, {|tmp| FErase( tmp ) } )

   IF nErrorLevel != 0
      PauseForKey()
   ENDIF

   RETURN nErrorLevel

STATIC FUNCTION FindInPath( cFileName )
   LOCAL cPATH
   LOCAL nCount
   LOCAL cDir
   LOCAL tmp

   IF hb_FileExists( cFileName )
      RETURN .T.
   ENDIF

   cPATH := GetEnv( "PATH" )
   IF ! Empty( cPATH )
      nCount := hb_TokenCount( cPATH, hb_osPathListSeparator() )

      FOR tmp := 1 TO nCount
         cDir := hb_TokenGet( cPATH, tmp, hb_osPathListSeparator() )
         IF ! Empty( cDir )
            IF Left( cDir, 1 ) == Chr( 34 ) .AND. Right( cDir, 1 ) == Chr( 34 )
               cDir := SubStr( cDir, 2, Len( cDir ) - 2 )
            ENDIF
            IF hb_FileExists( DirAddPathSep( cDir ) + cFileName )
               RETURN .T.
            ENDIF
         ENDIF
      NEXT
   ENDIF

   RETURN .F.

STATIC FUNCTION ArrayJoin( array1, array2 )
   LOCAL array := AClone( array1 )
   LOCAL nLen1 := Len( array )

   ASize( array, nLen1 + Len( array2 ) )

   RETURN ACopy( array2, array, , , nLen1 + 1 )

STATIC FUNCTION ArrayAJoin( arrays )
   LOCAL array := AClone( arrays[ 1 ] )
   LOCAL tmp
   LOCAL nLenArray := Len( arrays )
   LOCAL nLen
   LOCAL nPos := Len( array ) + 1

   nLen := 0
   FOR tmp := 1 TO nLenArray
      nLen += Len( arrays[ tmp ] )
   NEXT

   ASize( array, nLen )

   FOR tmp := 2 TO nLenArray
      ACopy( arrays[ tmp ], array, , , nPos )
      nPos += Len( arrays[ tmp ] )
   NEXT

   RETURN array

STATIC FUNCTION ListDelExt( array )
   LOCAL tmp

   FOR tmp := 1 TO Len( array )
      array[ tmp ] := DirNameGet( array[ tmp ] )
   NEXT

   RETURN array

/* Append optional prefix and optional extension to all members */
STATIC FUNCTION ListCook( array, cPrefix, cExt )
   LOCAL tmp

   DEFAULT cPrefix TO ""

   FOR tmp := 1 TO Len( array )
      array[ tmp ] := cPrefix + array[ tmp ]
      IF ISCHARACTER( cExt )
           array[ tmp ] := ExtSet( array[ tmp ], cExt )
      ENDIF
   NEXT

   RETURN array

STATIC FUNCTION ArrayToList( array )
   LOCAL cString := ""
   LOCAL tmp

   FOR tmp := 1 TO Len( array )
      cString += array[ tmp ]
      IF tmp < Len( array )
         cString += " "
      ENDIF
   NEXT

   RETURN cString

STATIC FUNCTION ListToArray( cList )
   LOCAL array := {}
   LOCAL nCount
   LOCAL item
   LOCAL tmp

   IF ! Empty( cList )
      nCount := hb_TokenCount( cList, " " )
      FOR tmp := 1 TO nCount
         item := hb_TokenGet( cList, tmp, " " )
         IF ! Empty( item )
            AAdd( array, item )
         ENDIF
      NEXT
   ENDIF

   RETURN array

STATIC FUNCTION DirAddPathSep( cDir )

   IF ! Empty( cDir ) .AND. !( Right( cDir, 1 ) == hb_osPathSeparator() )
      cDir += hb_osPathSeparator()
   ENDIF

   RETURN cDir

STATIC FUNCTION ExtSet( cFileName, cExt )
   LOCAL cDir, cName

   hb_FNameSplit( cFileName, @cDir, @cName )

   RETURN hb_FNameMerge( cDir, cName, cExt )

STATIC FUNCTION ExtGet( cFileName )
   LOCAL cExt

   hb_FNameSplit( cFileName, , , @cExt )

   RETURN cExt

STATIC FUNCTION DirNameGet( cFileName )
   LOCAL cDir, cName

   hb_FNameSplit( cFileName, @cDir, @cName )

   RETURN hb_FNameMerge( cDir, cName )

STATIC PROCEDURE ShowHeader()

   OutStd( "Harbour Make " + HBRawVersion() + hb_osNewLine() +;
           "Copyright (c) 2009, Viktor Szakats" + hb_osNewLine() +;
           "http://www.harbour-project.org/" + hb_osNewLine() +;
           hb_osNewLine() )

   RETURN

STATIC PROCEDURE HBP_ProcessAll( /* @ */ aLIBS,;
                                 /* @ */ aOPTPRG,;
                                 /* @ */ lGUI,;
                                 /* @ */ lMT,;
                                 /* @ */ lSHARED,;
                                 /* @ */ lDEBUG,;
                                 /* @ */ lNULRDD,;
                                 /* @ */ lSTRIP,;
                                 /* @ */ cGT )
   LOCAL aFiles := Directory( "*.hbp" )
   LOCAL aFile

   FOR EACH aFile IN aFiles
      OutStd( "hbmk: Processing: " + aFile[ F_NAME ] + hb_osNewLine() )
      HBP_ProcessOne( hb_MemoRead( aFile[ F_NAME ] ),;
         @aLIBS,;
         @aOPTPRG,;
         @lGUI,;
         @lMT,;
         @lSHARED,;
         @lDEBUG,;
         @lNULRDD,;
         @lSTRIP,;
         @cGT )
   NEXT

   RETURN

#define _EOL          Chr( 10 )

STATIC PROCEDURE HBP_ProcessOne( cFile,;
                                 /* @ */ aLIBS,;
                                 /* @ */ aOPTPRG,;
                                 /* @ */ lGUI,;
                                 /* @ */ lMT,;
                                 /* @ */ lSHARED,;
                                 /* @ */ lDEBUG,;
                                 /* @ */ lNULRDD,;
                                 /* @ */ lSTRIP,;
                                 /* @ */ cGT )
   LOCAL aLines
   LOCAL cLine
   LOCAL aList
   LOCAL cItem

   IF ! hb_osNewLine() == _EOL
      cFile := StrTran( cFile, hb_osNewLine(), _EOL )
   ENDIF
   IF ! hb_osNewLine() == Chr( 13 ) + Chr( 10 )
      cFile := StrTran( cFile, Chr( 13 ) + Chr( 10 ), _EOL )
   ENDIF
   aLines := hb_ATokens( cFile, _EOL )

   FOR EACH cLine IN aLines

      cLine := AllTrim( cLine )

      DO CASE
      CASE Lower( Left( cLine, Len( "libs="     ) ) ) == "libs="     ; cLine := SubStr( cLine, Len( "libs="     ) + 1 )
         aList := hb_ATokens( cLine, " " )

         FOR EACH cItem IN aList
            IF AScan( aLIBS, {| tmp | tmp == cItem } ) == 0
               AAdd( aLIBS, cItem )
            ENDIF
         NEXT

      CASE Lower( Left( cLine, Len( "prgflags=" ) ) ) == "prgflags=" ; cLine := SubStr( cLine, Len( "prgflags=" ) + 1 )
         aList := hb_ATokens( cLine, " " )

         FOR EACH cItem IN aList
            IF AScan( aOPTPRG, {| tmp | tmp == cItem } ) == 0
               AAdd( aOPTPRG, cItem )
            ENDIF
         NEXT

      CASE Lower( Left( cLine, Len( "gui="      ) ) ) == "gui="      ; cLine := SubStr( cLine, Len( "gui="      ) + 1 )
         DO CASE
         CASE Lower( cLine ) == "yes" ; lGUI := .T.
         CASE Lower( cLine ) == "no"  ; lGUI := .F.
         ENDCASE

      CASE Lower( Left( cLine, Len( "mt="       ) ) ) == "mt="       ; cLine := SubStr( cLine, Len( "mt="       ) + 1 )
         DO CASE
         CASE Lower( cLine ) == "yes" ; lMT := .T.
         CASE Lower( cLine ) == "no"  ; lMT := .F.
         ENDCASE

      CASE Lower( Left( cLine, Len( "shared="   ) ) ) == "shared="   ; cLine := SubStr( cLine, Len( "shared="   ) + 1 )
         DO CASE
         CASE Lower( cLine ) == "yes" ; lSHARED := .T.
         CASE Lower( cLine ) == "no"  ; lSHARED := .F.
         ENDCASE

      CASE Lower( Left( cLine, Len( "debug="    ) ) ) == "debug="    ; cLine := SubStr( cLine, Len( "debug="    ) + 1 )
         DO CASE
         CASE Lower( cLine ) == "yes" ; lDEBUG := .T.
         CASE Lower( cLine ) == "no"  ; lDEBUG := .F.
         ENDCASE

      CASE Lower( Left( cLine, Len( "nulrdd="   ) ) ) == "nulrdd="   ; cLine := SubStr( cLine, Len( "nulrdd="   ) + 1 )
         DO CASE
         CASE Lower( cLine ) == "yes" ; lNULRDD := .T.
         CASE Lower( cLine ) == "no"  ; lNULRDD := .F.
         ENDCASE

      CASE Lower( Left( cLine, Len( "strip="    ) ) ) == "strip="    ; cLine := SubStr( cLine, Len( "strip="    ) + 1 )
         DO CASE
         CASE Lower( cLine ) == "yes" ; lSTRIP := .T.
         CASE Lower( cLine ) == "no"  ; lSTRIP := .F.
         ENDCASE

      CASE Lower( Left( cLine, Len( "gt="       ) ) ) == "gt="       ; cLine := SubStr( cLine, Len( "gt="       ) + 1 )
         DEFAULT cGT TO cLine

      ENDCASE
   NEXT

   RETURN

STATIC PROCEDURE HBM_Load( aParams, cFileName )
   LOCAL cFile := hb_MemoRead( cFileName )
   LOCAL aLines
   LOCAL cLine
   LOCAL aOptions
   LOCAL cOption

   IF ! hb_osNewLine() == _EOL
      cFile := StrTran( cFile, hb_osNewLine(), _EOL )
   ENDIF
   IF ! hb_osNewLine() == Chr( 13 ) + Chr( 10 )
      cFile := StrTran( cFile, Chr( 13 ) + Chr( 10 ), _EOL )
   ENDIF
   aLines := hb_ATokens( cFile, _EOL )

   FOR EACH cLine IN aLines
      aOptions := hb_ATokens( cLine, " " )
      FOR EACH cOption IN aOptions
         IF ! Empty( cOption )
            AAdd( aParams, cOption )
         ENDIF
      NEXT
   NEXT

   RETURN

STATIC PROCEDURE PauseForKey()

   IF hb_gtInfo( HB_GTI_ISGRAPHIC )
      OutStd( "Press any key to continue..." )
      Inkey( 0 )
   ENDIF

   RETURN

STATIC PROCEDURE ShowHelp()

   LOCAL aText := {;
      "Usage: hbmk [options] [@<script>] <source[s][.prg|.c]> [-l<lib>] [-o<obj>]" ,;
      "" ,;
      "Options:" ,;
      "  -o<outputfilename>  output file name" ,;
      "  -l<libname>         link with <libname> library" ,;
      "  -L<libpath>         additional path to search for libraries" ,;
      "  -static|-shared     link with static/shared libs" ,;
      "  -mt|-st             link with multi-thread/single-thread libs" ,;
      "  -gui|-std           create GUI/console executable" ,;
      "  -gt<name>           link with GT<name> GT driver, can be repeated to" ,;
      "                      link with more GTs. The first one will be" ,;
      "                      the default at runtime" ,;
      "  -nulrdd             link with nulrdd" ,;
      "  -debug|-nodebug     add/exclude debug info" ,;
      "  -strip|-nostrip     strip (no strip) binaries" ,;
      "  -trace              show commands executed" ,;
      "  -nohbp              do not process .hbp files" ,;
      "" ,;
      "Notes:" ,;
      "" ,;
      "  - Don't forget to create a MAIN() function in your application." ,;
      "  - Multiple -l, -o, @ parameters are accepted." ,;
      "  - .hbp option files in current dir are automatically processed." ,;
      "  - .hbp options (they should come in separate lines):" ,;
      "    libs=[<libname[s]>], prgflags=[Harbour options]," ,;
      "    gui=[yes|no], mt=[yes|no], shared=[yes|no], gt=[yes|no]" ,;
      "    debug=[yes|no], nulrdd=[yes|no], strip=[yes|no]" ,;
      "  - Defaults and feature support vary by architecture/compiler." ,;
      "" ,;
      "    HB_COMPILER values supported for each HB_ARCHITECURE:" ,;
      "" ,;
      "    bsd, darwin, hbupx, sunos: gcc",;
      "    linux                    : gcc, owatcom" ,;
      "    win                      : gcc, mingw, msvc, bcc32, owatcom, pocc, dmc" ,;
      "    os2                      : gcc, owatcom, icc" ,;
      "    dos                      : gcc, djgpp, owatcom, rsx32" }

   AEval( aText, {|tmp| OutStd( tmp + hb_osNewLine() ) } )

   RETURN

STATIC FUNCTION HBRawVersion()
   RETURN StrTran( Version(), "Harbour ", "" )
