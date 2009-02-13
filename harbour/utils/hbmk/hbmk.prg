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

FUNCTION Main()

   /* Add missing libs */
   LOCAL aLIB_BASE := {;
      "hbcpage" ,;
      "hbdebug" ,;
      "hbrtl" ,;
      "gtcgi" ,;
      "gtstd" ,;
      "gtpca" ,;
      "hblang" ,;
      "hbrdd" ,;
      "hbmacro" ,;
      "hbpp" ,;
      "hbhsx" ,;
      "hbsix" ,;
      "hbcommon" ,;
      "hbpcre" ,;
      "hbzlib" ,;
      "hbuddall" ,;
      "hbusrrdd" ,;
      "rddntx" ,;
      "rddnsx" ,;
      "rddcdx" ,;
      "rddfpt" }

   LOCAL aLIB_BASE_ST     := { "hbvm" }
   LOCAL aLIB_BASE_MT     := { "hbvmmt" }

   LOCAL aLIB_BASE_bsd    := { "gttrm", "gtxwc" }
   LOCAL aLIB_BASE_darwin := { "gttrm", "gtxwc" }
   LOCAL aLIB_BASE_dos    := { "gtdos" }
   LOCAL aLIB_BASE_hpux   := { "gttrm", "gtxwc" }
   LOCAL aLIB_BASE_linux  := { "gttrm", "gtxwc" }
   LOCAL aLIB_BASE_os2    := { "gtos2" }
   LOCAL aLIB_BASE_sunos  := { "gttrm", "gtxwc" }
   LOCAL aLIB_BASE_win    := { "gtwin", "gtwvt" }

   LOCAL s_COMP
   LOCAL s_ARCH

   LOCAL s_HB_INSTALL_PREFIX
   LOCAL s_HB_BIN_INSTALL
   LOCAL s_HB_LIB_INSTALL
   LOCAL s_HB_INC_INSTALL

   LOCAL s_aPRG
   LOCAL s_aC
   LOCAL s_aLIB
   LOCAL s_aOPT
   LOCAL s_cPROGNAME

   LOCAL s_lGUI
   LOCAL s_lMT
   LOCAL s_lSHARED
   LOCAL s_lDEBUG

   LOCAL s_BASEDIR

   LOCAL aCOMPDET
   LOCAL aCOMPSUP

   LOCAL cLibPrefix
   LOCAL cLibExt

   LOCAL cCommand
   LOCAL tmp

   ShowHeader()

   IF PCount() == 0
      ShowHelp()
      RETURN 9
   ENDIF

   s_BASEDIR := hb_DirBase()

   IF Empty( GetEnv( "HB_INSTALL_PREFIX" ) )
      DO CASE
      CASE File( "harbour.*" )           ; s_HB_INSTALL_PREFIX := DirAddPathSep( hb_DirBase() ) + ".."
      CASE File( "bin\harbour.*" )       ; s_HB_INSTALL_PREFIX := DirAddPathSep( hb_DirBase() )
      CASE File( "..\bin\harbour.*" )    ; s_HB_INSTALL_PREFIX := DirAddPathSep( hb_DirBase() ) + ".."
      CASE File( "..\..\bin\harbour.*" ) ; s_HB_INSTALL_PREFIX := DirAddPathSep( hb_DirBase() ) + ".." + hb_osPathSeparator() + ".."
      OTHERWISE
         OutErr( "Error: HB_INSTALL_PREFIX not set, failed to autodetect." + hb_osNewLine() )
         RETURN 1
      ENDCASE
   ELSE
      s_HB_INSTALL_PREFIX := GetEnv( "HB_INSTALL_PREFIX" )
   ENDIF
   IF Empty( GetEnv( "HB_BIN_INSTALL" ) )
      s_HB_BIN_INSTALL := DirAddPathSep( s_HB_INSTALL_PREFIX ) + "bin"
   ENDIF
   IF Empty( GetEnv( "HB_LIB_INSTALL" ) )
      s_HB_LIB_INSTALL := DirAddPathSep( s_HB_INSTALL_PREFIX ) + "lib"
   ENDIF
   IF Empty( GetEnv( "HB_INC_INSTALL" ) )
      s_HB_INC_INSTALL := DirAddPathSep( s_HB_INSTALL_PREFIX ) + "include"
   ENDIF

   s_ARCH := GetEnv( "HB_ARCHITECTURE" )
   IF Empty( GetEnv( "HB_ARCHITECTURE" ) )
#if defined( __PLATFORM__BSD )
      s_ARCH := "bsd"
#elif defined( __PLATFORM__DARWIN )
      s_ARCH := "darwin"
#elif defined( __PLATFORM__DOS )
      s_ARCH := "dos"
#elif defined( __PLATFORM__HPUX )
      s_ARCH := "hpux"
#elif defined( __PLATFORM__LINUX )
      s_ARCH := "linux"
#elif defined( __PLATFORM__OS2 )
      s_ARCH := "os2"
#elif defined( __PLATFORM__SUNOS )
      s_ARCH := "sunos"
#elif defined( __PLATFORM__WINDOWS )
      s_ARCH := "win"
#endif
      OutStd( "Autodetected HB_ARCHITECTURE: " + s_ARCH + hb_osNewLine() )
   ENDIF

   DO CASE
   CASE s_arch == "bsd"
      aCOMPSUP := { "gcc" }
      s_lSHARED := .T.
   CASE s_arch == "darwin"
      aCOMPSUP := { "gcc" }
      s_lSHARED := .F.
   CASE s_arch == "dos"
      aCOMPDET := { { "gcc.exe", "djgpp" } }
      aCOMPSUP := { "bcc16", "djgpp", "owatcom", "rsx32" }
      s_lSHARED := .F.
   CASE s_arch == "hpux"
      aCOMPSUP := { "gcc" }
      s_lSHARED := .T.
   CASE s_arch == "linux"
      aCOMPSUP := { "gcc", "gpp", "owatcom" }
      s_lSHARED := .T.
   CASE s_arch == "os2"
      aCOMPSUP := { "gcc", "icc" }
      s_lSHARED := .F.
   CASE s_arch == "sunos"
      aCOMPSUP := { "gcc" }
      s_lSHARED := .T.
   CASE s_arch == "win"
      aCOMPDET := { { "gcc.exe"   , "mingw"   },;
                    { "cl.exe"    , "msvc"    },;
                    { "bcc32.exe" , "bcc32"   },;
                    { "wpp386.exe", "owatcom" } }
      aCOMPSUP := { "bcc32", "dm", "gcc", "icc", "mingw", "mingwce", "msvc", "msvcce", "owatcom", "pocc", "pocc64", "poccce", "rsxnt", "xcc" }
      s_lSHARED := .F.
   OTHERWISE
      OutErr( "Error: Architecture not properly set." + hb_osNewLine() )
      RETURN 1
   ENDCASE

   s_COMP := GetEnv( "HB_COMPILER" )
   IF Empty( GetEnv( "HB_COMPILER" ) )
      FOR tmp := 1 TO Len( aCOMPDET )
         IF FindInPath( aCOMPDET[ tmp ][ 1 ] )
            s_COMP := aCOMPDET[ tmp ][ 2 ]
            EXIT
         ENDIF
      NEXT
      OutStd( "Autodetected HB_COMPILER: " + s_COMP + hb_osNewLine() )
   ENDIF

   IF AScan( aCOMPSUP, {|tmp| tmp == s_COMP } ) == 0
      OutErr( "Error: Compiler not properly set." + hb_osNewLine() )
      RETURN 2
   ENDIF

   IF Lower( GetEnv( "GUI"    ) ) == "yes" ; s_lGUI    := .T. ; ENDIF
   IF Lower( GetEnv( "MT"     ) ) == "yes" ; s_lMT     := .T. ; ENDIF
   IF Lower( GetEnv( "SHARED" ) ) == "yes" ; s_lSHARED := .T. ; ENDIF
   IF Lower( GetEnv( "DEBUG"  ) ) == "yes" ; s_lDEBUG  := .T. ; ENDIF

   s_aPRG := {}
   s_aC   := {}
   s_aLIB := {}
   s_aOPT := {}
   s_cPROGNAME := NIL

   FOR tmp := 1 TO PCount()

      DO CASE
      CASE Lower( hb_PValue( tmp ) ) == "-gui"    ; s_lGUI    := .T.
      CASE Lower( hb_PValue( tmp ) ) == "-mt"     ; s_lMT     := .T.
      CASE Lower( hb_PValue( tmp ) ) == "-shared" ; s_lSHARED := .T.
      CASE Lower( hb_PValue( tmp ) ) == "-static" ; s_lSHARED := .F.
      CASE Lower( hb_PValue( tmp ) ) == "-debug"  ; s_lDEBUG  := .T.
      CASE Left( hb_PValue( tmp ), 1 ) == "-"              ; AAdd( s_aOPT, hb_PValue( tmp ) )
      CASE Left( hb_PValue( tmp ), 2 ) == "-l"             ; AAdd( s_aLIB, SubStr( hb_PValue( tmp ), 2 ) )
      CASE Lower( ExtGet( hb_PValue( tmp ) ) ) == ".prg"   ; AAdd( s_aPRG, hb_PValue( tmp ) ) ; DEFAULT s_cPROGNAME TO hb_PValue( tmp )
      CASE Lower( ExtGet( hb_PValue( tmp ) ) ) $ ".c|.cpp" ; AAdd( s_aC  , hb_PValue( tmp ) ) ; DEFAULT s_cPROGNAME TO hb_PValue( tmp )
      CASE Lower( ExtGet( hb_PValue( tmp ) ) ) $ ".lib|.a" ; AAdd( s_aLIB, hb_PValue( tmp ) )
      OTHERWISE                                            ; AAdd( s_aPRG, hb_PValue( tmp ) ) ; DEFAULT s_cPROGNAME TO hb_PValue( tmp )
      ENDCASE
   NEXT

   IF ( Len( s_aPRG ) + Len( s_aC ) ) == 0
      OutErr( "Error: No source files were specified." + hb_osNewLine() )
      RETURN 3
   ENDIF

   s_cPROGNAME := DirNameGet( s_cPROGNAME )

   /* Harbour */

   IF Len( s_aPRG ) > 0

      cCommand := DirAddPathSep( s_HB_BIN_INSTALL ) +;
                  "harbour" +;
                  " " + ArrayToList( s_aPRG ) +;
                  " -n -q0" +;
                  " -i" + s_HB_INC_INSTALL +;
                  iif( ! Empty( GetEnv( "HB_USER_PRGFLAGS" ) ), " " + GetEnv( "HB_USER_PRGFLAGS" ), "" ) +;
                  iif( ! Empty( s_aOPT ), " " + ArrayToList( s_aOPT ), "" )

      IF ( tmp := hb_run( cCommand ) ) != 0
         OutErr( "Error: Running Harbour compiler. " + hb_ntos( tmp ) + " '" + cCommand + "'" + hb_osNewLine() )
         RETURN 4
      ENDIF
   ENDIF

   /* C */

   DO CASE
   CASE s_arch == "bsd"
   CASE s_arch == "darwin"
   CASE s_arch == "dos"
   CASE s_arch == "hpux"
   CASE s_arch == "linux"
   CASE s_arch == "os2"
   CASE s_arch == "sunos"
   CASE s_arch == "win"
   ENDCASE

   /* Cleanup */

   RETURN 0

STATIC FUNCTION FindInPath( cFileName )
   LOCAL cPATH
   LOCAL nCount
   LOCAL cDir
   LOCAL tmp

   IF hb_FileExists( cFileName )
      RETURN .T.
   ENDIF

   cPATH := GetEnv( "PATH" )
   nCount := hb_TokenCount( cPATH, hb_osPathListSeparator() )

   FOR tmp := 1 TO nCount
      cDir := hb_TokenGet( cPATH, tmp, hb_osPathListSeparator() )
      IF Left( cDir, 1 ) == Chr( 34 ) .AND. Right( cDir, 1 ) == Chr( 34 )
         cDir := SubStr( cDir, 2, Len( cDir ) - 2 )
      ENDIF
      IF hb_FileExists( DirAddPathSep( cDir ) + cFileName )
         RETURN .T.
      ENDIF
   NEXT

   RETURN .F.

STATIC FUNCTION ArrayToList( aArray )
   LOCAL cString := ""
   LOCAL tmp

   FOR tmp := 1 TO Len( aArray )
      cString += aArray[ tmp ]
      IF tmp < Len( aArray )
         cString += " "
      ENDIF
   NEXT

   RETURN cString

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

STATIC PROCEDURE ShowHelp()

   LOCAL aText := {;
      "Usage: hbmk [-mt] [-gui] [-shared] [-debug] <filename[s][.prg|.c]>" ,;
      "" ,;
      "Notes:" ,;
      "" ,;
      "  - 'filename' is the .prg filename (without extension on pre-NT systems)." ,;
      "  - Don't forget to create a MAIN() function in your application." ,;
      "  - Environment variables HB_ARCHITECTURE, HB_COMPILER must be set." ,;
      "    The following values are currently supported:" ,;
      "" ,;
      "    HB_ARCHITECTURE:" ,;
      "      - dos" ,;
      "      - win" ,;
      "      - os2" ,;
      "" ,;
      "    HB_COMPILER:" ,;
      "      - When HB_ARCHITECTURE=win" ,;
      "        - msvc    (Microsoft Visual C++, Windows 32/64-bit)" ,;
      "        - mingw   (MinGW GNU C, Windows 32-bit)" ,;
      "        - gcc     (Cygnus/Cygwin GNU C, Windows 32-bit)" ,;
      "        - bcc32   (Borland C++ 4.x, 5.x, 6.x, Windows 32-bit)" ,;
      "        - owatcom (OpenWatcom, Windows 32-bit)" ,;
      "        - rxsnt   (EMX/RSXNT/Windows GNU C, Windows 32-bit)" ,;
      "      - When HB_ARCHITECTURE=os2" ,;
      "        - gcc     (EMX GNU C, OS/2 32-bit)" ,;
      "        - icc     (IBM Visual Age C++ 3.0, OS/2 32-bit)" ,;
      "        - owatcom (OpenWatcom, OS/2 32-bit)" ,;
      "      - When HB_ARCHITECTURE=dos" ,;
      "        - djgpp   (Delorie GNU C, DOS 32-bit)" ,;
      "        - owatcom (OpenWatcom, DOS 32-bit)" ,;
      "        - rxs32   (EMX/RSXNT/DOS GNU C, DOS 32-bit)" }

   AEval( aText, {|tmp| OutStd( tmp + hb_osNewLine() ) } )

   RETURN

STATIC FUNCTION HBRawVersion()
   RETURN StrTran( Version(), "Harbour ", "" )
