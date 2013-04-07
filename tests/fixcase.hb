/*
 * Function naming casing fixer
 *
 * The script takes proper casing from .hbx files
 * and applies it to whole source tree. (except
 * C sources and some certain files)
 *
 * BEWARE: ugly code
 *
 * Copyright 2012 Viktor Szakats (harbour syenar.net)
 * www - http://harbour-project.org
 *
 */

#pragma -w3
#pragma -km+
#pragma -ko+

#include "directry.ch"
#include "simpleio.ch"

PROCEDURE Main( cFile )

   LOCAL aFile
   LOCAL cExt

   LOCAL hAll := { => }

   STATIC sc_hExtExceptions := { ;
      ".dll"   =>, ;
      ".dxe"   =>, ;
      ".dylib" =>, ;
      ".so"    =>, ;
      ".sl"    =>, ;
      ".zip"  =>, ;
      ".7z"   =>, ;
      ".exe"  =>, ;
      ".o"    =>, ;
      ".obj"  =>, ;
      ".js"   =>, ;
      ".dif"  =>, ;
      ".exe"  =>, ;
      ".y"    =>, ;
      ".yyc"  =>, ;
      ".yyh"  =>, ;
      ".a"    =>, ;
      ".afm"  =>, ;
      ".bmp"  =>, ;
      ".dat"  =>, ;
      ".dbf"  =>, ;
      ".exe"  =>, ;
      ".frm"  =>, ;
      ".gif"  =>, ;
      ".icns" =>, ;
      ".ico"  =>, ;
      ".jpg"  =>, ;
      ".lbl"  =>, ;
      ".lib"  =>, ;
      ".mdb"  =>, ;
      ".ng"   =>, ;
      ".odt"  =>, ;
      ".pdf"  =>, ;
      ".pfb"  =>, ;
      ".png"  =>, ;
      ".sq3"  =>, ;
      ".tif"  => }

   STATIC sc_hFileExceptions := { ;
      "ChangeLog.txt" =>, ;
      "std.ch"        =>, ;
      "wcecon.prg"    =>, ;
      "uc16_gen.prg"  =>, ;
      "clsscope.prg"  =>, ;
      "speedstr.prg"  =>, ;
      "cpinfo.prg"    =>, ;
      "clsccast.prg"  =>, ;
      "clsicast.prg"  =>, ;
      "clsscast.prg"  =>, ;
      "big5_gen.prg"  =>, ;
      "foreach2.prg"  =>, ;
      "speedtst.prg"  =>, ;
      "keywords.prg"  =>, ;
      "xhb-diff.txt"  =>, ;
      "pp.txt"        =>, ;
      "locks.txt"     =>, ;
      "oldnews.txt"   =>, ;
      "news.html"     =>, ;
      "news1.html"    =>, ;
      "c_std.txt"     =>, ;
      "tracing.txt"   =>, ;
      "pcode.txt"     => }

   STATIC sc_aMaskExceptions := { ;
      "src/3rd/*"                  , ;
      "contrib/3rd/*"              , ;
      "contrib/*/3rd/*"            , ;
      "contrib/xhb/thtm.prg"       , ;
      "contrib/hbnetio/tests/*"    , ;
      "extras/httpsrv/home/*"      , ;
      "tests/hbpptest/*"           , ;
      "tests/mt/*"                 , ;
      "tests/multifnc/*"           , ;
      "tests/rddtest/*"            }

   hb_cdpSelect( "EN" )

   hb_HCaseMatch( hAll, .F. )

   __hbformat_BuildListOfFunctions( hAll )

   IF HB_ISSTRING( cFile )
      ProcFile( hAll, cFile )
   ELSE
      FOR EACH aFile IN hb_DirScan( "", hb_osFileMask() )
         cExt := hb_FNameExt( aFile[ F_NAME ] )
         IF ! Empty( cExt ) .AND. ;
            !( cExt $ sc_hExtExceptions ) .AND. ;
            !( hb_FNameNameExt( aFile[ F_NAME ] ) $ sc_hFileExceptions ) .AND. ;
            AScan( sc_aMaskExceptions, {| tmp | hb_FileMatch( StrTran( aFile[ F_NAME ], "\", "/" ), tmp ) } ) == 0
            ProcFile( hAll, aFile[ F_NAME ] )
         ENDIF
      NEXT
   ENDIF

   RETURN

STATIC PROCEDURE ProcFile( hAll, cFileName )

   STATIC sc_hPartial := { ;
      ".c"    =>, ;
      ".cpp"  =>, ;
      ".h"    =>, ;
      ".api"  => }

   LOCAL cFile := MemoRead( cFileName )
   LOCAL cFileStripped

   LOCAL match
   LOCAL cProper

   LOCAL lPartial
   LOCAL nChanged := 0

   lPartial := hb_FNameExt( cFileName ) $ sc_hPartial

   IF lPartial
      cFileStripped := GetCComments( cFile )
   ELSE
      cFileStripped := cFile
   ENDIF

   #define _MATCH_cStr    1
   #define _MATCH_nStart  2
   #define _MATCH_nEnd    3

   FOR EACH match IN hb_regexAll( "([A-Za-z] |[^A-Za-z_:]|^)([A-Za-z_][A-Za-z0-9_]+\()", cFileStripped,,,,, .F. )
      IF Len( match[ 2 ][ _MATCH_cStr ] ) != 2 .OR. !( Left( match[ 2 ][ _MATCH_cStr ], 1 ) $ "D" /* "METHOD" */ )
         cProper := ProperCase( hAll, hb_StrShrink( match[ 3 ][ _MATCH_cStr ] ) ) + "("
         IF !( cProper == match[ 3 ][ _MATCH_cStr ] ) .AND. ;
            !( Upper( cProper ) == Upper( "FILE(" ) ) .AND. ;   /* interacts with "file(s)" text */
            !( Upper( cProper ) == Upper( "TOKEN(" ) ) .AND. ;  /* interacts with "token(s)" text */
            !( Upper( cProper ) == Upper( "INT(" ) ) .AND. ;    /* interacts with SQL statements */
            ( ! lPartial .OR. !( "|" + Lower( cProper ) + "|" $ Lower( "|Max(|Min(|FOpen(|Abs(|Log10(|GetEnv(|Sqrt(|Rand(|IsDigit(|IsAlpha(|" ) ) )
            cFile := Left( cFile, match[ 3 ][ _MATCH_nStart ] - 1 ) + cProper + SubStr( cFile, match[ 3 ][ _MATCH_nEnd ] + 1 )
            ? cFileName, match[ 3 ][ _MATCH_cStr ], cProper, "|" + match[ 1 ][ _MATCH_cStr ] + "|"
            nChanged++
         ENDIF
      ENDIF
   NEXT

   IF !( "hbclass.ch" $ cFileName ) .AND. ! lPartial
      FOR EACH match IN hb_regexAll( "(?:REQUEST|EXTERNAL|EXTERNA|EXTERN)[ \t]+([A-Za-z_][A-Za-z0-9_]+)", cFile,,,,, .F. )
         cProper := ProperCase( hAll, match[ 2 ][ _MATCH_cStr ] )
         IF !( cProper == match[ 2 ][ _MATCH_cStr ] )
            cFile := Left( cFile, match[ 2 ][ _MATCH_nStart ] - 1 ) + cProper + SubStr( cFile, match[ 2 ][ _MATCH_nEnd ] + 1 )
            OutStd( cFileName, match[ 2 ][ _MATCH_cStr ], cProper, "|" + match[ 1 ][ _MATCH_cStr ] + "|" + hb_eol() )
            nChanged++
         ENDIF
      NEXT
   ENDIF

   IF nChanged > 0
      ? cFileName, "changed: ", nChanged
      hb_MemoWrit( cFileName, cFile )
   ENDIF

   RETURN

STATIC FUNCTION ProperCase( hAll, cName )

   IF cName $ hAll
      RETURN hb_HKeyAt( hAll, hb_HPos( hAll, cName ) )
   ENDIF

   RETURN cName

STATIC PROCEDURE __hbformat_BuildListOfFunctions( hFunctions )

   WalkDir( hb_DirBase() + ".." + hb_ps() + "include", hFunctions )
   WalkDir( hb_DirBase() + ".." + hb_ps() + "contrib", hFunctions )
   WalkDir( hb_DirBase() + ".." + hb_ps() + "extras", hFunctions )

   RETURN

STATIC PROCEDURE WalkDir( cDir, hFunctions )

   LOCAL aFile

   cDir := hb_DirSepAdd( cDir )

   FOR EACH aFile IN hb_DirScan( cDir, "*.hbx" )
      HBXToFuncList( hFunctions, hb_MemoRead( cDir + aFile[ F_NAME ] ) )
   NEXT

   RETURN

STATIC PROCEDURE HBXToFuncList( hFunctions, cHBX )
   LOCAL cLine

   FOR EACH cLine IN hb_ATokens( StrTran( cHBX, Chr( 13 ) ), Chr( 10 ) )
      IF Left( cLine, Len( "DYNAMIC " ) ) == "DYNAMIC "
         hFunctions[ SubStr( cLine, Len( "DYNAMIC " ) + 1 ) ] := NIL
      ENDIF
   NEXT

   RETURN

/* retains positions in file */
STATIC FUNCTION GetCComments( cFile )

   LOCAL nPos := 1
   LOCAL aHits := {}
   LOCAL tmp
   LOCAL tmp1
   LOCAL lStart := .T.

   LOCAL cComments

   /* bare bones */
   DO WHILE ( tmp := hb_BAt( iif( lStart, "/*", "*/" ), cFile, nPos ) ) > 0
      AAdd( aHits, tmp + iif( lStart, 0, 2 ) )
      nPos := tmp
      lStart := ! lStart
   ENDDO

   /* unbalanced */
   IF Len( aHits ) % 2 != 0
      AAdd( aHits, hb_BLen( cFile ) )
   ENDIF

   cComments := Space( hb_BLen( cFile ) )

   FOR tmp := 1 TO Len( aHits ) STEP 2
      FOR tmp1 := aHits[ tmp ] TO aHits[ tmp + 1 ]
         hb_BPoke( @cComments, tmp1, hb_BPeek( cFile, tmp1 ) )
      NEXT
   NEXT

   RETURN cComments
