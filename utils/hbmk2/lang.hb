/*
 * Generates .md help files for all languages
 *
 * Copyright 2013 Viktor Szakats (harbour syenar.net)
 * www - http://harbour-project.org
 *
 * Requires: Harbour in PATH
 *
 */

#pragma -w3

#include "directry.ch"

PROCEDURE Main()

   LOCAL cBase := hb_DirBase()

   LOCAL file
   LOCAL cLang
   LOCAL cTemp

   LOCAL cMain := cBase + "hbmk2.prg"  /* must be runnable as script */
   LOCAL cDocOptions := "-lang={LANG} -longhelpmd"
   LOCAL cBaseLang := "en"
   LOCAL cPO_Dir := cBase + hb_DirSepToOS( "po/" )
   LOCAL cDoc_Dir := cBase + hb_DirSepToOS( "doc/" )

   cTemp := hb_FNameExtSet( cMain, ".hrb" )

   ? "generating .md help:"

   hb_run( hb_StrFormat( "hbmk2 -hbraw -q0 %1$s -gh -o%2$s", cMain, cTemp ) )

   FOR EACH cLang IN hb_ATokens( cBaseLang + "," + hb_regexAll( "-lng=([a-zA-Z0-9_\-,]*)", hb_MemoRead( hb_FNameExtSet( cMain, ".hbp" ) ),,,,, .T. )[ 1 ][ 2 ], "," )

      ?? "", cLang

      hb_run( hb_StrFormat( "hbi18n -q -g %1$s -o%2$s", ;
         cPO_Dir + hb_FNameName( cMain ) + "." + cLang + ".po", ;
         hb_FNameDir( cMain ) + hb_FNameName( cMain ) + "." + cLang + ".hbl" ) )

      file := cDoc_Dir + hb_FNameName( cMain ) + "." + cLang + ".md"
      hb_run( hb_StrFormat( "hbrun %1$s %2$s > %3$s", cTemp, StrTran( cDocOptions, "{LANG}", cLang ), file ) )
      FToNativeEOL( file )

      /* special case */
      IF hb_FNameName( cMain ) == "hbmk2"
         file := cBase + hb_DirSepToOS( "../../contrib/hbrun/doc/" ) + "hbrun" + "." + cLang + ".md"
         hb_run( hb_StrFormat( "hbrun %1$s -lang=%2$s -longhelpmdsh > %3$s", cTemp, cLang, file ) )
         FToNativeEOL( file )
      ENDIF

      FErase( hb_FNameDir( cMain ) + hb_FNameName( cMain ) + "." + cLang + ".hbl" )
   NEXT

   FErase( cTemp )

   RETURN

STATIC FUNCTION FToNativeEOL( cFile )
   RETURN hb_MemoWrit( cFile, StrTran( hb_MemoRead( cFile ), e"\n", hb_eol() ) )
