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

   ? "preparing"

   FOR EACH file IN Directory( cPO_Dir + "*.po" )
      hb_run( hb_StrFormat( "hbi18n -q -g -o%1$s %2$s", ;
         hb_FNameDir( cMain ) + hb_FNameName( file[ F_NAME ] ) + ".hbl", ;
         cPO_Dir + file[ F_NAME ] ) )
   NEXT

   cTemp := hb_FNameExtSet( cMain, ".hrb" )

   hb_run( hb_StrFormat( "hbmk2 -hbraw -q0 %1$s -gh -o%2$s", cMain, cTemp ) )

   ? "generating .md help:"

   FOR EACH cLang IN hb_ATokens( cBaseLang + "," + hb_regexAll( "-lng=([a-zA-Z0-9_\-,]*)", hb_MemoRead( hb_FNameExtSet( cMain, ".hbp" ) ),,,,, .T. )[ 1 ][ 2 ], "," )

      ?? "", cLang

      file := cDoc_Dir + hb_FNameName( cMain ) + "." + cLang + ".md"
      hb_run( hb_StrFormat( "hbrun %1$s %2$s > %3$s", cTemp, StrTran( cDocOptions, "{LANG}", cLang ), file ) )
      FToNativeEOL( file )

      /* special case */
      IF hb_FNameName( cMain ) == "hbmk2"
         file := cBase + hb_DirSepToOS( "../../contrib/hbrun/doc/" ) + "hbrun" + "." + cLang + ".md"
         hb_run( hb_StrFormat( "hbrun %1$s -lang=%2$s -longhelpmdsh > %3$s", cTemp, cLang, file ) )
         FToNativeEOL( file )
      ENDIF
   NEXT

   FErase( cTemp )

   AEval( Directory( hb_FNameDir( cMain ) + "*.hbl" ), ;
      {| tmp | FErase( hb_FNameDir( cMain ) + tmp[ F_NAME ] ) } )

   RETURN

STATIC FUNCTION FToNativeEOL( cFile )
   RETURN hb_MemoWrit( cFile, StrTran( hb_MemoRead( cFile ), e"\n", hb_eol() ) )
