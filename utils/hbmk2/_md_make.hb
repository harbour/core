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

   ? "preparing"

   FOR EACH file IN Directory( cBase + hb_DirSepToOS( "po/*.po" ) )
      hb_run( hb_StrFormat( "hbi18n -q -g -o%1$s %2$s", ;
         cBase + hb_FNameName( file[ F_NAME ] ) + ".hbl", ;
         cBase + hb_DirSepToOS( "po/" + file[ F_NAME ] ) ) )
   NEXT

   cTemp := cBase + "hbmk2.hrb"

   hb_run( hb_StrFormat( "hbmk2 -hbraw -q0 -gh %1$s -o%2$s", cBase + "hbmk2.prg", cTemp ) )

   ? "generating .md help:"

   FOR EACH cLang IN hb_ATokens( "en," + hb_regexAll( "-lng=([a-zA-Z0-9_,]*)", hb_MemoRead( cBase + "hbmk2.hbp" ),,,,, .T. )[ 1 ][ 2 ], "," )

      ?? "", cLang

      file := cBase + hb_DirSepToOS( "doc/hbmk2." + cLang + ".md" )
      hb_run( hb_StrFormat( "hbrun %1$s -lang=%2$s -longhelpmd > %3$s", cTemp, cLang, file ) )
      FToNativeEOL( file )

      file := cBase + hb_DirSepToOS( "../../contrib/hbrun/doc/hbrun." + cLang + ".md" )
      hb_run( hb_StrFormat( "hbrun %1$s -lang=%2$s -longhelpmdsh > %3$s", cTemp, cLang, file ) )
      FToNativeEOL( file )
   NEXT

   FErase( cTemp )

   AEval( Directory( cBase + "*.hbl" ), {| tmp | FErase( cBase + tmp[ F_NAME ] ) } )

   RETURN

STATIC FUNCTION FToNativeEOL( cFile )
   RETURN hb_MemoWrit( cFile, StrTran( hb_MemoRead( cFile ), e"\n", hb_eol() ) )
