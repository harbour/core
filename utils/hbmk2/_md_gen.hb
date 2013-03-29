/*
 * Generates .md help files for all languages
 *
 * Copyright 2013 Viktor Szakats (harbour syenar.net)
 * www - http://harbour-project.org
 *
 */

#pragma -w3

#include "directry.ch"

PROCEDURE Main()

   LOCAL file
   LOCAL cLang

   FOR EACH file IN Directory( hb_DirSepToOS( "po/*.po" ) )
      hb_run( hb_StrFormat( "hbi18n -q -g -o%1$s %2$s", ;
         hb_FNameName( file[ F_NAME ] ) + ".hbl", ;
         hb_DirSepToOS( "po/" + file[ F_NAME ] ) ) )
   NEXT

   FOR EACH cLang IN hb_ATokens( hb_regexAll( "-lng=([a-zA-Z_,]*)", hb_MemoRead( "hbmk2.hbp" ),,,,, .T. )[ 1 ][ 2 ], "," )
      ? file := "doc/hbmk2." + cLang + ".md"
      hb_run( hb_DirSepToOS( hb_StrFormat( "hbrun hbmk2.prg -lang=%1$s -longhelpmd > %2$s", cLang, file ) ) )
      hb_MemoWrit( file, StrTran( hb_MemoRead( file ), e"\n", hb_eol() ) )

      ? file := "../../contrib/hbrun/doc/hbrun." + cLang + ".md"
      hb_run( hb_DirSepToOS( hb_StrFormat( "hbrun hbmk2.prg -lang=%1$s -longhelpmdsh > %2$s", cLang, file ) ) )
      hb_MemoWrit( file, StrTran( hb_MemoRead( file ), e"\n", hb_eol() ) )
   NEXT

   AEval( Directory( "*.hbl" ), {| tmp | FErase( tmp[ F_NAME ] ) } )

   RETURN
