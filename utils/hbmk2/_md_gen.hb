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

   LOCAL file
   LOCAL cLang

   FOR EACH file IN Directory( hb_DirBase() + hb_DirSepToOS( "po/*.po" ) )
      hb_run( hb_StrFormat( "hbi18n -q -g -o%1$s %2$s", ;
         hb_FNameName( file[ F_NAME ] ) + ".hbl", ;
         hb_DirSepToOS( hb_DirBase() + "po/" + file[ F_NAME ] ) ) )
   NEXT

   FOR EACH cLang IN hb_ATokens( hb_regexAll( "-lng=([a-zA-Z0-9_,]*)", hb_MemoRead( hb_DirBase() + "hbmk2.hbp" ),,,,, .T. )[ 1 ][ 2 ], "," )
      ? file := hb_DirSepToOS( hb_DirBase() + "doc/hbmk2." + cLang + ".md" )
      hb_run( hb_StrFormat( "hbrun %1$s -lang=%2$s -longhelpmd > %3$s", hb_DirBase() + "hbmk2.prg", cLang, file ) )
      hb_MemoWrit( file, StrTran( hb_MemoRead( file ), e"\n", hb_eol() ) )

      ? file := hb_DirSepToOS( hb_DirBase() + "../../contrib/hbrun/doc/hbrun." + cLang + ".md" )
      hb_run( hb_StrFormat( "hbrun %1$s -lang=%2$s -longhelpmdsh > %3$s", hb_DirBase() + "hbmk2.prg", cLang, file ) )
      hb_MemoWrit( file, StrTran( hb_MemoRead( file ), e"\n", hb_eol() ) )
   NEXT

   AEval( Directory( "*.hbl" ), {| tmp | FErase( tmp[ F_NAME ] ) } )

   RETURN
