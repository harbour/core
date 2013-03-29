/*
 * Downloads .po files from Transifex translating site
 *
 * Copyright 2013 Viktor Szakats (harbour syenar.net)
 * www - http://harbour-project.org
 *
 * Requires: curl (built with SSL)
 *
 */

#pragma -w3

PROCEDURE Main( cLogin )

   LOCAL json
   LOCAL cLang
   LOCAL cTemp

   FClose( hb_FTempCreateEx( @cTemp ) )

   FOR EACH cLang IN hb_ATokens( hb_regexAll( "-lng=([a-zA-Z0-9_,]*)", hb_MemoRead( hb_DirBase() + "hbmk2.hbp" ),,,,, .T. )[ 1 ][ 2 ], "," )

      ? cLang

      hb_run( hb_StrFormat( "curl -s -i -L --user %1$s -X GET https://www.transifex.com/api/2/project/harbour/resource/hbmk2/translation/%2$s/ > %3$s", ;
         cLogin, cLang, cTemp ) )

      IF hb_jsonDecode( GetJSON( hb_MemoRead( cTemp ) ), @json ) > 0
         hb_MemoWrit( hb_DirSepToOS( hb_DirBase() + "po/hbmk2." + cLang + ".po" ), StrTran( json[ "content" ], e"\n", hb_eol() ) )
      ELSE
         ? "error"
      ENDIF

      FErase( cTemp )
   NEXT

   RETURN

STATIC FUNCTION GetJSON( cString )

   cString := SubStr( cString, At( "{", cString ) )
   cString := Left( cString, RAt( "}", cString ) )

   RETURN cString
