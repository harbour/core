/*
 * Uploads source .pot file to Transifex localization site
 *
 * Copyright 2013 Viktor Szakats (harbour syenar.net)
 * www - http://harbour-project.org
 *
 * Requires: curl (built with SSL), Harbour in PATH
 * Reference: http://help.transifex.com/features/api/api-v2.1.html
 *
 */

#pragma -w3

PROCEDURE Main( cLogin )

   LOCAL cBase := hb_DirBase()

   LOCAL json
   LOCAL cTemp, cTemp2
   LOCAL cContent

   LOCAL cMain := cBase + "hbmk2.prg"

   IF Empty( cLogin )
      cLogin := GetEnv( "HB_TRANSIFEX_LOGIN" )  /* Format: username:password */
   ENDIF

   FClose( hb_FTempCreateEx( @cTemp, , , ".pot" ) )
   FClose( hb_FTempCreateEx( @cTemp2 ) )

   ? "generating pot"

   hb_run( hb_StrFormat( "hbmk2 -hbraw -q0 %1$s -j%2$s -s", cMain, cTemp ) )

   ? "saving locally"

   cContent := hb_StrFormat( ;
      '#, c-format' + hb_eol() + ;
      'msgid ""' + hb_eol() + ;
      'msgstr ""' + hb_eol() + ;
      '"Project-Id-Version: %1$s\n"' + hb_eol() + ;
      '"Language: en\n"' + hb_eol() + ;
      '"MIME-Version: 1.0\n"' + hb_eol() + ;
      '"Content-Type: text/plain; charset=UTF-8\n"' + hb_eol() + ;
      '"Content-Transfer-Encoding: 8bit\n"', hb_FNameName( cMain ) ) + hb_eol() + ;
      hb_eol() + ;
      hb_MemoRead( cTemp )

   hb_MemoWrit( hb_DirSepToOS( cBase + "po/" + hb_FNameName( cMain ) + ".en.po" ), cContent )

   ? "uploading", "size", Len( cContent )
   ?

   hb_MemoWrit( cTemp, hb_jsonEncode( { "content" => StrTran( cContent, hb_eol(), e"\n" ) } ) )

   hb_run( hb_StrFormat( 'curl -s -i -L --user %1$s -X ' + ;
      'PUT -d @%2$s -H "Content-Type: application/json" ' + ;
      'https://www.transifex.com/api/2/project/harbour/resource/%3$s/content/' + ;
      ' -o %4$s', ;
      cLogin, cTemp, hb_FNameName( cMain ), cTemp2 ) )

   IF hb_jsonDecode( GetJSON( hb_MemoRead( cTemp2 ) ), @json ) > 0
      ? hb_ValToExp( json )
   ELSE
      ? "API error"
   ENDIF

   FErase( cTemp )
   FErase( cTemp2 )

   RETURN

STATIC FUNCTION GetJSON( cString )

   cString := SubStr( cString, At( "{", cString ) )
   cString := Left( cString, RAt( "}", cString ) )

   RETURN cString
