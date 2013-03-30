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

   LOCAL cTemp
   LOCAL cContent

   IF Empty( cLogin )
      cLogin := GetEnv( "HB_TRANSIFEX_LOGIN" )  /* Format: username:password */
   ENDIF

   FClose( hb_FTempCreateEx( @cTemp, , , ".pot" ) )

   ? "generating pot"

   hb_run( hb_StrFormat( "harbour -q0 hbmk2.prg -j%1$s -i%2$s -s", cTemp, hb_DirSepToOS( hb_DirBase() + "../../include" )) )

   ? "saving locally"

   cContent := ;
      '#, c-format' + hb_eol() + ;
      'msgid ""' + hb_eol() + ;
      'msgstr ""' + hb_eol() + ;
      '"Project-Id-Version: hbmk2\n"' + hb_eol() + ;
      '"Language: en\n"' + hb_eol() + ;
      '"MIME-Version: 1.0\n"' + hb_eol() + ;
      '"Content-Type: text/plain; charset=UTF-8\n"' + hb_eol() + ;
      '"Content-Transfer-Encoding: 8bit\n"' + hb_eol() + hb_eol() + ;
      hb_MemoRead( cTemp )

   hb_MemoWrit( hb_DirSepToOS( hb_DirBase() + "po/hbmk2.en.po" ), cContent )

   ? "uploading"

   hb_MemoWrit( cTemp, hb_jsonEncode( { "content" => StrTran( cContent, hb_eol(), e"\n" ) } ) )

   hb_run( hb_StrFormat( 'curl -s -i -L --user %1$s -X ' + ;
      'PUT -d @%2$s -H "Content-Type: application/json" ' + ;
      'https://www.transifex.com/api/2/project/harbour/resource/hbmk2/content/', ;
      cLogin, cTemp ) )

   FErase( cTemp )

   RETURN
