/*
 * Uploads source .po file to Transifex localization site
 *
 * Copyright 2013 Viktor Szakats (harbour syenar.net)
 * www - http://harbour-project.org
 *
 * Requires: curl (built with SSL)
 * Reference: http://help.transifex.com/features/api/api-v2.1.html
 *
 */

#pragma -w3

PROCEDURE Main( cLogin )

   LOCAL cTemp

   IF Empty( cLogin )
      cLogin := GetEnv( "HB_TRANSIFEX_LOGIN" )  /* Format: username:password */
   ENDIF

   ? "uploading", "en"

   FClose( hb_FTempCreateEx( @cTemp ) )

   hb_MemoWrit( cTemp, hb_jsonEncode( ;
      { "content" => hb_MemoRead( hb_DirSepToOS( hb_DirBase() + "po/hbmk2.en.po" ) ) } ) )

   hb_run( hb_StrFormat( 'curl -s -i -L --user %1$s -X ' + ;
      'PUT -d @%2$s -H "Content-Type: application/json" ' + ;
      'https://www.transifex.com/api/2/project/harbour/resource/hbmk2/content/', ;
      cLogin, cTemp ) )

   FErase( cTemp )

   RETURN
