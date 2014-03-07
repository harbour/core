/*
 * Check alignment dependent lang item(s)
 *
 * Copyright 2013 Viktor Szakats (vszakats.net/harbour)
 * www - http://harbour-project.org
 *
 */

#pragma -w3
#pragma -km+
#pragma -ko+

#include "hblang.ch"

PROCEDURE Main()

   LOCAL tmp
   LOCAL nCount
   LOCAL cName

   ? ;
      PadR( "en", 6 ), '"' + ;
      PadR( "", 15, "*" ) + ;
      Str( 999999999999, 12 ) + "    " + ;
      DToC( Date() ) + ;
      Str( 999999999999, 12 ) + '"'

   nCount := __dynsCount()
   FOR tmp := 1 TO nCount
      cName := __dynsGetName( tmp )
      IF hb_LeftEq( cName, "HB_LANG_" )
         cName := SubStr( cName, Len( "HB_LANG_" ) + 1 )
         IF ( Len( cName ) != 5 .OR. "_" $ cName ) .AND. ;
            ! "|" + cName + "|" $ "|RUKOI8|UAKOI8|ZHB5|ZHGB|"  /* HB_LEGACY_LEVEL4 */
            ? PadR( Lower( cName ), 6 ), '"' + hb_langMessage( HB_LANG_ITEM_BASE_NATMSG, cName ) + '"'
         ENDIF
      ENDIF
   NEXT

   RETURN
