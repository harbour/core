/*
 * $Id$
 */

/*
 * Check alignment dependent lang item(s)
 *
 * Copyright 2013 Viktor Szakats (harbour syenar.net)
 * www - http://harbour-project.org
 *
 */

#include "hblang.ch"

#define LEFTEQUAL( l, r )       ( Left( l, Len( r ) ) == r )

PROCEDURE Main()

   LOCAL tmp, tmp1
   LOCAL nCount
   LOCAL cName
   LOCAL cPO

   cPO := ;
      PadR( "en", 6 ) + ' "' + ;
      PadR( "", 15, "*" ) + ;
      Str( 999999999999, 12 ) + "    " + ;
      DToC( Date() ) + ;
      Str( 999999999999, 12 ) + '"' + hb_eol()

   nCount := __dynSCount()
   FOR tmp := 1 TO nCount
      cName := __dynSGetName( tmp )
      IF LEFTEQUAL( cName, "HB_LANG_" )
         cName := SubStr( cName, Len( "HB_LANG_" ) + 1 )
         IF Len( cName ) != 5 .AND. ;
            ! "|" + cName + "|" $ "|RUKOI8|UAKOI8|ZHB5|ZHGB|"
            cPO += PadR( Lower( cName ), 6 ) + ' "' + hb_langMessage( HB_LANG_ITEM_BASE_NATMSG, cName ) + '"' + hb_eol()
         ENDIF
      ENDIF
   NEXT

   hb_MemoWrit( hb_FNameExtSet( __FILE__, ".txt" ), cPO )

   RETURN
