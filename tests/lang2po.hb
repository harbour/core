/*
 * $Id$
 */

/*
 * Converts lang modules to standard .po files
 *
 * Copyright 2013 Viktor Szakats (harbour syenar.net)
 * www - http://harbour-project.org
 *
 */

#pragma -w3
#pragma -km+
#pragma -ko+

#include "hblang.ch"

#define LEFTEQUAL( l, r )       ( Left( l, Len( r ) ) == r )

PROCEDURE Main()

   LOCAL tmp, tmp1
   LOCAL nCount
   LOCAL cName
   LOCAL cPO

   nCount := __dynsCount()
   FOR tmp := 1 TO nCount
      cName := __dynsGetName( tmp )
      IF LEFTEQUAL( cName, "HB_LANG_" )
         cName := SubStr( cName, Len( "HB_LANG_" ) + 1 )
         IF Len( cName ) != 5 .AND. ;
            ! "|" + cName + "|" $ "|RUKOI8|UAKOI8|ZHB5|ZHGB|"
            cPO := Item( "", Meta() )
            /* TODO: do something with the metadata (position 0 to 5) */
            FOR tmp1 := HB_LANG_ITEM_BASE_MONTH TO HB_LANG_ITEM_MAX_ - 1
               IF ! Empty( hb_langMessage( tmp1, "en" ) )
                  cPO += Item( hb_langMessage( tmp1, "en" ), hb_langMessage( tmp1, cName ) )
               ENDIF
            NEXT
            hb_MemoWrit( Lower( hb_FNameName( cName ) ) + ".po", hb_StrShrink( cPO, Len( hb_eol() ) ) )
         ENDIF
      ENDIF
   NEXT

   RETURN

FUNCTION Meta()
   RETURN ;
      "Project-Id-Version: Harbour\n" + ;
      "MIME-Version: 1.0\n" + ;
      "Content-Transfer-Encoding: 8bit\n" + ;
      "Content-Type: text/plain; charset=UTF-8\n"

FUNCTION Item( cEN, cTrs )
   RETURN hb_StrFormat( ;
      "#, c-format" + hb_eol() + ;
      'msgid "%1$s"' + hb_eol() + ;
      'msgstr "%2$s"' + hb_eol() + ;
      hb_eol(), cEN, cTrs )
