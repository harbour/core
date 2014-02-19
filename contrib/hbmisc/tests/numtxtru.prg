#require "hbmisc"

#include "inkey.ch"

REQUEST HB_CODEPAGE_UTF8EX

PROCEDURE Main()

   CLS

   hb_cdpSelect( "UTF8EX" )
   hb_SetTermCP( hb_cdpTerm() )

   ? "Press ESC to break"
   ? "Russian"
   Test( "ru" )
   ? "Ukrainian"
   Test( "uk" )
   ? "Belorussian"
   Test( "be" )

   RETURN

STATIC PROCEDURE Test( cLang )

   LOCAL nTemp

   FOR nTemp := 1 TO 1000000000
      ? ;
         PadR( MnyToTxtRU( nTemp + ( nTemp % 100 ) * 0.01, cLang, , 3 ), 100 ), ;
         PadR( NumToTxtRU( nTemp, cLang, , .T. ), 100 ), ;
               DateToTxtRU( Date() + nTemp, cLang, .T. )
      IF nTemp % 1000 == 0
         ? nTemp
      ENDIF
      IF nTemp % 10000 == 0 .AND. Inkey() == K_ESC
         EXIT
      ENDIF
   NEXT

   RETURN
