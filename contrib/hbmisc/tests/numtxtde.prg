#require "hbmisc"

REQUEST HB_CODEPAGE_UTF8EX

PROCEDURE Main()

   LOCAL nTemp

   CLS

   hb_cdpSelect( "UTF8EX" )
   hb_SetTermCP( hb_cdpTerm() )

   FOR nTemp := 1 TO 1000000000
      ? NumToTxtDE( nTemp )
   NEXT

   RETURN
