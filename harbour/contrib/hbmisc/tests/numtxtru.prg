/*
 * $Id$
 */

REQUEST HB_CODEPAGE_RU866

PROCEDURE Main()

   ? "Press ESC to break"
   ? "Russian"
   Test( "ru" )
   ? "Ukrainian"
   Test( "uk" )
   ? "Belorussian"
   Test( "be" )

   RETURN

PROCEDURE test( nLang )

   LOCAL nTemp

   dbCreate( "_num" + hb_ntos( nLang ), ;
      { { "NUM" , "N",  19, 0 },;
        { "STR1", "C", 100, 0 },;
        { "STR2", "C", 100, 0 },;
        { "STR3", "C",  50, 0 } }, , .T. , "num" )
   FOR nTemp := 1 TO 1000000000
      num->( dbAppend() )
      num->Num := nTemp
      num->Str1 := MnyToTxtRU( nTemp + ( nTemp % 100 ) * 0.01, nLang, , 3 )
      num->Str2 := NumToTxtRU( nTemp, nLang, , .T. )
      num->Str3 := DateToTxtRU( Date() + nTemp, nLang, .T. )
      IF nTemp % 1000 == 0
         ? nTemp
      ENDIF
      IF nTemp % 10000 == 0
         IF Inkey() == 27
            EXIT
         ENDIF
      ENDIF
   NEXT
   CLOSE

   RETURN
