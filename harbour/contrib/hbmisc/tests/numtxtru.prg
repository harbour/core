/*
 * $Id$
 */

REQUEST HB_CODEPAGE_RU1251

PROCEDURE Main()

   HB_CDPSelect( "RU1251" )

   ? "Press ESC to break"
   ? "Russian"
   Test( "ru" )
   ? "Ukrainian"
   Test( "uk" )
   ? "Belorussian"
   Test( "be" )

   RETURN

PROCEDURE test( cLang )

   LOCAL nTemp

   dbCreate( "_num_" + cLang, ;
      { { "NUM" , "N",  19, 0 },;
        { "STR1", "C", 100, 0 },;
        { "STR2", "C", 100, 0 },;
        { "STR3", "C",  50, 0 } }, , .T. , "num" )
   FOR nTemp := 1 TO 1000000000
      num->( dbAppend() )
      num->Num := nTemp
      num->Str1 := MnyToTxtRU( nTemp + ( nTemp % 100 ) * 0.01, cLang, , 3 )
      num->Str2 := NumToTxtRU( nTemp, cLang, , .T. )
      num->Str3 := DateToTxtRU( Date() + nTemp, cLang, .T. )
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
