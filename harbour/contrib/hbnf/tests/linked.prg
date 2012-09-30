/*
 * $Id$
 */

PROCEDURE Main()

   LOCAL aString := { ;
      "RTrim('abc ')"                                     ,;
      "NotARealFunc()"                                    ,;
      "FT_DispMsg()"                                      ,;
      'RTrim(cVar+"abc"+Left(cString)), Found()'          ,;
      "IsItLinked()"                                      ,;
      "lRetVal := Found()"                                ,;
      "!EOF() .AND. Month(Date())=12 .AND. YeeHa()"       ,;
      "!EOF() .AND. Month(Date())=12"                     ,;
      "!EOF() .AND. Month(Date(YeeHa()))=12"              ,;
      "Left(SubStr(nNum,4,Val(cChar+Asc(c))))"            ,;
      "EOF(>> Note: Syntax IS NOT checked! <<)"           }

   CLS
   @ 1, 0 SAY "String Tested                               Result"
   @ 2, 0 TO 2, MaxCol()
   AEval( aString, {| ele | QOut( ele, Space( 45 - Len( ele ) ), FT_Linked( ele ) ) } )
   @ MaxRow() - 2, 0

   RETURN
