/*
 * $Id$
 */

#require "hbnf"

PROCEDURE Main()

   LOCAL aString := { ;
      "RTrim('abc ')", ;
      "NotARealFunc()", ;
      "ft_DispMsg()", ;
      'RTrim(cVar+"abc"+Left(cString)), Found()', ;
      "IsItLinked()", ;
      "lRetVal := Found()", ;
      "!Eof() .AND. Month(Date())=12 .AND. YeeHa()", ;
      "!Eof() .AND. Month(Date())=12", ;
      "!Eof() .AND. Month(Date(YeeHa()))=12", ;
      "Left(SubStr(nNum,4,Val(cChar+Asc(c))))", ;
      "Eof(>> Note: Syntax IS NOT checked! <<)"           }

   CLS
   @ 1, 0 SAY "String Tested                               Result"
   @ 2, 0 TO 2, MaxCol()
   AEval( aString, {| ele | QOut( ele, Space( 45 - Len( ele ) ), ft_Linked( ele ) ) } )
   @ MaxRow() - 2, 0

   RETURN
