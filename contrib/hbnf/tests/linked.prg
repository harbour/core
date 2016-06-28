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

   ? PadR( "String Tested", 45 ), "Result"
   ?
   AEval( aString, {| ele | QOut( PadR( ele, 45 ), ft_Linked( ele ) ) } )
   ?

   RETURN
