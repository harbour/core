//
// OurStrCmp() tests
//
// Date : 24/4/99 Time : 17:50
//
// Written by Eddie Runia <eddie@runia.com>
// www - http://www.harbour-project.org
//
// Placed in the public domain
//

procedure main()

  local CRLF := chr(13)+chr(10)

  QQOut( "Testing <first> <comparison> <second>. <Second>='Hello'", CRLF )

  QQOut( "<First>:", CRLF )

  QQOut( "  Hallo  Hello  Hell   Hellow J      1''    2''    all''  ", CRLF)

  QQOut( "==", "Hallo" == "Hello", "    ", "Hello" == "Hello", "    ", ;
               "Hell"  == "Hello", "    ", "Hellow"== "Hello", "    ", ;
               "J"     == "Hello", "    ", ""      == "Hello", "    ", ;
               "J"     == ""     , "    ", ""      == ""     , CRLF )
  QQOut( "!=", "Hallo" != "Hello", "    ", "Hello" != "Hello", "    ", ;
               "Hell"  != "Hello", "    ", "Hellow"!= "Hello", "    ", ;
               "J"     != "Hello", "    ", ""      != "Hello", "    ", ;
               "J"     != ""     , "    ", ""      != ""     , CRLF )
  QQOut( "> ", "Hallo" >  "Hello", "    ", "Hello" >  "Hello", "    ", ;
               "Hell"  >  "Hello", "    ", "Hellow">  "Hello", "    ", ;
               "J"     >  "Hello", "    ", ""      >  "Hello", "    ", ;
               "J"     >  ""     , "    ", ""      >  ""     , CRLF )
  QQOut( ">=", "Hallo" >= "Hello", "    ", "Hello" >= "Hello", "    ", ;
               "Hell"  >= "Hello", "    ", "Hellow">= "Hello", "    ", ;
               "J"     >= "Hello", "    ", ""      >= "Hello", "    ", ;
               "J"     >= ""     , "    ", ""      >= ""     , CRLF )
  QQOut( "<=", "Hallo" <= "Hello", "    ", "Hello" <= "Hello", "    ", ;
               "Hell"  <= "Hello", "    ", "Hellow"<= "Hello", "    ", ;
               "J"     <= "Hello", "    ", ""      <= "Hello", "    ", ;
               "J"     <= ""     , "    ", ""      <= ""     , CRLF )
  QQOut( "< ", "Hallo" <  "Hello", "    ", "Hello" <  "Hello", "    ", ;
               "Hell"  <  "Hello", "    ", "Hellow"<  "Hello", "    ", ;
               "J"     <  "Hello", "    ", ""      <  "Hello", "    ", ;
               "J"     <  ""     , "    ", ""      <  ""     , CRLF )
return


