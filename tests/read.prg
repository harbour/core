// Harbour Get system sample

#include "inkey.ch"

PROCEDURE Main()

   LOCAL cName   := PadR( "Harbour", 12 )
   LOCAL cWish   := PadR( "Power", 8 )
   LOCAL cEffort := PadR( "Join us!", 13 )
   LOCAL acVars  := { { "Hello", "World" } }
   LOCAL nCounter

   LOCAL GetList := {}

   SetColor( "GR+/B, W+/BG" )
   CLS

   SetKey( K_F2, {|| Alert( ReadVar() ) } )

   @ 1, 2 SAY "Press <F2> for ReadVar()"

   @ 3, 2 SAY "Enter your name        " GET cName PICTURE "@K!"
   @ 4, 2 SAY "Enter your wish        " GET cWish
   @ 5, 2 SAY "Enter your effort      " GET cEffort
   @ 6, 2 SAY "Object Data            " GET GetList[ 1 ]:Picture
   FOR nCounter := 1 TO Len( acVars[ 1 ] )
      @ Row() + 1, 2 SAY "Array Element[ 1 ][ " + hb_ntos( nCounter ) + " ]" GET acVars[ 1 ][ nCounter ]
   NEXT
   READ

   SetPos( 10, 0 )
   ? cName
   ? cWish
   ? cEffort
   ? acVars[ 1 ][ 1 ]
   ? acVars[ 1 ][ 2 ]
   Inkey( 0 )

   RETURN
