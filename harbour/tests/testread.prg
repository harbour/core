/*
 * $Id$
 */

// Harbour Get System sample

PROCEDURE Main()

   LOCAL cName   := "Harbour     "
   LOCAL cWish   := "Power   "
   LOCAL cEffort := "Join us!    "
   LOCAL acVars  := { { "Hello", "World" } }, Counter

   LOCAL GetList := {}

   SET COLOR TO GR +/ B, W +/ BG
   CLS

   SET KEY -1 TO ShowVar()

   @ 2, 2 SAY "Enter your name  :" GET cName PICTURE "@K!"
   @ 4, 2 SAY "Enter your wish  :" GET cWish
   @ 6, 2 SAY "Enter your effort:" GET cEffort
   @ 8, 2 SAY "Object Data      :" GET GetList[ 1 ]:Picture

   FOR Counter := 1 TO Len( acVars[ 1 ] )
      @ Row() + 2, 2 SAY "Array Element[1][" + Str( Counter, 1 ) + "]: " GET acVars[ 1 ][ Counter ]
   NEXT

   READ

   @ Row() + 2, 2
   ? cName
   ? cWish
   ? cEffort
   ? acVars[ 1 ][ 1 ]
   ? acVars[ 1 ][ 2 ]

   RETURN

PROCEDURE ShowVar()

   Alert( ReadVar() )

   RETURN
