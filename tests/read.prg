// Harbour Get system sample

#include "inkey.ch"

PROCEDURE Main()

   LOCAL cName   := "Harbour     "
   LOCAL cWish   := "Power   "
   LOCAL cEffort := "Join us!    "
   LOCAL acVars  := { { "Hello", "World" } }
   LOCAL nCounter

   LOCAL GetList := {}

   SET COLOR TO GR+/B, W+/BG
   CLS

   SET KEY K_F2 TO ShowVar()

   @ 2, 2 SAY "Enter your name  :" GET cName PICTURE "@K!"
   @ 4, 2 SAY "Enter your wish  :" GET cWish
   @ 6, 2 SAY "Enter your effort:" GET cEffort
   @ 8, 2 SAY "Object Data      :" GET GetList[ 1 ]:Picture

   FOR nCounter := 1 TO Len( acVars[ 1 ] )
      @ Row() + 2, 2 SAY "Array Element[ 1 ][ " + hb_ntos( nCounter ) + " ]: " GET acVars[ 1 ][ nCounter ]
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
