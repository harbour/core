/*
 * $Id$
 */

// Harbour Get System sample

function Main()

   local cName   := "Harbour     "
   local cWish   := "Power   "
   local cEffort := "Join us!    "
   local acVars  := { "Hello", "World" }, Counter

   local GetList := {}

   SET COLOR TO GR+/B, W+/BG
   CLS

   SET KEY -1 TO ShowVar()

   @ 2, 2 SAY "Enter your name  :" GET cName PICTURE "@K!"
   @ 4, 2 SAY "Enter your wish  :" GET cWish
   @ 6, 2 SAY "Enter your effort:" GET cEffort
   @ 8, 2 SAY "Object Data      :" GET GetList[1]:Picture

   FOR Counter := 1 TO Len( acVars )
      @ Row() + 2, 2 SAY "Array Element [" + Str( Counter, 1 ) + "]: " GET acVars[ Counter ]
   NEXT

   READ

   @ Row() + 2, 2
   ? cName
   ? cWish
   ? cEffort
   ? acVars[1]
   ? acVars[2]

return nil

function ShowVar()

   Alert( Readvar() )

return nil
