/*
 * $Id$
 */

// Harbour Get System sample

function Main()

   local cName   := "Harbour     "
   local cWish   := "Power   "
   local cEffort := "Join us!    "
   local GetList := {}

   SET COLOR TO GR+/B, W+/BG
   CLS

   SET KEY -1 TO ShowVar()

   @ 2, 2 SAY "  Enter your name:" GET cName
   @ 4, 2 SAY "  Enter your wish:" GET cWish
   @ 6, 2 SAY "Enter your effort:" GET cEffort
   READ

   @ 8, 2
   ? cName
   ? cWish
   ? cEffort

return nil

function ShowVar()

   Alert( Readvar() )

return nil
