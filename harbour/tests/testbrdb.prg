/*
 * $Id$
 */

// Testing Browse()

function Main()
   LOCAL cColor

   cColor := SETCOLOR("W+/B")
   CLS

   USE Test
   Browse()

   SETCOLOR(cColor)
   CLS

return nil
