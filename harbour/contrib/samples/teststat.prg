/*
 * $Id$
 */

function test()

   local aStat := StatusNew( 10, 5, "R/N" )
   local i

   CLS
   for i := 1 to 40
      StatusUpdate( aStat )
      Inkey( .1 )
   next

return nil
