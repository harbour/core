//
// $Id$
//

// while loop test

function Main()

   local i := 0
   local cb := {|| QOut("test")}

   while i < 1000
      QOut(i)
      eval(cb)
      i++
   end

return nil
