//
// $Id$
//

// Testing SET

function Main()
local n, cNewLine := HB_OSNewLine()

   for n := 1 to 39
      outstd (cNewLine)
      outstd (set (n))
   next

return nil
