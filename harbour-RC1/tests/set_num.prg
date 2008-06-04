//
// $Id$
//

// Testing SET

#include "set.ch"

function Main()
local n, cNewLine := HB_OSNewLine()

   for n := 1 to _SET_COUNT
      outstd (cNewLine)
      outstd (set (n))
   next

return nil
