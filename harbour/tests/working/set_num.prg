//
// $Id$
//

// Testing SET

function Main()
local n, NEWLINE := CHR (10) + CHR(13)

   for n := 1 to 39
      outstd (NEWLINE)
      outstd (set (n))
   next

return nil
