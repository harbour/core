/* Test SECONDS() */
/* Harbour Project source code
   http://www.Harbour-Project.org/
   Copyright 1999 David G. Holm
   See doc/hdr_tpl.txt, Version 1.2 or later, for licensing terms.
*/

function Main()
local n

   QOUT(SECONDS())
   FOR n := 1 TO 10
      __ACCEPT("Pause: ")
      QOUT(SECONDS())
   NEXT

return NIL
