/* Test SECONDS() */

function Main()
local n

   QOUT(SECONDS())
   FOR n := 1 TO 10
      __ACCEPT("Pause: ")
      QOUT(SECONDS())
   NEXT

return NIL
