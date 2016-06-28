/* Written by Eddie Runia <eddie@runia.com>. Placed in the public domain. */

/* Testing Harbour FOR NEXT loops */

PROCEDURE Main()

   LOCAL n

   ? "Testing Harbour For Next loops. Going up quick"

   FOR n := 1 TO 10 STEP 4
      ? n
   NEXT

   ? "Going down"

   FOR n := 10 TO 1 STEP -1
      ? n
   NEXT

   ? "No step"

   FOR n := 1 TO 10
      ? n
   NEXT

   ? "No production"

   FOR n := 1 TO 10 STEP -1
      ? n
   NEXT

   ? "Ok!"

   RETURN
