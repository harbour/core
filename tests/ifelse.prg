// Testing Harbour IF ELSEIF ELSE ENDIF

PROCEDURE Main()

   LOCAL i

   ? "Testing Harbour IF ELSEIF ELSE ENDIF"
   FOR i := 1 TO 5
      TestValue( i )
   NEXT

   RETURN

PROCEDURE TestValue( x )

   IF x == 1
      ? "x is 1"

   ELSEIF x == 2
      ? "x is 2"

   ELSEIF x == 3
      ? "x is 3"

   ELSEIF x == 4
      ? "x is 4"

   ELSE
      ? "x is not 1 or 2 or 3 or 4"
   ENDIF

   ? "Ok!"

   RETURN
