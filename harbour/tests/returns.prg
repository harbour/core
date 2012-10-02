/*
 * $Id$
 */

// Testing multiple returns into a function

PROCEDURE Main()

   ? "From Main()"

   Two( 1 )

   ? "back to Main()"

   Two( 2 )

   ? "back to Main()"

   RETURN

FUNCTION Two( n )

   DO CASE
   CASE n == 1
      ? "n == 1"
      RETURN NIL

   CASE n == 2
      ? "n == 2"
      RETURN NIL
   ENDCASE

   ? "This message should not been seen"

   RETURN NIL
