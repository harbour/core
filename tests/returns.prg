// Testing multiple returns into a function

PROCEDURE Main()

   ? "From Main()"

   Two( 1 )

   ? "back to Main()"

   Two( 2 )

   ? "back to Main()"

   RETURN

STATIC PROCEDURE Two( n )

   DO CASE
   CASE n == 1
      ? "n == 1"
      RETURN
   CASE n == 2
      ? "n == 2"
      RETURN
   ENDCASE

   ? "This message should not been seen"

   RETURN
