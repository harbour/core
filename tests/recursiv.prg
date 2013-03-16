// testing recursive calls

PROCEDURE Main()

   ? "Testing recursive calls"
   ?

   ? f( 10 )

   ? 10 * 9 * 8 * 7 * 6 * 5 * 4 * 3 * 2 * 1

   RETURN

FUNCTION f( a )

   RETURN iif( a < 2, 1, a * f( a - 1 ) )
