/* test of pass by reference @ */

PROCEDURE Main()

   LOCAL a := 10
   LOCAL b := "X"

   ? 'a := 10', a
   ? 'b := "X"', b

   testfun( @a, @b )
   ? 'return of "a" should = 20', a, iif( a == 20, "worked", "failed" )
   ? 'return of "b" should = A', b, iif( b == "A", "worked", "failed" )

   RETURN

STATIC PROCEDURE testfun( b, c )

   b += 10
   c := "A"
   ? 'a pointer + 10 =', b
   ? 'b pointer := "A" =', c

   RETURN
