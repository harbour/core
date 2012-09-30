/*
 * $Id$
 */

/* test of pass by reference @ */

PROCEDURE Main()

   LOCAL a := 10
   LOCAL b := "X"

   QOut( 'a := 10', a )
   QOut( 'b := "X"', b )

   testfun( @a, @b )
   QOut( 'return of "a" should = 20', a, iif( a == 20,"worked","failed" ) )
   QOut( 'return of "b" should = A', b, iif( b == "A","worked","failed" ) )

   RETURN

FUNCTION testfun( b, c )

   b := b + 10
   c := "A"
   QOut( 'a pointer+10 =', b )
   QOut( 'b pointer := "A" =', c )

   RETURN NIL
