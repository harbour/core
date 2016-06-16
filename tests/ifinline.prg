/* Testing Harbour iif() inline */

PROCEDURE Main()

   LOCAL n := 1

   ? "Testing Harbour If inline"

   if( n == 1, OutCR( 1 ), OutCR( 2 ) )

   iif( n != NIL, OutCR( "not NIL" ), )

   ? "Now changing n to 2"

   n := 2

   if( n == 1, OutCR( 1 ), OutCR( 2 ) )

   iif( n != NIL, OutCR( "not NIL" ), )

   ? "ok!"

   RETURN

STATIC PROCEDURE OutCR( x )

   ? x

   RETURN
