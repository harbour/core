/* Testing Harbour If inline */

PROCEDURE Main()

   LOCAL n := 1

   QOut( "Testing Harbour If inline" )

   if( n == 1, QOut( 1 ), QOut( 2 ) )

   iif( n != NIL, QOut( "not NIL" ), )

   QOut( "Now changing n to 2" )

   n := 2

   if( n == 1, QOut( 1 ), QOut( 2 ) )

   iif( n != NIL, QOut( "not NIL" ), )

   QOut( "ok!" )

   RETURN
