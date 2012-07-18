/*
 * $Id$
 */

// testing recursive calls

PROCEDURE Main()

   QOut( "Testing recursive calls" + Chr( 13 ) + Chr( 10 ) )

   QOut( f( 10 ) )

   QOut( 10 * 9 * 8 * 7 * 6 * 5 * 4 * 3 * 2 * 1 )

   RETURN

FUNCTION f( a )

   RETURN iif( a < 2, 1, a * f( a - 1 ) )
