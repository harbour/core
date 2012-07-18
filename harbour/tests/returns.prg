/*
 * $Id$
 */

// Testing multiple returns into a function

PROCEDURE Main()

   QOut( "From Main()" )

   Two( 1 )

   QOut( "back to Main()" )

   Two( 2 )

   QOut( "back to Main()" )

   RETURN

FUNCTION Two( n )

   DO CASE
   CASE n == 1
      QOut( "n == 1" )
      RETURN nil

   CASE n == 2
      QOut( "n == 2" )
      RETURN nil
   ENDCASE

   QOut( "This message should not been seen" )

   RETURN nil
