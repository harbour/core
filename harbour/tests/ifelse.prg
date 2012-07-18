/*
 * $Id$
 */

// Testing Harbour If elseif else endif

PROCEDURE Main()

   LOCAL i

   QOut( "Testing Harbour If elseif else endif" )
   FOR i := 1 TO 5
      TestValue( i )
   NEXT

   RETURN

FUNCTION TestValue( x )

   IF x = 1
      QOut( "x is 1" )

   ELSEIF x = 2
      QOut( "x is 2" )

   ELSEIF x = 3
      QOut( "x is 3" )

   ELSEIF x = 4
      QOut( "x is 4" )

   ELSE
      QOut( "x is not 1 or 2 or 3 or 4" )
   ENDIF

   QOut( "Ok!" )

   RETURN nil
