/*
 * $Id$
 */

// while loop test

PROCEDURE Main()

   LOCAL i := 0
   LOCAL cb := {|| QOut( "test" ) }

   WHILE i < 1000
      QOut( i )
      Eval( cb )
      i++
   END

   RETURN
