/*
 * $Id$
 */

// while loop test

FUNCTION Main()

   LOCAL i := 0
   LOCAL cb := {|| QOut( "test" ) }

   WHILE i < 1000
      QOut( i )
      Eval( cb )
      i ++
   END

   RETURN nil
