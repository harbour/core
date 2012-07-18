/*
 * $Id$
 */

// Testing Harbour DO CASE

PROCEDURE Main()

   LOCAL n := 2

   QOut( "testing Harbour Do case" )

   DO CASE
   CASE n == 1
      QOut( "n is 1" )
      QOut( "first case" )

   CASE n == 2
      QOut( "n is 2" )
      QOut( "second case" )

   CASE n == 3
      QOut( "n is 3" )
      QOut( "third case" )

      OTHERWISE
      QOut( "Sorry, I don't know what n is :-)" )
      QOut( "otherwise" )
   ENDCASE

   QOut( "Ok!" )

   RETURN
