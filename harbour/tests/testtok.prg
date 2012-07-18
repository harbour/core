/*
 * $Id$
 */

#include "set.ch"

PROCEDURE Main()

   LOCAL a
   LOCAL i

   SET( _SET_EXACT, .T. )

   a := strtoarray( "this is a great big test of strtoken" )
   FOR i := 1 TO Len( a )
      QOut( a[ i ] )
   NEXT

   RETURN

FUNCTION strtoarray( s )

   LOCAL aResult := {}
   LOCAL t, l

   DO WHILE s != ""
      t := StrToken( s, 1, , @l )
      AAdd( aResult, t )
      s := SubStr( s, l + 2 ) /* skip the delimiter */

      QOut( t, Str( l ), s )
   ENDDO

   RETURN aResult
