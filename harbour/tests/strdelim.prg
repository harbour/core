/*
 * $Id$
 */

PROCEDURE Main()

   LOCAL aArray := { { NIL } }

   aArray[ 1 /*first*/ ][ 1 /* second */ ] := [Hello]

   QOut( aArray[ 1 ][ 1 ] )

   QOut( 'World "Peace[!]"' )

   QOut( "Harbour 'Power[!]'" )

   QOut( [King 'Clipper "!"'] )

   RETURN
