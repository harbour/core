/*
 * $Id$
 */

MEMVAR i

PROCEDURE Main( Param1 )

   LOCAL i, j, k

   i := 1
   j := 2

   Sub( @j )

   QOut( j )

   RETURN

FUNCTION Sub( j )

   m->i := 1
   j := 3

   RETURN NIL

FUNCTION arrvar()

// LOCAL i := { 1 }

   i[ 1 ] := 2

   RETURN NIL
