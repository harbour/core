#ifndef __HARBOUR__
#include "clipper.ch"
#endif

/* Testing warnings. Do not add HB_SYMBOL_UNUSED() or remove unused variables. */

MEMVAR i

PROCEDURE Main( Param1 )

   LOCAL i, j, k

   i := 1
   j := 2

   Sub( @j )

   ? j

   RETURN

STATIC PROCEDURE Sub( j )

   m->i := 1
   j := 3

   RETURN

STATIC PROCEDURE arrvar()

// LOCAL i := { 1 }

   i[ 1 ] := 2

   RETURN
