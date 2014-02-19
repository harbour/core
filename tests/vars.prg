#ifndef __HARBOUR__
#include "clipper.ch"
#endif

MEMVAR i

PROCEDURE Main( Param1 )

   LOCAL i, j, k

   i := 1
   j := 2

   Sub( @j )

   ? j

   HB_SYMBOL_UNUSED( Param1 )
   HB_SYMBOL_UNUSED( k )
   HB_SYMBOL_UNUSED( i )

   RETURN

STATIC PROCEDURE Sub( j )

   m->i := 1
   j := 3

   RETURN

STATIC PROCEDURE arrvar()

// LOCAL i := { 1 }

   i[ 1 ] := 2

   RETURN
