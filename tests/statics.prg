// Testing Harbour statics variables management

#ifndef __HARBOUR__
#include "clipper.ch"
#endif

STATIC s_z := "First"

PROCEDURE Main()

   STATIC a := "Hello"
   STATIC b := { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 }

   LOCAL i, cb

   ? a
   ? b[ 2 ]

   Two()

   ? "Ok!"

   FOR i := 1 TO 10
      NumStat()
   NEXT

   cb := DetachVar( 10 )
   FOR i := 1 TO 10
      ? Eval( cb, b[ i ] )
   NEXT

   RETURN

STATIC PROCEDURE Two()

   STATIC a := "Test"

   ? a

   RETURN

STATIC FUNCTION Three( p )

   ? p

   RETURN p

STATIC PROCEDURE NumStat( a )

   STATIC s_n := 1

   LOCAL cb

// STATIC s_m := s_n    // uncomment it to see an error
// STATIC s_m := Time() // uncomment it to see an error

   HB_SYMBOL_UNUSED( a )

   cb := {| x | s_z + Str( x ) }
   ? ++s_n
   ? Eval( cb, s_n )

   RETURN

STATIC FUNCTION DetachVar( xLocal )

   STATIC s_nStatic := 100

   RETURN {| x | ++s_nStatic, x + s_nStatic + xLocal }
