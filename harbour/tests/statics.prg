/*
 * $Id$
 */

// Testing Harbour statics variables management

STATIC s_z := "First"

PROCEDURE Main()

   LOCAL i, cb

   STATIC a := "Hello", b := { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 }

   QOut( a )
   QOut( b[ 2 ] )

   Two()

   QOut( "Ok!" )

   FOR i := 1 TO 10
      NumStat()
   NEXT

   cb := DetachVar( 10 )
   FOR i := 1 TO 10
      QOut( Eval( cb, b[ i ] ) )
   NEXT

   RETURN

FUNCTION Two()

   STATIC a := "Test"

   QOut( a )

   RETURN NIL

FUNCTION THREE( p )

   QOut( p )

   RETURN p

PROCEDURE NumStat( a )

   STATIC s_n := 1

   LOCAL cb

// STATIC m := s_n    // uncomment it to see an error
// STATIC m := Time() // uncomment it to see an error

   HB_SYMBOL_UNUSED( a )

   cb := {| x | s_z + Str( x ) }
   QOut( ++s_n )
   QOut( Eval( cb, s_n ) )

   RETURN

FUNCTION DetachVar( xLocal )

   STATIC xStatic := 100

   RETURN {| x | ++xStatic, x + xStatic + xLocal }
