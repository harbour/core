#ifndef __HARBOUR__
#include "clipper.ch"
#endif

PROCEDURE Main()

   TestIt( hb_osFileMask() )
   TestIt( hb_ps() )
   TestIt( ".." + hb_ps() )
   TestIt( ".." + hb_ps() + hb_osFileMask() )

   RETURN

STATIC PROCEDURE TestIt( cSpec )

   LOCAL a
   LOCAL a1
   LOCAL a2
   LOCAL a3
   LOCAL a4
   LOCAL a5

   // In order to account for documented behavour, this call will ensure
   // that ADir() returns the same length array in both cases.
   // ie: ADir( cSpec ) could return a different length array than
   // ADir( cSpec, , , , , {} )

   a := ADir( cSpec, , , , , {} )

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )

   a1 := Array( a )
   a2 := Array( a )
   a3 := Array( iif( a >= 1, a - 1, a ) )
   a4 := Array( a )
   a5 := Array( a + 1 )

   ? "--------------------------------------------------------"

   ADir( cSpec, a1, a2, a3, a4, a5 )

   AEval( a1, {| tmp | QOut( tmp ) } )
   AEval( a2, {| tmp | QOut( tmp ) } )
   AEval( a3, {| tmp | QOut( tmp ) } )
   AEval( a4, {| tmp | QOut( tmp ) } )
   AEval( a5, {| tmp | QOut( tmp ) } )

   ADir( cSpec, 10, "A", NIL, NIL, NIL )

   RETURN
