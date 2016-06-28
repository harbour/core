#ifndef __HARBOUR__
#include "clipper.ch"
#endif

PROCEDURE Main()

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )

   TestIt( hb_osFileMask() )
   TestIt( hb_ps() )
   TestIt( ".." + hb_ps() )
   TestIt( ".." + hb_ps() + hb_osFileMask() )

   RETURN

// In order to account for documented behavour, this call will ensure
// that ADir() returns the same length array in both cases.
// ie: ADir( cSpec ) could return a different length array than
// ADir( cSpec, , , , , {} )

STATIC PROCEDURE TestIt( cSpec )

   LOCAL a  := ADir( cSpec, , , , , {} )

   LOCAL a1 := Array( a )
   LOCAL a2 := Array( a )
   LOCAL a3 := Array( iif( a >= 1, a - 1, a ) )
   LOCAL a4 := Array( a )
   LOCAL a5 := Array( a + 1 )

   ? "---"

   ADir( cSpec, a1, a2, a3, a4, a5 )

   AEval( a1, {| tmp | QOut( tmp ) } )
   AEval( a2, {| tmp | QOut( tmp ) } )
   AEval( a3, {| tmp | QOut( tmp ) } )
   AEval( a4, {| tmp | QOut( tmp ) } )
   AEval( a5, {| tmp | QOut( tmp ) } )

   ADir( cSpec, 10, "A", , , )

   RETURN
