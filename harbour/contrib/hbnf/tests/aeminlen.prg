/*
 * $Id$ 
 */

PROCEDURE Main()

   LOCAL var0, myarray1 := Directory()

   CLS
   ? "TEST TO DEMONSTRATE EXAMPLES OF FT_AEMINLEN"
   ?
   ? "myarray1 := DIRECTORY()"
   ?
   AEval( myarray1, {| v | QOut( PadR( v[ 1 ], 12 ), v[ 2 ], v[ 3 ], v[ 4 ], v[ 5 ] ) } )
   var0 := FT_AEMINLEN( myarray1 )
   ? PadR( "FT_AEMINLEN( myarray1 ) ->", 30 )
   ?? var0
   ?
   var0 := FT_AEMINLEN( myarray1, 2 )
   ? PadR( "FT_AEMINLEN( myarray1, 2 ) ->", 30 )
   ?? var0
   ?
   ?
   var0 := FT_AEMINLEN( myarray1[ 2 ] )
   ? PadR( "FT_AEMINLEN( myarray1[ 2 ] ) ->", 30 )
   ?? var0
   ?
   ?
   var0 := FT_AEMINLEN( myarray1, 3 )
   ? PadR( "FT_AEMINLEN( myarray1, 3 ) ->", 30 )
   ?? var0
   ?

   RETURN

