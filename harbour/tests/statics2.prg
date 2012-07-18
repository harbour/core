/*
 * $Id$
 */

// Statics overlapped!
//
// Compile statics1.prg, statics2.prg and link both files

STATIC uA, uB

PROCEDURE Test()

   QOut( "INSIDE statics2.prg" )
   QOut( "   static uA, uB" )
   QOut( "" )
   QOut( "   ValType( uA ), ValType( uB ) =>", ValType( uA ), ",", ValType( uB ) )
   QOut( "   uA, uB =>", uA, ",", uB )
   uA := "a"
   uB := "b"
   QOut( '   uA := "a"' )
   QOut( '   uB := "b"' )
   QOut( "   uA, uB =>", uA, ",", uB )
   QOut( "" )

   RETURN
