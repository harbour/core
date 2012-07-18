/*
 * $Id$
 */

// Statics overlapped!
//
// Compile statics1.prg, statics2.prg and link both files

STATIC uOne, uTwo

PROCEDURE Main()

   QOut( "Statics overlapped!" )
   QOut( "===================" )
   QOut( "" )
   QOut( "INSIDE statics1.prg" )
   QOut( "   static uOne, uTwo" )
   QOut( "" )
   QOut( "   uOne, uTwo =>", uOne, ",", uTwo )
   uOne := 1
   uTwo := 2
   QOut( "   uOne := 1" )
   QOut( "   uOne := 2" )
   QOut( "   uOne, uTwo =>", uOne, ",", uTwo )
   QOut( "" )
   Test()
   QOut( "INSIDE statics1.prg" )
   QOut( "   uOne, uTwo =>", uOne, ",", uTwo )
   QOut( "" )

   RETURN
