// Statics overlapped!
//
// Compile statics1.prg, statics2.prg and link both files

static uOne, uTwo

function Main()

   QOut( "Statics overlapped!" )
   QOut( "===================" )
   QOut( "" )
   QOut( "INSIDE STATICS1.PRG" )
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
   QOut( "INSIDE STATICS1.PRG" )
   QOut( "   uOne, uTwo =>", uOne, ",", uTwo )
   QOut( "" )

return nil
