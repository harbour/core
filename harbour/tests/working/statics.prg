// Testing Harbour statics variables management

static z := "First"

function Main()

   static a := "Hello", b := { 1, 3 }

   QOut( a )
   QOut( b[ 2 ] )

   Two()

   QOut( "Ok!" )

return nil

function Two()

   static a := "Test"

   QOut( a )

return nil
