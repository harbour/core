//
// $Id$
//

// Testing the Harbour related error system functions

function Main()

   local oError := ErrorNew()

   QOut( oError:ClassName() )   // Be aware this will print ERROR

   oError:Description = "Its description"

   QOut( oError:Description )
   QOut( Len( oError ) )

return nil
