//
// Spawn()
//
// This program adds a .HRB at run-time
//
function Main()

   QOut( "We are now in spawn" )
   HB_Run( "Hello.hrb" )                // Load & Run Hello.hrb
   QOut( "We are back again" )
return nil
