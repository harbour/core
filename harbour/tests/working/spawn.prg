//
// Spawn()
//
// This program adds a .HRB at run-time
//
// Compile Spawn2() before compiling Spawn()
//
// Should be compiled using /gHRB or stub.
//
function Main()

   QOut( "We are now in spawn" )
   HB_Run( "Spawn2.hrb" )                // Load & Run Hello.hrb
   QOut( "We are back again" )
return nil

function SomeWhereElse()

   QOut( "I am being called from somewhere else" )
return nil
