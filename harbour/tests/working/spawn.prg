//NOTEST
//
// Spawn()
//
// This program adds a .HRB at run-time
//
// Compile Spawn2() before compiling Spawn()
//
// Should be compiled using /gHRB or stub.
//
// Written by Eddie Runia <eddie@runia.com>
// www - http://www.harbour-project.org
//
// Placed in the public domain
//
function Main( cRun )

   cRun := Default( cRun, "Spawn2.hrb" )
   QOut( "We are now in spawn" )
   HB_Run( cRun )                               // Load & Run program
   QOut( "We are back again" )
return nil

function SomeWhereElse()

   QOut( "I am being called from somewhere else" )
return nil
