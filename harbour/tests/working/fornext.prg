// Testing Harbour For Next loops
//
// Copyright (C) 1999  Eddie Runia <eddie@runia.com>
// www - http://www.harbour-project.org
//
// Placed in the public domain
//

function Main()

   local n := 1

   QOut( "Testing Harbour For Next loops. Going up quick" )

   for n:=1 to 10 step 4
     QOut( n )
   next n

   QOut( "Going down" )

   for n:=10 to 1 step -1
     QOut( n )
   next n

   QOut( "No step" )

   for n:=1 to 10
     QOut( n )
   next n

   QOut( "No production" )

   for n:=1 to 10 step -1
     QOut( n )
   next n

   QOut( "Ok!" )

return nil
