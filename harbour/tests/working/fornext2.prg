//
// $Id$
//

// Testing Harbour For Next loops

function Main()

   local n

   for n = 1 to Limit() Step Step()
      QOut( n )
   next

return nil

function Limit()

   QOut( "Limit" )

return 10

function Step()

   QOut( "Step" )

return 2
