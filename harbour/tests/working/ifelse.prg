//
// $Id$
//

// Testing Harbour If elseif else endif

function Main()

   local i

   QOut( "Testing Harbour If elseif else endif" )
   for i := 1 to 5
      TestValue( i )
   next

return nil

function TestValue( x )

   if x = 1
      QOut( "x is 1" )

   elseif x = 2
      QOut( "x is 2" )

   elseif x = 3
      QOut( "x is 3" )

   elseif x = 4
      QOut( "x is 4" )

   else
      QOut( "x is not 1 or 2 or 3 or 4" )
   endif

   QOut( "Ok!" )

return nil
