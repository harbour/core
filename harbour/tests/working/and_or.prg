// Testing Harbour AND OR operators

function And_Or()  // Main() is not required!

   QOut( "Testing logical shortcuts" )

   if .f. .and. QOut( "this should not show!" ) // and it should not break!
   endif

   QOut( "Testing .t. .t." )
   AndOr( .t., .t. )

   QOut( "Testing .t. .f." )
   AndOr( .t., .f. )

   QOut( "Testing .f. .f." )
   AndOr( .f., .f. )

   QOut( "Testing errors..." )
   AndOr( 1, .t. )

return nil

function AndOr( lValue1, lValue2 )

   if lValue1 .and. lValue2
      QOut( "They are both true" )
   else
      QOut( "They are not both true" )
   endif

   if lValue1 .or. lValue2
      QOut( "At least one of them is true" )
   else
      QOut( "None of them are true" )
   endif

return nil
