//
// Function Array syntax test
//

Function Main

   local a

   QOut( aFunc()[1] )

   a := aFunc()
   QOut( a[1] )

   aFunc()[1] := "Something different"
   QOut( aFunc()[1] )

   aFunc()[1] := 4
   QOut( aFunc()[1] )

   aFunc()[1] += 1
   QOut( aFunc()[1] )

   aFunc()[1] -= 1
   QOut( aFunc()[1] )

   aFunc()[1] *= 2
   QOut( aFunc()[1] )

   aFunc()[1] /= 2
   QOut( aFunc()[1] )

   aFunc()[1] %= 5
   QOut( aFunc()[1] )

   aFunc()[1] ^= 3
   QOut( aFunc()[1] )

   QOut( "Global stack" )
   HBDebug( __aGlobalStack() )        // Please note a is a reference to aArray !
   QOut( "Statics")
   HBDebug( __aStatic() )
return NIL

Function aFunc()

   static aArray := { [Test] }

return aArray
