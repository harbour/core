Function Main

   // Does NOT work ???
   QOut( aFunc()[1] )

   // Does work
   a := aFunc()
   QOut( a[1] )

return NIL

Function aFunc()

   local aArray := { [Test] }

return aArray
