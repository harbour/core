// Managing variables by reference

function Main()

   local x := 0

   QOut( "Managing variables by reference" )

   ref( @x )

   QOut( x )

return nil

function ref( x )

  x := 999

return nil

