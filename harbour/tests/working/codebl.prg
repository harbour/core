Function Main()
Local nTest
Local bBlock1 := MakeBlock()
Local bBlock2 := {|| DoThing( @nTest ), qout( nTest ) }

   eval( bBlock1 )
   eval( bBlock2 )

Return( NIL )

Function MakeBlock()
Local nTest
Return( {|| DoThing( @nTest ), qout( nTest ) } )

Function DoThing( n )

   n := 42

Return( NIL )
