Function Main()
Local a := TestBlocks()

   qout( eval( a[ 1 ] ) )      // 23
   qout( eval( a[ 2 ], 42 ) )  // 42
   qout( eval( a[ 1 ] ) )      // 42
   qout( eval( a[ 2 ], 15 ) )  // 15

   mqout( 15, eval( a[ 1 ] ) )      // 15 15
   mqout( 14, eval( a[ 1 ] ) )      // 14 15
   mqout( 42, eval( a[ 2 ], 42 ) )  // 42 42
   mqout( 14, eval( a[ 2 ], 42 ) )  // 14 42
   mqout( 42, eval( a[ 1 ] ) )      // 42 42
   mqout( 14, eval( a[ 1 ] ) )      // 14 42
   
Return( NIL )

Static Function TestBlocks()
Local nFoo := 23
Return( { {|| nFoo }, {|n| nFoo := n } } )

Static Function mqout( nExpected, nGot )

   qout( nExpected, nGot )
   
Return( NIL )
