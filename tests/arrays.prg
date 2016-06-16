// Harbour multidimensional arrays support

PROCEDURE Main()

   LOCAL a := { 100, 200, "Third" }
   LOCAL b := Array( 10000 )  // 10000 elements !

   ? ValType( a )
   ? ValType( { "A" } )

   AAdd( a, "new element" )
   ? Len( a )

   ? a[ 1 ]
   ? a[ 2 ]
   ? a[ 3 ]
   ? a[ 4 ]

   ? ATail( a )

   a[ 3 ] := { "this", { "seems", "to", { "work", "so", "well" } } }
   ? a[ 3 ][ 2 ][ 3 ][ 1 ] // "work"

   a[ 3, 2 ][ 3, 1 ] := "Harbour power!"  // different ways to specify the indexes
   ? a[ 3, 2, 3, 1 ]

   ? ValType( b )
   ? Len( b )

   b[ 8000 ] := "Harbour"

   ? b[ 8000 ]

   ASize( b, 2000 )
   ? Len( b )

   b[ 1000 ] := 10
   Test( b[ 1000 ]++ )
   ? b[ 1000 ]

   b[ 1000 ] := 10
   Test( ++b[ 1000 ] )
   ? b[ 1000 ]

   b := { 1, { 2, { 4, 5 } } }
   Test( b[ 2 ][ 2 ][ 1 ]++ )
   ? b[ 2 ][ 2 ][ 1 ]

   b[ 2 ][ 2 ][ 1 ] := 2
   Test( ++b[ 2 ][ 2 ][ 1 ] )
   ? b[ 2 ][ 2 ][ 1 ]

   ReleaseTest()

   RETURN

STATIC PROCEDURE Test( n )

   ? n

   RETURN

STATIC PROCEDURE ReleaseTest()

   LOCAL a := { 1, 2, 3 }

   HB_SYMBOL_UNUSED( a )

   RETURN
