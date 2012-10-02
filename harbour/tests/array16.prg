/*
 * $Id$
 */

// Harbour multidimensional arrays support

PROCEDURE Main()

   LOCAL a := { 100, 200, "Third" }
   LOCAL b := Array( 8832 )  // 8832 elements !!! Maximum for 16 Bit !!!

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

   b[ 8832 ] := "Harbour"

   ? b[ 8832 ]

   ? ATail( b )

   ASize( b, 200 )
   ? Len( b )

   b[ 100 ] := 10
   Test( b[ 100 ]++ )
   ? b[ 100 ]

   b[ 100 ] := 10
   Test( ++b[ 100 ] )
   ? b[ 100 ]

   b := { 1, { 2, { 4, 5 } } }
   Test( b[ 2 ][ 2 ][ 1 ]++ )
   ? b[ 2 ][ 2 ][ 1 ]

   b[ 2 ][ 2 ][ 1 ] := 2
   Test( ++b[ 2 ][ 2 ][ 1 ] )
   ? b[ 2 ][ 2 ][ 1 ]

   ReleaseTest()

   RETURN

FUNCTION Test( n )

   ? n

   RETURN NIL

FUNCTION ReleaseTest()

   LOCAL a := { 1, 2, 3 }

   HB_SYMBOL_UNUSED( a )

   RETURN NIL
