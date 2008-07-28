//
// $Id$
//

// Harbour multidimensional arrays support

function Main()

   local a := { 100, 200, "Third" }
   local b := Array( 8832 )  // 8832 elements !!! Maximum for 16 Bit !!!

   QOut( ValType( a ) )
   QOut( ValType( { "A" } ) )

   AAdd( a, "new element" )
   QOut( Len( a ) )

   QOut( a[ 1 ] )
   QOut( a[ 2 ] )
   QOut( a[ 3 ] )
   QOut( a[ 4 ] )

   QOut( ATail( a ) )

   a[ 3 ] := { "this", { "seems", "to", { "work", "so", "well" } } }
   QOut( a[ 3 ][ 2 ][ 3 ][ 1 ] ) // "work"

   a[ 3, 2 ][ 3, 1 ] := "Harbour power!"  // different ways to specify the indexes
   QOut( a[ 3, 2, 3, 1 ] )

   QOut( ValType( b ) )
   QOut( Len( b ) )

   b[ 8832 ] := "Harbour"

   QOut( b[ 8832 ] )

   QOut( atail( b ) )

   ASize( b, 200 )
   QOut( Len( b ) )

   b[ 100 ] := 10
   Test( b[ 100 ]++ )
   QOut( b[ 100 ] )

   b[ 100 ] := 10
   Test( ++b[ 100 ] )
   QOut( b[ 100 ] )

   b := { 1, { 2, { 4, 5 } } }
   Test( b[ 2 ][ 2 ][ 1 ]++ )
   QOut( b[ 2 ][ 2 ][ 1 ] )

   b[ 2 ][ 2 ][ 1 ] := 2
   Test( ++b[ 2 ][ 2 ][ 1 ] )
   QOut( b[ 2 ][ 2 ][ 1 ] )

   ReleaseTest()

return nil

function Test( n )

   QOut( n )

return nil

function ReleaseTest()

   local a := { 1, 2, 3 }

return nil
