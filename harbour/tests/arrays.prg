/*
 * $Id$
 */

// Harbour multidimensional arrays support

PROCEDURE Main()

   LOCAL a := { 100, 200, "Third" }
   LOCAL b := Array( 10000 )  // 10.000 elements !!!

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

   b[ 8000 ] := "Harbour"

   QOut( b[ 8000 ] )

   ASize( b, 2000 )
   QOut( Len( b ) )

   b[ 1000 ] := 10
   Test( b[ 1000 ]++ )
   QOut( b[ 1000 ] )

   b[ 1000 ] := 10
   Test( ++b[ 1000 ] )
   QOut( b[ 1000 ] )

   b := { 1, { 2, { 4, 5 } } }
   Test( b[ 2 ][ 2 ][ 1 ]++ )
   QOut( b[ 2 ][ 2 ][ 1 ] )

   b[ 2 ][ 2 ][ 1 ] := 2
   Test( ++b[ 2 ][ 2 ][ 1 ] )
   QOut( b[ 2 ][ 2 ][ 1 ] )

   ReleaseTest()

   RETURN

FUNCTION Test( n )

   QOut( n )

   RETURN nil

FUNCTION ReleaseTest()

   LOCAL a := { 1, 2, 3 }

   RETURN nil
