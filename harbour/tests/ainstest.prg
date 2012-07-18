/*
 * $Id$
 */

//
// Array test AIns / ADel / ASize / AFill
//

PROCEDURE Main()

   LOCAL aFirst
   LOCAL aSecond
   LOCAL aMore

   aFirst := AClone( { 1, 2, 4 } )
   AIns( aFirst, 3 )
   aFirst[ 3 ] := "3"
   QQOut( "Testing aIns .. " )
   aDump( aFirst )

   aSecond := { 1, 2, 4 }
   ASize( aSecond, 4 )
   QQOut( "Testing aSize .. " )
   aDump( aSecond )

   aSecond := { 1, 2, 4 }
   ASize( aSecond, 4 )
   AIns( aSecond, 3 )
   aSecond[ 3 ] := "3"
   QQOut( "Testing aSize + aIns .. " )
   aDump( aSecond )

   aSecond := { 1, 2, 3, 3, 4, 5 }
   ADel( aSecond, 3 )
   QQOut( "Testing aDel .. " )
   aDump( aSecond )

   aSecond := { 1, 2, 3, 3, 4, 5 }
   ADel( aSecond, 3 )
   ASize( aSecond, Len( aSecond ) - 1 )
   QQOut( "Testing aSize + aDel .. " )
   aDump( aSecond )

   AFill( aSecond, "!" )
   QQOut( "Testing aFill .. " )
   aDump( aSecond )

   aMore := { 1, 2, 3, 4, 5, 6 }
   AFill( aMore, "X", 3 )
   QQOut( "Testing aFill with start .. " )
   aDump( aMore )

   aMore := { 1, 2, 3, 4, 5, 6 }
   AFill( aMore, "X", 3, 2 )
   QQOut( "Testing aFill with start and count .. " )
   aDump( aMore )

   aMore := { { 1, 2 }, { 3, 4 } }
   ADel( aMore, 1 )
   aDump( aMore )

   RETURN

FUNCTION aDump( aShow )

   LOCAL n
   LOCAL CRLF := Chr( 13 ) + Chr( 10 )

   QQOut( "Len=", hb_ntos( Len( aShow ) ) )
   QQOut( ": " )
   FOR n := 1 TO Len( aShow )

      QQOut( "[" )
      QQOut( hb_ntos( n ) )
      QQOut( "]= " )
      QQOut( ValType( aShow[ n ] ) )
      QQOut( ":" )
      IF ValType( aShow[ n ] ) == "A"             /* Iterate array         */
         QQOut( CRLF )
         QQOut( "[" )
         aDump( aShow[ n ] )
         QQOut( "]" )
      ELSE
         QQOut( aShow[ n ] )
      ENDIF

      IF n != Len( aShow )
         QQOut( ", " )
      ENDIF

   NEXT
   QQOut( CRLF )

   RETURN nil
