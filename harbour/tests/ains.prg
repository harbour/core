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
   ?? "Testing aIns .. "
   aDump( aFirst )

   aSecond := { 1, 2, 4 }
   ASize( aSecond, 4 )
   ?? "Testing aSize .. "
   aDump( aSecond )

   aSecond := { 1, 2, 4 }
   ASize( aSecond, 4 )
   AIns( aSecond, 3 )
   aSecond[ 3 ] := "3"
   ?? "Testing aSize + aIns .. "
   aDump( aSecond )

   aSecond := { 1, 2, 3, 3, 4, 5 }
   ADel( aSecond, 3 )
   ?? "Testing aDel .. "
   aDump( aSecond )

   aSecond := { 1, 2, 3, 3, 4, 5 }
   ADel( aSecond, 3 )
   ASize( aSecond, Len( aSecond ) - 1 )
   ?? "Testing aSize + aDel .. "
   aDump( aSecond )

   AFill( aSecond, "!" )
   ?? "Testing aFill .. "
   aDump( aSecond )

   aMore := { 1, 2, 3, 4, 5, 6 }
   AFill( aMore, "X", 3 )
   ?? "Testing aFill with start .. "
   aDump( aMore )

   aMore := { 1, 2, 3, 4, 5, 6 }
   AFill( aMore, "X", 3, 2 )
   ?? "Testing aFill with start and count .. "
   aDump( aMore )

   aMore := { { 1, 2 }, { 3, 4 } }
   ADel( aMore, 1 )
   aDump( aMore )

   RETURN

FUNCTION aDump( aShow )

   LOCAL n

   ?? "Len=", hb_ntos( Len( aShow ) )
   ?? ": "
   FOR n := 1 TO Len( aShow )

      ?? "["
      ?? hb_ntos( n )
      ?? "]= "
      ?? ValType( aShow[ n ] )
      ?? ":"
      IF ValType( aShow[ n ] ) == "A"             /* Iterate array         */
         ?? hb_eol()
         ?? "["
         aDump( aShow[ n ] )
         ?? "]"
      ELSE
         ?? aShow[ n ]
      ENDIF

      IF n != Len( aShow )
         ?? ", "
      ENDIF

   NEXT
   ?? hb_eol()

   RETURN NIL
