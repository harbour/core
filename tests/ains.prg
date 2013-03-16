//
// Array test AIns() / ADel() / ASize() / AFill()
//

#ifndef __HARBOUR__
#include "clipper.ch"
#endif

PROCEDURE Main()

   LOCAL aFirst
   LOCAL aSecond
   LOCAL aMore

   aFirst := AClone( { 1, 2, 4 } )
   AIns( aFirst, 3 )
   aFirst[ 3 ] := "3"
   ?? "Testing AIns() ... "
   aDump( aFirst )

   aSecond := { 1, 2, 4 }
   ASize( aSecond, 4 )
   ?? "Testing ASize() ... "
   aDump( aSecond )

   aSecond := { 1, 2, 4 }
   ASize( aSecond, 4 )
   AIns( aSecond, 3 )
   aSecond[ 3 ] := "3"
   ?? "Testing ASize() + AIns() ... "
   aDump( aSecond )

   aSecond := { 1, 2, 3, 3, 4, 5 }
   ADel( aSecond, 3 )
   ?? "Testing ADel() ... "
   aDump( aSecond )

   aSecond := { 1, 2, 3, 3, 4, 5 }
   ADel( aSecond, 3 )
   ASize( aSecond, Len( aSecond ) - 1 )
   ?? "Testing ASize() + ADel() ... "
   aDump( aSecond )

   AFill( aSecond, "!" )
   ?? "Testing AFill() ... "
   aDump( aSecond )

   aMore := { 1, 2, 3, 4, 5, 6 }
   AFill( aMore, "X", 3 )
   ?? "Testing AFill() with start ... "
   aDump( aMore )

   aMore := { 1, 2, 3, 4, 5, 6 }
   AFill( aMore, "X", 3, 2 )
   ?? "Testing AFill() with start and count ... "
   aDump( aMore )

   aMore := { { 1, 2 }, { 3, 4 } }
   ADel( aMore, 1 )
   aDump( aMore )

   RETURN

PROCEDURE aDump( aShow )

   LOCAL n

   ?? "Len=", hb_ntos( Len( aShow ) )
   ?? ": "
   FOR n := 1 TO Len( aShow )

      ?? "["
      ?? hb_ntos( n )
      ?? "]= "
      ?? ValType( aShow[ n ] )
      ?? ":"
      IF HB_ISARRAY( aShow[ n ] ) /* Iterate array */
         ?
         ?? "["
         aDump( aShow[ n ] )
         ?? "]"
      ELSE
         ?? iif( HB_ISNUMERIC( aShow[ n ] ), hb_ntos( aShow[ n ] ), aShow[ n ] )
      ENDIF

      IF n != Len( aShow )
         ?? ", "
      ENDIF

   NEXT
   ?

   RETURN
