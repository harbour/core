#require "hbnf"

PROCEDURE Main()

   LOCAL dStart, dEnd, cTimeStart, cTimeEnd, n, aDataTest

   dStart := hb_SToD( "19901128" )
   dEnd   := hb_SToD( "19901130" )
   cTimeStart := "08:00:00"
   cTimeEnd   := "12:10:30"

   aDataTest := ft_Elapsed( dStart, dEnd, cTimeStart, cTimeEnd )
   FOR n := 1 TO 4
      ? aDataTest[ n, 1 ], Str( aDataTest[ n, 2 ], 12, 4 )
      ?? " "
      ?? iif( n == 1, "Days", iif( n == 2, "Hours", iif( n == 3, "Mins.", "Secs." ) ) )
   NEXT

   RETURN
