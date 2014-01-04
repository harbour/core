#require "hbnf"

PROCEDURE Main()

   LOCAL dStart := hb_SToD( "19901128" )
   LOCAL dEnd   := hb_SToD( "19901130" )

   LOCAL cTimeStart := "08:00:00"
   LOCAL cTimeEnd   := "12:10:30"

   LOCAL n
   LOCAL aDataTest := ft_Elapsed( dStart, dEnd, cTimeStart, cTimeEnd )

   FOR n := 1 TO 4
      ? aDataTest[ n, 1 ], Str( aDataTest[ n, 2 ], 12, 4 ), { "days", "hours", "minutes", "seconds" }[ n ]
   NEXT

   RETURN
