#require "hbnf"

PROCEDURE Main()

   LOCAL dStart := 0d19901128, cTimeStart := "08:00:00"
   LOCAL dEnd   := 0d19901130, cTimeEnd   := "12:10:30"

   LOCAL aData, n, m

   FOR EACH aData IN { ;
      ft_Elapsed( dStart, dEnd, cTimeStart, cTimeEnd ), ;
      ft_Elapsed( dStart, dEnd ), ;
      ft_Elapsed( cTimeStart, cTimeEnd ) }

      FOR EACH n, m IN aData, { "days", "hours", "minutes", "seconds" }
         ? n[ 1 ], Str( n[ 2 ], 12, 4 ), m
      NEXT
   NEXT

   RETURN
