#ifdef __HARBOUR__
#require "hbct"
#endif

// Testing Harbour dates management.

PROCEDURE Main()

   LOCAL i
   LOCAL dDate := Date()

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )

   FOR i := 7 TO 49 STEP 7
      CheckDate( dDate )
      dDate += i
   NEXT

   RETURN

STATIC PROCEDURE CheckDate( dDate )

   ? "Testing date:", dDate
   ? "Days in month..:", DaysInMonth( dDate )
   ? "Day of year....:", DoY( dDate )
   ? "Begin of month.:", BoM( dDate )
   ? "End of month...:", EoM( dDate )
   ? "Week of month..:", WoM( dDate )
   ? "Begin of year..:", BoY( dDate )
   ? "End of year....:", EoY( dDate )
   ?

   RETURN
