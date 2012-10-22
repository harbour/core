/*
 * $Id$
 */

// Testing Harbour dates management.

PROCEDURE Main()

   LOCAL i
   LOCAL dDate := Date()

   SET DATE ANSI
   SET CENTURY ON

   FOR i := 7 TO 49 STEP 7
      CheckDate( dDate )
      dDate += i
   NEXT

   RETURN

FUNCTION CheckDate( dDate )

   ? "Testing date:", dDate
   ? "Days in month..:", daysinmonth( dDate )
   ? "Day of year....:", doy( dDate )
   ? "Begin of month.:", bom( dDate )
   ? "End of month...:", eom( dDate )
   ? "Week of month..:", wom( dDate )
   ? "Begin of year..:", boy( dDate )
   ? "End of year....:", eoy( dDate )
   ?

   RETURN NIL
