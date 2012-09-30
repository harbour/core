/*
 * $Id$
 */

// Testing Harbour dates management.
#include "set.ch"

PROCEDURE Main()

   LOCAL i
   LOCAL dDate := Date()

   SET DATE ANSI

   FOR i := 7 TO 49 STEP 7
      CheckDate( dDate )
      dDate += i
   NEXT

   RETURN

FUNCTION CheckDate( dDate )

   OutStd( "Testing date:", dDate, hb_eol() )
   OutStd( "Days in month..:", daysinmonth( dDate ), hb_eol() )
   OutStd( "Day of year....:", doy( dDate ), hb_eol() )
   OutStd( "Begin of month.:", bom( dDate ), hb_eol() )
   OutStd( "End of month...:", eom( dDate ), hb_eol() )
   OutStd( "Week of month..:", wom( dDate ), hb_eol() )
   OutStd( "Week of year...:", woy( dDate ), hb_eol() )
   OutStd( "Begin of year..:", boy( dDate ), hb_eol() )
   OutStd( "End of year....:", eoy( dDate ), hb_eol() )
   __Accept( "Press ENTER to continue..." )
   OutStd( Chr( 10 ), Chr( 10 ) )

   RETURN NIL
