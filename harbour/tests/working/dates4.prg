/*
 * $Id$
 */

// Testing Harbour dates management.
#include "set.ch"

function main()

   LOCAL i
   LOCAL dDate := date()

   set( _SET_DATEFORMAT, "dd/mm/yyyy" )

   for i := 7 to 49 step 7
      CheckDate( dDate )
      dDate += i
   next

return nil

function CheckDate( dDate )
   local cNewLine := chr( 13 ) + chr( 10 )
   OutStd( "Testing date:", dDate , cNewLine )
   OutStd( "Days in month..:", daysinmonth( dDate ), cNewLine )
   OutStd( "Day of year....:", doy( dDate ), cNewLine )
   OutStd( "Begin of month.:", bom( dDate ), cNewLine )
   OutStd( "End of month...:", eom( dDate ), cNewLine )
   OutStd( "Week of month..:", wom( dDate ), cNewLine )
   OutStd( "Week of year...:", woy( dDate ), cNewLine )
   OutStd( "Begin of year..:", boy( dDate ), cNewLine )
   OutStd( "End of year....:", eoy( dDate ), cNewLine )
   OutStd( chr( 10 ), chr( 10 ) )
   __Accept( "Press ENTER to continue..." )

return nil
