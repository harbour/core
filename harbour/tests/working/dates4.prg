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

   OutStd( "Testing date:", dDate , chr( 10 ) )
   OutStd( "Days in month..:", daysinmonth( dDate ), chr( 10 ) )
   OutStd( "Day of year....:", doy( dDate ), chr( 10 ) )
   OutStd( "Begin of month.:", bom( dDate ), chr( 10 ) )
   OutStd( "End of month...:", eom( dDate ), chr( 10 ) )
   OutStd( "Week of month..:", wom( dDate ), chr( 10 ) )
   OutStd( "Week of year...:", woy( dDate ), chr( 10 ) )
   OutStd( "Begin of year..:", boy( dDate ), chr( 10 ) )
   OutStd( "End of year....:", eoy( dDate ), chr( 10 ) )
   OutStd( chr( 13 ), chr( 13 ) )
   __Accept( "Press ENTER to continue..." )

return nil
