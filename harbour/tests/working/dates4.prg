/*
 * $Id$
 */

// Testing Harbour dates management.
#include "set.ch"

STATIC s_cNewLine

function main()
   LOCAL i
   LOCAL dDate := date()

   s_cNewLine := OS_NewLine()

   set( _SET_DATEFORMAT, "dd/mm/yyyy" )

   for i := 7 to 49 step 7
      CheckDate( dDate )
      dDate += i
   next

return nil

function CheckDate( dDate )
   OutStd( "Testing date:", dDate , s_cNewLine )
   OutStd( "Days in month..:", daysinmonth( dDate ), s_cNewLine )
   OutStd( "Day of year....:", doy( dDate ), s_cNewLine )
   OutStd( "Begin of month.:", bom( dDate ), s_cNewLine )
   OutStd( "End of month...:", eom( dDate ), s_cNewLine )
   OutStd( "Week of month..:", wom( dDate ), s_cNewLine )
   OutStd( "Week of year...:", woy( dDate ), s_cNewLine )
   OutStd( "Begin of year..:", boy( dDate ), s_cNewLine )
   OutStd( "End of year....:", eoy( dDate ), s_cNewLine )
   __Accept( "Press ENTER to continue..." )
   OutStd( chr( 10 ), chr( 10 ) )

return nil
