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
   OutStd( "Testing date:", dDate , hb_eol() )
   OutStd( "Days in month..:", daysinmonth( dDate ), hb_eol() )
   OutStd( "Day of year....:", doy( dDate ), hb_eol() )
   OutStd( "Begin of month.:", bom( dDate ), hb_eol() )
   OutStd( "End of month...:", eom( dDate ), hb_eol() )
   OutStd( "Week of month..:", wom( dDate ), hb_eol() )
   OutStd( "Week of year...:", woy( dDate ), hb_eol() )
   OutStd( "Begin of year..:", boy( dDate ), hb_eol() )
   OutStd( "End of year....:", eoy( dDate ), hb_eol() )
   __Accept( "Press ENTER to continue..." )
   OutStd( chr( 10 ), chr( 10 ) )

   return nil
