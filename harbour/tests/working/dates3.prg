//
// $Id$
//

// Testing Harbour dates management.

#include "set.ch"

function main()

   LOCAL dDate, i

   set( _SET_DATEFORMAT, "dd/mm/yyyy" )
   dDate := cToD( "25/05/1999" )

   OutStd( dDate, dow( dDate ), chr( 10 ) )

   OutStd( LastMonday( dDate ), chr( 10 ) )

   dDate += 3
   OutStd( dDate, dow( dDate ), chr( 10 ) )

   dDate += 4
   OutStd( dDate, dow( dDate ), chr( 10 ) )

   set( _SET_DATEFORMAT, "mm/dd/yyyy" )
   dDate := cToD( "05/25/1999" )

   OutStd( dDate, dow( dDate ), chr( 10 ) )

   OutStd( LastMonday( dDate ), chr( 10 ) )

   dDate += 3
   OutStd( dDate, dow( dDate ), chr( 10 ) )

   dDate += 4
   OutStd( dDate, dow( dDate ), chr( 10 ) )
   
   OutStd( chr( 10 ) )
   dDate := DATE ()
   FOR i := 1 TO 7
      OutStd( dDate, dow( dDate ), chr( 10 ) )
      dDate++
   NEXT
   OutStd( ctod( "" ), dow( ctod( "" ) ), chr( 10 ) )

return nil

// Like NG's sample
function LastMonday( dDate )
return dDate - dow( dDate ) + 2
