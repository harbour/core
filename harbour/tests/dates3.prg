/*
 * $Id$
 */

// Testing Harbour dates management.

#include "set.ch"

PROCEDURE Main()

   LOCAL dDate, i

   SET( _SET_DATEFORMAT, "dd/mm/yyyy" )
   dDate := hb_SToD( "19990525" )

   OutStd( dDate, DOW( dDate ), hb_eol() )

   OutStd( LastMonday( dDate ), hb_eol() )

   dDate += 3
   OutStd( dDate, DOW( dDate ), hb_eol() )

   dDate += 4
   OutStd( dDate, DOW( dDate ), hb_eol() )

   SET( _SET_DATEFORMAT, "mm/dd/yyyy" )
   dDate := hb_SToD( "19990525" )

   OutStd( dDate, DOW( dDate ), hb_eol() )

   OutStd( LastMonday( dDate ), hb_eol() )

   dDate += 3
   OutStd( dDate, DOW( dDate ), hb_eol() )

   dDate += 4
   OutStd( dDate, DOW( dDate ), hb_eol() )

   OutStd( hb_eol() )
   dDate := Date ()
   FOR i := 1 TO 7
      OutStd( dDate, DOW( dDate ), hb_eol() )
      dDate++
   NEXT
   OutStd( CToD( "" ), DOW( CToD( "" ) ), hb_eol() )

   RETURN

// Like NG's sample

FUNCTION LastMonday( dDate )

   RETURN dDate - DOW( dDate ) + 2
