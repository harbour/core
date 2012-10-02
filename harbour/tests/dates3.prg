/*
 * $Id$
 */

// Testing Harbour dates management.

#include "set.ch"

PROCEDURE Main()

   LOCAL dDate, i

   Set( _SET_DATEFORMAT, "dd/mm/yyyy" )
   dDate := hb_SToD( "19990525" )

   ? dDate, DOW( dDate )

   ? LastMonday( dDate )

   dDate += 3
   ? dDate, DOW( dDate )

   dDate += 4
   ? dDate, DOW( dDate )

   Set( _SET_DATEFORMAT, "mm/dd/yyyy" )
   dDate := hb_SToD( "19990525" )

   ? dDate, DOW( dDate )

   ? LastMonday( dDate )

   dDate += 3
   ? dDate, DOW( dDate )

   dDate += 4
   ? dDate, DOW( dDate )

   ?
   dDate := Date ()
   FOR i := 1 TO 7
      ? dDate, DOW( dDate )
      dDate++
   NEXT
   ? CToD( "" ), DOW( CToD( "" ) )

   RETURN

// Like NG's sample

FUNCTION LastMonday( dDate )

   RETURN dDate - DOW( dDate ) + 2
