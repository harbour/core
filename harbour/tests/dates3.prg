/*
 * $Id$
 */

// Testing Harbour dates management.

#include "set.ch"

function main()

   LOCAL dDate, i

   set( _SET_DATEFORMAT, "dd/mm/yyyy" )
   dDate := cToD( "25/05/1999" )

   OutStd( dDate, dow( dDate ), hb_eol() )

   OutStd( LastMonday( dDate ), hb_eol() )

   dDate += 3
   OutStd( dDate, dow( dDate ), hb_eol() )

   dDate += 4
   OutStd( dDate, dow( dDate ), hb_eol() )

   set( _SET_DATEFORMAT, "mm/dd/yyyy" )
   dDate := cToD( "05/25/1999" )

   OutStd( dDate, dow( dDate ), hb_eol() )

   OutStd( LastMonday( dDate ), hb_eol() )

   dDate += 3
   OutStd( dDate, dow( dDate ), hb_eol() )

   dDate += 4
   OutStd( dDate, dow( dDate ), hb_eol() )

   OutStd( hb_eol() )
   dDate := DATE ()
   FOR i := 1 TO 7
      OutStd( dDate, dow( dDate ), hb_eol() )
      dDate++
   NEXT
   OutStd( ctod( "" ), dow( ctod( "" ) ), hb_eol() )

   return nil

// Like NG's sample
function LastMonday( dDate )
   return dDate - dow( dDate ) + 2
