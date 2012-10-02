/*
 * $Id$
 */

#require "hbnf"

PROCEDURE Main( cDate )

   cDate := iif( cDate == NIL, DToC( Date() ), cDate )
   QOut( "Setting date to: " + cDate  + "... " )
   FT_SETDATE( CToD( cDate ) )
   QOut( "Today is now: " + DToC( Date() ) )

   RETURN
