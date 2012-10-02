/*
 * $Id$
 */

#require "hbnf"

PROCEDURE Main( cDate )

   cDate := iif( cDate == NIL, DToC( Date() ), cDate )
   ? "Setting date to: " + cDate  + "... "
   FT_SETDATE( CToD( cDate ) )
   ? "Today is now: " + DToC( Date() )

   RETURN
