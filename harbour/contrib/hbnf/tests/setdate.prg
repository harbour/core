/*
 * $Id$
 */

#require "hbnf"

PROCEDURE Main( cDate )

   SET DATE ANSI
   SET CENTURY ON

   cDate := iif( cDate == NIL, DToS( Date() ), cDate )
   ? "Setting date to: " + cDate  + "... "
   FT_SETDATE( SToD( cDate ) )
   ? "Today is now: " + DToC( Date() )

   RETURN
