/*
 * $Id$
 */

#require "hbnf"

PROCEDURE Main( cDate )

   SET DATE ANSI
   SET CENTURY ON

   cDate := iif( cDate == NIL, DToS( Date() ), cDate )
   ? "Setting date to: " + cDate  + "... "
   ft_SetDate( hb_SToD( cDate ) )
   ? "Today is now: " + DToC( Date() )

   RETURN
