/*
 * $Id$
 */

#require "hbnf"

PROCEDURE Main( cTime )

   cTime := iif( cTime == NIL, Time(), cTime )
   ? "Setting time to: " + cTime  + "... "
   FT_SETTIME( cTime )
   ? "Time is now: " + Time()

   RETURN
