/*
 * $Id$ 
 */

PROCEDURE Main( cTime )

   cTime := iif( cTime == NIL, Time(), cTime )
   QOut( "Setting time to: " + cTime  + "... " )
   FT_SETTIME( cTime )
   QOut( "Time is now: " + Time() )

   RETURN

