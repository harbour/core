
#require "hbnf"

PROCEDURE Main( cTime )

   cTime := iif( cTime == NIL, Time(), cTime )
   ? "Setting time to: " + cTime  + "... "
   ft_SetTime( cTime )
   ? "Time is now: " + Time()

   RETURN
