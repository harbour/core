#require "hbnf"

PROCEDURE Main( cTime )

   hb_default( @cTime, Time() )

   ? "Setting time to:", cTime
   ft_SetTime( cTime )
   ? "Time is now:", Time()

   RETURN
