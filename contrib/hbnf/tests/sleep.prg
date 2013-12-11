#require "hbnf"

// Invoke by running SLEEP 1.0 to sleep 1.0 seconds

PROCEDURE Main( nSleep )

   hb_default( @nSleep, hb_ntos( 1 ) )

   ? "Time is now:", Time()
   ft_Sleep( Val( nSleep ) )
   ? "Time is now:", Time()

   RETURN
