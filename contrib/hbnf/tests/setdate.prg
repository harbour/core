
#require "hbnf"

PROCEDURE Main( cDate )

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )

   cDate := iif( cDate == NIL, DToS( Date() ), cDate )
   ? "Setting date to: " + cDate  + "... "
   ft_SetDate( hb_SToD( cDate ) )
   ? "Today is now: " + DToC( Date() )

   RETURN
