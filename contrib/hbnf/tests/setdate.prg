#require "hbnf"

PROCEDURE Main( cDate )

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )

   hb_default( @cDate, DToS( Date() ) )

   ? "Setting date to:", hb_SToD( cDate )
   ft_SetDate( hb_SToD( cDate ) )
   ? "Today is now:", Date()

   RETURN
