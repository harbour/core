#require "hbnf"

PROCEDURE Main( cDate, cDays )

   LOCAL nDays

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )

   hb_default( @cDate, DToS( Date() ) )
   hb_default( @cDays, "10" )

   nDays := ft_AddWkDy( hb_SToD( cDate ), Val( cDays ) )

   ? "Num days to add:", nDays
   ? "New date:       ", hb_SToD( cDate ) + nDays

   RETURN
