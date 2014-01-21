#require "hbnf"

PROCEDURE Main( cDate, cDays )

   LOCAL nDays := ft_AddWkDy( hb_SToD( cDate ), Val( cDays ) )

   ? "Num days to add: " + Str( nDays )
   ? "New date:        " + DToC( hb_SToD( cDate ) + nDays )

   RETURN
