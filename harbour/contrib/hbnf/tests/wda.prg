/*
 * $Id$
 */

#require "hbnf"

PROCEDURE Main( cDate, cDays )

   LOCAL nDays := ft_addWkDy( SToD( cDate ), Val( cDays ) )

   ? "Num days to add: " + Str( nDays )
   ? "New date:        " + DToC( SToD( cDate ) + nDays )

   RETURN
