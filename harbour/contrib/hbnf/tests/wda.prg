/*
 * $Id$
 */

#require "hbnf"

PROCEDURE Main( cDate, cDays )

   LOCAL nDays := ft_addWkDy( CToD( cDate ), Val( cDays ) )

   ? "Num days to add: " + Str( nDays )
   ? "New date:        " + DToC( CToD( cDate ) + nDays )

   RETURN
