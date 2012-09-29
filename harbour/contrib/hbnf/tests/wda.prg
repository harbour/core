/*
 * $Id$ 
 */

PROCEDURE Main( cDate, cDays )

   LOCAL nDays := ft_addWkDy( CToD( cDate ), Val( cDays ) )

   QOut( "Num days to add: " + Str( nDays ) )
   QOut( "New date:        " + DToC( CToD( cDate ) + nDays ) )

   RETURN

