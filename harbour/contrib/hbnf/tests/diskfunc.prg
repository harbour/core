/*
 * $Id$ 
 */

PROCEDURE Main( cDrv )

   QOut( "Disk size:   " + Str( FT_DSKSIZE( cDrv ) ) )
   QOut( "Free bytes:  " + Str( FT_DSKFREE( cDrv ) ) )

   RETURN

