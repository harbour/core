/*
 * $Id$
 */

#require "hbnf"

PROCEDURE Main( cDrv )

   ? "Disk size:   " + Str( FT_DSKSIZE( cDrv ) )
   ? "Free bytes:  " + Str( FT_DSKFREE( cDrv ) )

   RETURN
