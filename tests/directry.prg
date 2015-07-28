/* Directory listing tests */

#ifndef __HARBOUR__
#include "clipper.ch"
#endif

#include "directry.ch"

PROCEDURE Main( filespec, attribs )

   LOCAL aDir := hb_vfDirectory( filespec, attribs )
   LOCAL x

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )

   FOR x := 1 TO Len( aDir )
      ? ;
         PadR( aDir[ x ][ F_NAME ], 20 ), "|", ;
         Transform( aDir[ x ][ F_SIZE ], "9,999,999,999" ), "|", ;
         hb_TToD( aDir[ x ][ F_DATE ] ), "|", aDir[ x ][ F_TIME ], "|", ;
         aDir[ x ][ F_ATTR ]
   NEXT

#ifdef __HARBOUR__
   FOR EACH x IN ASort( hb_vfDirectory( filespec, attribs ),,, {| x, y | x[ F_DATE ] < y[ F_DATE ] } )
      ? ;
         PadR( x[ F_NAME ], 20 ), "|", ;
         Transform( x[ F_SIZE ], "9,999,999,999" ), "|", ;
         x[ F_DATE ], "|", ;
         x[ F_ATTR ]
   NEXT
#endif

   RETURN
