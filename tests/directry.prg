// Directory() test

#include "directry.ch"

PROCEDURE Main( filespec, attribs )

   LOCAL aDir
   LOCAL x

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )

   aDir := Directory( filespec, attribs )

   FOR x := 1 TO Len( aDir )
      ? ;
         PadR( aDir[ x ][ F_NAME ], 20 ), "|", ;
         Transform( aDir[ x ][ F_SIZE ], "9,999,999,999" ), "|", ;
         aDir[ x ][ F_DATE ], "|", ;
         aDir[ x ][ F_TIME ], "|", ;
         aDir[ x ][ F_ATTR ]
   NEXT

#ifdef __HARBOUR__
   FOR EACH x IN ASort( hb_Directory( filespec, attribs ),,, {| x, y | x[ F_DATE ] < y[ F_DATE ] } )
      ? ;
         PadR( x[ F_NAME ], 20 ), "|", ;
         Transform( x[ F_SIZE ], "9,999,999,999" ), "|", ;
         x[ F_DATE ], "|", ;
         x[ F_ATTR ]
   NEXT
#endif

   RETURN
