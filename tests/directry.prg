// Directory() test

#include "directry.ch"

PROCEDURE Main( filespec, attribs )

   LOCAL aDir
   LOCAL x

// aDir := ASort( Directory( filespec, attribs ),,, {| x, y | Upper( x[ F_NAME ] ) < Upper( y[ F_NAME ] ) } )
   aDir := Directory( filespec, attribs )

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )

   FOR x := 1 TO Len( aDir )
      ? ;
         PadR( aDir[ x, F_NAME ], 20 ), "|", ;
         Transform( aDir[ x, F_SIZE ], "9,999,999,999" ), "|", ;
         aDir[ x, F_DATE ], "|", ;
         aDir[ x, F_TIME ], "|", ;
         aDir[ x, F_ATTR ]
   NEXT

#ifdef __HARBOUR__
   FOR EACH x IN hb_Directory( filespec, attribs )
      ? ;
         PadR( x[ F_NAME ], 20 ), "|", ;
         Transform( x[ F_SIZE ], "9,999,999,999" ), "|", ;
         x[ HB_F_DATETIME ], "|", ;
         x[ F_ATTR ]
   NEXT
#endif

   RETURN
