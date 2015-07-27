/* Donated to the public domain on 2001-03-08 by David G. Holm <dholm@jsd-llc.com>
   Test Seconds() */

#ifndef __HARBOUR__
#include "clipper.ch"
#endif

PROCEDURE Main( cParam )

   LOCAL nLimit := Val( hb_defaultValue( cParam, "10" ) )
   LOCAL n

   CLS

   ? Seconds()
   FOR n := 1 TO nLimit
      IF Empty( cParam )
         ? "Pause:"
         Inkey( 0 )
      ENDIF
      ? Seconds()
   NEXT

   RETURN
