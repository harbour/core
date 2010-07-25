/*
 * $Id$
 */

/* Test SECONDS() */
/* Harbour Project source code
   http://harbour-project.org/
   Donated to the public domain on 2001-03-08 by David G. Holm <dholm@jsd-llc.com>
*/

function Main( cParam )
local n, limit := 10

   IF ! EMPTY( cParam )
      limit := VAL( cParam )
   ENDIF
   OUTSTD( hb_eol() )
   OUTSTD( SECONDS() )
   FOR n := 1 TO limit
      IF EMPTY( cParam )
         OUTSTD( hb_eol() )
         OUTSTD( "Pause: " )
         INKEY(0)
      ENDIF
      OUTSTD( hb_eol() )
      OUTSTD( SECONDS() )
   NEXT

RETURN NIL
