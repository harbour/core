/*
 * $Id$
 */

/* Test SECONDS() */
/* Harbour Project source code
   http://harbour-project.org/
   Donated to the public domain on 2001-03-08 by David G. Holm <dholm@jsd-llc.com>
*/

PROCEDURE Main( cParam )

   LOCAL n, limit := 10

   IF ! Empty( cParam )
      limit := Val( cParam )
   ENDIF
   OutStd( hb_eol() )
   OutStd( Seconds() )
   FOR n := 1 TO limit
      IF Empty( cParam )
         OutStd( hb_eol() )
         OutStd( "Pause: " )
         Inkey( 0 )
      ENDIF
      OutStd( hb_eol() )
      OutStd( Seconds() )
   NEXT

   RETURN
