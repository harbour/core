/*
 * $Id$
 */

PROCEDURE Main()

   LOCAL start := Seconds(), stop

   QOut( "start   ", start )
   Tone( 440, 9.1 )
   Tone( 880, 9.1 )
   Tone( 440, 9.1 )
   stop := Seconds()
   QOut( "stop    ", stop )
   QOut( "duration", ( stop - start ), "(should be close to 1.5)" )

   RETURN
