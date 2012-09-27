/*
 * $Id$
 */

/*
 * Author....: Leo Letendre
 * CIS ID....: 73607,233
 *
 * This is an original work by Leo Letendre and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.1   17 Oct 1992 16:18:18   GLENN
 * Leo cleaned up the doc and file header.
 *
 *    Rev 1.0   01 Jul 1992 02:19:12   GLENN
 * Initial revision.
 *
 */

#ifdef FT_TEST

// Test routine
// Invoke by running SLEEP 1.0 to sleep 1.0 seconds
//

PROCEDURE Main( nSleep )

   ? "Time is now: " + Time()
   FT_SLEEP( Val( nSleep ) )
   ? "Time is now: " + Time()

   RETURN

#endif

FUNCTION FT_SLEEP( nSeconds, nInitial )

   IF ! HB_ISNUMERIC( nInitial )
      nInitial := Seconds()
   ENDIF

   // correct for running at midnight

   IF nInitial + nSeconds > 86399
      nInitial -= 86399
      //  Wait until midnight
      DO WHILE Seconds() > 100  // no problem with a _very_ slow machine
      ENDDO
   ENDIF

   // calculate final time

   nSeconds += ninitial

   // Loop until we are done

   DO WHILE Seconds() < nSeconds
   ENDDO

   RETURN NIL
