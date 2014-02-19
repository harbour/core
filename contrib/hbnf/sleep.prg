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

PROCEDURE ft_Sleep( nSeconds, nInitial )

   IF HB_ISNUMERIC( nInitial )
      nInitial -= Seconds()
      IF nInitial > 0
         nInitial -= 86399
      ENDIF
      nSeconds += ninitial  // calculate final time
   ENDIF

   hb_idleSleep( nSeconds )

   RETURN
