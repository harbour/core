/*
 * File......: SLEEP.PRG
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

  * Test routine
  * Invoke by running SLEEP 1.0 to sleep 1.0 seconds
  *

  FUNCTION MAIN(nSleep)

       ? "Time is now: " + time()
       FT_SLEEP(VAL(nSleep))
       ? "Time is now: " + time()

  RETURN ( nil )

#endif

/*  $DOC$
 *  $FUNCNAME$
 *     FT_SLEEP
 *  $CATEGORY$
 *     Menus/Prompts
 *  $ONELINER$
 *     Wait for a specified amount of time
 *  $SYNTAX$
 *     FT_SLEEP( <nSeconds>, [<nInitial>] ) -> nil
 *  $ARGUMENTS$
 *    <nSeconds> is the number of seconds to pause
 *
 *    <nInitial> is an optional clock value (from a call to SECONDS())
 *               from which the <nSeconds> seconds are to elapse. Useful
 *               for setting a minimum time between the start of events
 *               which could take a variable amount of time due to the
 *               execution of intervening code.
 *  $RETURNS$
 *     NIL
 *  $DESCRIPTION$
 *     This routine will wait a specified period of time. It provides
 *     resolution based upon the execution of the SECONDS() function.
 *     It does not use an input state such as INKEY(). The specified time
 *     is the minimum time sleeping and will usually be slightly longer.
 *
 *     The second optional argument allows one to begin timing an event
 *     prior to executing some operation. This is useful when, for example,
 *     you input a key or mouse click and wish to do something but still want
 *     to note if the user double entered (mouse or key) within a certain time
 *     which in turn may have meaning within your program's context.
 *
 *     The routine correctly handles passing through midnight but will not
 *     work for more than 24 hours.
 *  $EXAMPLES$
 *     Example 1:
 *         FT_SLEEP(10.0)    && Sleep for 10.0 seconds
 *     Example 2:
 *         nTime=SECONDS()   && usually after some interupt from mouse or
 *                           && keyboard
 *
 *         ... intervening code ...
 *
 *         FT_SLEEP(0.5, nTime) && Sleep until the sytem clock is
 *                              && nTime+0.5 seconds.
 *
 *  $END$
 */

FUNCTION FT_SLEEP( nSeconds, nInitial )

  IF nInitial == NIL .OR. VALTYPE( nInitial ) != "N"
	   nInitial := SECONDS()
  ENDIF

  // correct for running at midnight

  IF nInitial + nSeconds > 86399
	   nInitial -= 86399
     *  Wait until midnight
     DO WHILE SECONDS() > 100  // no problem with a _very_ slow machine
     ENDDO
  ENDIF

  // calculate final time

  nSeconds += ninitial

  // Loop until we are done

  DO WHILE ( SECONDS() < nSeconds )
  ENDDO

  RETURN NIL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
