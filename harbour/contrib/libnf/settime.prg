/*
 * File......: SETTIME.PRG
 * Author....: Glenn Scott
 * CIS ID....: 71620,1521
 *
 * This is an original work by Glenn Scott and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.3   15 Aug 1991 23:06:08   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.2   14 Jun 1991 19:53:00   GLENN
 * Minor edit to file header
 *
 *    Rev 1.1   12 Jun 1991 02:34:58   GLENN
 * Documentation mods: change documented return value form "n" to "l" in
 * accordance with the new return value from ft_int86().
 *
 *    Rev 1.0   01 Apr 1991 01:02:16   GLENN
 * Nanforum Toolkit
 *
 */


/*  $DOC$
 *  $FUNCNAME$
 *     FT_SETTIME()
 *  $CATEGORY$
 *     DOS/BIOS
 *  $ONELINER$
 *     Set the DOS system time
 *  $SYNTAX$
 *     FT_SETTIME( <cTime> ) -> <lResult>
 *  $ARGUMENTS$
 *     <cTime> is a string in the form <hh:mm:ss> that you want to set
 *     the current DOS system time to.
 *
 *     Use 24-hour time.  It is up to you to send in a valid time.  If
 *     DOS doesn't think it is valid, it won't reset the time anyway.
 *  $RETURNS$
 *     <lResult> is simply the result of FT_INT86(), passed back
 *     to your program.
 *
 *  $DESCRIPTION$
 *     FT_SETTIME() uses NANFOR.LIB's FT_INT86() function to invoke
 *     the DOS Set Time service (Interrupt 33, service 45).
 *
 *  $EXAMPLES$
 *
 *  The following program takes a time string from the command line and sets
 *  the DOS system time:
 *
 *   FUNCTION main( cTime )
 *
 *      cTime := iif( cTime == nil, time(), cTime )
 *      QOut( "Setting time to: " + cTime  + "... " )
 *      FT_SETTIME( cTime )
 *      Qout( "Time is now: " + time() )
 *
 *   RETURN ( nil )
 *
 *  $END$
 */

#include "ftint86.ch"

#define DOS        33
#define SETTIME    45

#define SECS( ts )  ( val( substr( ts, 7    ) ) )
#define HRS( ts )   ( val( substr( ts, 1, 2 ) ) )
#define MINS( ts )  ( val( substr( ts, 4, 2 ) ) )

#ifdef FT_TEST
  FUNCTION MAIN( cTime )
    cTime := iif( cTime == nil, time(), cTime )
    QOut( "Setting time to: " + cTime  + "... " )
    FT_SETTIME( cTime )
    Qout( "Time is now: " + time() )
  return ( nil )
#endif

function FT_SETTIME( cTime )
  local aRegs[ INT86_MAX_REGS ]

  cTime := iif( cTime == nil, time(), cTime )

  //            -------- High Byte ------      ----- Low Byte -------

  aRegs[ AX ] = SETTIME       * ( 2 ^ 8 )
  aRegs[ CX ] = HRS( cTime  ) * ( 2 ^ 8 )   +    MINS( cTime )
  aRegs[ DX ] = SECS( cTime ) * ( 2 ^ 8 )

return( FT_INT86( DOS, aRegs ) )

