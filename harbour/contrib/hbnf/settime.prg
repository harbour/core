/*
 * $Id$
 */

/*
 * File......: settime.prg
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
  RETURN NIL
#endif

function FT_SETTIME( cTime )
  local aRegs[ INT86_MAX_REGS ]

  cTime := iif( cTime == nil, time(), cTime )

  //            -------- High Byte ------      ----- Low Byte -------

  aRegs[ AX ] := SETTIME       * ( 2 ^ 8 )
  aRegs[ CX ] := HRS( cTime  ) * ( 2 ^ 8 )   +    MINS( cTime )
  aRegs[ DX ] := SECS( cTime ) * ( 2 ^ 8 )

return( FT_INT86( DOS, aRegs ) )
