/*
 * $Id$
 */

/*
 * File......: vidmode.prg
 * Author....: Glenn Scott
 * CIS ID....: 71620,1521
 *
 * This is an original work by Glenn Scott and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.3   15 Aug 1991 23:06:12   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.2   14 Jun 1991 19:53:14   GLENN
 * Minor edit to file header
 *
 *    Rev 1.1   14 Jun 1991 18:00:42   GLENN
 * Documentation change (minor), and checked for compatibility with new
 * ft_int86().
 *
 *    Rev 1.0   01 Apr 1991 01:02:30   GLENN
 * Nanforum Toolkit
 *
 */

#include "ftint86.ch"

#define VIDEO      16
#define GETMODE    15

#ifdef FT_TEST
  FUNCTION MAIN( cMode )

     FT_SETMODE( val( cMode ) )
     QOut( "Video mode is: " + str( FT_GETMODE() ) )
     RETURN NIL

#endif

FUNCTION FT_SETMODE( nMode )
/*
  LOCAL aRegs[ INT86_MAX_REGS ]

  aRegs[ AX ] := nMode
  FT_INT86( VIDEO, aRegs )
*/
_ft_setmode(nMode)
  RETURN NIL

FUNCTION FT_GETMODE()
/*
  LOCAL aRegs[INT86_MAX_REGS]

  aRegs[ AX ] := MAKEHI( GETMODE )
  FT_INT86( VIDEO, aRegs )

  RETURN LOWBYTE( aRegs[ AX ] )
*/
 RETURN _ft_getmode()
