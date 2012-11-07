/*
 * $Id$
 */

/*
 * Author....: Glenn Scott (from John Kaster)
 * CIS ID....: 71620,1521
 *
 * This is an original work by Glenn Scott and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.3   15 Aug 1991 23:04:32   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.2   14 Jun 1991 19:52:52   GLENN
 * Minor edit to file header
 *
 *    Rev 1.1   12 Jun 1991 02:30:32   GLENN
 * Documentation mod and check for ft_int86() compatibility
 *
 *    Rev 1.0   01 Apr 1991 01:02:12   GLENN
 * Nanforum Toolkit
 *
 */

#include "ftint86.ch"

/* TODO: rewrite in C */

FUNCTION ft_ScanCode()

   LOCAL aRegs[ INT86_MAX_REGS ]

   aRegs[ AX ] := MAKEHI( 0 )
   ft_int86( 22, aRegs )

   RETURN hb_BChar( LOWBYTE( aRegs[ AX ] ) ) + hb_BChar( HIGHBYTE( aRegs[ AX ] ) )
