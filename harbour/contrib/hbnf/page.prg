/*
 * $Id$
 */

/*
 * Author....: Glenn Scott
 * CIS ID....: 71620,1521
 *
 * This is an original work by Glenn Scott and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.3   15 Aug 1991 23:05:18   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.2   14 Jun 1991 19:52:36   GLENN
 * Minor edit to file header
 *
 *    Rev 1.1   12 Jun 1991 02:29:14   GLENN
 * Documentation mods and check for ft_int86() compatibility
 *
 *    Rev 1.0   01 Apr 1991 01:01:58   GLENN
 * Nanforum Toolkit
 *
 */

#include "ftint86.ch"

#define VIDEO      16

FUNCTION FT_SETVPG( nPage )
/*
   LOCAL aRegs[ INT86_MAX_REGS ]

   aRegs[ AX ] := MAKEHI( 5 ) + nPage
   FT_INT86( VIDEO, aRegs )
  */

   _ft_setvpg( nPage )

   RETURN NIL

FUNCTION FT_GETVPG()
/*
   LOCAL aRegs[ INT86_MAX_REGS ]

   aRegs[ AX ] := MAKEHI( 15 )
   FT_INT86( VIDEO, aRegs )

   RETURN HIGHBYTE( aRegs[ BX ] ) */

   RETURN _ft_getvpg()
