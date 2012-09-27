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
 *    Rev 1.3   15 Aug 1991 23:03:30   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.2   14 Jun 1991 19:53:12   GLENN
 * Minor edit to file header
 *
 *    Rev 1.1   14 Jun 1991 17:59:18   GLENN
 * Documentation change (minor), and checked for compatibility with new
 * ft_int86().
 *
 *    Rev 1.0   01 Apr 1991 01:02:28   GLENN
 * Nanforum Toolkit
 *
 */

#include "ftint86.ch"

#define VIDEO      16

FUNCTION FT_SETVCUR( nPage, nRow, nCol )

   LOCAL aRegs[ INT86_MAX_REGS ]

   nPage := iif( nPage == NIL, FT_GETVPG()  , nPage )
   nRow  := iif( nRow  == NIL, 0            , nRow  )
   nCol  := iif( nCol  == NIL, 0            , nCol  )

   aRegs[ AX ] := MAKEHI(  2    )
   aRegs[ BX ] := MAKEHI( nPage )
   aRegs[ DX ] := MAKEHI( nRow  ) + nCol

   FT_INT86( VIDEO, aRegs )

   RETURN NIL

FUNCTION FT_GETVCUR( nPage )

   LOCAL aRegs[ INT86_MAX_REGS ]

   nPage := iif( nPage == NIL, FT_GETVPG(), nPage )
   aRegs[ AX ] := MAKEHI( 3     )
   aRegs[ BX ] := MAKEHI( nPage )
   FT_INT86( VIDEO, aRegs )

   RETURN { HIGHBYTE( aRegs[ CX ] ), LOWBYTE( aRegs[ CX ] ), HIGHBYTE( aRegs[ DX ] ), LOWBYTE( aRegs[ DX ] ) }
