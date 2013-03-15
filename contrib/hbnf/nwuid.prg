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
 *    Rev 1.4   15 Aug 1991 23:04:10   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.3   14 Jun 1991 19:52:34   GLENN
 * Minor edit to file header
 *
 *    Rev 1.2   14 Jun 1991 04:31:30   GLENN
 * Return value still needs to have nulls (hb_BChar(0)) removed.  Put that back
 * in.
 *
 *    Rev 1.1   12 Jun 1991 02:25:22   GLENN
 * Documentation correction and revision of ft_int86() call to account
 * for Ted's new string passing conventions.
 *
 *    Rev 1.0   01 Apr 1991 01:01:56   GLENN
 * Nanforum Toolkit
 *
 */

#include "ftint86.ch"

/* TODO: rewrite in C */

FUNCTION ft_NWUID( nConn )

   LOCAL aRegs[ INT86_MAX_REGS ]
   LOCAL cReqPkt
   LOCAL cRepPkt

   IF nConn == NIL
      nConn := ft_NWLStat()
   ENDIF

   // Set up request packet
   cReqPkt := hb_BChar( 22 )          // Get Connection Information
   cReqPkt += hb_BChar( nConn )
   cReqPkt := I2Bin( hb_BLen( cReqPkt ) ) + cReqPkt

   // Set up reply packet
   cRepPkt := Space( 63 )

   // Assign registers
   aRegs[ AX ] := MAKEHI( 227 ) // NW_LOG
   aRegs[ DS ] := cReqPkt
   aRegs[ SI ] := REG_DS
   aRegs[ ES ] := cRepPkt
   aRegs[ DI ] := REG_ES

   ft_int86( 33, aRegs )

   RETURN AllTrim( StrTran( hb_BSubStr( aRegs[ ES ], 9, 48 ), hb_BChar( 0 ) ) )
