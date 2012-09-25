/*
 * $Id$
 */

/*
 * File......: isshare.prg
 * Author....: Glenn Scott (from Tom Leylan C source)
 * CIS ID....: ?
 *
 * This is an original work by tom leylan and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.3   15 Aug 1991 23:03:48   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.2   14 Jun 1991 19:52:06   GLENN
 * Minor edit to file header
 *
 *    Rev 1.1   12 Jun 1991 02:14:56   GLENN
 * Documentation adjustment and checking ft_int86() call for compatibility
 * with new return value.
 *
 *    Rev 1.0   01 Apr 1991 01:01:34   GLENN
 * Nanforum Toolkit
 *
 */

#include "ftint86.ch"

#ifdef FT_TEST
  PROCEDURE Main()
     local nLoaded := ft_isshare()

     do case
     case nLoaded == 0
        Qout("Share not loaded, but ok to load")
     case nLoaded == 1
        Qout("Share not loaded, but NOT ok to load!")
     case nLoaded == 255
        Qout("Share is loaded!")
     endcase

     Qout("Retcode: " + str( nLoaded ) )

     return
#endif

FUNCTION ft_isshare()
   /*
  local aRegs[ INT86_MAX_REGS ]          // Declare the register array

  aRegs[ AX ] := makehi(16)              // share service
  aRegs[ CX ] := 0                       // Specify file attribute

  FT_Int86( 47, aRegs)                   // multiplex interrupt

RETURN lowbyte( aRegs[AX] )
  */
RETURN   _ft_isshare()
