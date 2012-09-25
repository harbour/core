/*
 * $Id$
 */

/*
 * File......: setdate.prg
 * Author....: Glenn Scott
 * CIS ID....: 71620,1521
 *
 * This is an original work by Glenn Scott and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.3   15 Aug 1991 23:04:36   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.2   14 Jun 1991 19:52:58   GLENN
 * Minor edit to file header
 *
 *    Rev 1.1   12 Jun 1991 02:32:28   GLENN
 * Documentation mod and change documented return value from "n" to "l"
 * reflecting Ted's update of ft_int86().
 *
 *    Rev 1.0   01 Apr 1991 01:02:16   GLENN
 * Nanforum Toolkit
 *
 */

#include "ftint86.ch"

#define DOS        33
#define SETDATE    43

#ifdef FT_TEST
  PROCEDURE Main( cDate )

     cDate := iif( cDate == nil, dtoc( date() ), cDate )
     QOut( "Setting date to: " + cDate  + "... " )
     FT_SETDATE( ctod( cDate ) )
     QOut( "Today is now: " + dtoc( date() ) )

  RETURN
#endif

function FT_SETDATE( dDate )
  local aRegs[ INT86_MAX_REGS ]

  dDate := iif( valtype(dDate) != "D", date(), dDate )

  aRegs[ AX ] := SETDATE * ( 2 ^ 8 )
  aregs[ CX ] := year( dDate )
  aregs[ DX ] := ( month( dDate ) * ( 2 ^ 8 ) )  + day( dDate )

return( FT_INT86( DOS, aRegs ) )
