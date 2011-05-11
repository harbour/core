/*
 * $Id$
 */

/*
 * File......: dosver.prg
 * Author....: Glenn Scott
 * CIS ID....: ?
 *
 * This is an original work by Glenn Scott and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:02:24   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   12 Jun 1991 02:38:24   GLENN
 * Documentation mod and removal of reference to constant INT86_SUCCESS.
 * Checked for ft_int86() compatibility.
 *
 *    Rev 1.0   01 Apr 1991 01:01:14   GLENN
 * Nanforum Toolkit
 *
 */

#include "ftint86.ch"

#define DOS        33
#define DOSVER     48

#ifdef FT_TEST
  FUNCTION MAIN()
  QOut( "Dos version: " + FT_DOSVER() )
  RETURN NIL
#endif

FUNCTION FT_DOSVER()
/*  local aRegs[ INT86_MAX_REGS ] */
  local cResult

/*  aRegs[ AX ] := MAKEHI( DOSVER )
  if FT_INT86( DOS, aRegs )
     cResult := alltrim( str( LOWBYTE( aRegs[ AX ] ) ) ) + "." + ;
                alltrim( str( HIGHBYTE( aRegs[ AX ] ) ) )
  endif
*/
cResult:= _get_dosver()
RETURN ( cResult )
