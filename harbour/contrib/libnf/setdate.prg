/*
 * File......: SETDATE.PRG
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


/*  $DOC$
 *  $FUNCNAME$
 *     FT_SETDATE()
 *  $CATEGORY$
 *     DOS/BIOS
 *  $ONELINER$
 *     Set the DOS system date
 *  $SYNTAX$
 *     FT_SETDATE( <dDate> ) -> <lResult>
 *  $ARGUMENTS$
 *     <dDate> is a Clipper date variable that you want to set the current
 *     DOS system date to.
 *
 *     It is up to you to send in a valid date.  The
 *     year must be within the range 1980 through 2099.  If DOS
 *     thinks the date is not valid, it won't change the date.
 *
 *  $RETURNS$
 *     <lResult> is simply the result of FT_INT86(), passed back
 *     to your program.
 *
 *  $DESCRIPTION$
 *     FT_SETDATE() uses NANFOR.LIB's FT_INT86() function to invoke
 *     the DOS Set Date service (Interrupt 33, service 43).
 *
 *  $EXAMPLES$
 *
 *  The following program takes a date from the command line and sets
 *  the DOS system date:
 *
 *   FUNCTION main( cDate )
 *
 *      cDate := iif( cDate == nil, dtoc( date() ), cDate )
 *      QOut( "Setting date to: " + cDate  + "... " )
 *      FT_SETDATE( ctod( cDate ) )
 *      Qout( "Today is now: " + dtoc( date() ) )
 *
 *   RETURN ( nil )
 *
 *  $END$
 */

#include "ftint86.ch"

#define DOS        33
#define SETDATE    43


#ifdef FT_TEST
  FUNCTION MAIN( cDate )

     cDate := iif( cDate == nil, dtoc( date() ), cDate )
     QOut( "Setting date to: " + cDate  + "... " )
     FT_SETDATE( ctod( cDate ) )
     Qout( "Today is now: " + dtoc( date() ) )

  return ( nil )
#endif

function FT_SETDATE( dDate )
  local aRegs[ INT86_MAX_REGS ]

  dDate := iif( valtype(dDate) != "D", date(), dDate )

  aRegs[ AX ] = SETDATE * ( 2 ^ 8 )
  aregs[ CX ] = year( dDate )
  aregs[ DX ] = ( month( dDate ) * ( 2 ^ 8 ) )  + day( dDate )

return( FT_INT86( DOS, aRegs ) )

