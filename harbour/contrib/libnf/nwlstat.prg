/*
 * File......: NWLSTAT.PRG
 * Author....: Glenn Scott
 * CIS ID....: ?
 *
 * This is an original work by Glenn Scott and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:06:04   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   12 Jun 1991 02:19:46   GLENN
 * Documentation correction and check for compatibility with new return
 * value for ft_int86().
 *
 *    Rev 1.0   01 Apr 1991 01:01:54   GLENN
 * Nanforum Toolkit
 *
 */


/*  $DOC$
 *  $FUNCNAME$
 *     FT_NWLSTAT()
 *  $CATEGORY$
 *     NetWare
 *  $ONELINER$
 *     Return the current Novell NetWare logical station number
 *  $SYNTAX$
 *     FT_NWLSTAT() -> nStatNum
 *  $ARGUMENTS$
 *     None
 *  $RETURNS$
 *     A numeric corresponding to the current logical station number
 *     assigned by NetWare.
 *  $DESCRIPTION$
 *     In order to find out information about a particular node logged
 *     in to a NetWare server, you will need the logical
 *     station number, also known as a "connection number."  This
 *     function will return that number.  This will be a number from 1 to 100
 *     under NetWare 286, or from 1 to 250 under NetWare 386.  This is *not*
 *     the same as a physical station number.
 *
 *     This function requires FT_INT86().
 *
 *     This function does NOT test for the existence of the NetWare shell.
 *     The behavior is undefined if no shell is loaded.
 *  $EXAMPLES$
 *     QOut( "Logical station: " + str( FT_NWLSTAT() ) )
 *  $END$
 */

#include "ftint86.ch"

#define DOS         33
#define STATNUM    220

#ifdef FT_TEST
  FUNCTION MAIN()
  QOut( "Logical station: " + str( FT_NWLSTAT() ) )
  return ( nil )
#endif

FUNCTION FT_NWLSTAT()
/*  LOCAL aRegs[ INT86_MAX_REGS ] */
  LOCAL nStation
/*
  aRegs[ AX ] = MAKEHI( STATNUM )
  FT_INT86( DOS, aRegs )
  */
  nStation := _ft_nwkstat() /* LOWBYTE( aRegs[ AX ] ) */
  if nStation < 0
    nStation += 256
  endif

  RETURN nStation

