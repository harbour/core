/*
 * File......: ISBITON.PRG
 * Author....: Ted Means
 * CIS ID....: 73067,3332
 *
 * This function is an original work by Ted Means and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.3   15 Aug 1991 23:02:26   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.2   17 Jul 1991 22:15:12   GLENN
 * Ted sent a minor bug fix
 *
 *    Rev 1.1   14 Jun 1991 19:52:04   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:01:34   GLENN
 * Nanforum Toolkit
 *
 */


/*  $DOC$
 *  $FUNCNAME$
 *     FT_ISBITON()
 *  $CATEGORY$
 *     String
 *  $ONELINER$
 *     Determine the state of individual bits in a number
 *  $SYNTAX$
 *     FT_ISBITON( <nNumber>, <nBit> ) -> lResult
 *  $ARGUMENTS$
 *     <nNumber> is an integer for which a bit state needs to be checked.
 *
 *     <nBit> is a number from 0 to 15 that indicates which bit to test.
 *  $RETURNS$
 *     .T. if the specified bit was on., .F. if off.
 *  $DESCRIPTION$
 *     This function is useful when dealing with binary integers.  It will
 *     come in very handy if you use the FT_INT86() function, because the
 *     CPU flags are returned as a series of bits.  Using this function, you
 *     can determine the state of each CPU flag.
 *  $EXAMPLES$
 *     if FT_ISBITON( nCPUFlags, 0 )
 *        Qout( "The carry flag was set." )
 *     endif
 *
 *     if FT_ISBITON( nCPUFlags, 7 )
 *        Qout( "The sign flag was set." )
 *     endif
 *  $END$
 */


function FT_ISBITON( nWord, nBit )

  nWord := iif(nWord < 0, nWord + 65536, nWord)
  nWord := int(nWord * (2 ^ (15 - nBit)))
  nWord := int(nWord % 65536)
  nWord := int(nWord / 32768)

  return (nWord == 1)
