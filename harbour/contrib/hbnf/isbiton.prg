/*
 * $Id$
 */

/*
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

FUNCTION FT_ISBITON( nWord, nBit )

   nWord := iif( nWord < 0, nWord + 65536, nWord )
   nWord := Int( nWord * ( 2 ^ ( 15 - nBit ) ) )
   nWord := Int( nWord % 65536 )
   nWord := Int( nWord / 32768 )

   RETURN ( nWord == 1 )
