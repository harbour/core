/*
 * Author....: Greg Lief
 * CIS ID....: 72460,1760
 *
 * This function is an original work by Mr. Grump and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:03:22   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:51:30   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:01:06   GLENN
 * Nanforum Toolkit
 *
 */

FUNCTION ft_Dec2Bin( x )

   LOCAL buffer := ""
   LOCAL i

   FOR i := 7 TO 0 STEP -1
      buffer += iif( hb_bitTest( x, i ), "1", "0" )
   NEXT

   RETURN buffer
