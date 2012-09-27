/*
 * $Id$
 */

/*
 * Author....: Alexander B. Spencer
 * CIS ID....: 76276,1012
 *
 * This is an original work by Alexander B. Spencer and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.3   17 Aug 1991 15:33:50   GLENN
 * Don Caton fixed some spelling errors in the doc
 *
 *    Rev 1.2   15 Aug 1991 23:04:46   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:52:26   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   07 Jun 1991 23:39:50   GLENN
 * Initial revision.
 *
 */

FUNCTION FT_MIN2DHM( nMINS )
   RETURN {;
      hb_ntos( Int(   nMINS / 1440        ) ) ,;
      hb_ntos( Int( ( nMINS % 1440 ) / 60 ) ) ,;
      hb_ntos( Int( ( nMINS % 1440 ) % 60 ) ) }