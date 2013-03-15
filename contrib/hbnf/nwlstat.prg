/*
 * $Id$
 */

/*
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

FUNCTION ft_NWLStat()

   LOCAL nStation := _ft_NWKStat()

   IF nStation < 0
      nStation += 256
   ENDIF

   RETURN nStation
