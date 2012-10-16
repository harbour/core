/*
 * $Id$
 */

/*
 * Author....: Forest Belt, Computer Diagnostic Services, Inc.
 * CIS ID....: ?
 *
 * This is an original work by Forest Belt and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:03:04   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:51:14   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:00:52   GLENN
 * Nanforum Toolkit
 *
 */

FUNCTION FT_BYTENEG( cByte )

   IF HB_ISSTRING( cByte )
      RETURN hb_BChar( ( 256 - hb_BCode( cByte ) ) % 256 )
   ENDIF

   RETURN NIL
