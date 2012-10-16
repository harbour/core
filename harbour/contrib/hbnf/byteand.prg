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
 *    Rev 1.2   15 Aug 1991 23:03:02   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:51:12   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:00:50   GLENN
 * Nanforum Toolkit
 *
 */

FUNCTION FT_BYTEAND( cByte1, cByte2 )

   IF HB_ISSTRING( cByte1 ) .AND. HB_ISSTRING( cByte2 )
      RETURN hb_BChar( hb_bitAnd( hb_BCode( cByte1 ), hb_BCode( cByte2 ) ) )
   ENDIF

   RETURN NIL
