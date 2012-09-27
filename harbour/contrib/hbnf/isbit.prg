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
 *    Rev 1.2   15 Aug 1991 23:03:46   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:52:02   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:01:32   GLENN
 * Nanforum Toolkit
 *
 */

FUNCTION FT_ISBIT( cInbyte, nBitPos )

   LOCAL lBitStat

   IF HB_ISSTRING( cInbyte ) .AND. HB_ISNUMERIC( nBitPos )
      IF nBitPos > 7 .OR. nBitPos < 0 .OR. nBitPos != Int( nBitPos )
         lBitStat := NIL
      ELSE
         lBitStat := Int( ( ( hb_BCode( cInByte ) * ( 2 ^ ( 7 - nBitPos ) ) ) % 256 ) / 128 ) == 1
      ENDIF
   ELSE
      lBitStat := NIL
   ENDIF

   RETURN lBitStat
