/*
 * $Id$
 */

/*
 * File......: bitclr.prg
 * Author....: Forest Belt, Computer Diagnostic Services, Inc.
 * CIS ID....: ?
 *
 * This is an original work by Forest Belt and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:02:50   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:50:58   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:00:40   GLENN
 * Nanforum Toolkit
 *
 */

FUNCTION FT_BITCLR( cInbyte, nBitpos )

   LOCAL cByte

   IF HB_ISSTRING( cInbyte ) .AND. HB_ISNUMERIC( nBitpos )
      IF nBitPos > 7 .OR. nBitPos < 0 .OR. nBitPos != Int( nBitPos )
         cByte := NIL
      ELSE
         cByte := iif( ! FT_ISBIT( cInByte, nBitpos ), cInByte, ;
            hb_BChar( hb_BCode( cInByte ) - ( 2 ^ nBitpos ) ) )
      ENDIF
   ELSE
      cByte := NIL
   ENDIF

   RETURN cByte
