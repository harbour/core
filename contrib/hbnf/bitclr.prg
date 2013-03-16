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

FUNCTION ft_BitClr( cInbyte, nBitpos )

   IF HB_ISSTRING( cInbyte ) .AND. HB_ISNUMERIC( nBitpos )
      RETURN hb_BChar( hb_bitReset( hb_BCode( cInbyte ), nBitpos ) )
   ENDIF

   RETURN NIL
