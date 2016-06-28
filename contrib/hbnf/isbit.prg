/* This is an original work by Forest Belt (Computer Diagnostic Services, Inc.)
   and is placed in the public domain.

      Rev 1.2   15 Aug 1991 23:03:46   GLENN
   Forest Belt proofread/edited/cleaned up doc

      Rev 1.1   14 Jun 1991 19:52:02   GLENN
   Minor edit to file header

      Rev 1.0   01 Apr 1991 01:01:32   GLENN
   Nanforum Toolkit
 */

FUNCTION ft_IsBit( cInbyte, nBitPos )

   IF HB_ISSTRING( cInbyte ) .AND. HB_ISNUMERIC( nBitPos )
      RETURN hb_bitTest( hb_BCode( cInbyte ), nBitpos )
   ENDIF

   RETURN NIL
