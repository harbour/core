/* This is an original work by Forest Belt (Computer Diagnostic Services, Inc.)
   and is placed in the public domain.

      Rev 1.2   15 Aug 1991 23:03:06   GLENN
   Forest Belt proofread/edited/cleaned up doc

      Rev 1.1   14 Jun 1991 19:51:16   GLENN
   Minor edit to file header

      Rev 1.0   01 Apr 1991 01:00:56   GLENN
   Nanforum Toolkit
 */

FUNCTION ft_ByteOr( cByte1, cByte2 )

   IF HB_ISSTRING( cByte1 ) .AND. HB_ISSTRING( cByte2 )
      RETURN hb_BChar( hb_bitOr( hb_BCode( cByte1 ), hb_BCode( cByte2 ) ) )
   ENDIF

   RETURN NIL
