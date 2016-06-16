/* This is an original work by Forest Belt (Computer Diagnostic Services, Inc.)
   and is placed in the public domain.

      Rev 1.2   15 Aug 1991 23:05:00   GLENN
   Forest Belt proofread/edited/cleaned up doc

      Rev 1.1   10 May 1991 23:54:40   GLENN
   Documentation correction.  The "oneliner" said two characters were NOTted,
   but this function just takes one byte.

      Rev 1.0   01 Apr 1991 01:00:54   GLENN
   Nanforum Toolkit
 */

FUNCTION ft_ByteNot( cByte )

   IF HB_ISSTRING( cByte )
      RETURN hb_BChar( hb_bitNot( hb_BCode( cByte ) ) )
   ENDIF

   RETURN NIL
