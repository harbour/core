/* This is an original work by Forest Belt (Computer Diagnostic Services, Inc.)
   and is placed in the public domain.

      Rev 1.2   15 Aug 1991 23:02:58   GLENN
   Forest Belt proofread/edited/cleaned up doc

      Rev 1.1   14 Jun 1991 19:51:08   GLENN
   Minor edit to file header

      Rev 1.0   01 Apr 1991 01:00:48   GLENN
   Nanforum Toolkit
 */

FUNCTION ft_Byt2Bit( cByte )

   LOCAL nCounter, cBitstring

   IF HB_ISSTRING( cByte )
      cByte := hb_BCode( cByte )
      cBitString := ""
      FOR nCounter := 7 TO 0 STEP -1
         cBitString += iif( hb_bitTest( cByte, nCounter ), "1", "0" )
      NEXT
      RETURN cBitString
   ENDIF

   RETURN NIL
