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
 *    Rev 1.2   15 Aug 1991 23:02:58   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:51:08   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:00:48   GLENN
 * Nanforum Toolkit
 *
 */

FUNCTION FT_BYT2BIT( cByte )

   LOCAL nCounter, xBitstring

   IF HB_ISSTRING( cByte )
      xBitString := ""
      FOR nCounter := 7 TO 0 STEP -1
         xBitString += iif( FT_ISBIT( cByte, nCounter ), "1", "0" )
      NEXT
   ELSE
      xBitString := NIL
   ENDIF

   RETURN xBitString
