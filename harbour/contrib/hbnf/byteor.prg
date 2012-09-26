/*
 * $Id$
 */

/*
 * File......: byteor.prg
 * Author....: Forest Belt, Computer Diagnostic Services, Inc.
 * CIS ID....: ?
 *
 * This is an original work by Forest Belt and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:03:06   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:51:16   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:00:56   GLENN
 * Nanforum Toolkit
 *
 */

FUNCTION FT_BYTEOR( cByte1, cByte2 )

   LOCAL nCounter, cNewByte

   IF HB_ISSTRING( cByte1 ) .AND. HB_ISSTRING( cByte2 )
      cNewByte := hb_BChar( 0 )
      FOR nCounter := 0 TO 7           // test each bit position
         IF FT_ISBIT( cByte1, nCounter ) .OR. FT_ISBIT( cByte2, nCounter )
            cNewByte := FT_BITSET( cNewByte, nCounter )
         ENDIF
      NEXT
   ELSE
      cNewByte := NIL
   ENDIF

   RETURN cNewByte
