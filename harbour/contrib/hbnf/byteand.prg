/*
 * $Id$
 */

/*
 * File......: byteand.prg
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

   LOCAL nCounter, cNewByte

   IF ValType( cByte1 ) != "C" .OR. ValType( cByte2 ) != "C" // parameter check
      cNewByte := NIL
   ELSE
      cNewByte := Chr( 0 )
      FOR nCounter := 0 TO 7           // test each bit position
         IF FT_ISBIT( cByte1, nCounter ) .AND. FT_ISBIT( cByte2, nCounter )
            cNewByte := FT_BITSET( cNewByte, nCounter )
         ENDIF
      NEXT
   ENDIF

   RETURN cNewByte
