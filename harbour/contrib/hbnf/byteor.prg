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

FUNCTION FT_BYTEOR(cByte1, cByte2)

  LOCAL nCounter, cNewByte

  IF valtype(cByte1) != "C" .or. valtype(cByte2) != "C" // parameter check
     cNewByte := NIL
  ELSE
     cNewByte := chr(0)
     for nCounter := 0 to 7           // test each bit position
        if FT_ISBIT(cByte1, nCounter) .or. FT_ISBIT(cByte2, nCounter)
           cNewByte := FT_BITSET(cNewByte, nCounter)
        endif
     next
  ENDIF

RETURN cNewByte
