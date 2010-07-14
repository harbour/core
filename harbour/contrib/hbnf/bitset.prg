/*
 * $Id$
 */

/*
 * File......: bitset.prg
 * Author....: Forest Belt, Computer Diagnostic Services, Inc.
 * CIS ID....: ?
 *
 * This is an original work by Forest Belt and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:02:52   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:51:00   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:00:42   GLENN
 * Nanforum Toolkit
 *
 */

FUNCTION FT_BITSET(cInByte, nBitpos)

  LOCAL cByte

  IF valtype(cInbyte) != "C" .or. valtype(nBitpos) != "N"  // parameter check
     cByte := NIL
  ELSE
     IF (nBitPos > 7) .or. (nBitPos < 0) .or. (nBitPos != int(nBitPos))
        cByte := NIL
     ELSE
        cByte := iif( FT_ISBIT(cInByte, nBitpos), cInByte, ;
                       chr(asc(cInByte) + (2 ^ nBitpos)))
     ENDIF
  ENDIF

RETURN cByte
