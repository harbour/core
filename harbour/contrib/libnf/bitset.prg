/*
 * File......: BITSET.PRG
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



/*  $DOC$
 *  $FUNCNAME$
 *     FT_BITSET()
 *  $CATEGORY$
 *     String
 *  $ONELINER$
 *     Set selected bit in a byte
 *  $SYNTAX$
 *     FT_BITSET( <cByte>, <nBitPos> ) -> cByte
 *  $ARGUMENTS$
 *     <cByte> is a character from CHR(0) to CHR(255).
 *
 *     <nBitPos> is a number from 0 to 7 conforming to standard right-to-left
 *     bit numbering convention and representing the position of the bit
 *     within the byte.
 *  $RETURNS$
 *     Returns new byte, with designated bit set.  If parameters are faulty,
 *     returns NIL.
 *  $DESCRIPTION$
 *     In effect, ORs argument byte with a byte that has only the target bit
 *     set.  If bit is already set, it remains set.
 *     Note: Calls FT_ISBIT() which is also in this Library.
 *
 *     This function is presented to illustrate that bit-wise operations
 *     are possible with Clipper code.  For greater speed, write .C or
 *     .ASM versions and use the Clipper Extend system.
 *  $EXAMPLES$
 *     This code would set bit 4 in a byte represented by CHR(107):
 *
 *          cNewbyte := FT_BITSET( CHR(107), 4 )
 *          ? ASC( cNewbyte )             // result: 123
 *          ? cNewbyte                    // result: '{'
 *
 *
 *     This code would set bit 5 in the byte represented by the letter 'A'.
 *
 *          ? FT_BITSET( 'A', 5 )         // result: 'a'
 *                                        // bit 5 set
 *
 *     For a demonstration of Clipper bit manipulations, compile and
 *     link the program BITTEST.PRG in the Nanforum Toolkit source code.
 *  $SEEALSO$
 *     FT_BITCLR() FT_ISBIT()
 *  $END$
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

