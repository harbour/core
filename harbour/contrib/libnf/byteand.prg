/*
 * File......: BYTEAND.PRG
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



/*  $DOC$
 *  $FUNCNAME$
 *     FT_BYTEAND()
 *  $CATEGORY$
 *     String
 *  $ONELINER$
 *     Perform bit-wise AND on two ASCII characters (bytes)
 *  $SYNTAX$
 *     FT_BYTEAND( <cByte1>, <cByte2> ) -> cByte
 *  $ARGUMENTS$
 *     <cByte1> and <cByte2> are characters from CHR(0) TO CHR(255).
 *     May be passed in CHR() form, as character literals, or as expressions
 *     evaluating to CHR() values.
 *  $RETURNS$
 *     Returns resulting byte, in CHR() form.  If parameters are faulty,
 *     returns NIL.
 *  $DESCRIPTION$
 *     Can be used for any bit-wise masking operation.  In effect, this is a
 *     bit-by-bit AND operation.  Equivalent to AND assembler instruction.
 *
 *     This function is presented to illustrate that bit-wise operations
 *     are possible with Clipper code.  For greater speed, write .C or
 *     .ASM versions and use the Clipper Extend system.
 *  $EXAMPLES$
 *     This code would mask out the high nibble (four most significant bits)
 *     of the byte represented by chr(123) and leave the low nibble bits as in
 *     the parameter byte.
 *
 *          cNewbyte := FT_BYTEAND( CHR(123), CHR(15) )
 *          ? asc(cNewByte)  // result: 11
 *          ? cNewByte       // result: non-printable character
 *
 *     For a demonstration of Clipper bit manipulations, compile and
 *     link the program BITTEST.PRG in the Nanforum Toolkit source code.
 *  $SEEALSO$
 *     FT_BYTEOR() FT_BYTEXOR() FT_BYTENOT() FT_BYTENEG()
 *  $END$
 */

FUNCTION FT_BYTEAND(cByte1, cByte2)

  LOCAL nCounter, cNewByte

  IF valtype(cByte1) != "C" .or. valtype(cByte2) != "C" // parameter check
     cNewByte := NIL
  ELSE
     cNewByte := chr(0)
     for nCounter := 0 to 7           // test each bit position
        if FT_ISBIT(cByte1, nCounter) .and. FT_ISBIT(cByte2, nCounter)
           cNewByte := FT_BITSET(cNewByte, nCounter)
        endif
     next
  ENDIF

RETURN cNewByte

