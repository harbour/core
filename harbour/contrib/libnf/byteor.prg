/*
 * File......: BYTEOR.PRG
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


/*  $DOC$
 *  $FUNCNAME$
 *     FT_BYTEOR()
 *  $CATEGORY$
 *     String
 *  $ONELINER$
 *     Perform bit-wise OR on two ASCII characters (bytes)
 *  $SYNTAX$
 *     FT_BYTEOR( <cByte1>, <cByte2> ) -> cNewByte
 *  $ARGUMENTS$
 *     <cByte1> and <cByte2> are characters from CHR(0) TO CHR(255).
 *     May be passed in CHR() form, as character literals, or as
 *     expressions evaluating to CHR() values.
 *  $RETURNS$
 *     Returns resulting byte, in CHR() form.  If parameters are faulty,
 *     returns NIL.
 *  $DESCRIPTION$
 *     Can be used for bit-wise byte manipulation.  In effect, this is a
 *     bit-by-bit OR operation.  Equivalent to OR assembler instruction.
 *
 *     This function is presented to illustrate that bit-wise operations
 *     are possible with Clipper code.  For greater speed, write .C or
 *     .ASM versions and use the Clipper Extend system.
 *  $EXAMPLES$
 *          This code performs a bit-wise OR on two bytes represented
 *          by CHR(20) and CHR(10):
 *
 *          cNewByte := FT_BYTEOR( CHR(20), CHR(10) )
 *          ? ASC( cNewByte )  // result: 30
 *          ? cNewByte         // result: non-printable character
 *
 *     For a demonstration of Clipper bit manipulations, compile and
 *     link the program BITTEST.PRG in the Nanforum Toolkit source code.
 *  $SEEALSO$
 *     FT_BYTEXOR() FT_BYTENOT() FT_BYTENEG() FT_BYTEAND()
 *  $END$
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

