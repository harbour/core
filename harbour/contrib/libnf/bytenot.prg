/*
 * File......: BYTENOT.PRG
 * Author....: Forest Belt, Computer Diagnostic Services, Inc.
 * CIS ID....: ?
 *
 * This is an original work by Forest Belt and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:05:00   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   10 May 1991 23:54:40   GLENN
 * Documentation correction.  The "oneliner" said two characters were NOTted,
 * but this function just takes one byte.
 *
 *    Rev 1.0   01 Apr 1991 01:00:54   GLENN
 * Nanforum Toolkit
 *
 */



/*  $DOC$
 *  $FUNCNAME$
 *     FT_BYTENOT()
 *  $CATEGORY$
 *     String
 *  $ONELINER$
 *     Perform bit-wise NOT on an ASCII character (byte)
 *  $SYNTAX$
 *     FT_BYTENOT( <cByte> ) -> cNewByte
 *  $ARGUMENTS$
 *     <cByte> is a character from CHR(0) to CHR(255).
 *     May be passed in CHR() form, as character literal, or
 *     as expression evaluating to CHR() value.
 *  $RETURNS$
 *     Returns resulting byte, in CHR() form.  If parameters are faulty,
 *     returns NIL.
 *  $DESCRIPTION$
 *     Can be used for bitwise byte manipulation.  In effect, this is a
 *     bit-by-bit NOT (one's complement) operation.  Equivalent to the
 *     NOT assembler instruction.
 *
 *     This function is presented to illustrate that bit-wise operations
 *     are possible with Clipper code.  For greater speed, write .C or
 *     .ASM versions and use the Clipper Extend system.
 *  $EXAMPLES$
 *     This code performs a bitwise NOT on byte represented by CHR(32):
 *
 *          cNewByte := FT_BYTENOT( CHR(32) )
 *          ? ASC( cNewByte )     // result: 223
 *
 *     For a demonstration of Clipper bit manipulations, compile and
 *     link the program BITTEST.PRG in the Nanforum Toolkit source code.
 *  $SEEALSO$
 *     FT_BYTEOR() FT_BYTEXOR() FT_BYTENEG() FT_BYTEAND()
 *  $END$
 */

FUNCTION FT_BYTENOT(cByte)

  LOCAL nCounter, cNewByte

  IF valtype(cByte) != "C"
     cNewByte := NIL
  ELSE
     cNewByte := chr(0)
     FOR nCounter := 0 to 7           // test each bit position
        IF .not. FT_ISBIT(cByte, nCounter)
           cNewByte := FT_BITSET(cNewByte, nCounter)
        ENDIF
     NEXT
  ENDIF

RETURN cNewByte

