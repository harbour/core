/*
 * File......: BYTENEG.PRG
 * Author....: Forest Belt, Computer Diagnostic Services, Inc.
 * CIS ID....: ?
 *
 * This is an original work by Forest Belt and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:03:04   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:51:14   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:00:52   GLENN
 * Nanforum Toolkit
 *
 */


/*  $DOC$
 *  $FUNCNAME$
 *     FT_BYTENEG()
 *  $CATEGORY$
 *     String
 *  $ONELINER$
 *     Perform bit-wise negation on an ASCII character
 *  $SYNTAX$
 *     FT_BYTENEG( <cByte> ) -> cNewByte
 *  $ARGUMENTS$
 *     <cByte> is a character from CHR(0) to CHR(255).
 *     May be passed in CHR() form, as character literal, or
 *     as expression evaluating to CHR() value.
 *  $RETURNS$
 *     Returns resulting byte, in CHR() form.  If parameters are faulty,
 *     returns NIL.
 *  $DESCRIPTION$
 *     Can be used for bit-wise byte manipulation.  In effect, this is a
 *     bit-by-bit NEG (two's complement) operation.  Equivalent to NEG
 *     assembler instruction.
 *
 *     This function is presented to illustrate that bit-wise operations
 *     are possible with Clipper code.  For greater speed, write .C or
 *     .ASM versions and use the Clipper Extend system.
 *  $EXAMPLES$
 *     This code performs a bit-wise NEG on byte represented by CHR(32):
 *
 *          cNewByte := FT_BYTENOT(CHR(32))
 *          ? asc(cNewByte)                  // result: 224
 *
 *     For a demonstration of Clipper bit manipulations, compile and
 *     link the program BITTEST.PRG in the Nanforum Toolkit source code.
 *  $SEEALSO$
 *     FT_BYTEOR() FT_BYTEXOR() FT_BYTENOT() FT_BYTEAND()
 *  $END$
 */


FUNCTION FT_BYTENEG(cByte)
RETURN   iif(valtype(cByte) != "C", NIL, chr((256 - asc(cByte)) % 256))

