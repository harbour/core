/*
 * File......: BYT2BIT.PRG
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


/*  $DOC$
 *  $FUNCNAME$
 *     FT_BYT2BIT()
 *  $CATEGORY$
 *     Conversion
 *  $ONELINER$
 *     Convert byte to string of 1's and 0's
 *  $SYNTAX$
 *     FT_BYT2BIT( <cByte> ) -> cBitPattern
 *  $ARGUMENTS$
 *     <cByte> is the byte to convert.
 *  $RETURNS$
 *     9-character string, consisting of 1's and 0's, representing bits 0
 *     through 7 of parameter byte, with space between bits 3 and 4.  Returns
 *     NIL if parameters are faulty.
 *  $DESCRIPTION$
 *     Can be used to show results of bit manipulation, both before and after.
 *     Binary representation follows right-to-left convention of bit position
 *     numbering, 0 through 7.  Space between high and low nibbles for clarity
 *     and easy comparison to hexadecimal notation.
 *
 *     This function is presented to illustrate that bit-wise operations
 *     are possible with Clipper code.  For greater speed, write .C or
 *     .ASM versions and use the Clipper Extend system.
 *  $EXAMPLES$
 *     These three code lines perform a bitwise AND on bytes with values of
 *     CHR(20) and CHR(36), and deliver the result as a string in binary (bit)
 *     format.
 *
 *          ? FT_BYT2BIT(CHR(20))         // byte1: '0001 0100'
 *          ? FT_BYT2BIT(CHR(36))         // byte2: '0010 0100'
 *
 *          ? FT_BYT2BIT(FT_BYTEAND(CHR(20), CHR(36)))
 *                             // result: '0000 0100'
 *
 *     For a demonstration of Clipper bit manipulations, compile and
 *     link the program BITTEST.PRG in the Nanforum Toolkit source code.
 *  $SEEALSO$
 *     FT_BYT2HEX()
 *  $END$
 */

FUNCTION FT_BYT2BIT(cByte)

  local nCounter, xBitstring

  IF valtype(cByte) != "C"
     xBitString := NIL
  ELSE
     xBitString := ""
     FOR nCounter := 7 TO 0 step -1
        xBitString += iif(FT_ISBIT(cByte, nCounter), "1", "0")
     NEXT
  ENDIF

RETURN xBitString
