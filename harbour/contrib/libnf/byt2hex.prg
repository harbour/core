/*
 * File......: BYT2HEX.PRG
 * Author....: Forest Belt, Computer Diagnostic Services, Inc.
 * CIS ID....: ?
 *
 * This is an original work by Forest Belt and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:03:00   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:51:10   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:00:48   GLENN
 * Nanforum Toolkit
 *
 */


/*  $DOC$
 *  $FUNCNAME$
 *     FT_BYT2HEX()
 *  $CATEGORY$
 *     Conversion
 *  $ONELINER$
 *     Convert byte to hexadecimal version of its binary value
 *  $SYNTAX$
 *     FT_BYT2HEX( cByte ) -> cHexValue
 *  $ARGUMENTS$
 *     <cByte> is the byte to convert.
 *  $RETURNS$
 *     Three-character string, consisting of two digits of hexadecimal
 *     notation and letter 'h' to signify hex.  Returns NIL if parameters are
 *     faulty.
 *  $DESCRIPTION$
 *     Can be used to show results of bit manipulation, both before and after.
 *
 *     This function is presented to illustrate that bit-wise operations
 *     are possible with Clipper code.  For greater speed, write .C or
 *     .ASM versions and use the Clipper Extend system.
 *  $EXAMPLES$
 *     These three code lines perform a bitwise AND on bytes with values of
 *     CHR(20) and CHR(36), and deliver the result as a string in hexadecimal
 *     format, using 'h' to signify hexadecimal.
 *
 *          ? FT_BYT2HEX(CHR(20))         // byte1: '14h'
 *          ? FT_BYT2HEX(CHR(36))         // byte2: '24h'
 *
 *          ? FT_BYT2HEX(FT_BYTEAND(CHR(20), CHR(36)))
 *                             // result: '04h'
 *
 *     For a demonstration of Clipper bit manipulations, compile and
 *     link the program BITTEST.PRG in the Nanforum Toolkit source code.
 *  $SEEALSO$
 *     FT_BYT2BIT()
 *  $END$
 */

FUNCTION FT_BYT2HEX(cByte)

  local cHexTable := "0123456789ABCDEF"
  local xHexString

  if valtype(cByte) != "C"
     xHexString := NIL
  else
     xHexString := substr(cHexTable, int(asc(cByte) / 16) + 1, 1) ;
                 + substr(cHexTable, int(asc(cByte) % 16) + 1, 1) ;
                 + "h"
  endif

RETURN xHexString
