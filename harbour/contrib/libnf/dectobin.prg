/*
 * File......: DECTOBIN.PRG
 * Author....: Greg Lief
 * CIS ID....: 72460,1760
 *
 * This function is an original work by Mr. Grump and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:03:22   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:51:30   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:01:06   GLENN
 * Nanforum Toolkit
 *
 */

/*  $DOC$
 *  $FUNCNAME$
 *     FT_DEC2BIN()
 *  $CATEGORY$
 *     Conversion
 *  $ONELINER$
 *     Convert decimal to binary
 *  $SYNTAX$
 *     FT_DEC2BIN( <nNum> ) -> cBinaryNumber
 *  $ARGUMENTS$
 *     <nNum> is the numeric expression to be converted.
 *  $RETURNS$
 *     A character string representing <nNum> in binary format.
 *  $DESCRIPTION$
 *     This function can be used in conjunction with any bit-wise
 *     operations.
 *  $EXAMPLES$
 *     QOut( FT_DEC2BIN(255) )        // "11111111"
 *     QOut( FT_DEC2BIN(2) )          // "00000010"
 *  $END$
 */

#ifdef FT_TEST

FUNCTION MAIN
LOCAL X
FOR X = 1 TO 255
   QOUT( FT_DEC2BIN( x ))
next
return nil

#endif

function FT_DEC2BIN(x)
local i, buffer := { '0', '0', '0', '0', '0', '0', '0', '0' }
for i = 8 to 1 step -1
  if x >= 2 ^ (i - 1)
     x -= 2 ^ (i - 1)
     buffer[9 - i] = '1'
  endif
next
return ( buffer[1] + buffer[2] + buffer[3] + buffer[4] + ;
         buffer[5] + buffer[6] + buffer[7] + buffer[8] )

* end of file: dectobin.prg
