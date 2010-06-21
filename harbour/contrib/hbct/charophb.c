/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 string functions
 *     - CHARSUB()  (Harbour extension)
 *     - CHARSHL()  (Harbour extension)
 *     - CHARSHR()  (Harbour extension)
 *     - CHARRLL()  (Harbour extension)
 *     - CHARRLR()  (Harbour extension)
 *
 * Copyright 2001 IntTec GmbH, Neunlindenstr 32, 79106 Freiburg, Germany
 *        Author: Martin Vogel <vogel@inttec.de>
 *
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "ct.h"

/*  $DOC$
 *  $FUNCNAME$
 *      CHARSUB()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Subtracts corresponding ASCII value of two strings
 *  $SYNTAX$
 *      CHARSUB (<[@]cString1>, <cString2>) --> cSubString
 *  $ARGUMENTS$
 *      <[@]cString1>   first string
 *      <cString2>      second string
 *  $RETURNS$
 *      <cSubString>    string with subtracted ASCII values
 *  $DESCRIPTION$
 *      The CHARSUB() function constructs a new string from the two strings
 *      passed as parameters. To do this, it subtracts the ASCII values of the
 *      corresponding characters of both strings and places a character in
 *      the resulting string whose ASCII value equals to that difference (modulo 256).
 *      If the first string is passed by reference, the resulting string is
 *      stored in <cString1>, too. By setting the CSETREF()-switch to .T.,
 *      the return value can be omitted.
 *      If <cString2> is shorter than <cString1> and the last character of
 *      <cString2> has been processed, the function restarts with the first
 *      character of <cString2>.
 *  $EXAMPLES$
 *      ? charsub ("012345678", chr(1)) --> "/01234567"
 *      ? charsub ("123456789", chr(255)) --> "23456789:"
 *      ? charsub ("9999", chr(0)+chr(1)+chr(2)+chr(3)) --> "9876"
 *  $TESTS$
 *      charsub ("123456789", chr(1)) == "012345678"
 *      charsub ("123456789", chr(1)+chr(2)) == "002244668"
 *      charsub ("012345678", chr(255)) == "123456789"
 *      charsub ("012345678", chr(255)+chr(254)) == "133557799"
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      CHARSUB() is a new function that is only available in Harbour's CT3 lib.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is charop.c, library is ct3.
 *  $SEEALSO$
 *      CHARADD()   CHARAND()   CHARNOT()
 *      CHAROR()    CHARXOR()   CHARSHL()
 *      CHARSHR()   CHARRLL()   CHARRLR()
 *      CSETREF()
 *  $END$
 */

HB_FUNC( CHARSUB )
{
   ct_charop( CT_CHAROP_CHARSUB );
}


/*  $DOC$
 *  $FUNCNAME$
 *      CHARSHL()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Process each character in a string with bitwise SHIFT LEFT operation
 *  $SYNTAX$
 *      CHARSHL (<[@]cString>, <nBitsToSHL> ) --> cSHLString
 *  $ARGUMENTS$
 *      <[@]cString>    string to be processed
 *      <nBitsToSHL>    number of bit positions to be shifted to the left
 *  $RETURNS$
 *      <cSHLString>    string with bitwise shifted left characters
 *  $DESCRIPTION$
 *      The CHARSHL() function constructs a new string from the string
 *      passed as parameter. To do this, it performs a bitwise SHIFT LEFT
 *      (SHL) operation to the characters of the string and places a character in
 *      the resulting string whose ASCII value equals to the result of that
 *      operation.
 *      Be aware that bits shifted out of the byte are lost. If you need
 *      a bit rotation, use the CHARRLL() function instead.
 *      If the string is passed by reference, the resulting string is
 *      stored in <cString>, too. By setting the CSETREF()-switch to .T.,
 *      the return value can be omitted.
 *  $EXAMPLES$
 *      ? charshl (chr(1)+chr(2)+chr(4)+chr(8)+chr(16)+chr(32)+chr(64)+chr(128), 3)
 *        --> chr(8)+chr(16)+chr(32)+chr(64)+chr(128)+chr(0)+chr(0)+chr(0)
 *  $TESTS$
 *      charshl (chr(1)+chr(2)+chr(4)+chr(8)+chr(16)+chr(32)+chr(64)+chr(128), 3) == chr(8)+chr(16)+chr(32)+chr(64)+chr(128)+chr(0)+chr(0)+chr(0)
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      CHARSHL() is a new function that is only available in Harbour's CT3 lib.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is charop.c, library is ct3.
 *  $SEEALSO$
 *      CHARADD()   CHARSUB()   CHARAND()
 *      CHAROR()    CHARXOR()   CHARNOT()
 *      CHARSHR()   CHARRLL()   CHARRLR()
 *      CSETREF()
 *  $END$
 */

HB_FUNC( CHARSHL )
{
   ct_charop( CT_CHAROP_CHARSHL );
}


/*  $DOC$
 *  $FUNCNAME$
 *      CHARSHR()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Process each character in a string with bitwise SHIFT RIGHT operation
 *  $SYNTAX$
 *      CHARSHR (<[@]cString>, <nBitsToSHR> ) --> cSHRString
 *  $ARGUMENTS$
 *      <[@]cString>    string to be processed
 *      <nBitsToSHR>    number of bit positions to be shifted to the right
 *  $RETURNS$
 *      <cSHRString>    string with bitwise shifted right characters
 *  $DESCRIPTION$
 *      The CHARSHR() function constructs a new string from the string
 *      passed as parameter. To do this, it performs a bitwise SHIFT RIGHT
 *      (SHR) operation to the characters of the string and places a character in
 *      the resulting string whose ASCII value equals to the result of that
 *      operation.
 *      Be aware that bits shifted out of the byte are lost. If you need
 *      a bit rotation, use the CHARRLR() function instead.
 *      If the string is passed by reference, the resulting string is
 *      stored in <cString>, too. By setting the CSETREF()-switch to .T.,
 *      the return value can be omitted.
 *  $EXAMPLES$
 *      ? charshr (chr(1)+chr(2)+chr(4)+chr(8)+chr(16)+chr(32)+chr(64)+chr(128), 3)
 *        --> chr(0)+chr(0)+chr(0)+chr(1)+chr(2)+chr(4)+chr(8)+chr(16)
 *  $TESTS$
 *      charshr (chr(1)+chr(2)+chr(4)+chr(8)+chr(16)+chr(32)+chr(64)+chr(128), 3) == chr(0)+chr(0)+chr(0)+chr(1)+chr(2)+chr(4)+chr(8)+chr(16)
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      CHARSHR() is a new function that is only available in Harbour's CT3 lib.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is charop.c, library is ct3.
 *  $SEEALSO$
 *      CHARADD()   CHARSUB()   CHARAND()
 *      CHAROR()    CHARXOR()   CHARNOT()
 *      CHARSHL()   CHARRLL()   CHARRLR()
 *      CSETREF()
 *  $END$
 */

HB_FUNC( CHARSHR )
{
   ct_charop( CT_CHAROP_CHARSHR );
}


/*  $DOC$
 *  $FUNCNAME$
 *      CHARRLL()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Process each character in a string with bitwise ROLL LEFT operation
 *  $SYNTAX$
 *      CHARRLL (<[@]cString>, <nBitsToRLL> ) --> cRLLString
 *  $ARGUMENTS$
 *      <[@]cString>    string to be processed
 *      <nBitsToRLL>    number of bit positions to be rolled to the left
 *  $RETURNS$
 *      <cRLLString>    string with bitwise rolled left characters
 *  $DESCRIPTION$
 *      The CHARRLL() function constructs a new string from the string
 *      passed as parameter. To do this, it performs a bitwise ROLL LEFT
 *      (RLL) operation to the characters of the string and places a character in
 *      the resulting string whose ASCII value equals to the result of that
 *      operation.
 *      Be aware that, in contrast to CHARSHL(), bits rolled out on
 *      the left are put in again on the right.
 *      If the string is passed by reference, the resulting string is
 *      stored in <cString>, too. By setting the CSETREF()-switch to .T.,
 *      the return value can be omitted.
 *  $EXAMPLES$
 *      ? charrll (chr(1)+chr(2)+chr(4)+chr(8)+chr(16)+chr(32)+chr(64)+chr(128), 3)
 *        --> chr(8)+chr(16)+chr(32)+chr(64)+chr(128)+chr(1)+chr(2)+chr(4)
 *  $TESTS$
 *      charrll (chr(1)+chr(2)+chr(4)+chr(8)+chr(16)+chr(32)+chr(64)+chr(128), 3) == chr(8)+chr(16)+chr(32)+chr(64)+chr(128)+chr(1)+chr(2)+chr(4)
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      CHARRLL() is a new function that is only available in Harbour's CT3 lib.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is charop.c, library is ct3.
 *  $SEEALSO$
 *      CHARADD()   CHARSUB()   CHARAND()
 *      CHAROR()    CHARXOR()   CHARNOT()
 *      CHARSHL()   CHARSHR()   CHARRLR()
 *      CSETREF()
 *  $END$
 */

HB_FUNC( CHARRLL )
{
   ct_charop( CT_CHAROP_CHARRLL );
}


/*  $DOC$
 *  $FUNCNAME$
 *      CHARRLR()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Process each character in a string with bitwise ROLL RIGHT operation
 *  $SYNTAX$
 *      CHARRLR (<[@]cString>, <nBitsToRLR> ) --> cRLRString
 *  $ARGUMENTS$
 *      <[@]cString>    string to be processed
 *      <nBitsToRLR>    number of bit positions to be rolled to the right
 *  $RETURNS$
 *      <cRLRString>    string with bitwise rolled right characters
 *  $DESCRIPTION$
 *      The CHARRLR() function constructs a new string from the string
 *      passed as parameter. To do this, it performs a bitwise ROLL RIGHT
 *      (RLR) operation to the characters of the string and places a character in
 *      the resulting string whose ASCII value equals to the result of that
 *      operation.
 *      Be aware that, in contrast to CHARSHR(), bits rolled out on
 *      the right are put in again on the left.
 *      If the string is passed by reference, the resulting string is
 *      stored in <cString>, too. By setting the CSETREF()-switch to .T.,
 *      the return value can be omitted.
 *  $EXAMPLES$
 *      ? charrlr (chr(1)+chr(2)+chr(4)+chr(8)+chr(16)+chr(32)+chr(64)+chr(128), 3)
 *        --> chr(32)+chr(64)+chr(128)+chr(1)+chr(2)+chr(4)+chr(8)+chr(16)
 *  $TESTS$
 *      charrlr (chr(1)+chr(2)+chr(4)+chr(8)+chr(16)+chr(32)+chr(64)+chr(128), 3) == chr(32)+chr(64)+chr(128)+chr(1)+chr(2)+chr(4)+chr(8)+chr(16)
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      CHARRLR() is a new function that is only available in Harbour's CT3 lib.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is charop.c, library is ct3.
 *  $SEEALSO$
 *      CHARADD()   CHARSUB()   CHARAND()
 *      CHAROR()    CHARXOR()   CHARNOT()
 *      CHARSHL()   CHARSHR()   CHARRLL()
 *      CSETREF()
 *  $END$
 */

HB_FUNC( CHARRLR )
{
   ct_charop( CT_CHAROP_CHARRLR );
}
