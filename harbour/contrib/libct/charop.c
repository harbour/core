/*
 * $Id$
 */

/*
 * Harbour Project source code: 
 *   CT3 string functions
 *     - CHARADD()
 *     - CHARSUB()  (NEW)
 *     - CHARAND()
 *     - CHARNOT()
 *     - CHAROR()
 *     - CHARXOR()  
 *     - CHARSHL()  (NEW)
 *     - CHARSHR()  (NEW)
 *     - CHARRLL()  (NEW)
 *     - CHARRLR()  (NEW)
 *
 * Copyright 2001 IntTec GmbH, Neunlindenstr 32, 79106 Freiburg, Germany
 *        Author: Martin Vogel <vogel@inttec.de>
 *
 * www - http://www.harbour-project.org
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


#define DO_CHAROP_CHARADD      0
#define DO_CHAROP_CHARSUB      1  /* new: character subtraction */
#define DO_CHAROP_CHARAND      2
#define DO_CHAROP_CHARNOT      3
#define DO_CHAROP_CHAROR       4
#define DO_CHAROP_CHARXOR      5
#define DO_CHAROP_CHARSHL      6  /* new: shift left */
#define DO_CHAROP_CHARSHR      7  /* new: shift right */
#define DO_CHAROP_CHARRLL      8  /* new: left rotation */
#define DO_CHAROP_CHARRLR      9  /* new: right rotation */

/* helper function */
static void do_charop (int iSwitch)
{

  int iNoRet;

  /* suppressing return value ? */
  iNoRet = ct_getref();

  if (ISCHAR (1))
  {
    
    size_t sStrLen = hb_parclen (1);
    size_t sPos;
    unsigned char *pucString = ( unsigned char * ) hb_parc (1);
    unsigned char *pucResult = ( unsigned char * ) hb_xgrab (sStrLen);
    
    if (pucResult == NULL)
    {
      hb_ret();
      return;
    }

    switch (iSwitch)
    {
      /* NOT */
      case DO_CHAROP_CHARNOT:
      {

        for (sPos = 0; sPos < sStrLen; sPos++)
          pucResult[sPos] = ~pucString[sPos];

      }; break;

      /* SHL */
      case DO_CHAROP_CHARSHL:
      {
        
        int iSHL = (hb_parni (2))%8;  /* defaults to 0 */

        if (iSHL == 0)
          hb_xmemcpy (pucResult, pucString, sStrLen);
        else
          for (sPos = 0; sPos < sStrLen; sPos++)
            pucResult[sPos] = pucString[sPos] << iSHL;

      }; break;

      /* SHR */
      case DO_CHAROP_CHARSHR:      
      {
        
        int iSHR = (hb_parni (2))%8;  /* defaults to 0 */

        if (iSHR == 0)
          hb_xmemcpy (pucResult, pucString, sStrLen);
        else
          for (sPos = 0; sPos < sStrLen; sPos++)
            pucResult[sPos] = pucString[sPos] >> iSHR;

      }; break;

      /* RLL */
      case DO_CHAROP_CHARRLL:      
      {
        
        int iRLL = (hb_parni (2))%8;  /* defaults to 0 */

        hb_xmemcpy (pucResult, pucString, sStrLen);
        
        if (iRLL != 0)
          for (sPos = 0; sPos < sStrLen; sPos++)
          {
            int iRLLCnt;

            for (iRLLCnt = 0; iRLLCnt < iRLL; iRLLCnt++)
              if (pucResult[sPos]&0x80)  /* most left bit set -> roll over */
              {
                pucResult[sPos] <<= 1;
                pucResult[sPos] |= 0x01;
              }
              else
              {
                pucResult[sPos] <<= 1;
              }
          }

      }; break;

      /* RLR */
      case DO_CHAROP_CHARRLR:      
      {
        
        int iRLR = (hb_parni (2))%8;  /* defaults to 0 */

        hb_xmemcpy (pucResult, pucString, sStrLen);
        
        if (iRLR != 0)
          for (sPos = 0; sPos < sStrLen; sPos++)
          {
            int iRLRCnt;

            for (iRLRCnt = 0; iRLRCnt < iRLR; iRLRCnt++)
              if (pucResult[sPos]&0x01)  /* most right bit set -> roll over */
              {
                pucResult[sPos] >>= 1;
                pucResult[sPos] |= 0x80;
              }
              else
              {
                pucResult[sPos] >>= 1;
              }
          }

      }; break;

      /* ADD */
      case DO_CHAROP_CHARADD: 
      {
        if (ISCHAR (2))
        {
          char *pucString2 = hb_parc (2);
          size_t sStrLen2 = hb_parclen (2);

          for (sPos = 0; sPos < sStrLen; sPos++)
            pucResult[sPos] = (char)(pucString[sPos]+pucString2[sPos%sStrLen2]);

        }
        else
          hb_xmemcpy (pucResult, pucString, sStrLen);

      }; break;

      /* SUB */
      case DO_CHAROP_CHARSUB: 
      {
        if (ISCHAR (2))
        {
          char *pucString2 = hb_parc (2);
          size_t sStrLen2 = hb_parclen (2);

          for (sPos = 0; sPos < sStrLen; sPos++)
            pucResult[sPos] = (char)(pucString[sPos]-pucString2[sPos%sStrLen2]);

        }
        else
          hb_xmemcpy (pucResult, pucString, sStrLen);

      }; break;

      /* AND */
      case DO_CHAROP_CHARAND: 
      {
        if (ISCHAR (2))
        {
          char *pucString2 = hb_parc (2);
          size_t sStrLen2 = hb_parclen (2);

          for (sPos = 0; sPos < sStrLen; sPos++)
            pucResult[sPos] = (char)(pucString[sPos] & pucString2[sPos%sStrLen2]);

        }
        else
          hb_xmemcpy (pucResult, pucString, sStrLen);

      }; break;

      /* OR */
      case DO_CHAROP_CHAROR:  
      {
        if (ISCHAR (2))
        {
          char *pucString2 = hb_parc (2);
          size_t sStrLen2 = hb_parclen (2);

          for (sPos = 0; sPos < sStrLen; sPos++)
            pucResult[sPos] = (char)(pucString[sPos] | pucString2[sPos%sStrLen2]);

        }
        else
          hb_xmemcpy (pucResult, pucString, sStrLen);

      }; break;

      /* XOR */
      case DO_CHAROP_CHARXOR: 
      {
        if (ISCHAR (2))
        {
          char *pucString2 = hb_parc (2);
          size_t sStrLen2 = hb_parclen (2);

          for (sPos = 0; sPos < sStrLen; sPos++)
            pucResult[sPos] = (char)(pucString[sPos] ^ pucString2[sPos%sStrLen2]);

        }
        else
          hb_xmemcpy (pucResult, pucString, sStrLen);

      }; break;

    }; /* endswitch (iSwitch) */

    if (ISBYREF (1))
      hb_storclen (( char * ) pucResult, sStrLen, 1);

    if (!iNoRet)
      hb_retclen (( char * ) pucResult, sStrLen);

    hb_xfree (pucResult);

  }
  else   /* if (ISCHAR (1)) */
  {
    hb_ret();
  }

  return;

}


/*  $DOC$
 *  $FUNCNAME$
 *      CHARADD()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Adds corresponding ASCII value of two strings
 *  $SYNTAX$
 *      CHARADD (<[@]cString1>, <cString2>) --> cAddString
 *  $ARGUMENTS$
 *      <[@]cString1>   first string
 *      <cString2>      second string
 *  $RETURNS$
 *      <cAddString>    string with added ASCII values
 *  $DESCRIPTION$
 *      The CHARADD() function constructs a new string from the two strings
 *      passed as parameters. To do this, it adds the ASCII values of the
 *      corresponding characters of both strings and places a character in
 *      the resulting string whose ASCII value equals to that sum (modulo 256).
 *      If the first string is passed by reference, the resulting string is
 *      stored in <cString1>, too. By setting the CSETREF()-switch to .T.,
 *      the return value can be omitted.
 *      If <cString2> is shorter than <cString1> and the last character of
 *      <cString2> has been processed, the function restarts with the first
 *      character of <cString2>.
 *  $EXAMPLES$
 *      ? charadd ("012345678", chr(1)) --> "123456789"
 *      ? charadd ("123456789", chr(255)) --> "012345678"
 *      ? charadd ("0000", chr(0)+chr(1)+chr(2)+chr(3)) --> "0123"
 *  $TESTS$
 *      charadd ("012345678", chr(1)) == "123456789"
 *      charadd ("012345678", chr(1)+chr(2)) == "133557799"
 *      charadd ("123456789", chr(255)) == "012345678"
 *      charadd ("123456789", chr(255)+chr(254)) == "002244668"
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      CHARADD() is compatible with CT3's CHARADD().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is charop.c, library is ct3.
 *  $SEEALSO$
 *      CHARSUB()   CHARAND()   CHARNOT()
 *      CHAROR()    CHARXOR()   CHARSHL()  
 *      CHARSHR()   CHARRLL()   CHARRLR()  
 *      CSETREF()
 *  $END$
 */

HB_FUNC (CHARADD)
{

  do_charop (DO_CHAROP_CHARADD);
  return;

}


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

HB_FUNC (CHARSUB)
{

  do_charop (DO_CHAROP_CHARSUB);
  return;

}


/*  $DOC$
 *  $FUNCNAME$
 *      CHARAND()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Combine corresponding ASCII value of two strings with bitwise AND
 *  $SYNTAX$
 *      CHARAND (<[@]cString1>, <cString2>) --> cAndString
 *  $ARGUMENTS$
 *      <[@]cString1>   first string
 *      <cString2>      second string
 *  $RETURNS$
 *      <cAndString>    string with bitwise AND combined ASCII values
 *  $DESCRIPTION$
 *      The CHARAND() function constructs a new string from the two strings
 *      passed as parameters. To do this, it combines the ASCII values of the
 *      corresponding characters of both strings with a bitwise AND-operation
 *      and places a character in the resulting string whose ASCII value
 *      equals to the result of that operation.
 *      If the first string is passed by reference, the resulting string is
 *      stored in <cString1>, too. By setting the CSETREF()-switch to .T.,
 *      the return value can be omitted.
 *      If <cString2> is shorter than <cString1> and the last character of
 *      <cString2> has been processed, the function restarts with the first
 *      character of <cString2>.
 *  $EXAMPLES$
 *      // clear the LSB 
 *      ? charand ("012345678", chr(254)) --> "002244668"
 *      ? charand ("012345678", chr(254)+chr(252)) --> "002044648"
 *  $TESTS$
 *      charand ("012345678", chr(254)) == "002244668"
 *      charand ("012345678", chr(254)+chr(252)) == "002044648"
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      CHARAND() is compatible with CT3's CHARAND().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is charop.c, library is ct3.
 *  $SEEALSO$
 *      CHARADD()   CHARSUB()   CHARNOT()
 *      CHAROR()    CHARXOR()   CHARSHL()  
 *      CHARSHR()   CHARRLL()   CHARRLR()  
 *      CSETREF()
 *  $END$
 */

HB_FUNC (CHARAND)
{

  do_charop (DO_CHAROP_CHARAND);
  return;

}


/*  $DOC$
 *  $FUNCNAME$
 *      CHARNOT()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Process each character in a string with bitwise NOT operation
 *  $SYNTAX$
 *      CHARNOT (<[@]cString>) --> cNotString
 *  $ARGUMENTS$
 *      <[@]cString>    string to be processed
 *  $RETURNS$
 *      <cNotString>    string with bitwise negated characters
 *  $DESCRIPTION$
 *      The CHARNOT() function constructs a new string from the string
 *      passed as parameter. To do this, it performs a bitwise NOT operation
 *      to the characters of the string and places a character in
 *      the resulting string whose ASCII value equals to the result of that
 *      operation. It can be easily seen that the resulting ASCII-value equals
 *      255 minus input ASCII value.
 *      If the string is passed by reference, the resulting string is
 *      stored in <cString>, too. By setting the CSETREF()-switch to .T.,
 *      the return value can be omitted.
 *  $EXAMPLES$
 *      ? charnot (chr(85)+chr(128)+chr(170)+chr(1)) --> chr(170)+chr(127)+chr(85)+chr(254)
 *      ? charnot (charnot ("This is a test!")) --> "This is a test!"
 *  $TESTS$
 *      charnot (chr(85)+chr(128)+chr(170)+chr(1)) == chr(170)+chr(127)+chr(85)+chr(254)
 *      charnot (charnot ("This is a test!")) == "This is a test!"
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      CHARNOT() is compatible with CT3's CHARNOT().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is charop.c, library is ct3.
 *  $SEEALSO$
 *      CHARADD()   CHARSUB()   CHARAND()
 *      CHAROR()    CHARXOR()   CHARSHL()  
 *      CHARSHR()   CHARRLL()   CHARRLR()  
 *      CSETREF()
 *  $END$
 */

HB_FUNC (CHARNOT)
{

  do_charop (DO_CHAROP_CHARNOT);
  return;

}


/*  $DOC$
 *  $FUNCNAME$
 *      CHAROR()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Combine corresponding ASCII value of two strings with bitwise OR
 *  $SYNTAX$
 *      CHAROR (<[@]cString1>, <cString2>) --> cOrString
 *  $ARGUMENTS$
 *      <[@]cString1>   first string
 *      <cString2>      second string
 *  $RETURNS$
 *      <cOrString>     string with bitwise OR combined ASCII values
 *  $DESCRIPTION$
 *      The CHAROR() function constructs a new string from the two strings
 *      passed as parameters. To do this, it combines the ASCII values of the
 *      corresponding characters of both strings with a bitwise OR-operation
 *      and places a character in the resulting string whose ASCII value
 *      equals to the result of that operation.
 *      If the first string is passed by reference, the resulting string is
 *      stored in <cString1>, too. By setting the CSETREF()-switch to .T.,
 *      the return value can be omitted.
 *      If <cString2> is shorter than <cString1> and the last character of
 *      <cString2> has been processed, the function restarts with the first
 *      character of <cString2>.
 *  $EXAMPLES$
 *      // set the LSB 
 *      ? charor ("012345678", chr(1)) --> "113355779"
 *      ? charor ("012345678", chr(1)+chr(3)) --> "133357779"
 *  $TESTS$
 *      charor ("012345678", chr(1)) == "113355779"
 *      charor ("012345678", chr(1)+chr(3)) == "133357779"
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      CHAROR() is compatible with CT3's CHAROR().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is charop.c, library is ct3.
 *  $SEEALSO$
 *      CHARADD()   CHARSUB()   CHARNOT()
 *      CHARAND()   CHARXOR()   CHARSHL()  
 *      CHARSHR()   CHARRLL()   CHARRLR()  
 *      CSETREF()
 *  $END$
 */

HB_FUNC (CHAROR)
{

  do_charop (DO_CHAROP_CHAROR);
  return;

}


/*  $DOC$
 *  $FUNCNAME$
 *      CHARXOR()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Combine corresponding ASCII value of two strings with bitwise XOR
 *  $SYNTAX$
 *      CHARXOR (<[@]cString1>, <cString2>) --> cXOrString
 *  $ARGUMENTS$
 *      <[@]cString1>   first string
 *      <cString2>      second string
 *  $RETURNS$
 *      <cXOrString>    string with bitwise XOR combined ASCII values
 *  $DESCRIPTION$
 *      The CHARXOR() function constructs a new string from the two strings
 *      passed as parameters. To do this, it combines the ASCII values of the
 *      corresponding characters of both strings with a bitwise XOR-operation
 *      and places a character in the resulting string whose ASCII value
 *      equals to the result of that operation.
 *      If the first string is passed by reference, the resulting string is
 *      stored in <cString1>, too. By setting the CSETREF()-switch to .T.,
 *      the return value can be omitted.
 *      If <cString2> is shorter than <cString1> and the last character of
 *      <cString2> has been processed, the function restarts with the first
 *      character of <cString2>.
 *  $EXAMPLES$
 *      // easy encryption 
 *      ? charxor ("This is top secret !", "My Password") --> <encrypted sentence>
 *  $TESTS$
 *      charxor (charxor ("This is top secret !", "My Password"), "My Password") == "This is top secret !"
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      CHARXOR() is compatible with CT3's CHARXOR().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is charop.c, library is ct3.
 *  $SEEALSO$
 *      CHARADD()   CHARSUB()   CHARNOT()
 *      CHARAND()   CHAROR()    CHARSHL()  
 *      CHARSHR()   CHARRLL()   CHARRLR()  
 *      CSETREF()
 *  $END$
 */

HB_FUNC (CHARXOR)
{

  do_charop (DO_CHAROP_CHARXOR);
  return;

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

HB_FUNC (CHARSHL)
{

  do_charop (DO_CHAROP_CHARSHL);
  return;

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

HB_FUNC (CHARSHR)
{

  do_charop (DO_CHAROP_CHARSHR);
  return;

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

HB_FUNC (CHARRLL)
{

  do_charop (DO_CHAROP_CHARRLL);
  return;

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

HB_FUNC (CHARRLR)
{

  do_charop (DO_CHAROP_CHARRLR);
  return;

}



