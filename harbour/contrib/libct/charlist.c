/*
 * $Id$
 */

/*
 * Harbour Project source code: 
 *   CT3 string functions
 *     - CHARLIST()
 *     - CHARSLIST()  (NEW)
 *     - CHARNOLIST()  
 *     - CHARHIST()  (NEW)
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


/* defines */
#define DO_LIST_CHARLIST        0
#define DO_LIST_CHARNOLIST      1
#define DO_LIST_CHARHIST        2
#define DO_LIST_CHARSLIST       3

/* helper function for the list function */
static void do_list (int iSwitch)
{

  char *pcString;
  size_t sStrLen;

  size_t asCharCnt[256];
  size_t sCnt;

  /* init asCharCnt */
  for (sCnt = 0; sCnt < 256; sCnt++)
  {
    asCharCnt[sCnt] = 0;
  }

  /* init params */
  if (ISCHAR (1))
  {
    pcString = hb_parc (1);
    sStrLen = (size_t)hb_parclen (1);
  }
  else
  {
    pcString = "";
    sStrLen = 0;
  }

  /* count characters */
  if (iSwitch == DO_LIST_CHARLIST)
  {

    char pcRet[256];
    size_t sRetStrLen = 0;
    
    for (sCnt = 0; sCnt < sStrLen; sCnt++)
    {
      if (asCharCnt[(size_t)(pcString[sCnt])] == 0)
      {
        pcRet[sRetStrLen++] = pcString[sCnt];
        asCharCnt[(size_t)(pcString[sCnt])] = 1;
      }
    }

    hb_retclen (pcRet, sRetStrLen);

  }
  else
  {
    
    for (sCnt = 0; sCnt < sStrLen; sCnt++)
    {
      size_t sIndex = (size_t)(unsigned char)(*(pcString+sCnt));
      asCharCnt[sIndex] = asCharCnt[sIndex]+1;
    }

    switch (iSwitch)
    {
      case DO_LIST_CHARSLIST:
      {
        
        char *pcRet;
        size_t sRetStrLen = 0;

        pcRet = ( char *) hb_xgrab (256);

        for (sCnt = 0; sCnt < 256; sCnt++)
        {
          if (asCharCnt[sCnt] != 0)
          {
            *(pcRet+sRetStrLen) = (unsigned char)sCnt;
            sRetStrLen++;
          }
        }

        hb_retclen (pcRet, sRetStrLen);
        hb_xfree (pcRet);

      }; break;
     
      case DO_LIST_CHARNOLIST:
      {
        
        char *pcRet;
        size_t sRetStrLen = 0;
    
        pcRet = ( char * ) hb_xgrab (256);

        for (sCnt = 0; sCnt < 256; sCnt++)
        {
          if (asCharCnt[sCnt] == 0)
          {
            *(pcRet+sRetStrLen) = (unsigned char)sCnt;
            sRetStrLen++;
          }
        }
        
        hb_retclen (pcRet, sRetStrLen);
        hb_xfree (pcRet);

      }; break;

      case DO_LIST_CHARHIST:
      {
        PHB_ITEM pArray, pCount;

        pArray = hb_itemArrayNew (256);
        for (sCnt = 0; sCnt < 256; sCnt++)
        {
          pCount = hb_itemPutNL (NULL, asCharCnt[sCnt]);
          hb_itemArrayPut (pArray, sCnt+1, pCount);
          hb_itemRelease (pCount);
        }
        hb_itemReturn (pArray);
        hb_itemRelease (pArray);
      }; break;

    }

  }

  return;

}


/*  $DOC$
 *  $FUNCNAME$
 *      CHARLIST()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Generates a list of all characters in a string
 *  $SYNTAX$
 *      CHARLIST ([<cString>]) -> cCharacterList
 *  $ARGUMENTS$
 *      [<cString>]       is the string for whom the function generates a list
 *                        of all characters
 *                        Default: "" (empty string)
 *  $RETURNS$
 *      <cCharacterList>  a list of the characters in <cString>
 *  $DESCRIPTION$
 *      The CHARLIST() function generates a list of those characters that
 *      are contained in <cString>. This list can contain each character
 *      only once, so that its maximum length is 256. The list lists those
 *      characters first that are occuring in <cString> first.
 *  $EXAMPLES$
 *      ? charlist ("Hello World !") --> "Helo Wrd!"
 *  $TESTS$
 *      charlist ("Hello World !") == "Helo Wrd!"
 *      charlist (nil) == ""
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      CHARLIST() is compatible with CT3's CHARLIST().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is charlist.c, library is libct.
 *  $SEEALSO$
 *      CHARNOLIST(),CHARSLIST(),CHARHIST()
 *  $END$
 */

HB_FUNC (CHARLIST)
{

  do_list (DO_LIST_CHARLIST);
  return;

}


/*  $DOC$
 *  $FUNCNAME$
 *      CHARSLIST()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Generates a sorted list of all characters in a string
 *  $SYNTAX$
 *      CHARSLIST ([<cString>]) -> cSortedCharacterList
 *  $ARGUMENTS$
 *      [<cString>]       is the string for whom the function generates a
 *                        sorted list of all characters
 *                        Default: "" (empty string)
 *  $RETURNS$
 *      <cSortedCharacterList>  a sorted list of the characters in <cString>
 *  $DESCRIPTION$
 *      The CHARLIST() function generates a sorted list of those characters that
 *      are contained in <cString>. This list can contain each character
 *      only once, so that its maximum length is 256. The function
 *      gives the same result as CHARSORT(CHARLIST(<cString>))
 *  $EXAMPLES$
 *      ? charslist ("Hello World !") --> " !HWdelor"
 *  $TESTS$
 *      charslist ("Hello World !") == " !HWdelor"
 *      charslist ("Hello World !") == charsort (charlist ("Hello World !"))
 *      charslist (nil) == ""
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      CHARSLIST() is only available in Harbour's CT3 library.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is charlist.c, library is libct.
 *  $SEEALSO$
 *      CHARNOLIST(),CHARLIST(),CHARHIST()
 *  $END$
 */

HB_FUNC (CHARSLIST)
{

  do_list (DO_LIST_CHARSLIST);
  return;

}


/*  $DOC$
 *  $FUNCNAME$
 *      CHARNOLIST()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Generates a list of all characters not contained in a string
 *  $SYNTAX$
 *      CHARNOLIST ([<cString>]) -> cCharacterList
 *  $ARGUMENTS$
 *      [<cString>]       is the string for whom the function generates a list
 *                        of all characters not contained in that string
 *                        Default: "" (empty string)
 *  $RETURNS$
 *      <cCharacterList>  a list of the characters that are not contained in <cString>
 *  $DESCRIPTION$
 *      The CHARNOLIST() function generates a list of those characters that
 *      are not contained in <cString>. This list can contain each character
 *      only once, so that its maximum length is 256. The list is alphabetically
 *      sorted.
 *  $EXAMPLES$
 *      ? charnolist (charnolist ("Hello World !")) --> " !HWdelor"
 *  $TESTS$
 *      charnolist (charnolist ("Hello World !")) == charslist ("Hello World !")
 *      charnolist (charnolist (nil)) == ""
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      CHARNOLIST() is compatible with CT3's CHARNOLIST().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is charlist.c, library is libct.
 *  $SEEALSO$
 *      CHARLIST(),CHARSLIST(),CHARHIST()
 *  $END$
 */

HB_FUNC (CHARNOLIST)
{

  do_list (DO_LIST_CHARNOLIST);
  return;

}


/*  $DOC$
 *  $FUNCNAME$
 *      CHARHIST()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Generates a character histogram of a string
 *  $SYNTAX$
 *      CHARHIST ([<cString>]) -> aCharacterCount
 *  $ARGUMENTS$
 *      [<cString>]       is the string for whom the function generates a
 *                        character histogram
 *                        Default: "" (empty string)
 *  $RETURNS$
 *      <aCharacterCount> an array with 256 elements where the nth element
 *                        contains the count of character #(n-1) in cString 
 *  $DESCRIPTION$
 *      The CHARHIST() function generates a character histogram of those
 *      characters that are contained in <cString>. This histogram is stored
 *      in an 256-element array where the nth element contains the count
 *      of ASCII character #(n-1) in <cString>.
 *  $EXAMPLES$
 *      ? charhist ("Hello World !")[109] --> 3  // chr(108)=="l"
 *  $TESTS$
 *      charhist ("Hello World !")[109] == 3
 *      eval ({||aeval (charhist ("Hello World !"),{|x|nTotal+=x}),nTotal==len("Hello World !")}
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      CHARHIST() is only available in Harbour's CT3 library.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is charlist.c, library is libct.
 *  $SEEALSO$
 *      CHARLIST(),CHARNOLIST(),CHARSLIST()
 *  $END$
 */

HB_FUNC (CHARHIST)
{

  do_list (DO_LIST_CHARHIST);
  return;

}


