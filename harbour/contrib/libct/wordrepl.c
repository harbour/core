/*
 * $Id$
 */

/*
 * Harbour Project source code: 
 *   CT3 string function WORDREPL()
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


/*  $DOC$
 *  $FUNCNAME$
 *      WORDREPL()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Replacement of double characters
 *  $SYNTAX$
 *      WORDREPL (<cDoubleCharacterSearchString>, <[@]cString>,
 *                <cDoubleCharacterReplaceString>, [<lMode>]) -> cString
 *  $ARGUMENTS$
 *      <cDoubleCharacterSearchString>   is a string of double characters
 *                                       that should be replaced
 *      <[@]cString>                     is the processed string
 *      <cDoubleCharacterReplaceString>  is a string of double characters that
 *                                       replace the one of <cSearchString>
 *      [<lMode>]                        sets the replacement method (see description)
 *                                       Default: .F.
 *  $RETURNS$
 *      cString                          the processed string
 *  $DESCRIPTION$
 *      The WORDREPL() takes the double characters of <cDoubleCharacterSearchString>
 *      one after the other and searches for them in <cString>.
 *      For <lMode> set to .F., this search is successful, if the double
 *      character sequence in <cString> starts at an odd position or at any
 *      position, if <lMode> is set to .T.
 *      If this happens, the double character sequence will be replaced with
 *      the corresponding double character sequence of <cDoubleCharacterReplaceString>.
 *      If <cDoubleCharacterReplaceString> is shorter than <cDoubleCharacterSearchString>
 *      the last double sequence of <cDoubleCharacterReplaceString> is used for
 *      the "rest" of <cDoubleCharacterSearchString>. Note that the last double
 *      character sequence in "AABBC" is "BB" in this context !!
 *      After the replacement the function restarts the search in <cString>
 *      BEHIND the replacement if the CSETATMUPA() switch is turned off, or
 *      BEHIND the first character of the replacement if the switch is turned on.
 *      (see examples for this !)
 *      One can omit the return value of this function by setting the CSETREF()
 *      to .T., but one must then pass <cString> by reference to get a result.
 *  $EXAMPLES$
 *      ? wordrepl("CC", "AABBCCDDEE", "XX") // "AABBXXDDEE"
 *      ? wordrepl("aa", "1aaaa", "ba")      // "1abaa" 
 *      ? wordrepl("aa", "1aaaa", "ba", .T.) // "1baba" 
 *      csetatmupa(.T.)
 *      ? wordrepl("aa", "1aaaa", "ba")      // "1abaa" 
 *      ? wordrepl("aa", "1aaaa", "ba", .T.) // "1bbba" 
 *  $TESTS$
 *      wordrepl("CC", "AABBCCDDEE", "XX") == "AABBXXDDEE"
 *      wordrepl("aa", "1aaaa", "ba")      == "1abaa" 
 *      wordrepl("aa", "1aaaa", "ba", .T.) == "1baba" 
 *      eval ({||csetatmupa(.T.),wordrepl("aa", "1aaaa", "ba")}) == "1abaa" 
 *      eval ({||csetatmupa(.T.),wordrepl("aa", "1aaaa", "ba", .T.)}) == "1bbba"
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      WORDREPL() is compatible with CT3's WORDREPL().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is wordrepl.c, library is ct3.
 *  $SEEALSO$
 *      CHARREPL()    RANGEREPL()   POSREPL()
 *      CSETREF()   CSETATMUPA()
 *  $END$
 */

HB_FUNC (WORDREPL)
{

  int iNoRet;
  int iMultiPass;

  size_t sSearchLen, sReplaceLen;

  /* suppressing return value ? */
  iNoRet = ct_getref();
  iMultiPass = ct_getatmupa();

  /* param check */
  if (((sSearchLen = (size_t)hb_parclen (1))/2 > 0) &&
      (ISCHAR (2)) &&
      ((sReplaceLen = (size_t)hb_parclen (3))/2 > 0))
  {

    /* get parameters */
    char *pcSearch = hb_parc (1);
    char *pcString = hb_parc (2);
    size_t sStrLen = (size_t)hb_parclen (2);
    char *pcReplace = hb_parc (3);
    int iMode;
    char *pcRet;
    size_t sIndex;

    if (ISLOG (4))
    {
      iMode = hb_parl (4);
    }
    else
    {
      iMode =0;
    }

    pcRet = hb_xgrab (sStrLen);
    hb_xmemcpy (pcRet, pcString, sStrLen);

    for (sIndex = 0; sIndex < (sSearchLen&0xFFFFFFFE); sIndex+=2)
    {
    
      size_t sMatchStrLen;
      char *pc;
      size_t sReplIndex = sIndex;

      if (sReplIndex > (sReplaceLen&0xFFFFFFFE))
      {
        sReplIndex = (sReplaceLen&0xFFFFFFFE);
      }

      pc = pcString;
      while ((pc = ct_at_exact_forward (pc, sStrLen-(pc-pcString),
                                        pcSearch+sIndex, 2,
                                        &sMatchStrLen)) != NULL)
      {
        if (iMode)
        {
          /* always replace */
          *(pcRet+(pc-pcString)) = *(pcReplace+sReplIndex);
          *(pcRet+(pc-pcString)+1) = *(pcReplace+sReplIndex+1);
          
          if (iMultiPass)
          {
            pc++;
          }
          else
          {
            pc+=2;
          }

        }
        else
        {
          /* replace only if pc is an even position */
          if (((pc-pcString)%2) == 0)
          {
            *(pcRet+(pc-pcString)) = *(pcReplace+sReplIndex);
            *(pcRet+(pc-pcString)+1) = *(pcReplace+sReplIndex+1);
            /* parse pcString in steps of two characters */
            pc+=2;
          }
          else
          {
            /* we are on an odd position, so add only 1 to pc */
            pc++;
          }
        }
      }
    }

    /* return string */
    if (ISBYREF (2))
    {
      hb_storclen (pcRet, sStrLen, 2);
    }

    if (iNoRet)
    {
      hb_retl (0);
    }
    else
    {
      hb_retclen (pcRet, sStrLen);
    }

    hb_xfree (pcRet);

  }
  else /* ((sSearchLen = (size_t)hb_parclen (1))/2 > 0) &&
          (ISCHAR (2)) &&
          ((sReplaceLen = (size_t)hb_parclen (3))/2 > 0))  */
  {
    if (iNoRet)
    {
      hb_retl (0);
    }
    else
    {
      if (ISCHAR (2))
      {
        hb_retclen (hb_parc (2), hb_parclen (2));
      }
      else
      {
        hb_retc ("");
      }
    }
  }

  return;

}




