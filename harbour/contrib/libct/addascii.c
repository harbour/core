/*
 * $Id$
 */

/*
 * Harbour Project source code: 
 *   ADDASCII() CT3 string function
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
 *      ADDASCII()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Add an integer value to an ascii value of a string
 *  $SYNTAX$
 *      ADDASCII (<[@]cString>, <nValue>, [<nPosition>], [<lCarryOver>]) --> cString
 *  $ARGUMENTS$
 *      <[@]cString>   is the string that should be edited
 *      <nValue>       is a integer value that should be added to the
 *                     ASCII value of the character at the <nPosition>th position
 *      [<nPosition>]  is the position of the character that should be edited.
 *                     If not supplied, the last character of <[@]cString> is
 *                     edited.
 *      [<lCarryOver>] NEW: is set to .T. if the substring from position 1 to
 *                     position <nPosition> should be treated as an integer
 *                     written to the base 256. Thus, the addition of <nValue>
 *                     can affect to whole substring (see EXAMPLES).
 *                     Default is .F., the original behaviour of this function.
 *  $RETURNS$
 *      The edited string is returned. The return value can be suppressed by
 *      using the CSETREF() function. The string must then be passed by
 *      reference [@].
 *  $DESCRIPTION$
 *      ADDASCII() can be used to add or subtract integer values from
 *      ASCII values in a string. The new <lCarryOver> parameter allows
 *      to treat a string as an integer written to the base 256. Since
 *      <nValue> is limited to a signed long, only substrings 4 characters
 *      long can be affected by one ADDASCII() call.
 *      If the length of <[@]cString> is smaller than <nPosition>, the
 *      string remains unchanged. The same happens, if uninterpretable
 *      parameters are passed to this function.
 *  $EXAMPLES$
 *      // Add 32 to the ASCII value of the character at the last position
 *      // in the string
 *      
 *      ? addascii ("SmitH", 32)  --> "Smith"
 *  $TESTS$
 *      addascii ("0000", 1, 1) == "1000"
 *      addascii ("0000", 1) == "0001" 
 *      addascii ("AAAA", -255, 1) == "BAAA"
 *      addascii ("AAAA", -255) == "AAAB"
 *      addascii ("AAAA", 1, 2, .T.) == "ABAA"
 *      addascii ("AAAA", 257, 2, .T.) == "BBAA"
 *      addascii ("AAAA", 257, 2, .F.) == "ABAA"
 *      addascii ("AAAA", 258,, .T.) == "AABC"
 *      addascii ("ABBA", -257, 3, .T.) == "AAAA"
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      ADDASCII() is compatible with CT3's ADDASCII().
 *      A new, 4th, parameter has been added who defaults to the original
 *      behaviour if omitted.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is addascii.c, library is ct3.
 *  $SEEALSO$
 *      CSETREF() 
 *  $END$
 */


HB_FUNC (ADDASCII)
{

  int iNoRet;

  /* suppressing return value ? */
  iNoRet = ct_getref();
  
  if (ISCHAR (1))
  {
    
    char *pcSource = hb_parc (1);
    size_t sLen = hb_parclen (1);
    char *pcResult;
    size_t sPos;
    long lValue;
    int iCarryOver;

    if (ISNUM (3))
      sPos = hb_parnl (3);
    else
      sPos = sLen;

    if ((sPos > sLen) || !(ISNUM (2)))
    {
      int iArgErrorMode = ct_getargerrormode();
      if (iArgErrorMode != CT_ARGERR_IGNORE)
      {
        ct_error ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_ADDASCII,
                  NULL, "ADDASCII", 0, EF_CANDEFAULT, 4,
                  hb_paramError (1), hb_paramError (2),
                  hb_paramError (3), hb_paramError (4));
      }

      /* return string unchanged */
      if (iNoRet) 
        hb_retl (0);
      else
        hb_retclen (pcSource, sLen);
    
      return;
    }

    pcResult = (char *)hb_xgrab (sLen);
    hb_xmemcpy (pcResult, pcSource, sLen);
    
    lValue   = hb_parnl (2);
    if (ISLOG (4))
      iCarryOver = hb_parl (4);
    else
      iCarryOver = 0;
    
    if (iCarryOver)
    {
      size_t sCurrent;
      long lResult;

      for (sCurrent = sPos; (sCurrent>0) && (lValue != 0); sCurrent--)
      {
        lResult = (long)pcSource[sCurrent-1]+(lValue%256);
        
        lValue /= 256;
        if (lResult > 255)
          lValue++;
        else if (lResult < 0)
          lValue--;

        pcResult[sCurrent-1] = (char)(lResult%256);
      }
    }
    else
    {
      pcResult[sPos-1] = (char)(((long)pcResult[sPos-1]+lValue)%256);
    }

    if (iNoRet)
      hb_retl (0);
    else
      hb_retclen (pcResult, sLen);

    if (ISBYREF (1))
      hb_storclen (pcResult, sLen, 1);

    hb_xfree (pcResult);
    return;

  }
  else
  {
    PHB_ITEM pSubst = NULL;
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_ADDASCII,
                               NULL, "ADDASCII", 0, EF_CANSUBSTITUTE, 4,
                               hb_paramError (1), hb_paramError (2),
                               hb_paramError (3), hb_paramError (4));
    }
    
    if (pSubst != NULL)
    {
      hb_itemReturn (pSubst);
      hb_itemRelease (pSubst);
    }
    else
    {
      if (iNoRet)
        hb_retl (0);
      else
        hb_retc ("");
    }
    return;
  }

}





