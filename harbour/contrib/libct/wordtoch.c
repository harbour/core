/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   WORDTOCHAR() CT3 string function
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
 *      WORDTOCHAR()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Replace double with single characters
 *  $SYNTAX$
 *      WORDTOCHAR (<cDoubleCharacterSearchString>, <cString>,
 *                  <cSingleCharacterReplaceString>) -> cString
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      WORDTOCHAR() is compatible with CT3's WORDTOCHAR().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is wordtoch.c, library is libct.
 *  $SEEALSO$
 *      CSETATMUPA(),CHARREPL(),WORDREPL()
 *  $END$
 */

HB_FUNC (WORDTOCHAR)
{

  int iMultiPass;

  size_t sSearchLen, sStrLen, sReplaceLen;

  iMultiPass = ct_getatmupa();

  /* param check */
  if (((sSearchLen = (size_t)hb_parclen (1))/2 > 0) &&
      ((sStrLen = (size_t)hb_parclen (2))/2 > 0) &&
      ((sReplaceLen = (size_t)hb_parclen (3)) > 0))
  {

    /* get parameters */
    char *pcSearch = (char *)hb_parc (1);
    char *pcString = (char *)hb_parc (2);
    char *pcReplace = (char *)hb_parc (3);
    char *pcRet;
    size_t sRetIndex, sIndex;
    int iNoReplace;

    pcRet = ( char * )hb_xgrab (sStrLen);

    sRetIndex = 0;
    sIndex = 0;
    iNoReplace = 0;

    *pcRet = *pcString;  /* copy first char */
    do
    {
      
      size_t sMatchStrLen;
      char *pc;
      size_t sReplIndex;

      *(pcRet+sRetIndex+1) = *(pcString+sIndex+1);

      if (!iNoReplace &&
          ((pc = ct_at_exact_forward (pcSearch, sSearchLen,
                                      pcRet+sRetIndex, 2,
                                      &sMatchStrLen)) != NULL) &&
          (((sReplIndex=(pc-pcSearch)) & 1) != 1))
      {
        sReplIndex /= 2;
        if (sReplIndex >= sReplaceLen)
        {
          sReplIndex = sReplaceLen-1;
        }
        *(pcRet+sRetIndex) = *(pcReplace+sReplIndex);
        
        if (!iMultiPass)
        {
          iNoReplace = 1;  /* just copy next char without searching & replacing */
        }
      }
      else
      {
        iNoReplace = 0;
        sRetIndex++;
      }

      sIndex++;

    } while (sIndex < sStrLen-1);

    /* return string */

    hb_retclen (pcRet, sRetIndex+1);
    hb_xfree (pcRet);

  }
  else /* ((sSearchLen = (size_t)hb_parclen (1))/2 > 0) 
          ((sStrLen = (size_t)hb_parclen (2))/2 > 0 &&
          ((sReplaceLen = (size_t)hb_parclen (3)) > 0)) */
  {
    PHB_ITEM pSubst = NULL;
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_WORDTOCHAR,
                               NULL, "WORDTOCHAR", 0, EF_CANSUBSTITUTE, 3,
                               hb_paramError (1), hb_paramError (2),
                               hb_paramError (3));
    }
    
    if (pSubst != NULL)
    {
      hb_itemReturn (pSubst);
      hb_itemRelease (pSubst);
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
    return;
  }

  return;

}




