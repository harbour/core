/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   STRSWAP() CT3 string function
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
 *      STRSWAP()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Swap the contents of two strings
 *  $SYNTAX$
 *      STRSWAP (<[@]cString1>, <[@]cString2>) -> cString
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      STRSWAP() is compatible with CT3's STRSWAP().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is strswap.c, library is libct.
 *  $SEEALSO$
 *  $END$
 */

HB_FUNC (STRSWAP)
{

  size_t sStrLen1, sStrLen2;

  /* param check */
  if (((sStrLen1 = (size_t)hb_parclen (1)) > 0) &&
      ((sStrLen2 = (size_t)hb_parclen (2)) > 0))
  {

    /* get parameters */
    char *pcString1 = (char *)hb_parc (1);
    char *pcString2 = (char *)hb_parc (2);
    char *pcRet1, *pcRet2;
    int iChange1, iChange2;
    size_t sIndex, sCmpLen;

    if ((iChange1=ISBYREF(1)) != 0)
    {
      pcRet1 = hb_xgrab (sStrLen1);
      hb_xmemcpy (pcRet1, pcString1, sStrLen1);
    }

    if ((iChange2=ISBYREF(2)) != 0)
    {
      pcRet2 = hb_xgrab (sStrLen2);
      hb_xmemcpy (pcRet2, pcString2, sStrLen2);
    }

    sCmpLen = (sStrLen1 < sStrLen2 ? sStrLen1 : sStrLen2);
    for (sIndex = 0; sIndex < sCmpLen; sIndex++)
    {
      char cExchange;

      if (iChange1)
      {
        cExchange = *(pcString1+sIndex);
        *(pcRet1+sIndex) = *(pcString2+sIndex);
        if (iChange2)
        {
          *(pcRet2+sIndex) = cExchange;
        }
      }
      else
      {
        *(pcRet2+sIndex) = *(pcString1+sIndex);
      }
    }

    /* strings */
    if (iChange1)
    {
      hb_storclen (pcRet1, sStrLen1, 1);
      hb_xfree (pcRet1);
    }

    if (iChange2)
    {
      hb_storclen (pcRet2, sStrLen2, 2);
      hb_xfree (pcRet2);
    }

    hb_retc ("");


  }
  else /* ((sStrLen1 = (size_t)hb_parclen (1)) > 0) &&
          ((sStrLen2 = (size_t)hb_parclen (2)) > 0))   */
  {
    PHB_ITEM pSubst = NULL;
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_STRSWAP,
                               NULL, "STRSWAP", 0, EF_CANSUBSTITUTE, 2,
                               hb_paramError (1), hb_paramError (2));
    }
    
    if (pSubst != NULL)
    {
      hb_itemReturn (pSubst);
      hb_itemRelease (pSubst);
    }
    else
    {
      hb_retc ("");
    }
    return;
  }

  return;

}




