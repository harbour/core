/*
 * $Id$
 */

/*
 * Harbour Project source code: 
 *   CT3 string functions
 *     - CHARSWAP()
 *     - WORDSWAP()
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
#define DO_CHARSWAP_CHARSWAP          0
#define DO_CHARSWAP_WORDSWAP          1
#define DO_CHARSWAP_WORDSWAP_CHARSWAP 2

/* helper function for the charswap and wordswap functions */
static void do_charswap (int iSwitch)
{

  int iNoRet;

  /* suppress return value ? */
  iNoRet = ct_getref();

  /* param check */
  if (ISCHAR (1))
  {

    char *pcString = hb_parc (1);
    size_t sStrLen = (size_t)hb_parclen (1);
    char *pcRet;
    size_t sRetIndex = 0;
    int iShift, iMod;
    char *pcSub;

    if (iSwitch == DO_CHARSWAP_WORDSWAP)
    {
      iShift = 4;
      if (ISLOG (2) && hb_parl (2))
      {
        iSwitch = DO_CHARSWAP_WORDSWAP_CHARSWAP;
      }
    }
    else
    {
      iShift = 2;
    }

    pcRet = ( char * ) hb_xgrab (sStrLen);

    for (pcSub = pcString; pcSub < pcString+sStrLen+1-iShift; pcSub += iShift)
    {
      switch (iSwitch)
      {
        case DO_CHARSWAP_WORDSWAP:
        {
          *(pcRet+sRetIndex) = *(pcSub+2);
          sRetIndex++;
          *(pcRet+sRetIndex) = *(pcSub+3);
          sRetIndex++;
          *(pcRet+sRetIndex) = *(pcSub);
          sRetIndex++;
          *(pcRet+sRetIndex) = *(pcSub+1);
          sRetIndex++;
        }; break;

        case DO_CHARSWAP_WORDSWAP_CHARSWAP:
        {
          *(pcRet+sRetIndex) = *(pcSub+3);
          sRetIndex++;
          *(pcRet+sRetIndex) = *(pcSub+2);
          sRetIndex++;
        }; /* no 'break' here !! */

        case DO_CHARSWAP_CHARSWAP:
        {
          *(pcRet+sRetIndex) = *(pcSub+1);
          sRetIndex++;
          *(pcRet+sRetIndex) = *(pcSub);
          sRetIndex++;
        };
      }
    }

    /* copy rest of string */
    if ((iSwitch == DO_CHARSWAP_WORDSWAP) ||
        (iSwitch == DO_CHARSWAP_WORDSWAP_CHARSWAP))
    {
      iMod = sStrLen%4;
    }
    else
    {
      iMod = sStrLen%2;
    }

    for (pcSub = pcString+sStrLen-iMod; pcSub < pcString+sStrLen; pcSub++)
    {
      *(pcRet+sRetIndex) = *pcSub;
      sRetIndex++;
    }

    /* return string */
    if (ISBYREF (1))
    {
      hb_storclen (pcRet, sRetIndex, 1);
    }

    if (iNoRet)
    {
      hb_retl (0);
    }
    else
    {
      hb_retclen (pcRet, sRetIndex);
    }

    hb_xfree (pcRet);

  }
  else /* if (ISCHAR (1)) */
  {
    PHB_ITEM pSubst = NULL;
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      
      if (iSwitch == DO_CHARSWAP_CHARSWAP)
      {
        pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_CHARSWAP,
                                 NULL, "CHARSWAP", 0, EF_CANSUBSTITUTE, 1,
                                 hb_paramError (1));
      }
      else
      {
        pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_WORDSWAP,
                                 NULL, "WORDSWAP", 0, EF_CANSUBSTITUTE, 2,
                                 hb_paramError (1), hb_paramError (2));
      }
    }

    if (pSubst != NULL)
    {
      hb_itemReturn (pSubst);
      hb_itemRelease (pSubst);
    }
    else
    {
      if (iNoRet)
      {
        hb_retl (0);
      }
      else
      {
        hb_retc ("");
      }
    }
  }

}


/*  $DOC$
 *  $FUNCNAME$
 *      CHARSWAP()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Swap neighbouring characters in a string
 *  $SYNTAX$
 *      CHARSWAP (<[@]cString>) -> cSwappedString
 *  $ARGUMENTS$
 *      <[@]cString>      is the string that should be processed
 *  $RETURNS$
 *      <cSwappedString>  a string where neighbour characters are swapped
 *  $DESCRIPTION$
 *      The CHARSWAP() function loops through <cString> in steps of two
 *      characters and exchanges the characters from the odd and the even
 *      positions.
 *      By setting the CSETREF() switch to .T., one can omit the return value
 *      of this functin, but one must then pass <cString> by reference.
 *  $EXAMPLES$
 *      ? CHARSWAP("0123456789")   // "1032547698"
 *      ? CHARSWAP("ABCDEFGHIJK")  // "BADCFEHGJIK"
 *  $TESTS$
 *      CHARSWAP("0123456789")  == "1032547698"
 *      CHARSWAP("ABCDEFGHIJK") == "BADCFEHGJIK"
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      CHARSWAP() is compatible with CT3's CHARSWAP().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is charswap.c, library is libct.
 *  $SEEALSO$
 *      WORDSWAP(),CSETREF()
 *  $END$
 */

HB_FUNC (CHARSWAP)
{

  do_charswap (DO_CHARSWAP_CHARSWAP);
  return;

}


/*  $DOC$
 *  $FUNCNAME$
 *      WORDSWAP()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Swap neighbouring double characters in a string
 *  $SYNTAX$
 *      WORDSWAP (<[@]cString> [, <lSwapCharacters>]) -> cSwappedString
 *  $ARGUMENTS$
 *      <[@]cString>         is the string that should be processed
 *      [<lSwapCharacters>]  specifies whether an additional swap should be
 *                           done within the double characters
 *                           Default: .F., no additional swap
 *  $RETURNS$
 *      <cSwappedString>  a string where neighbouring double characters are
 *                        swapped
 *  $DESCRIPTION$
 *      The WORDSWAP() function loops through <cString> in steps of four
 *      characters and exchanges the double characters from the first and
 *      second position with the one from the third and forth position.
 *      Additionally the function can perform a swap of the both char of
 *      each double character.
 *      By setting the CSETREF() switch to .T., one can omit the return value
 *      of this functin, but one must then pass <cString> by reference.
 *  $EXAMPLES$
 *      ? WORDSWAP("1234567890")        // "3412785690"
 *      ? WORDSWAP("1234567890", .t.)   // "4321876590"
 *  $TESTS$
 *      WORDSWAP("1234567890")      == "3412785690"
 *      WORDSWAP("1234567890", .t.) == "4321876590"
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      WORDSWAP() is compatible with CT3's WORDSWAP().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is charswap.c, library is libct.
 *  $SEEALSO$
 *      CHARSWAP(),CSETREF()
 *  $END$
 */

HB_FUNC (WORDSWAP)
{

  do_charswap (DO_CHARSWAP_WORDSWAP);
  return;

}
