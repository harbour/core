/*
 * $Id$
 */

/*
 * Harbour Project source code: 
 *   CT3 string functions
 *     - CHARONLY()
 *     - CHARREM()
 *     - WORDONLY()
 *     - WORDREM()
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
#define DO_CHARONLY_CHARONLY    0
#define DO_CHARONLY_WORDONLY    1
#define DO_CHARONLY_CHARREM     2
#define DO_CHARONLY_WORDREM     3

/* helper function for the *one functions */
static void do_charonly (int iSwitch)
{

  /* param check */
  if (ISCHAR (1) && ISCHAR (2))
  {

    char *pcString = hb_parc (2);
    size_t sStrLen = (size_t)hb_parclen (2);
    char *pcOnlySet = hb_parc (1);
    size_t sOnlySetLen = (size_t)hb_parclen (1);
    char *pcRet;
    size_t sRetStrLen = 0;
    int iShift, iBool;
    char *pcSub, *pc;

    if ((iSwitch == DO_CHARONLY_WORDONLY) || (iSwitch == DO_CHARONLY_WORDREM))
    {
      iShift = 2;
    }
    else
    {
      iShift = 1;
    }

    pcRet = ( char * ) hb_xgrab (sStrLen);

    for (pcSub = pcString; pcSub < pcString+sStrLen+1-iShift; pcSub += iShift)
    {
      pc = ct_at_exact_forward (pcOnlySet, sOnlySetLen,
                                pcSub, iShift, NULL);
      iBool = ((pc != NULL) && (((pc-pcOnlySet)%iShift) == 0));
      if ((iBool && ((iSwitch == DO_CHARONLY_CHARONLY) || (iSwitch == DO_CHARONLY_WORDONLY))) ||
          (!iBool && ((iSwitch == DO_CHARONLY_CHARREM) || (iSwitch == DO_CHARONLY_WORDREM))))
      {
        for (pc = pcSub; pc < pcSub+iShift; pc++)
        {
          pcRet[sRetStrLen++] = *pc;
        }
      }
    }

    /* copy last character if string len is odd */
    if ((iShift == 2) && (sStrLen%2==1))
    {
      pcRet[sRetStrLen++] = pcString[sStrLen-1];
    }

    hb_retclen (pcRet, sRetStrLen);
    hb_xfree (pcRet);

  }
  else /* if (ISCHAR (1) && ISCHAR (2)) */
  {
    PHB_ITEM pSubst = NULL;
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      switch (iSwitch)
      {
        case DO_CHARONLY_CHARONLY:
        {
          pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_CHARONLY,
                                   NULL, "CHARONLY", 0, EF_CANSUBSTITUTE, 2,
                                   hb_paramError (1), hb_paramError (2));
        }; break;

        case DO_CHARONLY_WORDONLY:
        {
          pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_WORDONLY,
                                   NULL, "WORDONLY", 0, EF_CANSUBSTITUTE, 2,
                                   hb_paramError (1), hb_paramError (2));
        }; break;

        case DO_CHARONLY_CHARREM:
        {
          pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_CHARREM,
                                   NULL, "CHARREM", 0, EF_CANSUBSTITUTE, 2,
                                   hb_paramError (1), hb_paramError (2));
        }; break;

        case DO_CHARONLY_WORDREM:
        {
          pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_WORDREM,
                                   NULL, "WORDREM", 0, EF_CANSUBSTITUTE, 2,
                                   hb_paramError (1), hb_paramError (2));
        }; break;
      }
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
  }

  return;

}


/*  $DOC$
 *  $FUNCNAME$
 *      CHARONLY()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Intersectional set of two strings based on characters
 *  $SYNTAX$
 *      CHARONLY (<cThisCharactersOnly>, <cString>) -> cReducedString
 *  $ARGUMENTS$
 *      <cThisCharactersOnly>   specifies the characters that must not be
 *                              deleted in <cString>.
 *      <cString>               is the string that should be processed
 *  $RETURNS$
 *      <cReducedString>        A string with all characters deleted but those
 *                              specified in <cThisCharactersOnly>.
 *  $DESCRIPTION$
 *      The CHARONLY() function calculates the intersectional set of two
 *      strings. To do this, it deletes all characters from <cString> that
 *      do not appear in <cThisCharacterOnly>.
 *  $EXAMPLES$
 *      ? CHARONLY("0123456789", "0211 - 38 99 77")  //  "0211389977"
 *      ? CHARONLY("0123456789", "0211/ 389 977")    //  "0211389977"
 *  $TESTS$
 *      CHARONLY("0123456789", "0211 - 38 99 77") == "0211389977"
 *      CHARONLY("0123456789", "0211/ 389 977")   == "0211389977"
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      CHARONLY() is compatible with CT3's CHARONLY().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is charonly.c, library is ct3.
 *  $SEEALSO$
 *      CHARREM()   WORDONLY()   WORDREM()
 *  $END$
 */

HB_FUNC (CHARONLY)
{

  do_charonly (DO_CHARONLY_CHARONLY);
  return;

}


/*  $DOC$
 *  $FUNCNAME$
 *      WORDONLY()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Intersectional set of two strings based on double characters
 *  $SYNTAX$
 *      WORDONLY (<cThisDoubleCharactersOnly>, <cString>) -> cReducedString
 *  $ARGUMENTS$
 *      <cThisDoubleCharactersOnly> specifies the double characters that must
 *                                  not be deleted in <cString>.
 *      <cString>                   is the string that should be processed
 *  $RETURNS$
 *      <cReducedString>        A string with all double characters deleted
 *                              but those specified in <cThisCharactersOnly>.
 *  $DESCRIPTION$
 *      The WORDONLY() function calculates the intersectional set of two
 *      strings based on double characters. To do this, it deletes all double
 *      characters from <cString> that do not appear in <cThisDoubleCharacterOnly>.
 *  $EXAMPLES$
 *      ? WORDONLY("AABBCCDD", "XXAAYYBBZZ")  // "AABB"
 *      ? WORDONLY("AABBCCDD", "XAAYYYBBZZ")  // "BB"
 *  $TESTS$
 *      WORDONLY("AABBCCDD", "XXAAYYBBZZ") == "AABB"
 *      WORDONLY("AABBCCDD", "XAAYYYBBZZ") == "BB"
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      WORDONLY() is compatible with CT3's WORDONLY().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is charonly.c, library is ct3.
 *  $SEEALSO$
 *      CHARONLY()   CHARREM()   WORDREM()
 *  $END$
 */

HB_FUNC (WORDONLY)
{

  do_charonly (DO_CHARONLY_WORDONLY);
  return;

}


/*  $DOC$
 *  $FUNCNAME$
 *      CHARREM()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Removes characters from a string
 *  $SYNTAX$
 *      CHARREM (<cDeleteThisCharacters>, <cString>) -> cReducedString
 *  $ARGUMENTS$
 *      <cDeleteThisCharacters>   specifies the characters that should
 *                                be deleted in <cString>
 *      <cString>)                is the string that should be processed
 *  $RETURNS$
 *      <cReducedString>          is a string where the characters specified
 *                                in <cDeleteThisCharacters> are deleted
 *  $DESCRIPTION$
 *      The CHARREM() function deletes the characters specified in
 *      <cDeleteThisCharacters> from <cString>.
 *  $EXAMPLES$
 *      ? CHARREM(" ", " 1  2  ")   // "12"
 *      ? CHARREM("3y", "xyz123")   // "xz12"
 *  $TESTS$
 *      CHARREM(" ", " 1  2  ") == "12"
 *      CHARREM("3y", "xyz123") == "xz12"
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      CHARREM() is compatible with CT3's CHARREM().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is charonly.c, library is ct3.
 *  $SEEALSO$
 *      CHARONLY()   WORDONLY()   WORDREM()
 *  $END$
 */

HB_FUNC (CHARREM)
{

  do_charonly (DO_CHARONLY_CHARREM);
  return;

}


/*  $DOC$
 *  $FUNCNAME$
 *      WORDREM()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Removes characters from a string
 *  $SYNTAX$
 *      WORDREM (<cDeleteThisDoubleCharacters>, <cString>) -> cReducedString
 *  $ARGUMENTS$
 *      <cDeleteThisDoubleCharacters>   specifies the double characters that
 *                                      should be deleted in <cString>
 *      <cString>)                      is the string that should be processed
 *  $RETURNS$
 *      <cReducedString>          is a string where the double characters
 *                                specified in <cDeleteThisDoubleCharacters>
 *                                are deleted
 *  $DESCRIPTION$
 *      The WORDREM() function deletes the double characters specified in
 *      <cDeleteThisDoubleCharacters> from <cString>.
 *  $EXAMPLES$
 *      ? WORDREM("abcd", "0ab1cd")   // "0ab1"
 *      ? WORDREM("abcd", "ab0cd1")   // "0cd1"
 *  $TESTS$
 *      WORDREM("abcd", "0ab1cd") == "0ab1"
 *      WORDREM("abcd", "ab0cd1") == "0cd1"
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      WORDREM() is a new function available only in Harbour's CT3.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is charonly.c, library is ct3.
 *  $SEEALSO$
 *      CHARONLY   CHARREM()   WORDREM()
 *  $END$
 */

HB_FUNC (WORDREM)
{

  do_charonly (DO_CHARONLY_WORDREM);
  return;

}
