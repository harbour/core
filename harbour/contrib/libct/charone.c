/*
 * $Id$
 */

/*
 * Harbour Project source code: 
 *   CT3 string functions
 *     - CHARONE()
 *     - WORDONE()
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
#define DO_CHARONE_CHARONE     0
#define DO_CHARONE_WORDONE     1

/* helper function for the *one functions */
static void do_charone (int iSwitch)
{

  char *pcString;
  size_t sStrLen;
  char *pcDeleteSet;
  size_t sDeleteSetLen;

  /* param check */
  if (ISCHAR (1))
  {

    if (ISCHAR (2))
    {
      pcString = hb_parc (2);
      sStrLen = (size_t)hb_parclen (2);
      pcDeleteSet = hb_parc (1);
      sDeleteSetLen = (size_t)hb_parclen (1);
    }
    else
    {
      pcString = hb_parc (1);
      sStrLen = (size_t)hb_parclen (1);
      pcDeleteSet = NULL;
      sDeleteSetLen = 0;
    }

    switch (iSwitch)
    {
      case DO_CHARONE_CHARONE:
      {
        if (sStrLen > 1)
        {
        
          char *pcSub;
          char *pcRet;
          size_t sRetStrLen = 0;
          char cCurrent = *pcString;
          int iDoDelete = 1;

          pcRet = ( char * ) hb_xgrab (sStrLen);

          /* copy first char */
          *(pcRet+sRetStrLen) = cCurrent;
          sRetStrLen++;

          for (pcSub = pcString+1; pcSub<pcString+sStrLen; pcSub++)
          {
            if (*pcSub != cCurrent)
            {
              char *pc;
              /* "new" character */
              cCurrent = *pcSub;
              *(pcRet+sRetStrLen) = cCurrent;
              sRetStrLen++;

              /* check if it should be deleted */
              if (pcDeleteSet == NULL)
              {
                iDoDelete = 1;
              }
              else
              {
                pc = ct_at_exact_forward (pcDeleteSet, sDeleteSetLen,
                                          pcSub, 1, NULL);
                if (pc != NULL)
                  iDoDelete = 1;
                else
                  iDoDelete = 0;
              }
            }
            else
            {
              if (!iDoDelete)
              {
                *(pcRet+sRetStrLen) = cCurrent;
                sRetStrLen++;
              }
            }
          }

          hb_retclen (pcRet, sRetStrLen);
          hb_xfree (pcRet);

        }
        else  /* if (sStrLen > 1) */
        {
          /* algorithm does nothing to 1-char-strings */
          hb_retclen (pcString, sStrLen);
        }
      }; break;

      case DO_CHARONE_WORDONE:
      {
        if (sStrLen > 3)
        {
          char *pcSub;
          char *pcRet;
          size_t sRetStrLen = 0;
          char cCurrent1 = *pcString;
          char cCurrent2 = *(pcString+1);
          int iDoDelete = 1;

          pcRet = ( char * ) hb_xgrab (sStrLen);
          /* copy first double char */
          *(pcRet+sRetStrLen) = cCurrent1;
          *(pcRet+sRetStrLen+1) = cCurrent2;
          sRetStrLen += 2;

          for (pcSub = pcString+2; pcSub<(pcString+sStrLen-1); pcSub+=2)
          {
            if (!((*pcSub == cCurrent1) && (*(pcSub+1) == cCurrent2)))
            {
              char *pc;
              /* "new" character */
              cCurrent1 = *pcSub;
              cCurrent2 = *(pcSub+1);
              *(pcRet+sRetStrLen) = cCurrent1;
              *(pcRet+sRetStrLen+1) = cCurrent2;
              sRetStrLen += 2;

              /* check if it should be deleted */
              if (pcDeleteSet == NULL)
              {
                iDoDelete = 1;
              }
              else
              {
                pc = ct_at_exact_forward (pcDeleteSet, sDeleteSetLen,
                                          pcSub, 2, NULL);
                if ((pc != NULL) && (((pc-pcDeleteSet)%2) == 0))
                  iDoDelete = 1;
                else
                  iDoDelete = 0;
              }
            }
            else
            {
              if (!iDoDelete)
              {
                *(pcRet+sRetStrLen) = cCurrent1;
                *(pcRet+sRetStrLen+1) = cCurrent2;
                sRetStrLen += 2;
              }
            }
          }

          /* copy last character if string len is odd */
          if (sStrLen%2==1)
          {
            *(pcRet+sRetStrLen) = *(pcString+sStrLen-1);
            sRetStrLen++;
          }
          hb_retclen (pcRet, sRetStrLen);
          hb_xfree (pcRet);

        }
        else  /* if (sStrLen > 3) */
        {
          /* algorithm does nothing to 3-char-strings */
          hb_retclen (pcString, sStrLen);
        }

      }; break;

    } /* switch (iSwitch) */

  }
  else /* if (ISCHAR (1)) */
  {
    hb_retc ("");
  }

  return;

}


/*  $DOC$
 *  $FUNCNAME$
 *      CHARONE()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Reduce multiple occurences of a character to one
 *  $SYNTAX$
 *      CHARONE ([<cCharactersToReduce>,] <cString>) -> cReducedString
 *  $ARGUMENTS$
 *      [<cCharactersToReduce>]    specifies the characters the multiple
 *                                 occurences of which should be reduced to one
 *                                 Default: All characters.
 *      <cString>                  specifies the processed string
 *  $RETURNS$
 *      <cReducedString>           the string with the reduced occurences
 *  $DESCRIPTION$
 *      The CHARONE() function reduces multiple occurences of characters in
 *      <cString> to a single one. It is important to note that the multiple
 *      occurences must occur directly one behind the other. This behaviour is
 *      is in contrast to the CHARLIST() function. 
 *  $EXAMPLES$
 *      ? CHARONE("122333a123")      // "123a123"
 *      ? CHARONE("A  B  CCCD")      // "A B CD"
 *      ? CHARONE(" ", "A  B  A  B") // "A B A B"
 *      ? CHARONE("o", "122oooB12o") // "122oB12o"
 *  $TESTS$
 *      CHARONE("122333a123")      == "123a123"
 *      CHARONE("A  B  CCCD")      == "A B CD"
 *      CHARONE(" ", "A  B  A  B") == "A B A B"
 *      CHARONE("o", "122oooB12o") == "122oB12o"
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      CHARONE() is compatible with CT3's CHARONE().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is charone.c, library is ct3.
 *  $SEEALSO$
 *      CHARREM()   WORDONE()
 *  $END$
 */

HB_FUNC (CHARONE)
{

  do_charone (DO_CHARONE_CHARONE);
  return;

}


/*  $DOC$
 *  $FUNCNAME$
 *      WORDONE()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Reduce multiple occurences of a double character to one
 *  $SYNTAX$
 *      WORDONE ([<cDoubleCharactersToReduce>,] <cString>) -> cReducedString
 *  $ARGUMENTS$
 *      [<cDoubleCharactersToReduce>]  specifies the double characters the multiple
 *                                     occurences of which should be reduced to one
 *                                     Default: All characters.
 *      <cString>                      specifies the processed string
 *  $RETURNS$
 *      <cReducedString>               the string with the reduced occurences
 *  $DESCRIPTION$
 *      The WORDONE() function reduces multiple occurences of double characters in
 *      <cString> to a single one. It is important to note that the multiple
 *      occurences must occur directly one behind the other.
 *  $EXAMPLES$
 *      ? WORDONE("12ABAB12")       // "12AB12"
 *      ? WORDONE("1AAAA2")         // "1AAAA2"
 *      ? WORDONE("12", "1212ABAB") // "12ABAB"
 *  $TESTS$
 *      WORDONE("12ABAB12")       == "12AB12"
 *      WORDONE("1AAAA2")         == "1AAAA2"
 *      WORDONE("12", "1212ABAB") == "12ABAB"
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      WORDONE() is compatible with CT3's WORDONE().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is charone.c, library is ct3.
 *  $SEEALSO$
 *      CHARONE()   CHARREM()
 *  $END$
 */

HB_FUNC (WORDONE)
{

  do_charone (DO_CHARONE_WORDONE);
  return;

}
