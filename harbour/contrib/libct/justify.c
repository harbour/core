/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   JUSTLEFT() and JUSTRIGHT() CT3 string functions
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
#define DO_JUSTIFY_JUSTLEFT   0
#define DO_JUSTIFY_JUSTRIGHT  1

/* helper function for the justxxx() functions */
static void do_justify (int iSwitch)
{

  int iNoRet;

  iNoRet = ct_getref();

  if (ISCHAR (1))
  {

    char *pcString = hb_parc (1);
    size_t sStrLen = hb_parclen (1);
    char cJustChar;
    char *pc, *pcRet;
    size_t sJustOffset;

    if (hb_parclen (2) > 0)
      cJustChar = *(hb_parc (2));
    else if (ISNUM (2))
      cJustChar = hb_parnl (2) % 256;
    else
      cJustChar = 0x20;

    pcRet = hb_xgrab (sStrLen);

    switch (iSwitch)
    {
      case DO_JUSTIFY_JUSTLEFT:
      {
        pc = pcString;
        sJustOffset = 0;
        while ((*pc == cJustChar) && (pc < pcString+sStrLen))
        {
          sJustOffset++;
          pc++;
        }

        hb_xmemcpy (pcRet, pcString+sJustOffset, sStrLen-sJustOffset);
        for (pc = pcRet+sStrLen-sJustOffset; pc < pcRet+sStrLen; pc++)
        {
          *pc = cJustChar;
        }

      }; break;

      case DO_JUSTIFY_JUSTRIGHT:
      {
        pc = pcString+sStrLen-1;
        sJustOffset = 0;
        while ((*pc == cJustChar) && (pc >= pcString))
        {
          sJustOffset++;
          pc--;
        }
      
        for (pc = pcRet; pc < pcRet+sJustOffset; pc++)
        {
          *pc = cJustChar;
        }
        hb_xmemcpy (pcRet+sJustOffset, pcString, sStrLen-sJustOffset);

      }; break;

    }

    if (ISBYREF (1))
      hb_storclen (pcRet, sStrLen, 1);

    if (iNoRet)
      hb_ret();
    else
      hb_retclen (pcRet, sStrLen);

    hb_xfree (pcRet);

  }
  else /* ISCHAR (1) */
  {
    PHB_ITEM pSubst = NULL;
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG,
                               (iSwitch == DO_JUSTIFY_JUSTLEFT ? CT_ERROR_JUSTLEFT : CT_ERROR_JUSTRIGHT),
                               NULL,
                               (iSwitch == DO_JUSTIFY_JUSTLEFT ? "JUSTLEFT" : "JUSTRIGHT"),
                               0, EF_CANSUBSTITUTE, 2,
                               hb_paramError (1), hb_paramError (2));
    }
    
    if (pSubst != NULL)
    {
      hb_itemReturn (pSubst);
      hb_itemRelease (pSubst);
    }
    else
    {
      if (iNoRet)
        hb_ret();
      else
        hb_retc ("");
    }
  }

  return;

}


/*  $DOC$
 *  $FUNCNAME$
 *      JUSTLEFT()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Move characters from the beginning to the end of a string
 *  $SYNTAX$
 *      JUSTLEFT (<[@]cString>, [<cChar>|<nChar>]) -> cJustifiedString
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      JUSTLEFT() is compatible with CT3's JUSTLEFT().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is justify.c, library is libct.
 *  $SEEALSO$
 *      JUSTRIGHT()
 *  $END$
 */

HB_FUNC (JUSTLEFT)
{

  do_justify (DO_JUSTIFY_JUSTLEFT);
  return;

}


/*  $DOC$
 *  $FUNCNAME$
 *      JUSTRIGHT()
 *  $CATEGORY$
 *      Harbour Tools string functions
 *  $ONELINER$
 *      Move characters from the end to the beginning of a string
 *  $SYNTAX$
 *      JUSTRIGHT (<[@]cString>, [<cChar>|<nChar>]) -> cJustifiedString
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      JUSTRIGHT() is compatible with CT3's JUSTRIGHT().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is justify.c, library is libct.
 *  $SEEALSO$
 *      JUSTLEFT()
 *  $END$
 */

HB_FUNC (JUSTRIGHT)
{

  do_justify (DO_JUSTIFY_JUSTRIGHT);
  return;

}


