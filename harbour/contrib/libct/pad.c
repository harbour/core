/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   PADLEFT() and PADRIGHT() CT3 string functions
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
#define DO_PAD_PADLEFT     0
#define DO_PAD_PADRIGHT    1

/* helper function for the pad functions */
static void do_pad (int iSwitch)
{

  if (ISCHAR (1) && ISNUM (2))
  {

    char *pcString = (char *)hb_parc (1);
    size_t sStrLen = (size_t)hb_parclen (1);
    char *pcRet, *pc;
    long lRetLen;
    size_t sRetLen;
    char cFill;

    lRetLen = hb_parnl (2);
    if (lRetLen <= 0)
    {
      int iArgErrorMode = ct_getargerrormode();
      if (iArgErrorMode != CT_ARGERR_IGNORE)
      {
        ct_error ((USHORT)iArgErrorMode, EG_ARG,
                  (iSwitch == DO_PAD_PADLEFT ? CT_ERROR_PADLEFT : CT_ERROR_PADRIGHT),
                  NULL,
                  (iSwitch == DO_PAD_PADLEFT ? "PADLEFT" : "ROR_PADRIGHT"),
                  0, EF_CANDEFAULT, 3,
                  hb_paramError (1), hb_paramError (2),
                  hb_paramError (3));
      }
      hb_retc ("");
      return;
    }
    sRetLen = (size_t)lRetLen;

    if (hb_parclen (3) > 0)
      cFill = *(hb_parc (3));
    else if (ISNUM (3))
      cFill = hb_parnl (3) % 256;
    else
      cFill = 0x20;

    pcRet = ( char * )hb_xgrab (sRetLen);

    if (iSwitch == DO_PAD_PADLEFT)
    {
      if (sRetLen > sStrLen)
      {
        /* fill with cFill */
        for (pc = pcRet; pc < pcRet+(sRetLen-sStrLen); pc++)
          *pc = cFill;
        hb_xmemcpy (pcRet+(sRetLen-sStrLen), pcString, sStrLen);
      }
      else
      {
        hb_xmemcpy (pcRet, pcString+(sStrLen-sRetLen), sRetLen);
      }
    }
    else
    {
      hb_xmemcpy (pcRet, pcString, (sRetLen < sStrLen ? sRetLen : sStrLen));
      if (sRetLen > sStrLen)
      {
        /* fill with cFill */
        for (pc = pcRet+sStrLen; pc < pcRet+sRetLen; pc++)
          *pc = cFill;
      }
    }

    hb_retclen (pcRet, sRetLen);
    hb_xfree (pcRet);

  }
  else /* ISCHAR (1) && ISNUM (2) */
  {
    PHB_ITEM pSubst = NULL;
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG,
                               (iSwitch == DO_PAD_PADLEFT ? CT_ERROR_PADLEFT : CT_ERROR_PADRIGHT),
                               NULL,
                               (iSwitch == DO_PAD_PADLEFT ? "PADLEFT" : "ROR_PADRIGHT"),
                               0, EF_CANSUBSTITUTE, 3,
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
      hb_retc ("");
    }
    return;
  }

  return;

}


/*  $DOC$
 *  $FUNCNAME$
 *      PADLEFT()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Fills string to a certain length on the left
 *  $SYNTAX$
 *      PADLEFT (<cString>,<nLength>, [<cChar|nChar>]) -> cString
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      PADLEFT() is compatible with CT3's PADLEFT().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is pad.c, library is libct.
 *  $SEEALSO$
 *      PADRIGHT()
 *  $END$
 */

HB_FUNC (PADLEFT)
{

  do_pad (DO_PAD_PADLEFT);
  return;

}


/*  $DOC$
 *  $FUNCNAME$
 *      PADRIGHT()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Fills string to a certain length on the right
 *  $SYNTAX$
 *      PADRIGHT (<cString>,<nLength>, [<cChar|nChar>]) -> cString
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      PADRIGHT() is compatible with CT3's PADRIGHT().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is pad.c, library is libct.
 *  $SEEALSO$
 *      PADLEFT()
 *  $END$
 */

HB_FUNC (PADRIGHT)
{

  do_pad (DO_PAD_PADRIGHT);
  return;

}
