/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   POSALPHA(), POSLOWER(), POSRANGE() and POSUPPER() CT3 string functions
 *
 * POSUPPER() Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
 *
 * POSALPHA(), POSLOWER(), POSRANGE()
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
#include <ctype.h>   


/* defines */
#define DO_POS1_POSALPHA       0
#define DO_POS1_POSLOWER       1
#define DO_POS1_POSRANGE       2
#define DO_POS1_POSUPPER       3

/* helper function for the posxxx() functions */
static void do_pos1 (int iSwitch)
{

  if ((ISCHAR (1))       /* all functions need string as 1st param */
      &&
      ((iSwitch != DO_POS1_POSRANGE)  /* that's the only condition for all funcs _except_ POSRANGE */
       ||
       ((iSwitch == DO_POS1_POSRANGE) /* In addition, POSRANGE needs .. */
        &&
        (ISCHAR (2))     /* .. string as 2nd .. */
        &&
        (ISCHAR (3))     /* .. and 3rd param */
       )
      )
     )
  {

    unsigned char *pcString;
    size_t sStrLen;
    unsigned char *puc, ucChar1, ucChar2;
    int iMode;
    size_t sIgnore;
    int iParamShift = 0;

    if (iSwitch == DO_POS1_POSRANGE)
    {
      
      if (hb_parclen (1) == 0)
      {
        hb_retnl (0);
        return;
      }
      else
      {
        ucChar1 = *(hb_parc (1));
      }

      if (hb_parclen (2) == 0)
      {
        hb_retnl (0);
        return;
      }
      else
      {
        ucChar2 = *(hb_parc (2));
      }
      
      iParamShift += 2;
    }

    pcString = (unsigned char *)hb_parc (iParamShift+1);
    sStrLen = (size_t)hb_parclen (iParamShift+1);

    if (ISLOG (iParamShift+2))
      iMode = hb_parl (iParamShift+2);
    else
      iMode = 0;

    if (ISNUM (iParamShift+3))
      sIgnore = (size_t)hb_parnl (iParamShift+3);
    else
      sIgnore = 0;

    for (puc = pcString+sIgnore; puc < pcString+sStrLen; puc++)
    {
      int iDoRet;
      switch (iSwitch)
      {
        case DO_POS1_POSALPHA:
        {
          iDoRet = isalpha(*puc);
        }; break;

        case DO_POS1_POSLOWER:
        {
          iDoRet = islower(*puc);
        }; break;

        case DO_POS1_POSRANGE:
        {
          iDoRet = ((ucChar1 <= *puc) && (ucChar2 >= *puc));
        }; break;
        
        case DO_POS1_POSUPPER:
        {
          iDoRet = isupper(*puc);
        }; break;
      }
  
      if ((iMode && iDoRet) || (!iMode && !iDoRet))
      {
        hb_retnl (puc-pcString+1);
        return;
      }
    }

    hb_retnl (0);

  }
  else /* ISCHAR (1) etc. */
  {
    PHB_ITEM pSubst = NULL;
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      switch (iSwitch)
      {
        case DO_POS1_POSALPHA:
        {
          pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_POSALPHA,
                                   NULL, "POSALPHA", 0, EF_CANSUBSTITUTE, 3,
                                   hb_paramError (1), hb_paramError (2), hb_paramError (3));
        }; break;

        case DO_POS1_POSLOWER:
        {
          pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_POSLOWER,
                                   NULL, "POSLOWER", 0, EF_CANSUBSTITUTE, 3,
                                   hb_paramError (1), hb_paramError (2), hb_paramError (3));
        }; break;

        case DO_POS1_POSRANGE:
        {
          pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_POSRANGE,
                                   NULL, "POSRANGE", 0, EF_CANSUBSTITUTE, 5,
                                   hb_paramError (1), hb_paramError (2), hb_paramError (3),
                                   hb_paramError (4), hb_paramError (5));
        }; break;
        
        case DO_POS1_POSUPPER:
        {
          pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_POSUPPER,
                                   NULL, "POSUPPER", 0, EF_CANSUBSTITUTE, 3,
                                   hb_paramError (1), hb_paramError (2), hb_paramError (3));
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
      hb_retnl (0);
    }
  }

  return;

}


/*  $DOC$
 *  $FUNCNAME$
 *      POSALPHA()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Left-most position of a letter in a string
 *  $SYNTAX$
 *      POSALPHA (<cString>, [<lMode>], [<nIgnore>]) -> nPosition
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      POSALPHA() is compatible with CT3's POSALPHA().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is pos1.c, library is libct.
 *  $SEEALSO$
 *      POSLOWER(),POSUPPER(),POSRANGE()
 *  $END$
 */

HB_FUNC (POSALPHA)
{

  do_pos1 (DO_POS1_POSALPHA);
  return;

}


/*  $DOC$
 *  $FUNCNAME$
 *      POSLOWER()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Left-most position of a lowercase letter in a string
 *  $SYNTAX$
 *      POSLOWER (<cString>, [<lMode>], [<nIgnore>]) -> nPosition
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      POSLOWER() is compatible with CT3's POSLOWER().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is pos1.c, library is libct.
 *  $SEEALSO$
 *      POSALPHA(),POSUPPER(),POSRANGE()
 *  $END$
 */

HB_FUNC (POSLOWER)
{

  do_pos1 (DO_POS1_POSLOWER);
  return;

}


/*  $DOC$
 *  $FUNCNAME$
 *      POSRANGE()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Left-most position of a character from a set in a string
 *  $SYNTAX$
 *      POSRANGE (<cChar1>, <cChar2>, <cString>, [<lMode>],
 *                [<nIgnore>]) -> nPosition
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      POSRANGE() is compatible with CT3's POSRANGE().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is pos1.c, library is libct.
 *  $SEEALSO$
 *      POSALPHA(),POSLOWER(),POSUPPER()
 *  $END$
 */

HB_FUNC (POSRANGE)
{

  do_pos1 (DO_POS1_POSRANGE);
  return;

}


/*  $DOC$
 *  $FUNCNAME$
 *      POSUPPER()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Left-most position of an uppercase letter in a string
 *  $SYNTAX$
 *      POSUPPER (<cString>, [<lMode>], [<nIgnore>]) -> nPosition
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      POSUPPER() is compatible with CT3's POSUPPER().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is pos1.c, library is libct.
 *  $SEEALSO$
 *      POSALPHA(),POSLOWER(),POSRANGE()
 *  $END$
 */

HB_FUNC (POSUPPER)
{

  do_pos1 (DO_POS1_POSUPPER);
  return;

}
